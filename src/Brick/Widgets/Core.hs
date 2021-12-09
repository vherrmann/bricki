{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module provides the core widget combinators and rendering
-- routines. Everything this library does is in terms of these basic
-- primitives.
module Brick.Widgets.Core
  ( -- * Basic rendering primitives
    TextWidth(..)
  , emptyWidget
  , raw
  , txt
  , txtWrap
  , txtWrapWith
  , str
  , strWrap
  , strWrapWith
  , fill
  , hyperlink

  -- * Padding
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padLeftRight
  , padTopBottom
  , padAll

  -- * Box layout
  , (<=>)
  , (<+>)
  , hBox
  , vBox

  -- * Limits
  , hLimit
  , hLimitPercent
  , vLimit
  , vLimitPercent
  , setAvailableSize

  -- * Attribute management
  , withDefAttr
  , modifyDefAttr
  , withAttr
  , forceAttr
  , overrideAttr
  , updateAttrMap

  -- * Border style management
  , withBorderStyle
  , joinBorders
  , separateBorders
  , freezeBorders

  -- * Cursor placement
  , showCursor
  , putCursor

  -- * Naming
  , Named(..)

  -- * Translation
  , translateBy

  -- * Cropping
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy
  , cropLeftTo
  , cropRightTo
  , cropTopTo
  , cropBottomTo

  -- * Extent reporting
  , reportExtent
  , clickable

  -- * Scrollable viewports
  , viewport
  , visible
  , visibleRegion
  , unsafeLookupViewport
  , cached

  -- ** Viewport scroll bars
  , withVScrollBars
  , withHScrollBars
  , withClickableHScrollBars
  , withClickableVScrollBars
  , withVScrollBarHandles
  , withHScrollBarHandles
  , withVScrollBarRenderer
  , withHScrollBarRenderer
  , ScrollbarRenderer(..)
  , verticalScrollbarRenderer
  , horizontalScrollbarRenderer
  , scrollbarAttr
  , scrollbarTroughAttr
  , scrollbarHandleAttr
  , verticalScrollbar
  , horizontalScrollbar

  -- ** Adding offsets to cursor positions and visibility requests
  , addResultOffset

  -- ** Cropping results
  , cropToContext
  )
where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Lens.Micro ((^.), (.~), (&), (%~), to, _1, _2, each, to, Lens')
import Lens.Micro.Mtl (use, (%=))
import Control.Monad ((>=>),when)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IMap as I
import qualified Data.Function as DF
import Data.List (sortBy, partition)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Control.DeepSeq

import Text.Wrap (wrapTextToLines, WrapSettings, defaultWrapSettings)

import Brick.Types
import Brick.Types.Internal
import Brick.Widgets.Border.Style
import Brick.Util (clOffset, clamp)
import Brick.AttrMap
import Brick.Widgets.Internal
import qualified Brick.BorderMap as BM

-- | The class of text types that have widths measured in terminal
-- columns. NEVER use 'length' etc. to measure the length of a string if
-- you need to compute how much screen space it will occupy; always use
-- 'textWidth'.
class TextWidth a where
    textWidth :: a -> Int

instance TextWidth T.Text where
    textWidth = V.wcswidth . T.unpack

instance (F.Foldable f) => TextWidth (f Char) where
    textWidth = V.wcswidth . F.toList

-- | The class of types that store interface element names.
class Named a n where
    -- | Get the name of the specified value.
    getName :: a -> n

-- | When rendering the specified widget, use the specified border style
-- for any border rendering.
withBorderStyle :: BorderStyle -> Widget n -> Widget n
withBorderStyle bs p = Widget (hSize p) (vSize p) $
    withReaderT (ctxBorderStyleL .~ bs) (render p)

-- | When rendering the specified widget, create borders that respond
-- dynamically to their neighbors to form seamless connections.
joinBorders :: Widget n -> Widget n
joinBorders p = Widget (hSize p) (vSize p) $
    withReaderT (ctxDynBordersL .~ True) (render p)

-- | When rendering the specified widget, use static borders. This
-- may be marginally faster, but will introduce a small gap between
-- neighboring orthogonal borders.
--
-- This is the default for backwards compatibility.
separateBorders :: Widget n -> Widget n
separateBorders p = Widget (hSize p) (vSize p) $
    withReaderT (ctxDynBordersL .~ False) (render p)

-- | After the specified widget has been rendered, freeze its borders. A
-- frozen border will not be affected by neighbors, nor will it affect
-- neighbors. Compared to 'separateBorders', 'freezeBorders' will not
-- affect whether borders connect internally to a widget (whereas
-- 'separateBorders' prevents them from connecting).
--
-- Frozen borders cannot be thawed.
freezeBorders :: Widget n -> Widget n
freezeBorders p = Widget (hSize p) (vSize p) $ (bordersL .~ BM.empty) <$> render p

-- | The empty widget.
emptyWidget :: Widget n
emptyWidget = raw V.emptyImage

-- | Add an offset to all cursor locations, visbility requests, and
-- extents in the specified rendering result. This function is critical
-- for maintaining correctness in the rendering results as they are
-- processed successively by box layouts and other wrapping combinators,
-- since calls to this function result in converting from widget-local
-- coordinates to (ultimately) terminal-global ones so they can be
-- used by other combinators. You should call this any time you render
-- something and then translate it or otherwise offset it from its
-- original origin.
addResultOffset :: Location -> Result n -> Result n
addResultOffset off = addCursorOffset off .
                      addVisibilityOffset off .
                      addExtentOffset off .
                      addDynBorderOffset off

addVisibilityOffset :: Location -> Result n -> Result n
addVisibilityOffset off r = r & visibilityRequestsL.each.vrPositionL %~ (off <>)

addExtentOffset :: Location -> Result n -> Result n
addExtentOffset off r = r & extentsL.each %~ (\(Extent n l sz) -> Extent n (off <> l) sz)

addDynBorderOffset :: Location -> Result n -> Result n
addDynBorderOffset off r = r & bordersL %~ BM.translate off

-- | Render the specified widget and record its rendering extent using
-- the specified name (see also 'lookupExtent').
reportExtent :: n -> Widget n -> Widget n
reportExtent n p =
    Widget (hSize p) (vSize p) $ do
        result <- render p
        let ext = Extent n (Location (0, 0)) sz
            sz = ( result^.imageL.to V.imageWidth
                 , result^.imageL.to V.imageHeight
                 )
        return $ result & extentsL %~ (ext:)

-- | Request mouse click events on the specified widget.
clickable :: n -> Widget n -> Widget n
clickable n p =
    Widget (hSize p) (vSize p) $ do
        clickableNamesL %= (n:)
        render $ reportExtent n p

addCursorOffset :: Location -> Result n -> Result n
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible l = l^.locationColumnL >= 0 && l^.locationRowL >= 0
    in r & cursorsL %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 100000

-- | Take a substring capable of fitting into the number of specified
-- columns. This function takes character column widths into
-- consideration.
takeColumns :: Int -> String -> String
takeColumns _ "" = ""
takeColumns numCols (c:cs) =
    let w = V.safeWcwidth c
    in if w > numCols
       then []
       else c : takeColumns (numCols - w) cs

-- | Make a widget from a string, but wrap the words in the input's
-- lines at the available width using the default wrapping settings. The
-- input string should not contain escape sequences or carriage returns.
--
-- Unlike 'str', this is greedy horizontally.
strWrap :: String -> Widget n
strWrap = strWrapWith defaultWrapSettings

-- | Make a widget from a string, but wrap the words in the input's
-- lines at the available width using the specified wrapping settings.
-- The input string should not contain escape sequences or carriage
-- returns.
--
-- Unlike 'str', this is greedy horizontally.
strWrapWith :: WrapSettings -> String -> Widget n
strWrapWith settings t = txtWrapWith settings $ T.pack t

safeTextWidth :: T.Text -> Int
safeTextWidth = V.safeWcswidth . T.unpack

-- | Make a widget from text, but wrap the words in the input's lines at
-- the available width using the default wrapping settings. The input
-- text should not contain escape sequences or carriage returns.
--
-- Unlike 'txt', this is greedy horizontally.
txtWrap :: T.Text -> Widget n
txtWrap = txtWrapWith defaultWrapSettings

-- | Make a widget from text, but wrap the words in the input's lines at
-- the available width using the specified wrapping settings. The input
-- text should not contain escape sequences or carriage returns.
--
-- Unlike 'txt', this is greedy horizontally.
txtWrapWith :: WrapSettings -> T.Text -> Widget n
txtWrapWith settings s =
    Widget Greedy Fixed $ do
      c <- getContext
      let theLines = fixEmpty <$> wrapTextToLines settings (c^.availWidthL) s
          fixEmpty l | T.null l = " "
                     | otherwise = l
      case force theLines of
          [] -> return emptyResult
          multiple ->
              let maxLength = maximum $ safeTextWidth <$> multiple
                  padding = V.charFill (c^.attrL) ' ' (c^.availWidthL - maxLength) (length lineImgs)
                  lineImgs = lineImg <$> multiple
                  lineImg lStr = V.text' (c^.attrL)
                                   (lStr <> T.replicate (maxLength - safeTextWidth lStr) " ")
              in return $ emptyResult & imageL .~ (V.horizCat [V.vertCat lineImgs, padding])

-- | Build a widget from a 'String'. Breaks newlines up and space-pads
-- short lines out to the length of the longest line.
--
-- The input string must not contain tab characters. If it does,
-- interface corruption will result since the terminal will likely
-- render it as taking up more than a single column. The caller should
-- replace tabs with the appropriate number of spaces as desired. The
-- input string should not contain escape sequences or carriage returns.
str :: String -> Widget n
str s =
    Widget Fixed Fixed $ do
      c <- getContext
      let theLines = fixEmpty <$> (dropUnused . lines) s
          fixEmpty :: String -> String
          fixEmpty [] = " "
          fixEmpty l = l
          dropUnused l = takeColumns (availWidth c) <$> take (availHeight c) l
      case force theLines of
          [] -> return emptyResult
          [one] -> return $ emptyResult & imageL .~ (V.string (c^.attrL) one)
          multiple ->
              let maxLength = maximum $ V.safeWcswidth <$> multiple
                  lineImgs = lineImg <$> multiple
                  lineImg lStr = V.string (c^.attrL) (lStr ++ replicate (maxLength - V.safeWcswidth lStr) ' ')
              in return $ emptyResult & imageL .~ (V.vertCat lineImgs)

-- | Build a widget from a 'T.Text' value. Behaves the same as 'str'
-- when the input contains multiple lines.
--
-- The input string must not contain tab characters. If it does,
-- interface corruption will result since the terminal will likely
-- render it as taking up more than a single column. The caller should
-- replace tabs with the appropriate number of spaces as desired. The
-- input text should not contain escape sequences or carriage returns.
txt :: T.Text -> Widget n
txt = str . T.unpack

-- | Hyperlink the given widget to the specified URL. Not all terminal
-- emulators support this. In those that don't, this should have no
-- discernible effect.
hyperlink :: T.Text -> Widget n -> Widget n
hyperlink url p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        let attr = attrMapLookup (c^.ctxAttrNameL) (c^.ctxAttrMapL) `V.withURL` url
        withReaderT (ctxAttrMapL %~ setDefaultAttr attr) (render p)

-- | Pad the specified widget on the left. If max padding is used, this
-- grows greedily horizontally; otherwise it defers to the padded
-- widget.
padLeft :: Padding -> Widget n -> Widget n
padLeft padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (hLimit i, hSize p)
    in Widget sz (vSize p) $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availWidthL
              Pad i -> c^.availWidthL - i
        result <- render $ hLimit lim p
        render $ (f $ vLimit (result^.imageL.to V.imageHeight) $ fill ' ') <+>
                 (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the right. If max padding is used,
-- this grows greedily horizontally; otherwise it defers to the padded
-- widget.
padRight :: Padding -> Widget n -> Widget n
padRight padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (hLimit i, hSize p)
    in Widget sz (vSize p) $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availWidthL
              Pad i -> c^.availWidthL - i
        result <- render $ hLimit lim p
        render $ (Widget Fixed Fixed $ return result) <+>
                 (f $ vLimit (result^.imageL.to V.imageHeight) $ fill ' ')

-- | Pad the specified widget on the top. If max padding is used, this
-- grows greedily vertically; otherwise it defers to the padded widget.
padTop :: Padding -> Widget n -> Widget n
padTop padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (vLimit i, vSize p)
    in Widget (hSize p) sz $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availHeightL
              Pad i -> c^.availHeightL - i
        result <- render $ vLimit lim p
        render $ (f $ hLimit (result^.imageL.to V.imageWidth) $ fill ' ') <=>
                 (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the bottom. If max padding is used,
-- this grows greedily vertically; otherwise it defers to the padded
-- widget.
padBottom :: Padding -> Widget n -> Widget n
padBottom padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (vLimit i, vSize p)
    in Widget (hSize p) sz $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availHeightL
              Pad i -> c^.availHeightL - i
        result <- render $ vLimit lim p
        render $ (Widget Fixed Fixed $ return result) <=>
                 (f $ hLimit (result^.imageL.to V.imageWidth) $ fill ' ')

-- | Pad a widget on the left and right. Defers to the padded widget for
-- growth policy.
padLeftRight :: Int -> Widget n -> Widget n
padLeftRight c w = padLeft (Pad c) $ padRight (Pad c) w

-- | Pad a widget on the top and bottom. Defers to the padded widget for
-- growth policy.
padTopBottom :: Int -> Widget n -> Widget n
padTopBottom r w = padTop (Pad r) $ padBottom (Pad r) w

-- | Pad a widget on all sides. Defers to the padded widget for growth
-- policy.
padAll :: Int -> Widget n -> Widget n
padAll v w = padLeftRight v $ padTopBottom v w

-- | Fill all available space with the specified character. Grows both
-- horizontally and vertically.
fill :: Char -> Widget n
fill ch =
    Widget Greedy Greedy $ do
      c <- getContext
      return $ emptyResult & imageL .~ (V.charFill (c^.attrL) ch (c^.availWidthL) (c^.availHeightL))

-- | Vertical box layout: put the specified widgets one above the other
-- in the specified order (uppermost first). Defers growth policies to
-- the growth policies of the contained widgets (if any are greedy, so
-- is the box).
{-# NOINLINE vBox #-}
vBox :: [Widget n] -> Widget n
vBox [] = emptyWidget
vBox [a] = a
vBox pairs = renderBox vBoxRenderer pairs

-- | Horizontal box layout: put the specified widgets next to each other
-- in the specified order (leftmost first). Defers growth policies to
-- the growth policies of the contained widgets (if any are greedy, so
-- is the box).
{-# NOINLINE hBox #-}
hBox :: [Widget n] -> Widget n
hBox [] = emptyWidget
hBox [a] = a
hBox pairs = renderBox hBoxRenderer pairs

-- | The process of rendering widgets in a box layout is exactly the
-- same except for the dimension under consideration (width vs. height),
-- in which case all of the same operations that consider one dimension
-- in the layout algorithm need to be switched to consider the other.
-- Because of this we fill a BoxRenderer with all of the functions
-- needed to consider the "primary" dimension (e.g. vertical if the
-- box layout is vertical) as well as the "secondary" dimension (e.g.
-- horizontal if the box layout is vertical). Doing this permits us to
-- have one implementation for box layout and parameterizing on the
-- orientation of all of the operations.
data BoxRenderer n =
    BoxRenderer { contextPrimary :: Lens' (Context n) Int
                , contextSecondary :: Lens' (Context n) Int
                , imagePrimary :: V.Image -> Int
                , imageSecondary :: V.Image -> Int
                , limitPrimary :: Int -> Widget n -> Widget n
                , limitSecondary :: Int -> Widget n -> Widget n
                , primaryWidgetSize :: Widget n -> Size
                , concatenatePrimary :: [V.Image] -> V.Image
                , concatenateSecondary :: [V.Image] -> V.Image
                , locationFromOffset :: Int -> Location
                , padImageSecondary :: Int -> V.Image -> V.Attr -> V.Image
                , loPrimary :: forall a. Lens' (Edges a) a -- lo: towards smaller coordinates in that dimension
                , hiPrimary :: forall a. Lens' (Edges a) a -- hi: towards larger  coordinates in that dimension
                , loSecondary :: forall a. Lens' (Edges a) a
                , hiSecondary :: forall a. Lens' (Edges a) a
                , locationFromPrimarySecondary :: Int -> Int -> Location
                , splitLoPrimary :: Int -> V.Image -> V.Image
                , splitHiPrimary :: Int -> V.Image -> V.Image
                , splitLoSecondary :: Int -> V.Image -> V.Image
                , splitHiSecondary :: Int -> V.Image -> V.Image
                , lookupPrimary :: Int -> BM.BorderMap DynBorder -> I.IMap DynBorder
                , insertSecondary :: Location -> I.Run DynBorder -> BM.BorderMap DynBorder -> BM.BorderMap DynBorder
                }

vBoxRenderer :: BoxRenderer n
vBoxRenderer =
    BoxRenderer { contextPrimary = availHeightL
                , contextSecondary = availWidthL
                , imagePrimary = V.imageHeight
                , imageSecondary = V.imageWidth
                , limitPrimary = vLimit
                , limitSecondary = hLimit
                , primaryWidgetSize = vSize
                , concatenatePrimary = V.vertCat
                , concatenateSecondary = V.horizCat
                , locationFromOffset = Location . (0 ,)
                , padImageSecondary = \amt img a ->
                    let p = V.charFill a ' ' amt (V.imageHeight img)
                    in V.horizCat [img, p]
                , loPrimary = eTopL
                , hiPrimary = eBottomL
                , loSecondary = eLeftL
                , hiSecondary = eRightL
                , locationFromPrimarySecondary = \r c -> Location (c, r)
                , splitLoPrimary = V.cropBottom
                , splitHiPrimary = \n img -> V.cropTop (V.imageHeight img-n) img
                , splitLoSecondary = V.cropRight
                , splitHiSecondary = \n img -> V.cropLeft (V.imageWidth img-n) img
                , lookupPrimary = BM.lookupRow
                , insertSecondary = BM.insertH
                }

hBoxRenderer :: BoxRenderer n
hBoxRenderer =
    BoxRenderer { contextPrimary = availWidthL
                , contextSecondary = availHeightL
                , imagePrimary = V.imageWidth
                , imageSecondary = V.imageHeight
                , limitPrimary = hLimit
                , limitSecondary = vLimit
                , primaryWidgetSize = hSize
                , concatenatePrimary = V.horizCat
                , concatenateSecondary = V.vertCat
                , locationFromOffset = Location . (, 0)
                , padImageSecondary = \amt img a ->
                    let p = V.charFill a ' ' (V.imageWidth img) amt
                    in V.vertCat [img, p]
                , loPrimary = eLeftL
                , hiPrimary = eRightL
                , loSecondary = eTopL
                , hiSecondary = eBottomL
                , locationFromPrimarySecondary = \c r -> Location (c, r)
                , splitLoPrimary = V.cropRight
                , splitHiPrimary = \n img -> V.cropLeft (V.imageWidth img-n) img
                , splitLoSecondary = V.cropBottom
                , splitHiSecondary = \n img -> V.cropTop (V.imageHeight img-n) img
                , lookupPrimary = BM.lookupCol
                , insertSecondary = BM.insertV
                }

-- | Render a series of widgets in a box layout in the order given.
--
-- The growth policy of a box layout is the most unrestricted of the
-- growth policies of the widgets it contains, so to determine the hSize
-- and vSize of the box we just take the maximum (using the Ord instance
-- for Size) of all of the widgets to be rendered in the box.
--
-- Then the box layout algorithm proceeds as follows. We'll use
-- the vertical case to concretely describe the algorithm, but the
-- horizontal case can be envisioned just by exchanging all
-- "vertical"/"horizontal" and "rows"/"columns", etc., in the
-- description.
--
-- The growth policies of the child widgets determine the order in which
-- they are rendered, i.e., the order in which space in the box is
-- allocated to widgets as the algorithm proceeds. This is because order
-- matters: if we render greedy widgets first, there will be no space
-- left for non-greedy ones.
--
-- So we render all widgets with size 'Fixed' in the vertical dimension
-- first. Each is rendered with as much room as the overall box has, but
-- we assume that they will not be greedy and use it all. If they do,
-- maybe it's because the terminal is small and there just isn't enough
-- room to render everything.
--
-- Then the remaining height is distributed evenly amongst all remaining
-- (greedy) widgets and they are rendered in sub-boxes that are as high
-- as this even slice of rows and as wide as the box is permitted to be.
-- We only do this step at all if rendering the non-greedy widgets left
-- us any space, i.e., if there were any rows left.
--
-- After rendering the non-greedy and then greedy widgets, their images
-- are sorted so that they are stored in the order the original widgets
-- were given. All cursor locations and visibility requests in each
-- sub-widget are translated according to the position of the sub-widget
-- in the box.
--
-- All images are padded to be as wide as the widest sub-widget to
-- prevent attribute over-runs. Without this step the attribute used by
-- a sub-widget may continue on in an undesirable fashion until it hits
-- something with a different attribute. To prevent this and to behave
-- in the least surprising way, we pad the image on the right with
-- whitespace using the context's current attribute.
--
-- Finally, the padded images are concatenated together vertically and
-- returned along with the translated cursor positions and visibility
-- requests.
renderBox :: BoxRenderer n -> [Widget n] -> Widget n
renderBox br ws =
    Widget (maximum $ hSize <$> ws) (maximum $ vSize <$> ws) $ do
      c <- getContext

      let pairsIndexed = zip [(0::Int)..] ws
          (his, lows) = partition (\p -> (primaryWidgetSize br $ snd p) == Fixed)
                        pairsIndexed

      let availPrimary = c^.(contextPrimary br)
          availSecondary = c^.(contextSecondary br)

          renderHis _ prev [] = return $ DL.toList prev
          renderHis remainingPrimary prev ((i, prim):rest) = do
              result <- render $ limitPrimary br remainingPrimary
                               $ limitSecondary br availSecondary
                               $ cropToContext prim
              renderHis (remainingPrimary - (result^.imageL.(to $ imagePrimary br)))
                        (DL.snoc prev (i, result)) rest

      renderedHis <- renderHis availPrimary DL.empty his

      renderedLows <- case lows of
          [] -> return []
          ls -> do
              let remainingPrimary = c^.(contextPrimary br) -
                                     (sum $ (^._2.imageL.(to $ imagePrimary br)) <$> renderedHis)
                  primaryPerLow = remainingPrimary `div` length ls
                  rest = remainingPrimary - (primaryPerLow * length ls)
                  secondaryPerLow = c^.(contextSecondary br)
                  primaries = replicate rest (primaryPerLow + 1) <>
                              replicate (length ls - rest) primaryPerLow

              let renderLow ((i, prim), pri) =
                      (i,) <$> (render $ limitPrimary br pri
                                       $ limitSecondary br secondaryPerLow
                                       $ cropToContext prim)

              if remainingPrimary > 0 then mapM renderLow (zip ls primaries) else return []

      let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
          allResults = snd <$> rendered
          allImages = (^.imageL) <$> allResults
          allPrimaries = imagePrimary br <$> allImages
          allTranslatedResults = (flip map) (zip [0..] allResults) $ \(i, result) ->
              let off = locationFromOffset br offPrimary
                  offPrimary = sum $ take i allPrimaries
              in addResultOffset off result
          -- Determine the secondary dimension value to pad to. In a
          -- vertical box we want all images to be the same width to
          -- avoid attribute over-runs or blank spaces with the wrong
          -- attribute. In a horizontal box we want all images to have
          -- the same height for the same reason.
          maxSecondary = maximum $ imageSecondary br <$> allImages
          padImage img = padImageSecondary br (maxSecondary - imageSecondary br img)
                         img (c^.attrL)
          (imageRewrites, newBorders) = catAllBorders br (borders <$> allTranslatedResults)
          rewrittenImages = zipWith (rewriteImage br) imageRewrites allImages
          paddedImages = padImage <$> rewrittenImages

      cropResultToContext $ Result (concatenatePrimary br paddedImages)
                            (concat $ cursors <$> allTranslatedResults)
                            (concat $ visibilityRequests <$> allTranslatedResults)
                            (concat $ extents <$> allTranslatedResults)
                            newBorders

catDynBorder
    :: Lens' (Edges BorderSegment) BorderSegment
    -> Lens' (Edges BorderSegment) BorderSegment
    -> DynBorder
    -> DynBorder
    -> Maybe DynBorder
catDynBorder towardsA towardsB a b
    -- Currently, we check if the 'BorderStyle's are exactly the same. In the
    -- future, it might be nice to relax this restriction. For example, if a
    -- horizontal border is being rewritten to accommodate a neighboring
    -- vertical border, all we care about is that the two 'bsVertical's line up
    -- sanely. After all, if the horizontal border's 'bsVertical' is the same
    -- as the vertical one's, and the horizontal border's 'BorderStyle' is
    -- self-consistent, then it will look "right" to rewrite according to the
    -- horizontal border's 'BorderStyle'.
    |  dbStyle a == dbStyle b
    && dbAttr  a == dbAttr  b
    && a ^. dbSegmentsL.towardsB.bsAcceptL
    && b ^. dbSegmentsL.towardsA.bsOfferL
    && not (a ^. dbSegmentsL.towardsB.bsDrawL) -- don't bother doing an update if we don't need to
    = Just (a & dbSegmentsL.towardsB.bsDrawL .~ True)
    | otherwise = Nothing

catDynBorders
    :: Lens' (Edges BorderSegment) BorderSegment
    -> Lens' (Edges BorderSegment) BorderSegment
    -> I.IMap DynBorder
    -> I.IMap DynBorder
    -> I.IMap DynBorder
catDynBorders towardsA towardsB am bm = I.mapMaybe id
    $ I.intersectionWith (catDynBorder towardsA towardsB) am bm

-- | Given borders that should be placed next to each other (the first argument
-- on the right or bottom, and the second argument on the left or top), compute
-- new borders and the rewrites that should be done along the edges of the two
-- images to keep the image in sync with the border information.
--
-- The input borders are assumed to be disjoint. This property is not checked.
catBorders
    :: (border ~ BM.BorderMap DynBorder, rewrite ~ I.IMap V.Image)
    => BoxRenderer n -> border -> border -> ((rewrite, rewrite), border)
catBorders br r l = if lCoord + 1 == rCoord
    then ((lRe, rRe), lr')
    else ((I.empty, I.empty), lr)
    where
    lr     = BM.expand (BM.coordinates r) l `BM.unsafeUnion`
             BM.expand (BM.coordinates l) r
    lr'    = id
           . mergeIMap lCoord lIMap'
           . mergeIMap rCoord rIMap'
           $ lr
    lCoord = BM.coordinates l ^. hiPrimary br
    rCoord = BM.coordinates r ^. loPrimary br
    lIMap  = lookupPrimary br lCoord l
    rIMap  = lookupPrimary br rCoord r
    lIMap' = catDynBorders (loPrimary br) (hiPrimary br) lIMap rIMap
    rIMap' = catDynBorders (hiPrimary br) (loPrimary br) rIMap lIMap
    lRe    = renderDynBorder <$> lIMap'
    rRe    = renderDynBorder <$> rIMap'
    mergeIMap p imap bm = F.foldl'
        (\bm' (s,v) -> insertSecondary br (locationFromPrimarySecondary br p s) v bm')
        bm
        (I.unsafeToAscList imap)

-- | Given a direction to concatenate borders in, and the border information
-- itself (which list is assumed to be already shifted so that borders do not
-- overlap and are strictly increasing in the primary direction), produce: a
-- list of rewrites for the lo and hi directions of each border, respectively,
-- and the borders describing the fully concatenated object.
catAllBorders ::
    BoxRenderer n ->
    [BM.BorderMap DynBorder] ->
    ([(I.IMap V.Image, I.IMap V.Image)], BM.BorderMap DynBorder)
catAllBorders _ [] = ([], BM.empty)
catAllBorders br (bm:bms) = (zip ([I.empty]++los) (his++[I.empty]), bm') where
    (rewrites, bm') = runState (traverse (state . catBorders br) bms) bm
    (his, los) = unzip rewrites

rewriteEdge ::
    (Int -> V.Image -> V.Image) ->
    (Int -> V.Image -> V.Image) ->
    ([V.Image] -> V.Image) ->
    I.IMap V.Image -> V.Image -> V.Image
rewriteEdge splitLo splitHi combine = (combine .) . go . offsets 0 . I.unsafeToAscList where

    -- convert absolute positions into relative ones
    offsets _ [] = []
    offsets n ((n', r):nrs) = (n'-n, r) : offsets (n'+I.len r) nrs

    go [] old = [old]
    -- TODO: might be nice to construct this image with fill rather than
    -- replicate+char
    go ((lo, I.Run len new):nrs) old
        =  [splitLo lo old]
        ++ replicate len new
        ++ go nrs (splitHi (lo+len) old)

rewriteImage :: BoxRenderer n -> (I.IMap V.Image, I.IMap V.Image) -> V.Image -> V.Image
rewriteImage br (loRewrite, hiRewrite) old = rewriteHi . rewriteLo $ old where
    size = imagePrimary br old
    go = rewriteEdge (splitLoSecondary br) (splitHiSecondary br) (concatenateSecondary br)
    rewriteLo img
        | I.null loRewrite || size == 0 = img
        | otherwise = concatenatePrimary br
            [ go loRewrite (splitLoPrimary br 1 img)
            , splitHiPrimary br 1 img
            ]
    rewriteHi img
        | I.null hiRewrite || size == 0 = img
        | otherwise = concatenatePrimary br
            [ splitLoPrimary br (size-1) img
            , go hiRewrite (splitHiPrimary br (size-1) img)
            ]

-- | Limit the space available to the specified widget to the specified
-- number of columns. This is important for constraining the horizontal
-- growth of otherwise-greedy widgets. This is non-greedy horizontally
-- and defers to the limited widget vertically.
hLimit :: Int -> Widget n -> Widget n
hLimit w p =
    Widget Fixed (vSize p) $
      withReaderT (availWidthL %~ (min w)) $ render $ cropToContext p

-- | Limit the space available to the specified widget to the specified
-- percentage of available width, as a value between 0 and 100
-- inclusive. Values outside the valid range will be clamped to the
-- range endpoints. This is important for constraining the horizontal
-- growth of otherwise-greedy widgets. This is non-greedy horizontally
-- and defers to the limited widget vertically.
hLimitPercent :: Int -> Widget n -> Widget n
hLimitPercent w' p =
    Widget Fixed (vSize p) $ do
      let w = clamp 0 100 w'
      ctx <- getContext
      let usableWidth = ctx^.availWidthL
          widgetWidth = round (toRational usableWidth * (toRational w / 100))
      withReaderT (availWidthL %~ (min widgetWidth)) $ render $ cropToContext p

-- | Limit the space available to the specified widget to the specified
-- number of rows. This is important for constraining the vertical
-- growth of otherwise-greedy widgets. This is non-greedy vertically and
-- defers to the limited widget horizontally.
vLimit :: Int -> Widget n -> Widget n
vLimit h p =
    Widget (hSize p) Fixed $
      withReaderT (availHeightL %~ (min h)) $ render $ cropToContext p

-- | Limit the space available to the specified widget to the specified
-- percentage of available height, as a value between 0 and 100
-- inclusive. Values outside the valid range will be clamped to the
-- range endpoints. This is important for constraining the vertical
-- growth of otherwise-greedy widgets. This is non-greedy vertically and
-- defers to the limited widget horizontally.
vLimitPercent :: Int -> Widget n -> Widget n
vLimitPercent h' p =
    Widget (hSize p) Fixed $ do
      let h = clamp 0 100 h'
      ctx <- getContext
      let usableHeight = ctx^.availHeightL
          widgetHeight = round (toRational usableHeight * (toRational h / 100))
      withReaderT (availHeightL %~ (min widgetHeight)) $ render $ cropToContext p

-- | Set the rendering context height and width for this widget. This
-- is useful for relaxing the rendering size constraints on e.g. layer
-- widgets where cropping to the screen size is undesirable.
setAvailableSize :: (Int, Int) -> Widget n -> Widget n
setAvailableSize (w, h) p =
    Widget Fixed Fixed $
      withReaderT (\c -> c & availHeightL .~ h & availWidthL .~ w) $
        render $ cropToContext p

-- | When drawing the specified widget, set the current attribute used
-- for drawing to the one with the specified name. Note that the widget
-- may use further calls to 'withAttr' to override this; if you really
-- want to prevent that, use 'forceAttr'. Attributes used this way still
-- get merged hierarchically and still fall back to the attribute map's
-- default attribute. If you want to change the default attribute, use
-- 'withDefAttr'.
withAttr :: AttrName -> Widget n -> Widget n
withAttr an p =
    Widget (hSize p) (vSize p) $
      withReaderT (ctxAttrNameL .~ an) (render p)

-- | Update the attribute map while rendering the specified widget: set
-- its new default attribute to the one that we get by applying specified
-- function to the current default attribute.
modifyDefAttr :: (V.Attr -> V.Attr) -> Widget n -> Widget n
modifyDefAttr f p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (ctxAttrMapL %~ (setDefaultAttr (f $ getDefaultAttr (c^.ctxAttrMapL)))) (render p)

-- | Update the attribute map while rendering the specified widget: set
-- its new default attribute to the one that we get by looking up the
-- specified attribute name in the map.
withDefAttr :: AttrName -> Widget n -> Widget n
withDefAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (ctxAttrMapL %~ (setDefaultAttr (attrMapLookup an (c^.ctxAttrMapL)))) (render p)

-- | When rendering the specified widget, update the attribute map with
-- the specified transformation.
updateAttrMap :: (AttrMap -> AttrMap) -> Widget n -> Widget n
updateAttrMap f p =
    Widget (hSize p) (vSize p) $
        withReaderT (ctxAttrMapL %~ f) (render p)

-- | When rendering the specified widget, force all attribute lookups
-- in the attribute map to use the value currently assigned to the
-- specified attribute name. This means that the attribute lookups will
-- behave as if they all used the name specified here. That further
-- means that the resolved attribute will still inherit from its parent
-- entry in the attribute map as would normally be the case. If you
-- want to have more control over the resulting attribute, consider
-- 'modifyDefAttr'.
forceAttr :: AttrName -> Widget n -> Widget n
forceAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (ctxAttrMapL .~ (forceAttrMap (attrMapLookup an (c^.ctxAttrMapL)))) (render p)

-- | Override the lookup of 'targetName' to return the attribute value
-- associated with 'fromName' when rendering the specified widget.
-- See also 'mapAttrName'.
overrideAttr :: AttrName -> AttrName -> Widget n -> Widget n
overrideAttr targetName fromName =
    updateAttrMap (mapAttrName fromName targetName)

-- | Build a widget directly from a raw Vty image.
raw :: V.Image -> Widget n
raw img = Widget Fixed Fixed $ return $ emptyResult & imageL .~ img

-- | Translate the specified widget by the specified offset amount.
-- Defers to the translated widget for growth policy.
translateBy :: Location -> Widget n -> Widget n
translateBy off p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ addResultOffset off
             $ result & imageL %~ (V.translate (off^.locationColumnL) (off^.locationRowL))

-- | Crop the specified widget on the left by the specified number of
-- columns. Defers to the cropped widget for growth policy.
cropLeftBy :: Int -> Widget n -> Widget n
cropLeftBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.imageL) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
      return $ addResultOffset (Location (-1 * cols, 0))
             $ result & imageL %~ cropped

-- | Crop the specified widget to the specified size from the left.
-- Defers to the cropped widget for growth policy.
cropLeftTo :: Int -> Widget n -> Widget n
cropLeftTo cols p =
    Widget (hSize p) (vSize p) $ do
        result <- render p
        let w = V.imageWidth $ result^.imageL
            amt = w - cols
        if w <= cols
           then return result
           else render $ cropLeftBy amt $ Widget Fixed Fixed $ return result

-- | Crop the specified widget on the right by the specified number of
-- columns. Defers to the cropped widget for growth policy.
cropRightBy :: Int -> Widget n -> Widget n
cropRightBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.imageL) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
      return $ result & imageL %~ cropped

-- | Crop the specified widget to the specified size from the right.
-- Defers to the cropped widget for growth policy.
cropRightTo :: Int -> Widget n -> Widget n
cropRightTo cols p =
    Widget (hSize p) (vSize p) $ do
        result <- render p
        let w = V.imageWidth $ result^.imageL
            amt = w - cols
        if w <= cols
           then return result
           else render $ cropRightBy amt $ Widget Fixed Fixed $ return result

-- | Crop the specified widget on the top by the specified number of
-- rows. Defers to the cropped widget for growth policy.
cropTopBy :: Int -> Widget n -> Widget n
cropTopBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.imageL) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
      return $ addResultOffset (Location (0, -1 * rows))
             $ result & imageL %~ cropped

-- | Crop the specified widget to the specified size from the top.
-- Defers to the cropped widget for growth policy.
cropTopTo :: Int -> Widget n -> Widget n
cropTopTo rows p =
    Widget (hSize p) (vSize p) $ do
        result <- render p
        let h = V.imageHeight $ result^.imageL
            amt = h - rows
        if h <= rows
           then return result
           else render $ cropTopBy amt $ Widget Fixed Fixed $ return result

-- | Crop the specified widget on the bottom by the specified number of
-- rows. Defers to the cropped widget for growth policy.
cropBottomBy :: Int -> Widget n -> Widget n
cropBottomBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.imageL) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
      return $ result & imageL %~ cropped

-- | Crop the specified widget to the specified size from the bottom.
-- Defers to the cropped widget for growth policy.
cropBottomTo :: Int -> Widget n -> Widget n
cropBottomTo rows p =
    Widget (hSize p) (vSize p) $ do
        result <- render p
        let h = V.imageHeight $ result^.imageL
            amt = h - rows
        if h <= rows
           then return result
           else render $ cropBottomBy amt $ Widget Fixed Fixed $ return result

-- | When rendering the specified widget, also register a cursor
-- positioning request using the specified name and location.
showCursor :: n -> Location -> Widget n -> Widget n
showCursor n cloc p = Widget (hSize p) (vSize p) $
    (cursorsL %~ (CursorLocation cloc (Just n) True:)) <$> (render p)

-- | When rendering the specified widget, also register a cursor
-- positioning request using the specified name and location.
-- The cursor will only be positioned but not made visible.
putCursor :: n -> Location -> Widget n -> Widget n
putCursor n cloc p = Widget (hSize p) (vSize p) $
    (cursorsL %~ (CursorLocation cloc (Just n) False:)) <$> (render p)

hRelease :: Widget n -> Maybe (Widget n)
hRelease p =
    case hSize p of
        Fixed -> Just $ Widget Greedy (vSize p) $
                        withReaderT (availWidthL .~ unrestricted) (render p)
        Greedy -> Nothing

vRelease :: Widget n -> Maybe (Widget n)
vRelease p =
    case vSize p of
        Fixed -> Just $ Widget (hSize p) Greedy $
                        withReaderT (availHeightL .~ unrestricted) (render p)
        Greedy -> Nothing

-- | If the specified resource name has an entry in the rendering cache,
-- use the rendered version from the cache. If not, render the specified
-- widget and update the cache with the result.
--
-- To ensure that mouse events are emitted correctly for cached widgets,
-- in addition to the rendered widget, we also cache (the names of) any
-- clickable extents that were rendered and restore that when utilizing
-- the cache.
--
-- See also 'invalidateCacheEntry'.
cached :: (Ord n) => n -> Widget n -> Widget n
cached n w =
    Widget (hSize w) (vSize w) $ do
        result <- cacheLookup n
        case result of
            Just (clickables, prevResult) -> do
                clickableNamesL %= (clickables ++)
                return prevResult
            Nothing  -> do
                wResult <- render w
                clickables <- renderedClickables wResult
                cacheUpdate n (clickables, wResult)
                return wResult
    where
        -- Given the rendered result of a Widget, collect the list of "clickable" names
        -- from the extents that were in the result.
        renderedClickables :: (Ord n) => Result n -> RenderM n [n]
        renderedClickables renderResult = do
            allClickables <- use clickableNamesL
            return [extentName e | e <- renderResult^.extentsL, extentName e `elem` allClickables]


cacheLookup :: (Ord n) => n -> RenderM n (Maybe ([n], Result n))
cacheLookup n = do
    cache <- lift $ gets (^.renderCacheL)
    return $ M.lookup n cache

cacheUpdate :: Ord n => n -> ([n], Result n) -> RenderM n ()
cacheUpdate n r = lift $ modify (renderCacheL %~ M.insert n r)

-- | Enable vertical scroll bars on all viewports in the specified
-- widget and draw them with the specified orientation.
withVScrollBars :: VScrollBarOrientation -> Widget n -> Widget n
withVScrollBars orientation w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxVScrollBarOrientationL .~ Just orientation) (render w)

-- | Enable scroll bar handles on all vertical scroll bars in the
-- specified widget. This will only have an effect if 'withVScrollBars'
-- is also called.
withVScrollBarHandles :: Widget n -> Widget n
withVScrollBarHandles w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxVScrollBarShowHandlesL .~ True) (render w)

-- | Render vertical viewport scroll bars in the specified widget with
-- the specified renderer. This is only needed if you want to override
-- the use of the default renderer, 'verticalScrollbarRenderer'.
withVScrollBarRenderer :: ScrollbarRenderer n -> Widget n -> Widget n
withVScrollBarRenderer r w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxVScrollBarRendererL .~ Just r) (render w)

-- | The default renderer for vertical viewport scroll bars. Override
-- with 'withVScrollBarRenderer'.
verticalScrollbarRenderer :: ScrollbarRenderer n
verticalScrollbarRenderer =
    ScrollbarRenderer { renderScrollbar = fill '█'
                      , renderScrollbarTrough = fill ' '
                      , renderScrollbarHandleBefore = str "^"
                      , renderScrollbarHandleAfter = str "v"
                      }

-- | Enable horizontal scroll bars on all viewports in the specified
-- widget and draw them with the specified orientation.
withHScrollBars :: HScrollBarOrientation -> Widget n -> Widget n
withHScrollBars orientation w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxHScrollBarOrientationL .~ Just orientation) (render w)

-- | Enable mouse click reporting on horizontal scroll bars in the
-- specified widget. This must be used with 'withHScrollBars'. The
-- provided function is used to build a resource name containing the
-- scroll bar element clicked and the viewport name associated with the
-- scroll bar. It is usually a data constructor of the @n@ type.
withClickableHScrollBars :: (ClickableScrollbarElement -> n -> n) -> Widget n -> Widget n
withClickableHScrollBars f w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxHScrollBarClickableConstrL .~ Just f) (render w)

-- | Enable mouse click reporting on vertical scroll bars in the
-- specified widget. This must be used with 'withVScrollBars'. The
-- provided function is used to build a resource name containing the
-- scroll bar element clicked and the viewport name associated with the
-- scroll bar. It is usually a data constructor of the @n@ type.
withClickableVScrollBars :: (ClickableScrollbarElement -> n -> n) -> Widget n -> Widget n
withClickableVScrollBars f w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxVScrollBarClickableConstrL .~ Just f) (render w)

-- | Enable scroll bar handles on all horizontal scroll bars in the
-- specified widget. This will only have an effect if 'withHScrollBars'
-- is also called.
withHScrollBarHandles :: Widget n -> Widget n
withHScrollBarHandles w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxHScrollBarShowHandlesL .~ True) (render w)

-- | Render horizontal viewport scroll bars in the specified widget with
-- the specified renderer. This is only needed if you want to override
-- the use of the default renderer, 'horizontalScrollbarRenderer'.
withHScrollBarRenderer :: ScrollbarRenderer n -> Widget n -> Widget n
withHScrollBarRenderer r w =
    Widget (hSize w) (vSize w) $
        withReaderT (ctxHScrollBarRendererL .~ Just r) (render w)

-- | The default renderer for horizontal viewport scroll bars. Override
-- with 'withHScrollBarRenderer'.
horizontalScrollbarRenderer :: ScrollbarRenderer n
horizontalScrollbarRenderer =
    ScrollbarRenderer { renderScrollbar = fill '█'
                      , renderScrollbarTrough = fill ' '
                      , renderScrollbarHandleBefore = str "<"
                      , renderScrollbarHandleAfter = str ">"
                      }

-- | Render the specified widget in a named viewport with the
-- specified type. This permits widgets to be scrolled without being
-- scrolling-aware. To make the most use of viewports, the specified
-- widget should use the 'visible' combinator to make a "visibility
-- request". This viewport combinator will then translate the resulting
-- rendering to make the requested region visible. In addition, the
-- 'Brick.Main.EventM' monad provides primitives to scroll viewports
-- created by this function if 'visible' is not what you want.
--
-- This function can automatically render vertical and horizontal scroll
-- bars if desired. To enable scroll bars, wrap your call to 'viewport'
-- with a call to 'withVScrollBars' and/or 'withHScrollBars'. If you
-- don't like the appearance of the resulting scroll bars (defaults:
-- 'verticalScrollbarRenderer' and 'horizontalScrollbarRenderer'),
-- you can customize how they are drawn by making your own
-- 'ScrollbarRenderer' and using 'withVScrollBarRenderer' and/or
-- 'withHScrollBarRenderer'. Note that when you enable scrollbars, the
-- content of your viewport will lose one column of available space if
-- vertical scroll bars are enabled and one row of available space if
-- horizontal scroll bars are enabled.
--
-- If a viewport receives more than one visibility request, then the
-- visibility requests are merged with the inner visibility request
-- taking preference. If a viewport receives more than one scrolling
-- request from 'Brick.Main.EventM', all are honored in the order in
-- which they are received.
--
-- Some caution should be advised when using this function. The viewport
-- renders its contents anew each time the viewport is drawn; in many
-- cases this is prohibitively expensive, and viewports should not be
-- used to display large contents for scrolling. This function is best
-- used when the contents are not too large OR when the contents are
-- large and render-cacheable.
--
-- Also, be aware that there is a rich API for accessing viewport
-- information from within the 'EventM' monad; check the docs for
-- @Brick.Main@ to learn more about ways to get information about
-- viewports after they're drawn.
viewport :: (Ord n, Show n)
         => n
         -- ^ The name of the viewport (must be unique and stable for
         -- reliable behavior)
         -> ViewportType
         -- ^ The type of viewport (indicates the permitted scrolling
         -- direction)
         -> Widget n
         -- ^ The widget to be rendered in the scrollable viewport
         -> Widget n
viewport vpname typ p =
    clickable vpname $ Widget Greedy Greedy $ do
      -- Obtain the scroll bar configuration.
      c <- getContext
      let vsOrientation = ctxVScrollBarOrientation c
          hsOrientation = ctxHScrollBarOrientation c
          vsRenderer = fromMaybe verticalScrollbarRenderer (ctxVScrollBarRenderer c)
          hsRenderer = fromMaybe horizontalScrollbarRenderer (ctxHScrollBarRenderer c)
          showVHandles = ctxVScrollBarShowHandles c
          showHHandles = ctxHScrollBarShowHandles c
          vsbClickableConstr = ctxVScrollBarClickableConstr c
          hsbClickableConstr = ctxHScrollBarClickableConstr c

      -- Observe the viewport name so we can detect multiple uses of the
      -- name.
      let observeName :: (Ord n, Show n) => n -> RenderM n ()
          observeName n = do
              observed <- use observedNamesL
              case S.member n observed of
                  False -> observedNamesL %= S.insert n
                  True ->
                      error $ "Error: while rendering the interface, the name " <> show n <>
                              " was seen more than once. You should ensure that all of the widgets " <>
                              "in each interface have unique name values. This means either " <>
                              "using a different name type or adding constructors to your " <>
                              "existing one and using those to name your widgets.  For more " <>
                              "information, see the \"Resource Names\" section of the Brick User Guide."

      observeName vpname

      -- Update the viewport size.
      let newVp = VP 0 0 newSize (0, 0)
          newSize = (c^.availWidthL - vSBWidth, c^.availHeightL - hSBHeight)
          vSBWidth = maybe 0 (const 1) vsOrientation
          hSBHeight = maybe 0 (const 1) hsOrientation
          doInsert (Just vp) = Just $ vp & vpSize .~ newSize
          doInsert Nothing = Just newVp

      lift $ modify (viewportMapL %~ (M.alter doInsert vpname))

      -- Then render the viewport content widget with the rendering
      -- layout constraint released (but raise an exception if we are
      -- asked to render an infinitely-sized widget in the viewport's
      -- scrolling dimension)
      let release = case typ of
            Vertical -> vRelease
            Horizontal -> hRelease
            Both -> vRelease >=> hRelease
          released = case release p of
            Just w -> w
            Nothing -> case typ of
                Vertical -> error $ "tried to embed an infinite-height " <>
                                    "widget in vertical viewport " <> (show vpname)
                Horizontal -> error $ "tried to embed an infinite-width " <>
                                      "widget in horizontal viewport " <> (show vpname)
                Both -> error $ "tried to embed an infinite-width or " <>
                                "infinite-height widget in 'Both' type " <>
                                "viewport " <> (show vpname)

      initialResult <- render released

      -- If the rendering state includes any scrolling requests for this
      -- viewport, apply those
      reqs <- lift $ gets $ (^.rsScrollRequestsL)
      let relevantRequests = snd <$> filter (\(n, _) -> n == vpname) reqs
      when (not $ null relevantRequests) $ do
          mVp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
          case mVp of
              Nothing -> error $ "BUG: viewport: viewport name " <> show vpname <> " absent from viewport map"
              Just vp -> do
                  let updatedVp = applyRequests relevantRequests vp
                      applyRequests [] v = v
                      applyRequests (rq:rqs) v =
                          case typ of
                              Horizontal -> scrollTo typ rq (initialResult^.imageL) $ applyRequests rqs v
                              Vertical -> scrollTo typ rq (initialResult^.imageL) $ applyRequests rqs v
                              Both -> scrollTo Horizontal rq (initialResult^.imageL) $
                                      scrollTo Vertical rq (initialResult^.imageL) $
                                      applyRequests rqs v
                  lift $ modify (viewportMapL %~ (M.insert vpname updatedVp))

      -- If the sub-rendering requested visibility, update the scroll
      -- state accordingly
      when (not $ null $ initialResult^.visibilityRequestsL) $ do
          mVp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
          case mVp of
              Nothing -> error $ "BUG: viewport: viewport name " <> show vpname <> " absent from viewport map"
              Just vp -> do
                  let rqs = initialResult^.visibilityRequestsL
                      updateVp vp' rq = case typ of
                          Both -> scrollToView Horizontal rq $ scrollToView Vertical rq vp'
                          Horizontal -> scrollToView typ rq vp'
                          Vertical -> scrollToView typ rq vp'
                  lift $ modify (viewportMapL %~ (M.insert vpname $ foldl updateVp vp rqs))

      -- If the size of the rendering changes enough to make the
      -- viewport offsets invalid, reset them
      mVp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
      vp <- case mVp of
          Nothing -> error $ "BUG: viewport: viewport name " <> show vpname <> " absent from viewport map"
          Just v -> return v

      let img = initialResult^.imageL
          fixTop v = if V.imageHeight img < v^.vpSize._2
                   then v & vpTop .~ 0
                   else v
          fixLeft v = if V.imageWidth img < v^.vpSize._1
                   then v & vpLeft .~ 0
                   else v
          updateContentSize v = v & vpContentSize .~ (V.imageWidth img, V.imageHeight img)
          updateVp = updateContentSize . case typ of
              Both -> fixLeft . fixTop
              Horizontal -> fixLeft
              Vertical -> fixTop
      lift $ modify (viewportMapL %~ (M.insert vpname (updateVp vp)))

      -- Get the viewport state now that it has been updated.
      mVpFinal <- lift $ gets (M.lookup vpname . (^.viewportMapL))
      vpFinal <- case mVpFinal of
          Nothing -> error $ "BUG: viewport: viewport name " <> show vpname <> " absent from viewport map"
          Just v -> return v

      -- Then perform a translation of the sub-rendering to fit into the
      -- viewport
      translated <- render $ translateBy (Location (-1 * vpFinal^.vpLeft, -1 * vpFinal^.vpTop))
                           $ Widget Fixed Fixed $ return initialResult

      -- If the vertical scroll bar is enabled, render the scroll bar
      -- area.
      let addVScrollbar = case vsOrientation of
              Nothing -> id
              Just orientation ->
                  let sb = verticalScrollbar vsRenderer vpname
                                                        vsbClickableConstr
                                                        showVHandles
                                                        (vpFinal^.vpSize._2)
                                                        (vpFinal^.vpTop)
                                                        (vpFinal^.vpContentSize._2)
                      combine = case orientation of
                          OnLeft -> (<+>)
                          OnRight -> flip (<+>)
                  in combine sb
          addHScrollbar = case hsOrientation of
              Nothing -> id
              Just orientation ->
                  let sb = horizontalScrollbar hsRenderer vpname
                                                          hsbClickableConstr
                                                          showHHandles
                                                          (vpFinal^.vpSize._1)
                                                          (vpFinal^.vpLeft)
                                                          (vpFinal^.vpContentSize._1)
                      combine = case orientation of
                          OnTop -> (<=>)
                          OnBottom -> flip (<=>)
                  in combine sb

      -- Return the translated result with the visibility requests
      -- discarded
      let translatedSize = ( translated^.imageL.to V.imageWidth
                           , translated^.imageL.to V.imageHeight
                           )
      case translatedSize of
          (0, 0) -> do
              let spaceFill = V.charFill (c^.attrL) ' ' (c^.availWidthL) (c^.availHeightL)
              return $ translated & imageL .~ spaceFill
                                  & visibilityRequestsL .~ mempty
                                  & extentsL .~ mempty
          _ -> render $ addVScrollbar
                      $ addHScrollbar
                      $ cropToContext
                      $ vLimit (vpFinal^.vpSize._2)
                      $ hLimit (vpFinal^.vpSize._1)
                      $ padBottom Max
                      $ padRight Max
                      $ Widget Fixed Fixed
                      $ return $ translated & visibilityRequestsL .~ mempty

-- | The base attribute for scroll bars.
scrollbarAttr :: AttrName
scrollbarAttr = "scrollbar"

-- | The attribute for scroll bar troughs. This attribute is a
-- specialization of @scrollbarAttr@.
scrollbarTroughAttr :: AttrName
scrollbarTroughAttr = scrollbarAttr <> "trough"

-- | The attribute for scroll bar handles. This attribute is a
-- specialization of @scrollbarAttr@.
scrollbarHandleAttr :: AttrName
scrollbarHandleAttr = scrollbarAttr <> "handle"

maybeClick :: n
           -> Maybe (ClickableScrollbarElement -> n -> n)
           -> ClickableScrollbarElement
           -> Widget n
           -> Widget n
maybeClick _ Nothing _ w = w
maybeClick n (Just f) el w = clickable (f el n) w

-- | Build a vertical scroll bar using the specified render and
-- settings.
--
-- You probably don't want to use this directly; instead,
-- use @viewport@, @withVScrollBars@, and, if needed,
-- @withVScrollBarRenderer@. This is exposed so that if you want to
-- render a scroll bar of your own, you can do so outside the @viewport@
-- context.
verticalScrollbar :: ScrollbarRenderer n
                  -- ^ The renderer to use.
                  -> n
                  -- ^ The viewport name associated with this scroll
                  -- bar.
                  -> Maybe (ClickableScrollbarElement -> n -> n)
                  -- ^ Constructor for clickable scroll bar element names.
                  -> Bool
                  -- ^ Whether to display handles.
                  -> Int
                  -- ^ The total viewport height in effect.
                  -> Int
                  -- ^ The viewport vertical scrolling offset in effect.
                  -> Int
                  -- ^ The total viewport content height.
                  -> Widget n
verticalScrollbar vsRenderer n constr False vpHeight vOffset contentHeight =
    verticalScrollbar' vsRenderer n constr vpHeight vOffset contentHeight
verticalScrollbar vsRenderer n constr True vpHeight vOffset contentHeight =
    vBox [ maybeClick n constr SBHandleBefore $
           hLimit 1 $ withDefAttr scrollbarHandleAttr $ renderScrollbarHandleBefore vsRenderer
         , verticalScrollbar' vsRenderer n constr vpHeight vOffset contentHeight
         , maybeClick n constr SBHandleAfter $
           hLimit 1 $ withDefAttr scrollbarHandleAttr $ renderScrollbarHandleAfter vsRenderer
         ]

verticalScrollbar' :: ScrollbarRenderer n
                   -- ^ The renderer to use.
                   -> n
                   -- ^ The viewport name associated with this scroll
                   -- bar.
                   -> Maybe (ClickableScrollbarElement -> n -> n)
                   -- ^ Constructor for clickable scroll bar element names.
                   -> Int
                   -- ^ The total viewport height in effect.
                   -> Int
                   -- ^ The viewport vertical scrolling offset in effect.
                   -> Int
                   -- ^ The total viewport content height.
                   -> Widget n
verticalScrollbar' vsRenderer _ _ vpHeight _ 0 =
    hLimit 1 $ vLimit vpHeight $ renderScrollbarTrough vsRenderer
verticalScrollbar' vsRenderer n constr vpHeight vOffset contentHeight =
    Widget Fixed Greedy $ do
        c <- getContext

        -- Get the proportion of the total content that is visible
        let visibleContentPercent :: Double
            visibleContentPercent = fromIntegral vpHeight /
                                    fromIntegral contentHeight

            ctxHeight = c^.availHeightL

            -- Then get the proportion of the scroll bar that
            -- should be filled in
            sbSize = min ctxHeight $
                     max 1 $
                     round $ visibleContentPercent * (fromIntegral ctxHeight)

            -- Then get the vertical offset of the scroll bar
            -- itself
            sbOffset = if vOffset == 0
                       then 0
                       else if vOffset == contentHeight - vpHeight
                            then ctxHeight - sbSize
                            else min (ctxHeight - sbSize - 1) $
                                 max 1 $
                                 round $ fromIntegral ctxHeight *
                                         (fromIntegral vOffset /
                                          fromIntegral contentHeight::Double)

            sbAbove = maybeClick n constr SBTroughBefore $
                      withDefAttr scrollbarTroughAttr $ vLimit sbOffset $
                      renderScrollbarTrough vsRenderer
            sbBelow = maybeClick n constr SBTroughAfter $
                      withDefAttr scrollbarTroughAttr $ vLimit (ctxHeight - (sbOffset + sbSize)) $
                      renderScrollbarTrough vsRenderer
            sbMiddle = maybeClick n constr SBBar $
                       withDefAttr scrollbarAttr $ vLimit sbSize $ renderScrollbar vsRenderer

            sb = hLimit 1 $
                 if sbSize == ctxHeight
                 then vLimit sbSize $
                      renderScrollbarTrough vsRenderer
                 else vBox [sbAbove, sbMiddle, sbBelow]

        render sb

-- | Build a horizontal scroll bar using the specified render and
-- settings.
--
-- You probably don't want to use this directly; instead, use
-- @viewport@, @withHScrollBars@, and, if needed,
-- @withHScrollBarRenderer@. This is exposed so that if you want to
-- render a scroll bar of your own, you can do so outside the @viewport@
-- context.
horizontalScrollbar :: ScrollbarRenderer n
                    -- ^ The renderer to use.
                    -> n
                    -- ^ The viewport name associated with this scroll
                    -- bar.
                    -> Maybe (ClickableScrollbarElement -> n -> n)
                    -- ^ Constructor for clickable scroll bar element
                    -- names.
                    -> Bool
                    -- ^ Whether to show handles.
                    -> Int
                    -- ^ The total viewport width in effect.
                    -> Int
                    -- ^ The viewport horizontal scrolling offset in effect.
                    -> Int
                    -- ^ The total viewport content width.
                    -> Widget n
horizontalScrollbar hsRenderer n constr False vpWidth hOffset contentWidth =
    horizontalScrollbar' hsRenderer n constr vpWidth hOffset contentWidth
horizontalScrollbar hsRenderer n constr True vpWidth hOffset contentWidth =
    hBox [ maybeClick n constr SBHandleBefore $
           vLimit 1 $ withDefAttr scrollbarHandleAttr $ renderScrollbarHandleBefore hsRenderer
         , horizontalScrollbar' hsRenderer n constr vpWidth hOffset contentWidth
         , maybeClick n constr SBHandleAfter $
           vLimit 1 $ withDefAttr scrollbarHandleAttr $ renderScrollbarHandleAfter hsRenderer
         ]

horizontalScrollbar' :: ScrollbarRenderer n
                     -- ^ The renderer to use.
                     -> n
                     -- ^ The viewport name associated with this scroll
                     -- bar.
                     -> Maybe (ClickableScrollbarElement -> n -> n)
                     -- ^ Constructor for clickable scroll bar element
                     -- names.
                     -> Int
                     -- ^ The total viewport width in effect.
                     -> Int
                     -- ^ The viewport horizontal scrolling offset in effect.
                     -> Int
                     -- ^ The total viewport content width.
                     -> Widget n
horizontalScrollbar' hsRenderer _ _ vpWidth _ 0 =
    vLimit 1 $ hLimit vpWidth $ renderScrollbarTrough hsRenderer
horizontalScrollbar' hsRenderer n constr vpWidth hOffset contentWidth =
    Widget Greedy Fixed $ do
        c <- getContext

        -- Get the proportion of the total content that is visible
        let visibleContentPercent :: Double
            visibleContentPercent = fromIntegral vpWidth /
                                    fromIntegral contentWidth

            ctxWidth = c^.availWidthL

            -- Then get the proportion of the scroll bar that
            -- should be filled in
            sbSize = min ctxWidth $
                     max 1 $
                     round $ visibleContentPercent * (fromIntegral ctxWidth)

            -- Then get the horizontal offset of the scroll bar itself
            sbOffset = if hOffset == 0
                       then 0
                       else if hOffset == contentWidth - vpWidth
                            then ctxWidth - sbSize
                            else min (ctxWidth - sbSize - 1) $
                                 max 1 $
                                 round $ fromIntegral ctxWidth *
                                         (fromIntegral hOffset /
                                          fromIntegral contentWidth::Double)

            sbLeft = maybeClick n constr SBTroughBefore $
                     withDefAttr scrollbarTroughAttr $ hLimit sbOffset $
                     renderScrollbarTrough hsRenderer
            sbRight = maybeClick n constr SBTroughAfter $
                      withDefAttr scrollbarTroughAttr $ hLimit (ctxWidth - (sbOffset + sbSize)) $
                      renderScrollbarTrough hsRenderer
            sbMiddle = maybeClick n constr SBBar $
                       withDefAttr scrollbarAttr $ hLimit sbSize $ renderScrollbar hsRenderer

            sb = vLimit 1 $
                 if sbSize == ctxWidth
                 then hLimit sbSize $
                      renderScrollbarTrough hsRenderer
                 else hBox [sbLeft, sbMiddle, sbRight]

        render sb

-- | Given a name, obtain the viewport for that name by consulting the
-- viewport map in the rendering monad. NOTE! Some care must be taken
-- when calling this function, since it only returns useful values
-- after the viewport in question has been rendered. If you call this
-- function during rendering before a viewport has been rendered, you
-- may get nothing or you may get a stale version of the viewport. This
-- is because viewports are updated during rendering and the one you are
-- interested in may not have been rendered yet. So if you want to use
-- this, be sure you know what you are doing.
unsafeLookupViewport :: (Ord n) => n -> RenderM n (Maybe Viewport)
unsafeLookupViewport name = lift $ gets (M.lookup name . (^.viewportMapL))

scrollTo :: ViewportType -> ScrollRequest -> V.Image -> Viewport -> Viewport
scrollTo Both _ _ _ = error "BUG: called scrollTo on viewport type 'Both'"
scrollTo Vertical req img vp = vp & vpTop .~ newVStart
    where
        newVStart = clamp 0 (V.imageHeight img - vp^.vpSize._2) adjustedAmt
        adjustedAmt = case req of
            VScrollBy amt -> vp^.vpTop + amt
            VScrollPage Up -> vp^.vpTop - vp^.vpSize._2
            VScrollPage Down -> vp^.vpTop + vp^.vpSize._2
            VScrollToBeginning -> 0
            VScrollToEnd -> V.imageHeight img - vp^.vpSize._2
            SetTop i -> i
            _ -> vp^.vpTop
scrollTo Horizontal req img vp = vp & vpLeft .~ newHStart
    where
        newHStart = clamp 0 (V.imageWidth img - vp^.vpSize._1) adjustedAmt
        adjustedAmt = case req of
            HScrollBy amt -> vp^.vpLeft + amt
            HScrollPage Up -> vp^.vpLeft - vp^.vpSize._1
            HScrollPage Down -> vp^.vpLeft + vp^.vpSize._1
            HScrollToBeginning -> 0
            HScrollToEnd -> V.imageWidth img - vp^.vpSize._1
            SetLeft i -> i
            _ -> vp^.vpLeft

scrollToView :: ViewportType -> VisibilityRequest -> Viewport -> Viewport
scrollToView Both _ _ = error "BUG: called scrollToView on 'Both' type viewport"
scrollToView Vertical rq vp = vp & vpTop .~ newVStart
    where
        curStart = vp^.vpTop
        curEnd = curStart + vp^.vpSize._2
        reqStart = rq^.vrPositionL.locationRowL

        reqEnd = rq^.vrPositionL.locationRowL + rq^.vrSizeL._2
        newVStart :: Int
        newVStart = if reqStart < vStartEndVisible
                   then reqStart
                   else vStartEndVisible
        vStartEndVisible = if reqEnd < curEnd
                           then curStart
                           else curStart + (reqEnd - curEnd)
scrollToView Horizontal rq vp = vp & vpLeft .~ newHStart
    where
        curStart = vp^.vpLeft
        curEnd = curStart + vp^.vpSize._1
        reqStart = rq^.vrPositionL.locationColumnL

        reqEnd = rq^.vrPositionL.locationColumnL + rq^.vrSizeL._1
        newHStart :: Int
        newHStart = if reqStart < hStartEndVisible
                   then reqStart
                   else hStartEndVisible
        hStartEndVisible = if reqEnd < curEnd
                           then curStart
                           else curStart + (reqEnd - curEnd)

-- | Request that the specified widget be made visible when it is
-- rendered inside a viewport. This permits widgets (whose sizes and
-- positions cannot be known due to being embedded in arbitrary layouts)
-- to make a request for a parent viewport to locate them and scroll
-- enough to put them in view. This, together with 'viewport', is what
-- makes the text editor and list widgets possible without making them
-- deal with the details of scrolling state management.
--
-- This does nothing if not rendered in a viewport.
visible :: Widget n -> Widget n
visible p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let imageSize = ( result^.imageL.to V.imageWidth
                      , result^.imageL.to V.imageHeight
                      )
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if imageSize^._1 > 0 && imageSize^._2 > 0
               then result & visibilityRequestsL %~ (VR (Location (0, 0)) imageSize :)
               else result

-- | Similar to 'visible', request that a region (with the specified
-- 'Location' as its origin and 'V.DisplayRegion' as its size) be made
-- visible when it is rendered inside a viewport. The 'Location' is
-- relative to the specified widget's upper-left corner of (0, 0).
--
-- This does nothing if not rendered in a viewport.
visibleRegion :: Location -> V.DisplayRegion -> Widget n -> Widget n
visibleRegion vrloc sz p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if sz^._1 > 0 && sz^._2 > 0
               then result & visibilityRequestsL %~ (VR vrloc sz :)
               else result

-- | Horizontal box layout: put the specified widgets next to each other
-- in the specified order. Defers growth policies to the growth policies
-- of both widgets.  This operator is a binary version of 'hBox'.
{-# NOINLINE (<+>) #-}
(<+>) :: Widget n
      -- ^ Left
      -> Widget n
      -- ^ Right
      -> Widget n
(<+>) a b = hBox [a, b]

-- | Vertical box layout: put the specified widgets one above the other
-- in the specified order. Defers growth policies to the growth policies
-- of both widgets.  This operator is a binary version of 'vBox'.
{-# NOINLINE (<=>) #-}
(<=>) :: Widget n
      -- ^ Top
      -> Widget n
      -- ^ Bottom
      -> Widget n
(<=>) a b = vBox [a, b]

{-# RULES
"baseHbox" forall a b   . a <+> b                 = hBox [a, b]
"hBox2"    forall as bs . hBox [hBox as, hBox bs] = hBox (as ++ bs)
"hboxL"    forall as b  . hBox [hBox as, b]       = hBox (as ++ [b])
"hboxR"    forall a bs  . hBox [a, hBox bs]       = hBox (a : bs)
"baseVbox" forall a b   . a <=> b                 = vBox [a, b]
"vBox2"    forall as bs . vBox [vBox as, vBox bs] = vBox (as ++ bs)
"vboxL"    forall as b  . vBox [vBox as, b]       = vBox (as ++ [b])
"vboxR"    forall a bs  . vBox [a, vBox bs]       = vBox (a : bs)
  #-}
