{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Monoid
import Data.Default
import qualified Graphics.Vty as V

import qualified Reflex as R
import qualified Reflex.Host.App as RH
import           Brick.MainReflex

import Brick.Types                         ( Widget )
import Brick.Widgets.Core                  ( vBox
                                           , str
                                           )



drawUI :: String -> [Widget ()]
drawUI st = [ui]
 where
  ui = vBox
    [ str $ "External input: \"" <> st <> "\""
    , str "(Press Esc to quit or Space to ask for input)"
    ]


main :: forall t . t ~ R.SpiderTimeline R.Global => IO ()
main = R.runSpiderHost $ RH.hostApp $ do

  brickWrapper $ \eventE finE suspendCB -> do

    RH.performPostBuild_ $ do
      pure $ RH.infoQuit $ pure finE

    resultE   <- suspendCB $ flip R.fmapMaybe eventE $ \case
      Just (V.EvKey (V.KChar ' ') []) -> Just $ do
        putStrLn "Suspended. Please enter something and press enter to resume:"
        getLine
      _                               -> Nothing

    resultDyn <- R.holdDyn "" resultE
    redrawDyn :: R.Dynamic t Int <- R.count eventE

    let shouldHaltE = R.fforMaybe eventE $ (=<<) $ \case
          V.EvKey V.KEsc _ -> Just ()
          _                -> Nothing

    return
      ( shouldHaltE
      , drawUI <$> (resultDyn <* redrawDyn)
      , pure $ neverShowCursor ()
      , pure $ def
      )
