{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main where



import qualified Reflex as R
import qualified Reflex.Host.App as RH

import Lens.Micro ((<&>))

import Data.Default

import Graphics.Vty.Input.Events
import Brick.Widgets.Core
import Brick.MainReflex
import Brick.Types



main :: forall t . t ~ R.SpiderTimeline R.Global => IO ()
main = do
  R.runSpiderHost $ RH.hostApp $ do

    brickWrapper $ \eventE finE _suspendSetupF -> do

      RH.performPostBuild_ $ do
        pure $ RH.infoQuit $ pure finE

      curPromptStr <- R.foldDyn id "" $ eventE <&> \case
        Just (EvKey key _mods) -> case key of
          KEsc    -> const ""
          KEnter  -> const ""
          KChar c -> (++[c])
          KBS     -> init
          _       -> id
        _                      -> id

      let promptWidget :: R.Dynamic t (Widget String) =
            curPromptStr <&> \s -> str $ if null s then " " else s
      let lengthWidget :: R.Dynamic t (Widget String) =
            curPromptStr <&> str . show . length

      let shouldHaltE = R.fforMaybe eventE $ (=<<) $ \case
            EvKey KEsc _ -> Just ()
            _            -> Nothing

      let widgetsDyn =
            (\wid1 wid2 -> [wid1 <=> wid2 <=> emptyWidget])
              <$> promptWidget
              <*> lengthWidget

      let cursorDyn = pure $ const Nothing
      let attrDyn   = pure $ def

      return $ (shouldHaltE, widgetsDyn, cursorDyn, attrDyn)
