{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Monoid
import Data.Default
import qualified Graphics.Vty as V
import Control.Concurrent.MVar
import Lens.Micro                       ( (<&>) )

import Brick.Types                      ( Widget )
import Brick.Widgets.Core               ( vBox
                                        , str
                                        )
import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
import           Brick.MainBanana



drawUI :: String -> [Widget ()]
drawUI st = [ui]
 where
  ui = vBox
    [ str $ "External input: \"" <> st <> "\""
    , str "(Press Esc to quit or Space to ask for input)"
    ]


main :: IO ()
main = do

  finMVar               <- newEmptyMVar
  startup@(_, startupH) <- Banana.newAddHandler

  network               <-
    Banana.compile $ brickNetwork startup $ \eventE finE suspendCB -> do

      Banana.reactimate $ finE <&> \() -> putMVar finMVar ()

      resultE <- suspendCB $ Banana.filterJust $ eventE <&> \case
        Just (V.EvKey (V.KChar ' ') []) -> Just $ do
          putStrLn
            "Suspended. Please enter something and press enter to resume:"
          getLine
        _                               -> Nothing

      resultB <- Banana.stepper "" resultE

      let nextE = eventE <&> \case
            Just (V.EvKey V.KEsc _) -> halt
            _                       -> redraw

      return (nextE, drawUI <$> resultB, pure $ const Nothing, pure $ def)

  Banana.actuate network
  startupH ()
  takeMVar finMVar
  Banana.pause network
