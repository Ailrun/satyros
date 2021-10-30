{-# LANGUAGE CPP #-}
module Main where

import           Language.Javascript.JSaddle (JSVal)
import           Satyros.UI

main :: IO ()
main = do
#ifndef GHCJS
  pure ()
#else
  s <- makeSatyrosInterface
  setSatyrosInterface s

foreign import javascript unsafe "window.satyrosInterface = $1" setSatyrosInterface :: JSVal -> IO ()
#endif
