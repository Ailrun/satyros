{-# LANGUAGE CPP #-}
module Main where

import           Language.Javascript.JSaddle (JSVal)
import           Satyros.UI
#ifdef __GHCJS__
import           GHCJS.Foreign.Callback
#endif

main :: IO ()
main = do
#ifndef __GHCJS__
  pure ()
#else
  s <- makeSatyrosInterface
  syncCallback1' s >>= setSatyrosInterface

foreign import javascript unsafe "window.satyrosInterface = $1" setSatyrosInterface :: Callback (JSVal -> IO JSVal) -> IO ()
#endif
