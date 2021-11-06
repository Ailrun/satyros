{-# LANGUAGE CPP #-}
module Main where

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (JSVal)
import           Satyros.UI
import           GHCJS.Foreign.Callback
#endif

main :: IO ()
main = do
#ifndef __GHCJS__
  pure ()
#else
  (makeSatyrosAPI >>= syncCallback1') >>= setSatyrosAPI

foreign import javascript unsafe "window.makeSatyrosAPI = $1" setSatyrosAPI :: Callback (JSVal -> IO JSVal) -> IO ()
#endif
