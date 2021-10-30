{-# LANGUAGE OverloadedStrings #-}
module Satyros.UI where

import           Control.Lens                ((^.))
import           Language.Javascript.JSaddle (JSM, JSString, JSVal,
                                              ToJSVal (toJSVal), create,
                                              function, global, js1, setProp, js)

makeSatyrosInterface :: JSM JSVal
makeSatyrosInterface = do
  i <- create
  v <- function (\_ _ [x] -> global ^. js ("console" :: JSString) . js1 ("log" :: JSString) x >> pure ()) >>= toJSVal
  setProp "mycode" v i
  toJSVal i
