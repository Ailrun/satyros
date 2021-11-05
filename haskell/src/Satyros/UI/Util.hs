module Satyros.UI.Util where

import           Language.Javascript.JSaddle (Function, JSM, JSVal, function)

function0 :: (JSVal -> JSVal -> JSM ()) -> JSM Function
function0 f = function f'
  where
    f' a b [] = f a b
    f' _ _ l  = error $ "Invalid number of arguments: " <> show (length l) <> " != 0"

function1 :: (JSVal -> JSVal -> JSVal -> JSM ()) -> JSM Function
function1 f = function f'
  where
    f' a b [x] = f a b x
    f' _ _ l   = error $ "Invalid number of arguments: " <> show (length l) <> " != 1"

function2 :: (JSVal -> JSVal -> JSVal -> JSVal -> JSM ()) -> JSM Function
function2 f = function f'
  where
    f' a b [x, y] = f a b x y
    f' _ _ l      = error $ "Invalid number of arguments: " <> show (length l) <> " != 2"
