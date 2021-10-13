module Satyros.Util where

import           Unsafe.Coerce        (unsafeCoerce)

intToWord :: Int -> Word
intToWord = unsafeCoerce
{-# INLINE intToWord #-}

wordToInt :: Word -> Int
wordToInt = unsafeCoerce
{-# INLINE wordToInt #-}

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) ->
    String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
    showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z
