module Satyros.Util where

import           Unsafe.Coerce        (unsafeCoerce)

intToWord :: Int -> Word
intToWord = unsafeCoerce
{-# INLINE intToWord #-}

wordToInt :: Word -> Int
wordToInt = unsafeCoerce
{-# INLINE wordToInt #-}
