

{-# LANGUAGE ForeignFunctionInterface
           , CPP
  #-}


module Data.Char.WCWidth
  ( wcwidth
  ) where

import Foreign.C




wcwidth                     ::  Char -> Int
wcwidth                      =  fromEnum . native . toEnum . fromEnum


#ifdef mingw32_HOST_OS
foreign import stdcall unsafe "wchar.h wcwidth" native :: CWchar -> CInt
#else
foreign import ccall unsafe "wchar.h wcwidth" native :: CWchar -> CInt
#endif


