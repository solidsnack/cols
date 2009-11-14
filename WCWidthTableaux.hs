#!/usr/bin/env runhaskell


{-# LANGUAGE StandaloneDeriving
  #-}


import Data.Char
import Data.List
import System.Environment
import System.IO
import System.Exit
import Text.Printf

import qualified System.IO.UTF8 as UTF8

import Data.Char.WCWidth




usage                        =  unlines
 [ "USAGE: wcwidth-stats > table"
 , "       wcwidth-stats --ranges > table"
 , "       wcwidth-stats -h,--help"
 , ""
 , "  This program polls your local wcwidth implementation for character width"
 , "  information and generates tables or a chart of ranges."
 , ""
 ]


main                         =  do
  programs                  <-  fmap ((Table:) . fmap program) getArgs
  case (head . sort) programs of
    Usage                   ->  putStrLn usage >> exitSuccess
    Range                   ->  putStrLn "range"
    Table                   ->  table widths
    Error s                 ->  do
      hPutStrLn stderr s
      hPutStrLn stderr usage
      exitFailure
 where
  table                      =  sequence_ . fmap (UTF8.putStrLn . uncurry fmt)
   where
    fmt c cols               =  printf "0x%08x  %2d  %s" (fromEnum c) cols rep
     where
      rep | ' ' == c         =  "\\SP"
          | isControl c      =  display c
          | isSpace c        =  '\\' : show (fromEnum c)
          | isPrint c        =  [c]
          | otherwise        =  display c
       where
        display              =  reverse . drop 1 . reverse . drop 1 . show




widths                       =  [ (c, wcwidth c) | c <- [minBound..maxBound] ] 




program opt                  =  case opt of
  "-h"                      ->  Usage
  "--help"                  ->  Usage
  "--range"                 ->  Range
  s                         ->  Error ("No such option/arg " ++ s)




data Program                 =  Usage | Error String | Range | Table
deriving instance Eq Program
instance Ord Program where
  compare a b
    | a == b                 =  EQ
    | otherwise              =  case a of
                                  Usage       ->  LT
                                  Range       ->  LT
                                  Table       ->  GT
                                  Error s     ->  case b of 
                                                    Error t ->  compare s t
                                                    _       ->  LT


