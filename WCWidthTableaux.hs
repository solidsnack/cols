#!/usr/bin/env runhaskell


{-# LANGUAGE StandaloneDeriving
  #-}


import Data.Char
import Data.List
import System.Environment
import System.IO
import System.Exit
import Text.Printf

import System.Locale.SetLocale
import qualified System.IO.UTF8 as UTF8

import Data.Char.WCWidth




usage                        =  unlines
 [ "USAGE:  wcwidth-stats > table"
 , "        wcwidth-stats --table > table"
 , "        wcwidth-stats --ranges > ranges"
 , "        wcwidth-stats -h,--help"
 , ""
 , "  This program polls your local wcwidth implementation for character width"
 , "  information and generates tables or a chart of ranges."
 , ""
 ]


main                         =  do
  setLocale LC_ALL (Just "")
  programs                  <-  fmap ((Table:) . fmap program) getArgs
  case (head . sort) programs of
    Usage                   ->  putStrLn usage >> exitSuccess
    Ranges                  ->  (rolling_print range_entry) ranges
    Table                   ->  (rolling_print table_entry) widths
    Error s                 ->  do
      hPutStrLn stderr s
      hPutStrLn stderr usage
      exitFailure
 where
  rolling_print f            =  sequence_ . fmap (UTF8.putStrLn . f)
  range_entry ((a,b),w)      =  printf fmt a' b' w count s
   where
    count                    =  1 + fromEnum b - fromEnum a
    fmt                      =  "0x%08x..0x%08x    %2d    %6d    %s"
    (a', b')                 =  (fromEnum a, fromEnum b)
    s                        =  represent a ++ " .. " ++ represent b
  table_entry (c,cols)       =  printf "0x%08x    %2d    %s" c' cols c''
   where
    c'                       =  fromEnum c
    c''                      =  represent c
  represent c
    | ' ' == c               =  "\\SP"
    | '\xA0' == c            =  "&nbsp;"
    | isControl c            =  display c
    | isSpace c              =  '\\' : show (fromEnum c)
    | isPrint c              =  [c]
    | otherwise              =  display c
   where
    display                  =  reverse . drop 1 . reverse . drop 1 . show




program opt                  =  case opt of
  "-h"                      ->  Usage
  "--help"                  ->  Usage
  "--ranges"                ->  Ranges
  "--table"                 ->  Table
  s                         ->  Error ("No such option/arg:        " ++ s)




data Program                 =  Usage | Error String | Ranges | Table
deriving instance Eq Program
instance Ord Program where
  compare a b
    | a == b                 =  EQ
    | otherwise              =  case a of
                                  Usage       ->  LT
                                  Ranges      ->  LT
                                  Table       ->  GT
                                  Error s     ->  case b of 
                                                    Error t ->  compare s t
                                                    _       ->  LT


