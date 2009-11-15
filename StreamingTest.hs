#!/usr/bin/env runhaskell


import System.Random
import Test.QuickCheck.Gen
import Data.Char

import qualified System.IO.UTF8 as UTF8

import Data.Char.Cols




main                         =  do
  nums                      <-  fmap randoms newStdGen
  sequence_ [(UTF8.putStrLn . fmt . short_line r) i | (r,i) <- zip nums [0..]]
 where
  fmt s                      =  "  " ++ s ++ "||" ++ spaces ++ "|"
   where
    width                    =  64
    spaces                   =  replicate (64 - 5 - len) ' '
    len                      =  (sum . fmap cols) s
  short_line                 =  unGen short_lines
  randoms generator          =  first : randoms second
   where
    (first, second)          =  split generator




short_lines                  =  do
  n                         <-  choose (16, 25)
  vectorOf n printable_lowest_plane
 where
  printable_lowest_plane     =  suchThat chars isPrint
   where
    chars                    =  (oneof . fmap choose)
      [ ('\x0000','\x00FF') --  Latin 1
      , ('\x3400','\x9FFF') --  UniHan, Book of Changes.
   -- , ('\x0000','\xFFFF') --  All characters.
      ]


