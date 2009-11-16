#!/usr/bin/env runhaskell


import Prelude hiding (lines)
import Data.List (foldl')
import Data.Char
import Data.Maybe
import Data.Either
import Control.Applicative hiding (empty)
import System.IO (stdin, stdout, stderr)
import Data.ByteString.Lazy.Char8 hiding (count, foldl', reverse)

import Data.ParserCombinators.Attoparsec.Char8
import Data.ByteString.Nums.Careless




main                         =  do
  ranges                    <-  rights . parse' . lines <$> hGetContents stdin
  --sequence_ . (hPut stdout . display <$>) $ collate ranges
  hPut stdout preamble
  (sequence_ . (hPut stdout . compile <$>) . collate) ranges
  hPut stdout postamble
 where
  parse'                     =  (snd . parse range <$>)
  display ((a,z),w)          =  a `append` pack ".." `append` z `snoc` '\n'
  compile ((a,z),w)          =  guard `append` eq `append` w `snoc` '\n'
   where
    guard                    =  pack "  | i <= " `append` z
    eq                       =  pack "              =  "
  preamble                   =  '\n' `cons` pack sig `append` pack "cols c\n"
   where
    sig                      =  "cols                        ::  Char -> Int\n"
  postamble                  =  otherwise_clause `append` where_clause
   where
    otherwise_clause         =  pack "  | otherwise                =  -1\n"
    where_clause             =  pack " where\n" `append` pack i `snoc` '\n'
     where
      i                      =  "  i                          =  fromEnum c\n"




range                        =  do
  start                     <-  short_hex
  string ".."
  end                       <-  short_hex
  some (char ' ')
  columns                   <-  little_int
  return ((start, end), columns)

short_hex                   ::  Parser ByteString
short_hex                    =  do
  ox                        <-  match (string "0x")
  count 4 (char '0')
  append ox <$> match (count 4 (satisfy isHexDigit))

little_int                  ::  Parser ByteString
little_int                   =  do
  sign                      <-  maybe empty id <$> optional minus
  digits                    <-  match (satisfy isDigit)
  return (append sign digits)
 where
  minus                      =  match (char '-')




collate                      =  reverse . foldl' collate' []
 where
  collate' [] range          =  [range]
  collate' (((a,z),w):t) ((a',z'),w')
    | w == w'                =  ((a,z'),w) : t
    | otherwise              =  ((a',z'),w') : ((a,z),w) : t


