

module Data.Char.Cols where




{-| Determines the number of columns introduced by an Unicode character, in a
    way that is supportive of parsing indentation based langauges.

    This function may be at variance with your native @wcwidth@. 

 *  We assign a width of 8 to @\t@.
 
 *  We assign a width of 1 to many characters that a native @wcwidth@ may
    assign a width of -1 (non-printing), such as
    @LATIN SMALL LETTER L WITH CURL@.

  Many, many characters are assigned a correct width; however, there are many
  (obscure) characters assigned a 0 width due to time constraints. Patches are
  happily accepted and the author will budget time for more nuanced spacing
  tables in the future.

 -}
cols                        ::  Char -> Int
cols c
  | '\x20' >=< '\x7E'        =  1   {-  Plain old printable ASCII.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\x3400' >=< '\x9FFF'    =  2   {-  UniHan, Book of Changes.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\t' == c                =  8   {-  The ASCII tab character.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\xA0' >=< '\x2FF'       =  1   {-  This includes non-breaking space,
                                     -  numerous accented letters, scare
                                     -  quotes, angle quotes, pilcrows, money
                                     -  signs and other characters. On OS X,
                                     -  some of these code points are taken to
                                     -  be 'non-printing' (width of negative
                                     -  one), namely:
                                     -
                                     -    ȡ  ȴ..ɏ  ʮ  ʯ  ˯..˿
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\b' == c                =  -1  {-  The ASCII backspace.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\x1E00' >=< '\x2B23'    =  1   {-  Many math symbols, arrows, technical
                                     -  symbols, extended Greek and Latin
                                     -  letters and other symbols.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\x300' >=< '\x36F'      =  -1  {-  Backwards traveling characters;
                                     -  combining diacritics and other,
                                     -  stranger things.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\x370' >=< '\x58F'      =  1   {-  Greek, Coptic, Cyrillic and Armenian
                                     -  and probably more. All assigned a
                                     -  width of one column.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | otherwise                =  0   {-  Nulls and everything else.
                                     - - - - - - - - - - - - - - - - - - - - -}
 where
  first >=< last             =  first <= c && last >= c








