

module Data.Char.Cols where



--  TODO: Ensure printable characters >= 0xFFFF are represented correctly.
--  Use fancy language to describe this. These are the characters on the
--  "Basic Multi-Lingual Plane".

{-| Determines the number of columns introduced by an Unicode character, in a
    way that is supportive of parsing indentation based langauges.

    This function may be at variance with your native @wcwidth@. 

 *  We assign a width of 8 to @\t@.
 
 *  We assign a width of 1 to many characters that a native @wcwidth@ may
    assign a width of -1 (non-printing), such as
    @LATIN SMALL LETTER L WITH CURL@.

    Many, many characters are assigned a correct width; however, there are
    many (obscure) characters assigned a 0 width due to time constraints.
    Patches are happily accepted and the author will budget time for more
    nuanced spacing tables in the future.

 -}
cols                        ::  Char -> Int
cols c
  | 0x0020 ... 0x007E        =  1   {-  Plain old printable ASCII.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3400 ... 0x9FFF        =  2   {-  UniHan, Book of Changes.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | '\t' == c                =  8   {-  The ASCII tab character.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x00A0 ... 0x02FF        =  1   {-  This includes non-breaking space,
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
  | 0x1E00 ... 0x2B23        =  1   {-  Many math symbols, arrows, technical
                                     -  symbols, extended Greek and Latin
                                     -  letters and other symbols.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFF01 ... 0xFF60        =  2   {-  Double width forms of Western chars,
                                     -  like:
                                     -
                                     -    （ｂｅｌｌ）
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x0300 ... 0x036F        =  -1  {-  Backwards traveling characters;
                                     -  combining diacritics and other,
                                     -  stranger things.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x0370 ... 0x058F        =  1   {-  Greek, Coptic, Cyrillic and Armenian
                                     -  and probably more. All assigned a
                                     -  width of one column.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFF61 ... 0xFFBE        =  1   {-  Single width forms of CJK chars, like:
                                     -
                                     -    ｡ ﾖ ﾶ
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFC2 ... 0xFFC7        =  1   {-  Single width Hangul characters.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFCA ... 0xFFCF        =  1   {-  Single width Hangul characters.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFD2 ... 0xFFD7        =  1   {-  Single width Hangul characters.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFDA ... 0xFFDC        =  1   {-  Single width Hangul characters.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFE0 ... 0xFFE6        =  2   {-  Double width money, negation, others.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFE8 ... 0xFFEE        =  1   {-  Single width arrows and others.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFE8 ... 0xFFEE        =  1   {-  Single width arrows and others.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFFC ... 0xFFFD        =  1   {-  Replacement characters, namely:
                                     -
                                     -  ￼ �
                                     - - - - - - - - - - - - - - - - - - - - -}
  | otherwise                =  0   {-  Nulls and everything else.
                                     - - - - - - - - - - - - - - - - - - - - -}
 where
  i                          =  fromEnum c
  first ... last             =  first <= i && last >= i





