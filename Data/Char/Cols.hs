

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
  | 0x3400 ... 0x4DB5        =  2   {-  UniHan.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3000 ... 0x303E        =  2   {-  UniHan spaces, quotes, brackets, &c.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x4E00 ... 0x9FB2        =  2   {-  UniHan.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x9FB4 ... 0x9FBB        =  2   {-  UniHan.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3041 ... 0x3096        =  2   {-  Hiragana and Katakana.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3099 ... 0x30FF        =  2   {-  Hiragana and Katakana.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3131 ... 0x318E        =  2   {-  Hangul. 
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
  | '\t' == c                =  8   {-  The ASCII tab character.
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
  | 0x3105 ... 0x312C        =  2   {-  Bopomofo. 
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x31A0 ... 0x31B7        =  2   {-  Extended Bopomofo.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFFC ... 0xFFFD        =  1   {-  Replacement characters, namely:
                                     -
                                     -    ￼ �
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFE8 ... 0xFFEE        =  1   {-  Single width arrows and others.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0xFFE0 ... 0xFFE6        =  2   {-  Double width money, negation, others.
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
  | 0x2E80 ... 0x2E99        =  2   {-  Supplementary CJK radicals.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x2E9B ... 0x2EF3        =  2   {-  Supplementary CJK radicals.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x2E9B ... 0x2EF3        =  2   {-  Kangxi radicals.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x303F ... 0x303F        =  2   {-  CJK half space symbol.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x2FF0 ... 0x2FFB        =  2   {-  Chinese character layout diagrams.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3190 ... 0x319F        =  2   {-  Kanbun. 
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x31C0 ... 0x31CF        =  2   {-  Chinese character strokes.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x31F0 ... 0x32FF        =  2   {-  Katakana phonetic extensions.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x31F0 ... 0x32FF        =  2   {-  Katakana phonetic extensions.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3200 ... 0x321E        =  2   {-  Parenthesized Hangul. 
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3220 ... 0x3243        =  2   {-  Parenthesized Chinese characters.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3251 ... 0x325F        =  2   {-  Circled numbers, 21..35.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3260 ... 0x327B        =  2   {-  Circled Hangul.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x327F ... 0x327F        =  2   {-  Korean Standard symbol.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3280 ... 0x32B0        =  2   {-  Circled Chinese characters.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x32B1 ... 0x32BF        =  2   {-  Circled numbers, 36..50.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x32C0 ... 0x32CB        =  2   {-  Ideographic television month symbols.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x32D0 ... 0x32FE        =  2   {-  Circled Katakana.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3300 ... 0x3357        =  2   {-  What are these?
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3358 ... 0x3370        =  2   {-  Ideographic television hour symbols.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3371 ... 0x3376        =  2   {-  Double-width measuring unit symbols.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x337B ... 0x337F        =  2   {-  The "square era" names.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x3380 ... 0x33D0        =  2   {-  Double-width measuring unit symbols.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x33E0 ... 0x33FE        =  2   {-  Ideographic television hour symbols.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x4DC0 ... 0x4DFF        =  1   {-  Hexagrams from the Book of Changes.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | 0x0000 ... 0x0000        =  0   {-  Null is null.
                                     - - - - - - - - - - - - - - - - - - - - -}
  | otherwise                =  -1  {-  Everything else.
                                     - - - - - - - - - - - - - - - - - - - - -}
 where
  i                          =  fromEnum c
  first ... last             =  first <= i && last >= i





