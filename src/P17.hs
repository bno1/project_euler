module P17
  ( solveP17
  ) where


primaries :: [String]
primaries = [ ""
            , "one"
            , "two"
            , "three"
            , "four"
            , "five"
            , "six"
            , "seven"
            , "eight"
            , "nine"
            , "ten"
            , "eleven"
            , "twelve"
            , "thirteen"
            , "fourteen"
            , "fifteen"
            , "sixteen"
            , "seventeen"
            , "eighteen"
            , "nineteen"
            ]

tens :: [String]
tens = [ undefined
       , undefined
       , "twenty"
       , "thirty"
       , "forty"
       , "fifty"
       , "sixty"
       , "seventy"
       , "eighty"
       , "ninety"
       ]


spellNumber :: Int -> [String]
spellNumber n
  | n < 20 = [primaries !! n]
  | n < 100 = tens !! q10 : spellNumber r10
  | n < 1000 = primaries !! q100 : "hundred" :
               if r100 == 0 then [] else "and" : spellNumber r100
  | n == 1000 = [primaries !! 1, "thousand"]
  | otherwise = undefined
  where
    (q10, r10) = n `quotRem` 10
    (q100, r100) = n `quotRem` 100

solveP17 :: Int
solveP17 = sum $ map length $ [1..1000] >>= spellNumber
