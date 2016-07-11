module Tracker.UrlEncoder (urlEncodeVars) where

import Data.Bits  ( (.&.) )
import Data.Char
import Data.List (isPrefixOf,partition,elemIndex)



{-- STOLEN FROM Network.HTTP-3001.0.3
THERE IS A BUG IN urlEncodeVars IN  4000.2.19 VERSION OF THE Network.HTTP PACKAGE --}

urlEncodeVars :: [(String,String)] -> String
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff
urlEncodeVars [] = []

urlEncode (h:t) =
    let str = if reserved (ord h) then escape h else [h]
    in str ++ urlEncode t
    where
        reserved x
            | x >= ord 'a' && x <= ord 'z' = False
            | x >= ord 'A' && x <= ord 'Z' = False
            | x >= ord '0' && x <= ord '9' = False
            | x <= 0x20 || x >= 0x7F = True
            | otherwise = x `elem` map ord [';','/','?',':','@','&'
                                           ,'=','+',',','$','{','}'
                                           ,'|','\\','^','[',']','`'
                                           ,'<','>','#','%','"']
        -- wouldn't it be nice if the compiler
        -- optimised the above for us?

        escape x =
            let y = ord x
            in [ '%', intToDigit ((y `div` 16) .&. 0xf), intToDigit (y .&. 0xf) ]

urlEncode [] = []
