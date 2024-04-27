{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash, ViewPatterns, FlexibleInstances, TupleSections #-}

module TestGithubCi where

import           NgxExport
import qualified Data.Char as C
import           Text.Regex.PCRE
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.Function (on)
import           Control.Monad
import           Safe

toUpper :: String -> String
toUpper = map C.toUpper
ngxExportSS 'toUpper

takeN :: String -> String -> String
takeN = take . readDef 0
ngxExportSSS 'takeN

ngxExportSS 'reverse

class UrlDecodable a
    where doURLDecode :: a -> Maybe a

instance UrlDecodable String where
    -- adopted from
    -- http://www.rosettacode.org/wiki/URL_decoding#Haskell
    doURLDecode [] = Just []
    doURLDecode ('%' : xs) =
        case xs of
            (a : b : xss) ->
                (:) . C.chr <$> readMay ('0' : 'x' : [a, b])
                            <*> doURLDecode xss
            _ -> Nothing
    doURLDecode ('+' : xs) = (' ' :) <$> doURLDecode xs
    doURLDecode (x : xs) = (x :) <$> doURLDecode xs

instance UrlDecodable L.ByteString where
    -- adopted for ByteString arguments from
    -- http://www.rosettacode.org/wiki/URL_decoding#Haskell
    doURLDecode (L.null -> True) = Just L.empty
    doURLDecode (L.uncons -> Just (37, xs))
        | L.length xs > 1 =
            let (C8L.unpack -> c, xss) = L.splitAt 2 xs
            in L.cons <$> readMay ('0' : 'x' : c)
                      <*> doURLDecode xss
        | otherwise = Nothing
    doURLDecode (L.uncons -> Just (43, xs)) = (32 `L.cons`) <$> doURLDecode xs
    doURLDecode (L.uncons -> Just (x, xs)) = (x `L.cons`) <$> doURLDecode xs
    doURLDecode _ = undefined

-- does not match when any of the 2 args is empty or not decodable
matches :: String -> String -> Bool
matches = (fromMaybe False .) . liftM2 (=~) `on` (doURLDecode <=< toMaybe)
    where toMaybe [] = Nothing
          toMaybe a  = Just a
ngxExportBSS 'matches

firstNotEmpty :: [String] -> String
firstNotEmpty = headDef "" . filter (not . null)
ngxExportSLS 'firstNotEmpty

isInList :: [String] -> Bool
isInList [] = False
isInList (x : xs) = x `elem` xs
ngxExportBLS 'isInList

jSONListOfInts :: B.ByteString -> Maybe [Int]
jSONListOfInts = decode <=< doURLDecode . L.fromStrict

isJSONListOfInts :: C8.ByteString -> Bool
isJSONListOfInts = isJust . jSONListOfInts
ngxExportBY 'isJSONListOfInts

jSONListOfIntsTakeN :: C8.ByteString -> C8L.ByteString
jSONListOfIntsTakeN x = encode $ maybe [] (take n) $ jSONListOfInts y
    where (readDef 0 . C8.unpack -> n, B.tail -> y) = B.break (== 124) x
ngxExportYY 'jSONListOfIntsTakeN

urlDecode :: String -> String
urlDecode = fromMaybe "" . doURLDecode
ngxExportSS 'urlDecode

