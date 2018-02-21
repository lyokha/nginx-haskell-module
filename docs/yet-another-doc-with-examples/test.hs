{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module NgxHaskellUserRuntime where

import           NgxExport
import qualified Data.Char as C
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Control.Concurrent
import           Safe
import           GHC.Prim
import           Data.ByteString.Unsafe
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           Codec.Picture
import           Network.HTTP.Client
import           Control.Exception
import           System.IO.Unsafe
import           Control.Monad
import           Data.IORef
import           Text.Regex.PCRE.ByteString
import           Text.Regex.Base.RegexLike
import qualified Data.Array as A
import           Data.List
import qualified Data.ByteString as B
import           Data.Maybe

toUpper :: String -> String
toUpper = map C.toUpper
ngxExportSS 'toUpper

ngxExportSS 'reverse

isInList :: [String] -> Bool
isInList [] = False
isInList (x : xs) = x `elem` xs
ngxExportBLS 'isInList

echo :: ByteString -> L.ByteString
echo = L.fromStrict
ngxExportDefHandler 'echo

reqFld :: L.ByteString -> ByteString -> IO L.ByteString
reqFld a fld = return $ maybe C8L.empty C8L.tail $
    lookup (C8L.fromStrict fld) $ map (C8L.break (== '=')) $ C8L.split '&' a
ngxExportAsyncOnReqBody 'reqFld

delay :: ByteString -> IO L.ByteString
delay v = do
    let t = readDef 0 $ C8.unpack v
    threadDelay $ t * 1000000
    return $ C8L.pack $ show t
ngxExportAsyncIOYY 'delay

packLiteral :: Int -> GHC.Prim.Addr# -> ByteString
packLiteral l s = accursedUnutterablePerformIO $ unsafePackAddressLen l s

delayContent :: ByteString -> IO ContentHandlerResult
delayContent v = do
    v' <- delay v
    return $ (, packLiteral 10 "text/plain"#, 200) $
        L.concat ["Waited ", v', " sec\n"]
ngxExportAsyncHandler 'delayContent

convertToPng :: L.ByteString -> ByteString -> IO ContentHandlerResult
convertToPng t = const $ return $
    case decodeImage $ L.toStrict t of
        Left e -> (C8L.pack e, packLiteral 10 "text/plain"#, 500)
        Right image -> case encodeDynamicPng image of
                Left e -> (C8L.pack e, packLiteral 10 "text/plain"#, 500)
                Right png -> (png, packLiteral 9 "image/png"#, 200)
ngxExportAsyncHandlerOnReqBody 'convertToPng

httpManager :: Manager
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

getUrl :: ByteString -> IO C8L.ByteString
getUrl url = catchHttpException $ getResponse url $ flip httpLbs httpManager
    where getResponse u = fmap responseBody . (parseRequest (C8.unpack u) >>=)

catchHttpException :: IO C8L.ByteString -> IO C8L.ByteString
catchHttpException = (`catch` \e ->
        return $ C8L.pack $ "HTTP EXCEPTION: " ++ show (e :: HttpException))

getUrlServiceLink :: IORef (Maybe ByteString)
getUrlServiceLink = unsafePerformIO $ newIORef Nothing
{-# NOINLINE getUrlServiceLink #-}

getUrlServiceLinkUpdated :: IORef Bool
getUrlServiceLinkUpdated = unsafePerformIO $ newIORef True
{-# NOINLINE getUrlServiceLinkUpdated #-}

getUrlService :: ByteString -> Bool -> IO L.ByteString
getUrlService url = const $ do
    url' <- fromMaybe url <$> readIORef getUrlServiceLink
    updated <- readIORef getUrlServiceLinkUpdated
    atomicWriteIORef getUrlServiceLinkUpdated False
    unless updated $ threadDelay $ 20 * 1000000
    getUrl url'
ngxExportServiceIOYY 'getUrlService

getUrlServiceHook :: ByteString -> IO L.ByteString
getUrlServiceHook url = do
    writeIORef getUrlServiceLink $ if B.null url
                                       then Nothing
                                       else Just url
    atomicWriteIORef getUrlServiceLinkUpdated True
    return $ if B.null url
                 then "getUrlService reset URL"
                 else L.fromChunks ["getUrlService set URL ", url]
ngxExportServiceHook 'getUrlServiceHook

gHttpbinLinks :: IORef [ByteString]
gHttpbinLinks = unsafePerformIO $ newIORef []
{-# NOINLINE gHttpbinLinks #-}

grepLinks :: ByteString -> [ByteString]
grepLinks =
    map (fst . snd) . filter ((1 ==) . fst) . concatMap A.assocs .
        filter (not . null) . concatMap (matchAllText regex) .
            C8.lines
    where regex = makeRegex $ C8.pack "a href=\"([^\"]+)\"" :: Regex

grepHttpbinLinks :: ByteString -> IO L.ByteString
grepHttpbinLinks "" = return ""
grepHttpbinLinks v  = do
    writeIORef gHttpbinLinks $ grepLinks $ B.copy v
    return ""
ngxExportIOYY 'grepHttpbinLinks

sortLinks :: ByteString -> IO L.ByteString
sortLinks "httpbin" =
    L.fromChunks . sort . map (`C8.snoc` '\n') <$> readIORef gHttpbinLinks
sortLinks _ = return ""
ngxExportIOYY 'sortLinks

cbHttpbin :: ByteString -> Bool -> IO L.ByteString
cbHttpbin url firstRun = do
    when firstRun $ threadDelay $ 5 * 1000000
    getUrl url
ngxExportServiceIOYY 'cbHttpbin

grepHttpbinLinksHook :: ByteString -> IO L.ByteString
grepHttpbinLinksHook v  = do
    let links = grepLinks v
        linksList = let ls = B.intercalate " " links
                    in if B.null ls
                        then "<NULL>"
                        else ls
    writeIORef gHttpbinLinks links
    return $ L.fromChunks ["getUrlService set links ", linksList]
ngxExportServiceHook 'grepHttpbinLinksHook

