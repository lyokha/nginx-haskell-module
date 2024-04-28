{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash, ViewPatterns, TupleSections #-}

module TestTsung where

import           NgxExport
import           Data.FileEmbed
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.ByteString.Unsafe
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           Network.HTTP.Client
import           Control.Concurrent
import qualified Control.Concurrent.MSem as S
import           Control.Exception
import           Control.Monad
import           System.IO.Unsafe
import           Safe

fromFile :: C8.ByteString -> UnsafeContentHandlerResult
fromFile (tailSafe . C8.unpack -> f) =
    case lookup f $(embedDir "/var/lib/nginx/test/tsung") of
        Just p  -> (p,                                text_plain, 200)
        Nothing -> (packLiteral 14 "File not found"#, text_plain, 404)
    where packLiteral l s =
              accursedUnutterablePerformIO $ unsafePackAddressLen l s
          text_plain = packLiteral 10 "text/plain"#
ngxExportUnsafeHandler 'fromFile

fromFile1Byte :: C8.ByteString -> UnsafeContentHandlerResult
fromFile1Byte = const $ fromFile $ C8.pack "/static/content-1byte.txt"
ngxExportUnsafeHandler 'fromFile1Byte

catchHttpException :: IO C8L.ByteString -> IO C8L.ByteString
catchHttpException = (`catch` \e ->
        return $ C8L.pack $ "HTTP EXCEPTION: " ++ show (e :: HttpException))

getResponse :: C8.ByteString -> (Request -> IO (Response C8L.ByteString)) ->
    IO C8L.ByteString
getResponse (C8.unpack -> url) = fmap responseBody . (parseRequest url >>=)

httpManager :: Manager
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

getUrl :: C8.ByteString -> IO C8L.ByteString
getUrl url = catchHttpException $ getResponse url $ flip httpLbs httpManager
ngxExportAsyncIOYY 'getUrl

httpManagerConnTimeout :: Manager
httpManagerConnTimeout = unsafePerformIO $ newManager defaultManagerSettings
    { managerResponseTimeout = responseTimeoutMicro 1 }
{-# NOINLINE httpManagerConnTimeout #-}

unsafeGetUrlConnTimeout :: C8.ByteString -> IO C8L.ByteString
unsafeGetUrlConnTimeout url = getResponse url $
    flip httpLbs httpManagerConnTimeout
ngxExportAsyncIOYY 'unsafeGetUrlConnTimeout

sem1 :: S.MSem Integer
sem1 = unsafePerformIO $ S.new 1
{-# NOINLINE sem1 #-}

getUrl1 :: C8.ByteString -> IO C8L.ByteString
getUrl1 url = do
    man <- newManager defaultManagerSettings
    catchHttpException $ getResponse url $ S.with sem1 . flip httpLbs man
ngxExportAsyncIOYY 'getUrl1

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 10^(6 :: Integer))

delay :: C8.ByteString -> IO C8L.ByteString
delay (readDef 0 . C8.unpack -> v) =
    threadDelaySec v >> return (C8L.pack $ show v)
ngxExportAsyncIOYY 'delay

delay1 :: C8.ByteString -> IO C8L.ByteString
delay1 (readDef 0 . C8.unpack -> v) =
    S.with sem1 (threadDelaySec v) >> return (C8L.pack $ show v)
ngxExportAsyncIOYY 'delay1

getUrlService :: C8.ByteString -> Bool -> IO C8L.ByteString
getUrlService url firstRun = do
    unless firstRun $ threadDelaySec 20
    getUrl url
ngxExportServiceIOYY 'getUrlService

getIOValue :: C8.ByteString -> IO C8L.ByteString
getIOValue = const $ return $ C8L.pack "HELLO WORLD!"
ngxExportIOYY 'getIOValue

reqBody :: C8L.ByteString -> C8.ByteString -> IO C8L.ByteString
reqBody = const . return
ngxExportAsyncOnReqBody 'reqBody

reqHead :: C8L.ByteString -> C8.ByteString -> IO C8L.ByteString
reqHead a n = return $ C8L.concat $ take (readDef 0 $ C8.unpack n) $
    map (`C8L.append` C8L.pack "\\n") $ C8L.lines a
ngxExportAsyncOnReqBody 'reqHead

reqFld :: C8L.ByteString -> C8.ByteString -> IO C8L.ByteString
reqFld a fld = return $ maybe C8L.empty C8L.tail $
    lookup (C8L.fromStrict fld) $ map (C8L.break (== '=')) $
    C8L.split '&' a
ngxExportAsyncOnReqBody 'reqFld

reqBodyTouch :: C8L.ByteString -> C8.ByteString -> IO C8L.ByteString
reqBodyTouch = (return .) . const . return C8L.empty
ngxExportAsyncOnReqBody 'reqBodyTouch

getUrlContent :: C8.ByteString -> IO ContentHandlerResult
getUrlContent url = (, packLiteral 9 "text/html"#, 200, []) <$> getUrl url
    where packLiteral l s =
              accursedUnutterablePerformIO $ unsafePackAddressLen l s
ngxExportAsyncHandler 'getUrlContent

