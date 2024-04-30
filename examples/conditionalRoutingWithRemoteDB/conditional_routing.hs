{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables #-}

module ConditionalRouting where

import           NgxExport
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Network.HTTP.Client
import           Control.Exception
import           Control.Monad
import           System.IO.Unsafe
import           Data.Maybe
import           Data.Aeson

catchBadResponse :: IO C8L.ByteString -> IO C8L.ByteString
catchBadResponse = handle $ \(_ :: SomeException) -> return C8L.empty

getResponse :: String -> (Request -> IO (Response C8L.ByteString)) ->
    IO C8L.ByteString
getResponse url = fmap responseBody . (parseRequest url >>=)

httpManager :: Manager
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

getUrl :: String -> IO C8L.ByteString
getUrl url = catchBadResponse $ getResponse url $ flip httpLbs httpManager

fromRemoteDB :: ByteString -> IO C8L.ByteString
fromRemoteDB (fromMaybe ("", []) . decodeStrict -> (dst, keys)) = fst <$>
    foldM (\(a, s) (k, v) -> do
                let emptyK = null k
                    emptyV = null v
                if s || not (C8L.null a)
                    then return (a, True)
                    else if emptyK
                             then return (C8L.pack v, not emptyV)
                             else (, False) <$> if emptyV
                                                    then return C8L.empty
                                                    else askRemote k v
          ) (C8L.empty, False) keys
    where askRemote k v = getUrl $
            "http://" ++ dst ++ "/db/" ++ k ++ "/" ++ v ++ "/"
ngxExportAsyncIOYY 'fromRemoteDB

reqFld :: C8L.ByteString -> ByteString -> IO C8L.ByteString
reqFld a fld = return $ maybe C8L.empty C8L.tail $
    lookup (C8L.fromStrict fld) $ map (C8L.break (== '=')) $
    C8L.split '&' a
ngxExportAsyncOnReqBody 'reqFld

