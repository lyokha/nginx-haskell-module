{-# LANGUAGE TemplateHaskell, DeriveGeneric, RecordWildCards #-}

module TestTools where

import           NgxExport
import           NgxExport.Tools

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Aeson
import           Control.Monad
import           GHC.Generics

test :: ByteString -> Bool -> IO L.ByteString
test = const . return . L.fromStrict
ngxExportSimpleService 'test $
    PersistentService $ Just $ Sec 10

testRead :: (Read a, Show a) => a -> IO L.ByteString
testRead = return . C8L.pack . show

testReadInt :: Int -> Bool -> IO L.ByteString
testReadInt = const testRead
ngxExportSimpleServiceTyped 'testReadInt ''Int $
    PersistentService $ Just $ Sec 10

newtype Conf = Conf Int deriving (Read, Show)

testReadConf :: Conf -> Bool -> IO L.ByteString
testReadConf = const testRead
ngxExportSimpleServiceTyped 'testReadConf ''Conf $
    PersistentService $ Just $ Sec 10

data ConfWithDelay = ConfWithDelay { delay :: TimeInterval
                                   , value :: Int
                                   } deriving (Read, Show)

testReadConfWithDelay :: ConfWithDelay -> Bool -> IO L.ByteString
testReadConfWithDelay c@ConfWithDelay {..} fstRun = do
    unless fstRun $ threadDelaySec $ toSec delay
    testRead c
ngxExportSimpleServiceTyped 'testReadConfWithDelay ''ConfWithDelay $
    PersistentService Nothing

testReadJSON :: (FromJSON a, Show a) => a -> IO L.ByteString
testReadJSON = return . C8L.pack . show

data ConfJSON = ConfJSONCon1 Int
              | ConfJSONCon2 deriving (Generic, Show)
instance FromJSON ConfJSON

testReadConfJSON :: ConfJSON -> Bool -> IO L.ByteString
testReadConfJSON = const testReadJSON
ngxExportSimpleServiceTypedAsJSON 'testReadConfJSON ''ConfJSON
    SingleShotService

testReadIntHandler :: ByteString -> L.ByteString
testReadIntHandler = C8L.pack . show .
    (readFromByteString :: ByteString -> Maybe Int)
ngxExportYY 'testReadIntHandler

testReadConfHandler :: ByteString -> L.ByteString
testReadConfHandler = C8L.pack . show .
    (readFromByteString :: ByteString -> Maybe Conf)
ngxExportYY 'testReadConfHandler

testReadConfJSONHandler :: ByteString -> L.ByteString
testReadConfJSONHandler = C8L.pack . show .
    (readFromByteStringAsJSON :: ByteString -> Maybe ConfJSON)
ngxExportYY 'testReadConfJSONHandler

