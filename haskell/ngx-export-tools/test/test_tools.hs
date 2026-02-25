{-# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module TestTools where

import           NgxExport
import           NgxExport.Tools

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Aeson
import           Data.IORef
import           Control.Monad
import           GHC.Generics
import           System.Environment

test :: ByteString -> Bool -> IO LazyByteString
test = const . return . C8L.fromStrict
ngxExportSimpleService 'test $
    PersistentService $ Just $ Sec 10

showAsLazyByteString :: Show a => a -> LazyByteString
showAsLazyByteString = C8L.pack . show

testRead :: Show a => a -> IO LazyByteString
testRead = return . showAsLazyByteString

testReadInt :: Int -> Bool -> IO LazyByteString
testReadInt = const . testRead
ngxExportSimpleServiceTyped 'testReadInt ''Int $
    PersistentService $ Just $ Sec 10

newtype Conf = Conf Int deriving (Read, Show)

testReadConf :: Conf -> Bool -> IO LazyByteString
testReadConf = const . testRead
ngxExportSimpleServiceTyped 'testReadConf ''Conf $
    PersistentService $ Just $ Sec 10

testConfStorage :: ByteString -> IO LazyByteString
testConfStorage = const $
    showAsLazyByteString <$> readIORef storage_Conf_testReadConf
ngxExportIOYY 'testConfStorage

testReadConfNoStore :: Conf -> Bool -> IO LazyByteString
testReadConfNoStore = testReadConf
ngxExportSimpleServiceTyped' 'testReadConfNoStore ''Conf $
    PersistentService $ Just $ Sec 10

data ConfWithDelay = ConfWithDelay { delay :: TimeInterval
                                   , value :: Int
                                   } deriving (Read, Show)

testReadConfWithDelay :: ConfWithDelay -> Bool -> IO LazyByteString
testReadConfWithDelay c@ConfWithDelay {..} fstRun = do
    unless fstRun $ threadDelaySec $ toSec delay
    testRead c
ngxExportSimpleServiceTyped 'testReadConfWithDelay ''ConfWithDelay $
    PersistentService Nothing

data ConfJSON = ConfJSONCon1 Int
              | ConfJSONCon2 deriving (Generic, Show)
instance FromJSON ConfJSON

testReadConfJSON :: ConfJSON -> Bool -> IO LazyByteString
testReadConfJSON = ignitionService testRead
ngxExportSimpleServiceTypedAsJSON 'testReadConfJSON ''ConfJSON
    SingleShotService

testReadIntHandler :: ByteString -> LazyByteString
testReadIntHandler = showAsLazyByteString .
    readFromByteString @Int
ngxExportYY 'testReadIntHandler

testReadConfHandler :: ByteString -> LazyByteString
testReadConfHandler = showAsLazyByteString .
    readFromByteString @Conf
ngxExportYY 'testReadConfHandler

testReadConfJSONHandler :: ByteString -> IO LazyByteString
testReadConfJSONHandler = return . showAsLazyByteString .
    readFromByteStringAsJSON @ConfJSON
ngxExportAsyncIOYY 'testReadConfJSONHandler

testReadConfWithRPtrHandler :: ByteString -> LazyByteString
testReadConfWithRPtrHandler = showAsLazyByteString .
    readFromByteStringWithRPtr @Conf
ngxExportYY 'testReadConfWithRPtrHandler

testReadConfWithRPtrJSONHandler :: ByteString -> LazyByteString
testReadConfWithRPtrJSONHandler = showAsLazyByteString .
    readFromByteStringWithRPtrAsJSON @ConfJSON
ngxExportYY 'testReadConfWithRPtrJSONHandler

testLoadConf :: Conf -> Bool -> IO LazyByteString
testLoadConf = voidService

ngxExportSimpleServiceTyped 'testLoadConf ''Conf restartPromptly

testLoadConfStorage :: ByteString -> IO LazyByteString
testLoadConfStorage = const $
    showAsLazyByteString <$> readIORef storage_Conf_testLoadConf
ngxExportIOYY 'testLoadConfStorage

initTestReadInt :: IO ()
initTestReadInt = do
    _ : v : _ <- dropWhile (/= "--testReadInt") <$> getArgs
    let i = read v
    i `seq` writeIORef storage_Int_testReadInt (Just i)
ngxExportInitHook 'initTestReadInt

