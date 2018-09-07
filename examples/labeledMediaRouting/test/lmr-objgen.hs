{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module LMRObjGen where

import           LabeledMediaRouter

import           Test.QuickCheck
import           Data.DeriveTH

import qualified Text.Pretty.Simple as PP
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TextIO (putStrLn)
import qualified Data.Text.Lazy as LText (Text)
import qualified Data.Text.Lazy.IO as LTextIO (putStrLn)

import           Control.Monad

deriving instance Show Possession
deriving instance Show Mode
deriving instance Show BackendData
deriving instance Show LabelData

instance {-# OVERLAPS #-} Arbitrary String where
    arbitrary = do
        l <- choose (1, 30)
        replicateM l $ elements $
            ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-."

derive makeArbitrary ''Possession
derive makeArbitrary ''Mode
derive makeArbitrary ''BackendData
derive makeArbitrary ''LabelData
derive makeArbitrary ''Op
derive makeArbitrary ''BackendStatus
derive makeArbitrary ''Msg

pPrint :: Show a => a -> IO ()
pPrint = PP.pPrint

pPrintJSON :: ToJSON a => a -> IO ()
pPrintJSON = putLazyTextLn . PP.pString . lazyByteStringToString . encode

lazyByteStringToText :: L.ByteString -> Text
lazyByteStringToText = decodeUtf8 . L.toStrict

lazyByteStringToString :: L.ByteString -> String
lazyByteStringToString = Text.unpack . lazyByteStringToText

putLazyByteStringLn :: L.ByteString -> IO ()
putLazyByteStringLn = TextIO.putStrLn . lazyByteStringToText

putLazyTextLn :: LText.Text -> IO ()
putLazyTextLn = LTextIO.putStrLn

genGeneric :: Arbitrary a => Int -> IO a
genGeneric = generate . flip resize arbitrary

genCollectedData :: Int -> IO CollectedData
genCollectedData = genGeneric

genRoutes :: Int -> IO Routes
genRoutes = genGeneric

genMsg :: IO Msg
genMsg = genGeneric 0

