{-# LANGUAGE DerivingVia, StandaloneDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans -fconstraint-solver-iterations=0 #-}

-- A tool for generating sample objects of various types in GHCi
--
-- Run GHCi:
-- ghci -fobject-code lmr-objgen.hs ../lmr.hs
--
-- Load modules and generate some data:
-- Prelude LMRObjGen> import LabeledMediaRouter
-- Prelude LabeledMediaRouter LMRObjGen> d <- genCollectedData 3
-- Prelude LabeledMediaRouter LMRObjGen> pPrint d
--  ...
-- Prelude LabeledMediaRouter LMRObjGen> pPrintJSON d
--  ...
-- Prelude LabeledMediaRouter LMRObjGen> pPrint $ toRoutes d
--  ...
-- Prelude LabeledMediaRouter LMRObjGen> pPrintJSON $ toRoutes d
--  ...
-- Prelude LabeledMediaRouter LMRObjGen> ld <- genGeneric 3 :: IO LabelData
-- Prelude LabeledMediaRouter LMRObjGen> pPrint ld
--  ...

module LMRObjGen where

import           LabeledMediaRouter

import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           GHC.Generics

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

deriving via GenericArbitrary Possession instance Arbitrary Possession
deriving via GenericArbitrary Mode instance Arbitrary Mode
deriving via GenericArbitrary BackendData instance Arbitrary BackendData
deriving via GenericArbitrary LabelData instance Arbitrary LabelData
deriving instance Generic Op
deriving via GenericArbitrary Op instance Arbitrary Op
deriving instance Generic BackendStatus
deriving via GenericArbitrary BackendStatus instance Arbitrary BackendStatus
deriving instance Generic Msg
deriving via GenericArbitrary Msg instance Arbitrary Msg

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

