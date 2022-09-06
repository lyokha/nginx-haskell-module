{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Read
-- Copyright   :  (c) Alexey Radkov 2018-2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides a number of functions to support /typed/ exchange
-- between Nginx and Haskell handlers. Functions 'readFromByteString' and
-- 'readFromByteStringAsJSON' expect serialized values of custom types deriving
-- or implementing instances of 'Read' and 'FromJSON' respectively. Functions
-- 'readFromByteStringWithRPtr' and 'readFromByteStringWithRPtrAsJSON'
-- additionally expect a binary value of a C pointer size marshalled in front
-- of the value of the custom type. This pointer should correspond to the value
-- of Nginx variable __/$_r_ptr/__.
--
-- Below is a simple example.
--
-- ==== File /test_tools.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications \#-}
--
-- module TestTools where
--
-- import           NgxExport
-- import           NgxExport.Tools.Read
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as C8L
-- import           Data.Aeson
-- import           GHC.Generics
--
-- showAsLazyByteString :: Show a => a -> L.ByteString
-- showAsLazyByteString = C8L.pack . show
--
-- newtype Conf = Conf Int deriving (Read, Show)
--
-- data ConfJSON = ConfJSONCon1 Int
--               | ConfJSONCon2 deriving (Generic, Show)
-- instance FromJSON ConfJSON
--
-- testReadIntHandler :: ByteString -> L.ByteString
-- __/testReadIntHandler/__ = showAsLazyByteString .
--     'readFromByteString' \@Int
-- 'NgxExport.ngxExportYY' \'testReadIntHandler
--
-- testReadConfHandler :: ByteString -> L.ByteString
-- __/testReadConfHandler/__ = showAsLazyByteString .
--     'readFromByteString' \@Conf
-- 'NgxExport.ngxExportYY' \'testReadConfHandler
--
-- testReadConfJSONHandler :: ByteString -> IO L.ByteString
-- __/testReadConfJSONHandler/__ = return . showAsLazyByteString .
--     'readFromByteStringAsJSON' \@ConfJSON
-- 'NgxExport.ngxExportAsyncIOYY' \'testReadConfJSONHandler
--
-- testReadConfWithRPtrHandler :: ByteString -> L.ByteString
-- __/testReadConfWithRPtrHandler/__ = showAsLazyByteString .
--     'readFromByteStringWithRPtr' \@Conf
-- 'NgxExport.ngxExportYY' \'testReadConfWithRPtrHandler
--
-- testReadConfWithRPtrJSONHandler :: ByteString -> L.ByteString
-- __/testReadConfWithRPtrJSONHandler/__ = showAsLazyByteString .
--     'readFromByteStringWithRPtrAsJSON' \@ConfJSON
-- 'NgxExport.ngxExportYY' \'testReadConfWithRPtrJSONHandler
-- @
--
-- Here five Haskell handlers are defined: /testReadIntHandler/,
-- /testReadConfHandler/, /testReadConfJSONHandler/,
-- /testReadConfWithRPtrHandler/, and /testReadConfWithRPtrJSONHandler/. Four
-- of them are /synchronous/ and one is /asynchronous/ for the sake of variety.
--
-- ==== File /nginx.conf/
-- @
-- user                    nobody;
-- worker_processes        2;
--
-- events {
--     worker_connections  1024;
-- }
--
-- http {
--     default_type        application\/octet-stream;
--     sendfile            on;
--
--     haskell load \/var\/lib\/nginx\/test_tools.so;
--
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         location \/ {
--             haskell_run __/testReadIntHandler/__
--                     $hs_testReadIntHandler
--                     -456;
--             haskell_run __/testReadConfHandler/__
--                     $hs_testReadConfHandler
--                     \'Conf 21\';
--             haskell_run_async __/testReadConfJSONHandler/__
--                     $hs_testReadConfJSONHandler
--                     \'{\"tag\":\"ConfJSONCon2\"}\';
--             haskell_run_async __/testReadConfJSONHandler/__
--                     $hs_testReadConfJSONHandlerBadInput
--                     \'{\"tag\":\"Unknown\"}\';
--             haskell_run __/testReadConfWithRPtrHandler/__
--                     $hs_testReadConfWithRPtrHandler
--                     \'${_r_ptr}Conf 21\';
--             haskell_run __/testReadConfWithRPtrJSONHandler/__
--                     $hs_testReadConfWithRPtrJSONHandler
--                     \'$_r_ptr
--                      {\"tag\":\"ConfJSONCon1\", \"contents\":4}
--                     \';
--
--             echo \"Handler variables:\";
--             echo \"  hs_testReadIntHandler: $hs_testReadIntHandler\";
--             echo \"  hs_testReadConfHandler: $hs_testReadConfHandler\";
--             echo \"  hs_testReadConfJSONHandler: $hs_testReadConfJSONHandler\";
--             echo \"  hs_testReadConfJSONHandlerBadInput: $hs_testReadConfJSONHandlerBadInput\";
--             echo \"  hs_testReadConfWithRPtrHandler: $hs_testReadConfWithRPtrHandler\";
--             echo \"  hs_testReadConfWithRPtrJSONHandler: $hs_testReadConfWithRPtrJSONHandler\";
--         }
--     }
-- }
-- @
--
-- ==== A simple test
-- > $ curl 'http://localhost:8010/'
-- > Handler variables:
-- >   hs_testReadIntHandler: Just (-456)
-- >   hs_testReadConfHandler: Just (Conf 21)
-- >   hs_testReadConfJSONHandler: Just ConfJSONCon2
-- >   hs_testReadConfJSONHandlerBadInput: Nothing
-- >   hs_testReadConfWithRPtrHandler: (0x00000000016fc790,Just (Conf 21))
-- >   hs_testReadConfWithRPtrJSONHandler: (0x00000000016fc790,Just (ConfJSONCon1 4))
-----------------------------------------------------------------------------


module NgxExport.Tools.Read (readFromByteString
                            ,readFromByteStringAsJSON
                            ,readFromByteStringWithRPtr
                            ,readFromByteStringWithRPtrAsJSON
                            ,skipRPtr
                            ) where

import           NgxExport.Tools.System

import           Foreign.Ptr
import           Foreign.Storable
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Aeson
import           Data.Proxy
import           Control.Arrow
import           Safe

data Readable a
data ReadableAsJSON a

class FromByteString a where
    type WrappedT a
    fromByteString :: Proxy a -> ByteString -> Maybe (WrappedT a)

instance Read a => FromByteString (Readable a) where
    type WrappedT (Readable a) = a
    fromByteString = const $ readMay . C8.unpack

instance FromJSON a => FromByteString (ReadableAsJSON a) where
    type WrappedT (ReadableAsJSON a) = a
    fromByteString = const decodeStrict

instance FromByteString ByteString where
    type WrappedT ByteString = ByteString
    fromByteString = const Just

-- | Reads an object of a custom type implementing an instance of 'Read'
--   from a 'ByteString'.
--
-- Returns 'Nothing' if reading fails.
readFromByteString :: Read a => ByteString -> Maybe a
readFromByteString = fromByteString (Proxy :: Proxy (Readable a))

-- | Reads an object of a custom type implementing an instance of 'FromJSON'
--   from a 'ByteString'.
--
-- Returns 'Nothing' if reading fails.
readFromByteStringAsJSON :: FromJSON a => ByteString -> Maybe a
readFromByteStringAsJSON = fromByteString (Proxy :: Proxy (ReadableAsJSON a))

-- | Reads a pointer to the Nginx request object followed by an object of
--   a custom type implementing an instance of 'Read' from a 'ByteString'.
--
-- Throws an exception if unmarshalling of the request pointer fails. In the
-- second element of the tuple returns 'Nothing' if reading of the custom
-- object fails. Notice that the value of the returned request pointer is not
-- checked against /NULL/.
readFromByteStringWithRPtr :: Read a => ByteString -> (Ptr (), Maybe a)
readFromByteStringWithRPtr = ngxRequestPtr &&& readFromByteString . skipRPtr

-- | Reads a pointer to the Nginx request object followed by an object of
--   a custom type implementing an instance of 'FromJSON' from a 'ByteString'.
--
-- Throws an exception if unmarshalling of the request pointer fails. In the
-- second element of the tuple returns 'Nothing' if decoding of the custom
-- object fails. Notice that the value of the returned request pointer is not
-- checked against /NULL/.
readFromByteStringWithRPtrAsJSON :: FromJSON a =>
    ByteString -> (Ptr (), Maybe a)
readFromByteStringWithRPtrAsJSON =
    ngxRequestPtr &&& readFromByteStringAsJSON . skipRPtr

-- | Skips the number of bytes equal to the size of a pointer from the beginning
--   of a 'ByteString'.
--
-- This can be useful to drop a pointer to the Nginx request object passed at
-- the beginning of a handler's argument.
skipRPtr :: ByteString -> ByteString
skipRPtr = B.drop $ sizeOf (undefined :: Word)

