{-# LANGUAGE DeriveGeneric, StandaloneDeriving, RecordWildCards #-}

{- A tool for benchmarking encoding and decoding of data Msg,
 - it tests Read/Show and JSON Aeson encode/decode.
 -
 - Compile:
 - ghc --make -O2 lmr-bench.hs ../lmr.hs
 -
 - Run:
 - ./lmr-bench -o lmr-bench.html
 -
 -}

import           LabeledMediaRouter;

import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import           GHC.Generics

import           Criterion.Main

deriving instance Generic Op
instance FromJSON Op
instance ToJSON Op

deriving instance Generic BackendStatus
instance FromJSON BackendStatus
instance ToJSON BackendStatus

deriving instance Generic Msg
instance FromJSON Msg
instance ToJSON Msg

newtype PSMsg = PSMsg Msg
instance Show PSMsg where
    show (PSMsg Msg {..}) =
        concat [ sToDir  op
               , toDir   hnt
               , toDir   label
               , sToDir  seqn
               , sToDir  key
               , sToDir  start
               , sToDir  idx
               , toDir   backend
               , sToDir  status
               ]
            where toDir  = ('/' :)
                  sToDir :: Show a => a -> String
                  sToDir = toDir . show


--   |          |  input                         |  output          |
--   |----------|--------------------------------|------------------|
--   |  encode  |  msgo       :: Msg (or PSMsg)  |  C8L.ByteString  |
--   |  decode  |  msgs, msgb :: C8.ByteString   |  (Maybe) Msg     |

main = defaultMain
    [ bench "Show encode" $
        whnf    (C8L.pack . show)                               msgo
    , bench "Path-style Show encode" $
        whnf    (C8L.pack . show)                       $ PSMsg msgo
    , bench "Read decode" $
        whnf    (read . C8.unpack :: C8.ByteString -> Msg)      msgs
    , bench "JSON encode" $
        whnf    encode                                          msgo
    , bench "JSON decode" $
        whnf    (decodeStrict :: C8.ByteString -> Maybe Msg)    msgb
    ]
    where msgo = Msg { op = Read
                     , hnt = "default"
                     , label = "dir_1"
                     , seqn = 3
                     , key = 1
                     , start = 0
                     , idx = 0
                     , backend = "192.168.0.1:8080"
                     , status = NotAccessible
                     }
          msgs = C8.pack "Msg { op = Read                       \
                             \, hnt = \"default\"               \
                             \, label = \"dir_1\"               \
                             \, seqn = 3                        \
                             \, key = 1                         \
                             \, start = 0                       \
                             \, idx = 0                         \
                             \, backend = \"192.168.0.1:8080\"  \
                             \, status = NotAccessible          \
                             \}"
          msgb = C8.pack "{ \"backend\" : \"192.168.0.1:8080\"  \
                         \, \"hnt\"     : \"default\"           \
                         \, \"idx\"     :  0                    \
                         \, \"key\"     :  1                    \
                         \, \"label\"   : \"dir_1\"             \
                         \, \"op\"      : \"Read\"              \
                         \, \"seqn\"    :  3                    \
                         \, \"start\"   :  0                    \
                         \, \"status\"  : \"NotAccessible\"     \
                         \}"

