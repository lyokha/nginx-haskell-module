{-# LANGUAGE DeriveGeneric #-}

{- A tool for benchmarking encoding and decoding of data Msg,
 - it tests Read/Show and JSON Aeson encode/decode.
 -
 - Compile:
 - ghc --make -O2 lmr-bench.hs
 -
 - Run:
 - ./lmr-bench -o lmr-bench.html
 -
 -}

import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L

import           Criterion.Main

type Label = String
type Hint = String
type Destination = String   -- IP address or domain name
type SeqNumber = Integer

data Op = Read
        | Write
        | Delete
        deriving (Generic, Read, Show)
instance FromJSON Op
instance ToJSON Op

data BackendStatus = Ok             -- In / Out
                   | NotFound       -- In
                   | NotAccessible  -- In
                   | NotReadable    -- In
                   | NonExistent    -- Out
                   deriving (Generic, Read, Show, Eq)
instance FromJSON BackendStatus
instance ToJSON BackendStatus

data Msg = Msg { op      :: Op
               , hnt     :: Hint
               , label   :: Label
               , seqn    :: SeqNumber
               , key     :: Int
               , start   :: Int
               , idx     :: Int
               , backend :: Destination
               , status  :: BackendStatus
               } deriving (Generic, Read, Show)
instance FromJSON Msg
instance ToJSON Msg


--   |          |  input                        |  output          |
--   |----------|-------------------------------|------------------|
--   |  encode  |  msgo       :: Msg            |  C8L.ByteString  |
--   |  decode  |  msgs, msgb :: C8.ByteString  |  (Maybe) Msg     |

main = defaultMain
    [ bench "Show encode" $
        whnf    (C8L.pack . show)                               msgo
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
          msgs = C8.pack "Msg { op = Read\
                             \, hnt = \"default\"\
                             \, label = \"dir_1\"\
                             \, seqn = 3\
                             \, key = 1\
                             \, start = 0\
                             \, idx = 0\
                             \, backend = \"192.168.0.1:8080\"\
                             \, status = NotAccessible\
                             \}"
          msgb = C8.pack "{ \"backend\" : \"192.168.0.1:8080\"\
                         \, \"hnt\"     : \"default\"\
                         \, \"idx\"     :  0\
                         \, \"key\"     :  1\
                         \, \"label\"   : \"dir_1\"\
                         \, \"op\"      : \"Read\"\
                         \, \"seqn\"    :  3\
                         \, \"start\"   :  0\
                         \, \"status\"  : \"NotAccessible\"\
                         \}"

