{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, DeriveLift, NumDecimals #-}

module NgxExport.Tools (
    -- * Various useful functions and data
                        exitWorkerProcess
                       ,terminateWorkerProcess
                       ,ngxNow
                       ,threadDelaySec
                       ,TimeInterval (..)
                       ,toSec
    -- * Exporters of simple services
                       ,ngxExportSimpleService
                       ,ngxExportSimpleServiceTyped
                       ,ngxExportSimpleServiceTypedAsJSON
    -- * Re-exported functions for building simple services
                       ,unsafePerformIO
                       ) where

import           NgxExport

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           Data.Maybe
import           Data.Aeson
import           Control.Monad
import           Control.Concurrent
import           GHC.Generics
import           System.IO.Unsafe (unsafePerformIO)
import           Safe

foreign import ccall "exit" exit :: CInt -> IO ()

exitWorkerProcess :: IO ()
exitWorkerProcess = exit 1          -- this makes Nginx respawn a worker

terminateWorkerProcess :: IO ()
terminateWorkerProcess = exit 2     -- this makes Nginx not respawn a worker

-- This is a small type casting hack: getting the first element of type time_t
-- from a C struct
ngxNow :: IO CTime
ngxNow = ngxCachedTimePtr >>= peek >>= peek . castPtr

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1e6)

data TimeInterval = Hr Int
                  | Min Int
                  | Sec Int
                  | HrMin Int Int
                  | MinSec Int Int
                  deriving (Generic, Lift, Read)
instance FromJSON TimeInterval

toSec :: TimeInterval -> Int
toSec (Hr h)       = 3600 * h
toSec (Min m)      = 60 * m
toSec (Sec s)      = s
toSec (HrMin h m)  = 3600 * h + 60 * m
toSec (MinSec m s) = 60 * m + s

newtype Readable a = Readable a
newtype ReadableAsJSON a = ReadableAsJSON a

class FromByteString a where
    type WrappedT a
    fromByteString :: Maybe a -> ByteString -> Maybe (WrappedT a)

instance Read a => FromByteString (Readable a) where
    type WrappedT (Readable a) = a
    fromByteString = const $ readMay . C8.unpack

instance FromJSON a => FromByteString (ReadableAsJSON a) where
    type WrappedT (ReadableAsJSON a) = a
    fromByteString = const decodeStrict

instance FromByteString ByteString where
    type WrappedT ByteString = ByteString
    fromByteString = const Just

simpleServiceWrap ::
    (a -> Bool -> IO L.ByteString) -> a -> Bool -> IO L.ByteString
simpleServiceWrap f = f

ngxExportSimpleService' ::
    Name -> Maybe (Name, Bool) -> Maybe TimeInterval -> Q [Dec]
ngxExportSimpleService' f c i = concat <$> sequence
    [sequence $
        (if hasConf
             then [sigD nameC [t|IORef (Maybe $(conT nameConC))|]
                  ,funD nameC [clause []
                                  (normalB
                                      [|unsafePerformIO $ newIORef Nothing|]
                                  )
                                  []
                              ]
                  ,pragInlD nameC NoInline FunLike AllPhases
                  ]
             else []
        )
        ++
        [sigD nameSf [t|ByteString -> Bool -> IO L.ByteString|]
        ,funD nameSf
            [clause [[p|confBs_|], [p|fstRun_|]]
                (normalB
                    [|do
                          conf_data_ <- $(initConf)
                          $(waitTime)
                          simpleServiceWrap $(varE f)
                              (fromJust conf_data_) fstRun_
                    |]
                )
                []
            ]
        ]
    ,ngxExportServiceIOYY nameSf
    ]
    where sfName   = "simpleService_" ++ nameBase f
          nameSf   = mkName sfName
          hasConf  = isJust c
          (cName, isJSON) = if hasConf
                                then let c' = fromJust c
                                     in ("storage_" ++ nameBase (fst c')
                                        , snd c'
                                        )
                                else ("dummy__", False)
          nameC    = mkName cName
          nameConC = mkName $ if hasConf
                                   then let c' = fromJust c
                                        in nameBase (fst c')
                                   else "Dummy__"
          confType = if isJSON
                         then [t|ReadableAsJSON|]
                         else [t|Readable|]
          initConf =
              if hasConf
                  then [|readIORef $(varE nameC) >>=
                             maybe (do
                                        let conf_data__ =
                                                fromByteString
                                                    (Nothing :: Maybe
                                                        ($(confType)
                                                         $(conT nameConC)
                                                        )
                                                    ) confBs_
                                        when (isNothing conf_data__)
                                            terminateWorkerProcess
                                        writeIORef $(varE nameC) conf_data__
                                        return conf_data__
                                   ) (return . Just)
                       |]
                  else [|do
                             let conf_data__ =
                                     fromByteString
                                         (Nothing :: Maybe ByteString) confBs_
                             when (isNothing conf_data__)
                                 terminateWorkerProcess
                             return conf_data__
                       |]
          waitTime =
              if isJust i
                  then let ival = fromJust i
                       in [|unless fstRun_ $ threadDelaySec $ toSec ival|]
                  else [|return ()|]

ngxExportSimpleService ::
    Name -> Maybe TimeInterval -> Q [Dec]
ngxExportSimpleService f =
    ngxExportSimpleService' f Nothing

ngxExportSimpleServiceTyped ::
    Name -> Name -> Maybe TimeInterval -> Q [Dec]
ngxExportSimpleServiceTyped f c =
    ngxExportSimpleService' f $ Just (c, False)

ngxExportSimpleServiceTypedAsJSON ::
    Name -> Name -> Maybe TimeInterval -> Q [Dec]
ngxExportSimpleServiceTypedAsJSON f c =
    ngxExportSimpleService' f $ Just (c, True)

