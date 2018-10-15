{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, DeriveLift, NumDecimals #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools
-- Copyright   :  (c) Alexey Radkov 2018
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- Extra tools for using in custom Haskell code with
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools (
    -- * Various useful functions and data
                        exitWorkerProcess
                       ,terminateWorkerProcess
                       ,ngxNow
                       ,threadDelaySec
                       ,TimeInterval (..)
                       ,toSec
    -- * Exporters of simple services
                       ,ServiceMode (..)
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

-- | Terminates current Nginx worker process.
--
-- Nginx master process shall spawn a new worker process thereafter.
exitWorkerProcess :: IO ()
exitWorkerProcess = exit 1

-- | Terminates current Nginx worker process.
--
-- Nginx master process shall /not/ spawn a new worker process thereafter.
terminateWorkerProcess :: IO ()
terminateWorkerProcess = exit 2

-- | Returns current time as the number of seconds elapsed since UNIX epoch.
--
-- The value is taken from Nginx core, so no additional system calls get
-- involved. On the other hand, this means that it's only safe to use from
-- an Nginx worker's main thread, i.e. in /synchronous/ Haskell handlers and
-- /service hooks/. Be also aware that this is a small type casting hack:
-- the value is taken from the first element of type @time_t@ wrapped in a
-- bigger C struct.
ngxNow :: IO CTime
ngxNow = ngxCachedTimePtr >>= peek >>= peek . castPtr

-- | Delays current thread for the specified number of seconds.
threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1e6)

-- | Time intervals.
data TimeInterval = Hr Int          -- ^ Hours
                  | Min Int         -- ^ Minutes
                  | Sec Int         -- ^ Seconds
                  | HrMin Int Int   -- ^ Hours and minutes
                  | MinSec Int Int  -- ^ Minutes and seconds
                  deriving (Generic, Lift, Read)
instance FromJSON TimeInterval

-- | Converts a time interval into seconds.
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

data ServiceMode = PersistentService (Maybe TimeInterval)
                 | SingleShotService

simpleServiceWrap ::
    (a -> Bool -> IO L.ByteString) -> a -> Bool -> IO L.ByteString
simpleServiceWrap f = f

ngxExportSimpleService' :: Name -> Maybe (Name, Bool) -> ServiceMode -> Q [Dec]
ngxExportSimpleService' f c m = concat <$> sequence
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
                          $(serviceWrap)
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
          (waitTime, serviceWrap) =
              case m of
                  PersistentService i ->
                      (if isJust i
                           then let t = fromJust i
                                in [|unless fstRun_ $
                                         threadDelaySec $ toSec t
                                   |]
                           else [|return ()|]
                      ,[|simpleServiceWrap
                             $(varE f) (fromJust conf_data_) fstRun_
                       |]
                      )
                  SingleShotService ->
                      ([|unless fstRun_ $
                             threadDelaySec $ toSec $ Hr 1
                       |]
                      ,[|if fstRun_
                             then simpleServiceWrap
                                      $(varE f) (fromJust conf_data_) fstRun_
                             else return L.empty
                       |]
                      )

ngxExportSimpleService :: Name -> ServiceMode -> Q [Dec]
ngxExportSimpleService f =
    ngxExportSimpleService' f Nothing

ngxExportSimpleServiceTyped :: Name -> Name -> ServiceMode -> Q [Dec]
ngxExportSimpleServiceTyped f c =
    ngxExportSimpleService' f $ Just (c, False)

ngxExportSimpleServiceTypedAsJSON :: Name -> Name -> ServiceMode -> Q [Dec]
ngxExportSimpleServiceTypedAsJSON f c =
    ngxExportSimpleService' f $ Just (c, True)

