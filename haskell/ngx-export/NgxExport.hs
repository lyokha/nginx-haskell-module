{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport
-- Copyright   :  (c) Alexey Radkov 2016-2021
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires POSIX)
--
-- Export regular haskell functions for using in directives of
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------

module NgxExport (
    -- * Type declarations
                  ContentHandlerResult
                 ,UnsafeContentHandlerResult
                 ,HTTPHeaders
    -- * Exporters
    -- *** Synchronous handlers
                 ,ngxExportSS
                 ,ngxExportSSS
                 ,ngxExportSLS
                 ,ngxExportBS
                 ,ngxExportBSS
                 ,ngxExportBLS
                 ,ngxExportYY
                 ,ngxExportBY
                 ,ngxExportIOYY
    -- *** Asynchronous handlers and services
                 ,ngxExportAsyncIOYY
                 ,ngxExportAsyncOnReqBody
                 ,ngxExportServiceIOYY
    -- *** Content handlers
                 ,ngxExportHandler
                 ,ngxExportDefHandler
                 ,ngxExportUnsafeHandler
                 ,ngxExportAsyncHandler
                 ,ngxExportAsyncHandlerOnReqBody
    -- *** Service hooks
                 ,ngxExportServiceHook
    -- * Accessing Nginx global objects
    -- *** Opaque pointers
                 ,ngxCyclePtr
                 ,ngxUpstreamMainConfPtr
                 ,ngxCachedTimePtr
    -- *** Primitive objects
                 ,ngxCachedPid
    -- * Accessing Nginx core functionality from Haskell handlers
                 ,TerminateWorkerProcess (..)
                 ,RestartWorkerProcess (..)
                 ,WorkerProcessIsExiting
                 ,FinalizeHTTPRequest (..)
    -- * Re-exported data constructors from /Foreign.C/
    -- | Re-exports are needed by exporters for marshalling in foreign calls.
                 ,Foreign.C.CInt (..)
                 ,Foreign.C.CUInt (..)
                 ) where

import           NgxExport.Internal.SafeFileLock

#if MIN_VERSION_template_haskell(2,12,0)
import           Language.Haskell.TH hiding (interruptible)
#else
import           Language.Haskell.TH
#endif
import           Foreign.C
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Data.IORef
import           System.IO.Unsafe
import           System.IO.Error
import           System.Posix.IO
import           System.Posix.Types
import           System.Posix.Signals hiding (Handler)
import           Control.Monad
import           Control.Monad.Loops
import           Control.DeepSeq
import qualified Control.Exception as E
import           Control.Exception hiding (Handler)
import           GHC.IO.Exception (ioe_errno)
import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Binary.Put
import           Data.Bits
import           Paths_ngx_export (version)
import           Data.Version

#if MIN_VERSION_template_haskell(2,11,0)
#define EXTRA_WILDCARD_BEFORE_CON _
#else
#define EXTRA_WILDCARD_BEFORE_CON
#endif

#if MIN_TOOL_VERSION_ghc(8,0,1)
pattern I :: (Num i, Integral a) => i -> a
#endif
pattern I i <- (fromIntegral -> i)
#if MIN_TOOL_VERSION_ghc(8,2,1)
{-# COMPLETE I :: CInt #-}
#endif

#if MIN_TOOL_VERSION_ghc(8,0,1)
pattern PtrLen :: Num l => Ptr s -> l -> (Ptr s, Int)
#endif
pattern PtrLen s l <- (s, I l)

#if MIN_TOOL_VERSION_ghc(8,0,1)
pattern ToBool :: (Num i, Eq i) => Bool -> i
#endif
pattern ToBool i <- (toBool -> i)
#if MIN_TOOL_VERSION_ghc(8,2,1)
{-# COMPLETE ToBool :: CUInt #-}
#endif

-- | The /4-tuple/ contains
--   /(content, content-type, HTTP-status, response-headers)/.
type ContentHandlerResult = (L.ByteString, B.ByteString, Int, HTTPHeaders)

-- | The /3-tuple/ contains /(content, content-type, HTTP-status)/.
--
-- Both the /content/ and the /content-type/ are supposed to be referring to
-- low-level string literals that do not need to be freed upon an HTTP request
-- termination and must not be garbage-collected in the Haskell RTS.
type UnsafeContentHandlerResult = (B.ByteString, B.ByteString, Int)

-- | A list of HTTP headers comprised of /name-value/ pairs.
type HTTPHeaders = [(B.ByteString, B.ByteString)]

data NgxExport = SS              (String -> String)
               | SSS             (String -> String -> String)
               | SLS             ([String] -> String)
               | BS              (String -> Bool)
               | BSS             (String -> String -> Bool)
               | BLS             ([String] -> Bool)
               | YY              (B.ByteString -> L.ByteString)
               | BY              (B.ByteString -> Bool)
               | IOYY            (B.ByteString -> Bool -> IO L.ByteString)
               | IOYYY           (L.ByteString -> B.ByteString ->
                                     IO L.ByteString)
               | Handler         (B.ByteString -> ContentHandlerResult)
               | UnsafeHandler   (B.ByteString -> UnsafeContentHandlerResult)
               | AsyncHandler    (B.ByteString -> IO ContentHandlerResult)
               | AsyncHandlerRB  (L.ByteString -> B.ByteString ->
                                     IO ContentHandlerResult)

data NgxExportTypeAmbiguityTag = Unambiguous
                               | YYSync
                               | YYDefHandler
                               | IOYYSync
                               | IOYYAsync

do
    TyConI (DataD _ _ _ EXTRA_WILDCARD_BEFORE_CON tCs _) <-
        reify ''NgxExport
    TyConI (DataD _ _ _ EXTRA_WILDCARD_BEFORE_CON aCs _) <-
        reify ''NgxExportTypeAmbiguityTag
    let tName = mkName "exportType"
        aName = mkName "exportTypeAmbiguity"
        tCons = map (\(NormalC con [(_, typ)]) -> (con, typ)) tCs
        aCons = map (\(NormalC con []) -> con) aCs
    sequence $
        [sigD tName [t|NgxExport -> IO CInt|]
        ,funD tName $
             map (\(fst -> c, i) ->
                    clause [conP c [wildP]] (normalB [|return i|]) []
                 ) (zip tCons [1 ..] :: [((Name, Type), Int)])
        ,sigD aName [t|NgxExportTypeAmbiguityTag -> IO CInt|]
        ,funD aName $
             map (\(c, i) ->
                    clause [conP c []] (normalB [|return i|]) []
                 ) (zip aCons [0 ..] :: [(Name, Int)])
        ]
        ++
        map (\(c, t) -> tySynD (mkName $ nameBase c) [] $ return t) tCons

ngxExport' :: (Name -> Q Exp) ->
    Name -> Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExport' m e a h t f = sequence
    [sigD nameFt typeFt
    ,funD nameFt $ body [|exportType $cefVar|]
    ,ForeignD . ExportF CCall ftName nameFt <$> typeFt
    ,sigD nameFta typeFta
    ,funD nameFta $ body [|exportTypeAmbiguity $(conE a)|]
    ,ForeignD . ExportF CCall ftaName nameFta <$> typeFta
    ,sigD nameF t
    ,funD nameF $ body [|$(varE h) $efVar|]
    ,ForeignD . ExportF CCall fName nameF <$> t
    ]
    where efVar   = m f
          cefVar  = conE e `appE` efVar
          fName   = "ngx_hs_" ++ nameBase f
          nameF   = mkName fName
          ftName  = "type_" ++ fName
          nameFt  = mkName ftName
          typeFt  = [t|IO CInt|]
          ftaName = "ambiguity_" ++ fName
          nameFta = mkName ftaName
          typeFta = [t|IO CInt|]
          body b  = [clause [] (normalB b) []]

ngxExport :: Name -> Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExport = ngxExport' varE

ngxExportC :: Name -> Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExportC = ngxExport' $ infixE (Just $ varE 'const) (varE '(.)) . Just . varE

-- | Exports a function of type
--
-- @
-- 'String' -> 'String'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportSS :: Name -> Q [Dec]
ngxExportSS =
    ngxExport 'SS 'Unambiguous 'sS
    [t|CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'String' -> 'String' -> 'String'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportSSS :: Name -> Q [Dec]
ngxExportSSS =
    ngxExport 'SSS 'Unambiguous 'sSS
    [t|CString -> CInt -> CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- ['String'] -> 'String'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportSLS :: Name -> Q [Dec]
ngxExportSLS =
    ngxExport 'SLS 'Unambiguous 'sLS
    [t|Ptr NgxStrType -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'String' -> 'Bool'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportBS :: Name -> Q [Dec]
ngxExportBS =
    ngxExport 'BS 'Unambiguous 'bS
    [t|CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'String' -> 'String' -> 'Bool'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportBSS :: Name -> Q [Dec]
ngxExportBSS =
    ngxExport 'BSS 'Unambiguous 'bSS
    [t|CString -> CInt -> CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- ['String'] -> 'Bool'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportBLS :: Name -> Q [Dec]
ngxExportBLS =
    ngxExport 'BLS 'Unambiguous 'bLS
    [t|Ptr NgxStrType -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'L.ByteString'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportYY :: Name -> Q [Dec]
ngxExportYY =
    ngxExport 'YY 'YYSync 'yY
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'Bool'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportBY :: Name -> Q [Dec]
ngxExportBY =
    ngxExport 'BY 'Unambiguous 'bY
    [t|CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'IO' 'L.ByteString'
-- @
--
-- for using in directive __/haskell_run/__.
ngxExportIOYY :: Name -> Q [Dec]
ngxExportIOYY =
    ngxExportC 'IOYY 'IOYYSync 'ioyY
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'IO' 'L.ByteString'
-- @
--
-- for using in directive __/haskell_run_async/__.
ngxExportAsyncIOYY :: Name -> Q [Dec]
ngxExportAsyncIOYY =
    ngxExportC 'IOYY 'IOYYAsync 'asyncIOYY
    [t|CString -> CInt -> CInt -> CInt -> Ptr CUInt -> CUInt -> CUInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))|]

-- | Exports a function of type
--
-- @
-- 'L.ByteString' -> 'B.ByteString' -> 'IO' 'L.ByteString'
-- @
--
-- for using in directive __/haskell_run_async_on_request_body/__.
--
-- The first argument of the exported function contains buffers of the client
-- request body.
ngxExportAsyncOnReqBody :: Name -> Q [Dec]
ngxExportAsyncOnReqBody =
    ngxExport 'IOYYY 'Unambiguous 'asyncIOYYY
    [t|Ptr NgxStrType -> Ptr NgxStrType -> CInt ->
       CString -> CInt -> CInt -> CUInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'Bool' -> 'IO' 'L.ByteString'
-- @
--
-- for using in directives __/haskell_run_service/__ and
-- __/haskell_service_var_update_callback/__.
--
-- The boolean argument of the exported function marks that the service is
-- being run for the first time.
ngxExportServiceIOYY :: Name -> Q [Dec]
ngxExportServiceIOYY =
    ngxExport 'IOYY 'IOYYAsync 'asyncIOYY
    [t|CString -> CInt -> CInt -> CInt -> Ptr CUInt -> CUInt -> CUInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'ContentHandlerResult'
-- @
--
-- for using in directives __/haskell_content/__ and
-- __/haskell_static_content/__.
ngxExportHandler :: Name -> Q [Dec]
ngxExportHandler =
    ngxExport 'Handler 'Unambiguous 'handler
    [t|CString -> CInt -> Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr CString -> Ptr CSize -> Ptr (StablePtr B.ByteString) -> Ptr CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr (StablePtr L.ByteString) ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'L.ByteString'
-- @
--
-- for using in directives __/haskell_content/__ and
-- __/haskell_static_content/__.
ngxExportDefHandler :: Name -> Q [Dec]
ngxExportDefHandler =
    ngxExport 'YY 'YYDefHandler 'defHandler
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'UnsafeContentHandlerResult'
-- @
--
-- for using in directive __/haskell_unsafe_content/__.
ngxExportUnsafeHandler :: Name -> Q [Dec]
ngxExportUnsafeHandler =
    ngxExport 'UnsafeHandler 'Unambiguous 'unsafeHandler
    [t|CString -> CInt -> Ptr CString -> Ptr CSize ->
       Ptr CString -> Ptr CSize -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'IO' 'ContentHandlerResult'
-- @
--
-- for using in directive __/haskell_async_content/__.
ngxExportAsyncHandler :: Name -> Q [Dec]
ngxExportAsyncHandler =
    ngxExport 'AsyncHandler 'Unambiguous 'asyncHandler
    [t|CString -> CInt -> CInt -> CUInt ->
       Ptr CString -> Ptr CSize -> Ptr (StablePtr B.ByteString) -> Ptr CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr (StablePtr L.ByteString) ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))|]

-- | Exports a function of type
--
-- @
-- 'L.ByteString' -> 'B.ByteString' -> 'IO' 'ContentHandlerResult'
-- @
--
-- for using in directive __/haskell_async_content_on_request_body/__.
--
-- The first argument of the exported function contains buffers of the client
-- request body.
ngxExportAsyncHandlerOnReqBody :: Name -> Q [Dec]
ngxExportAsyncHandlerOnReqBody =
    ngxExport 'AsyncHandlerRB 'Unambiguous 'asyncHandlerRB
    [t|Ptr NgxStrType -> Ptr NgxStrType -> CInt ->
       CString -> CInt -> CInt -> CUInt ->
       Ptr CString -> Ptr CSize -> Ptr (StablePtr B.ByteString) -> Ptr CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr (StablePtr L.ByteString) ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))|]

-- | Exports a function of type
--
-- @
-- 'B.ByteString' -> 'IO' 'L.ByteString'
-- @
--
-- for using in directives __/haskell_service_hook/__ and
-- __/haskell_service_update_hook/__.
ngxExportServiceHook :: Name -> Q [Dec]
ngxExportServiceHook =
    ngxExportC 'IOYY 'IOYYSync 'ioyYWithFree
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

data NgxStrType = NgxStrType CSize CString

instance Storable NgxStrType where
    alignment = const $ max (alignment (undefined :: CSize))
                            (alignment (undefined :: CString))
    sizeOf = (2 *) . alignment  -- must always be correct for
                                -- aligned struct ngx_str_t
    peek p = do
        n <- peekByteOff p 0
        s <- peekByteOff p $ alignment (undefined :: NgxStrType)
        return $ NgxStrType n s
    poke p x@(NgxStrType n s) = do
        poke (castPtr p) n
        poke (plusPtr p $ alignment x) s

data ServiceHookInterrupt = ServiceHookInterrupt

instance Exception ServiceHookInterrupt
instance Show ServiceHookInterrupt where
    show = const "Service was interrupted by a service hook"

newtype ServiceSomeInterrupt = ServiceSomeInterrupt String

instance Exception ServiceSomeInterrupt
instance Show ServiceSomeInterrupt where
    show (ServiceSomeInterrupt s) = s

-- | Terminates the worker process.
--
-- Being thrown from a service, this exception makes Nginx log the supplied
-- message and terminate the worker process without respawning. This can be
-- useful when the service is unable to read its configuration from the Nginx
-- configuration script or to perform an important initialization action.
--
-- @since 1.6.2
newtype TerminateWorkerProcess =
    TerminateWorkerProcess String  -- ^ Contains the message to log
    deriving Eq

instance Exception TerminateWorkerProcess
instance Show TerminateWorkerProcess where
    show (TerminateWorkerProcess s) = s

-- | Restarts the worker process.
--
-- The same as 'TerminateWorkerProcess', except that a new worker process shall
-- be spawned by the Nginx master process in place of the current one.
--
-- @since 1.6.3
newtype RestartWorkerProcess =
    RestartWorkerProcess String  -- ^ Contains the message to log
    deriving Eq

instance Exception RestartWorkerProcess
instance Show RestartWorkerProcess where
    show (RestartWorkerProcess s) = s

-- | Signals that the worker process is exiting.
--
-- This asynchronous exception is thrown from the Nginx core to all services
-- with 'cancelWith' when the working process is exiting. An exception handler
-- that catches this exception is expected to perform the service's specific
-- cleanup and finalization actions.
--
-- @since 1.6.4
data WorkerProcessIsExiting = WorkerProcessIsExiting deriving (Show, Eq)

instance Exception WorkerProcessIsExiting where
    fromException = asyncExceptionFromException
    toException = asyncExceptionToException

-- | Finalizes the HTTP request.
--
-- Being thrown from an asynchronous variable handler, this exception makes
-- Nginx finalize the current HTTP request with the supplied HTTP status and
-- an optional body. If the body is /Nothing/ then the response will be styled
-- by the Nginx core.
--
-- @since 1.6.3
data FinalizeHTTPRequest =
    FinalizeHTTPRequest Int (Maybe String)  -- ^ Contains HTTP status and body
    deriving Eq

instance Exception FinalizeHTTPRequest
instance Show FinalizeHTTPRequest where
    show (FinalizeHTTPRequest _ (Just s)) = s
    show (FinalizeHTTPRequest _ Nothing) = ""

safeMallocBytes :: Int -> IO (Ptr a)
safeMallocBytes =
    flip catchIOError (const $ return nullPtr) . mallocBytes
{-# INLINE safeMallocBytes #-}

safeNewCStringLen :: String -> IO CStringLen
safeNewCStringLen =
    flip catchIOError (const $ return (nullPtr, -1)) . newCStringLen
{-# INLINE safeNewCStringLen #-}

peekNgxStringArrayLen :: (CStringLen -> IO a) -> Ptr NgxStrType -> Int -> IO [a]
peekNgxStringArrayLen f x = sequence .
    foldr (\k ->
            ((peekElemOff x k >>= (\(NgxStrType (I m) y) -> f (y, m))) :)
          ) [] . flip take [0 ..]
{-# SPECIALIZE INLINE peekNgxStringArrayLen ::
    (CStringLen -> IO String) -> Ptr NgxStrType -> Int ->
        IO [String] #-}
{-# SPECIALIZE INLINE peekNgxStringArrayLen ::
    (CStringLen -> IO B.ByteString) -> Ptr NgxStrType -> Int ->
        IO [B.ByteString] #-}

peekNgxStringArrayLenLS :: Ptr NgxStrType -> Int -> IO [String]
peekNgxStringArrayLenLS =
    peekNgxStringArrayLen peekCStringLen

peekNgxStringArrayLenY :: Ptr NgxStrType -> Int -> IO L.ByteString
peekNgxStringArrayLenY =
    (fmap L.fromChunks .) . peekNgxStringArrayLen B.unsafePackCStringLen

pokeCStringLen :: Storable a => CString -> a -> Ptr CString -> Ptr a -> IO ()
pokeCStringLen x n p s = poke p x >> poke s n
{-# SPECIALIZE INLINE pokeCStringLen ::
    CString -> CInt -> Ptr CString -> Ptr CInt -> IO () #-}
{-# SPECIALIZE INLINE pokeCStringLen ::
    CString -> CSize -> Ptr CString -> Ptr CSize -> IO () #-}

toBuffers :: L.ByteString -> Ptr NgxStrType -> IO (Ptr NgxStrType, Int)
toBuffers (L.null -> True) _ =
    return (nullPtr, 0)
toBuffers s p = do
    let n = L.foldlChunks (const . succ) 0 s
    if n == 1 && p /= nullPtr
        then do
            B.unsafeUseAsCStringLen (head $ L.toChunks s) $
                \(x, I l) -> poke p $ NgxStrType l x
            return (p, 1)
        else do
            t <- safeMallocBytes $ n * sizeOf (undefined :: NgxStrType)
            if t == nullPtr
                then return (nullPtr, -1)
                else (t, ) <$>
                        L.foldlChunks
                            (\a c -> do
                                off <- a
                                B.unsafeUseAsCStringLen c $
                                    \(x, I l) ->
                                        pokeElemOff t off $ NgxStrType l x
                                return $ off + 1
                            ) (return 0) s

pokeLazyByteString :: L.ByteString ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO ()
pokeLazyByteString s p pl spd = do
    pv <- peek p
    PtrLen t l <- toBuffers s pv
    when (l /= 1 || pv == nullPtr) (poke p t) >> poke pl l
    when (t /= nullPtr) $ newStablePtr s >>= poke spd

pokeContentTypeAndStatus :: B.ByteString ->
    Ptr CString -> Ptr CSize -> Ptr CInt -> CInt -> IO CSize
pokeContentTypeAndStatus ct pct plct pst st = do
    PtrLen sct lct <- B.unsafeUseAsCStringLen ct return
    pokeCStringLen sct lct pct plct >> poke pst st
    return lct

peekRequestBodyChunks :: Ptr NgxStrType -> Ptr NgxStrType -> Int ->
    IO L.ByteString
peekRequestBodyChunks tmpf b m =
    if tmpf /= nullPtr
        then do
            c <- peek tmpf >>=
                (\(NgxStrType (I l) s) -> peekCStringLen (s, l)) >>=
                    L.readFile
            L.length c `seq` return c
        else peekNgxStringArrayLenY b m

pokeAsyncHandlerData :: B.ByteString -> Ptr CString -> Ptr CSize ->
    Ptr (StablePtr B.ByteString) -> Ptr CInt -> CInt -> HTTPHeaders ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr (StablePtr L.ByteString) -> IO ()
pokeAsyncHandlerData ct pct plct spct pst st rhs prhs plrhs sprhs = do
    lct <- pokeContentTypeAndStatus ct pct plct pst st
    when (lct > 0) $ newStablePtr ct >>= poke spct
    pokeLazyByteString (fromHTTPHeaders rhs) prhs plrhs sprhs

safeHandler :: Ptr CString -> Ptr CInt -> IO CUInt -> IO CUInt
safeHandler p pl = handle $ \e -> do
    PtrLen x l <- safeNewCStringLen $ show (e :: SomeException)
    pokeCStringLen x l p pl
    return 1

safeYYHandler :: IO (L.ByteString, CUInt) -> IO (L.ByteString, CUInt)
safeYYHandler = handle $ \e ->
    return (C8L.pack $ show (e :: SomeException), 1)
{-# INLINE safeYYHandler #-}

safeAsyncYYHandler :: IO (L.ByteString, (CUInt, Bool)) ->
    IO (L.ByteString, (CUInt, Bool))
safeAsyncYYHandler = handle $ \e ->
    return (C8L.pack $ show e,
            (case fromException e of
                Just ServiceHookInterrupt -> 2
                _ -> case fromException e of
                    Just (TerminateWorkerProcess _) -> 3
                    _ -> case fromException e of
                        Just (RestartWorkerProcess _) -> 4
                        _ -> case fromException e of
                            Just (FinalizeHTTPRequest st (Just _)) ->
                                0x80000000 .|. fromIntegral st
                            Just (FinalizeHTTPRequest st Nothing) ->
                                0xC0000000 .|. fromIntegral st
                            _ -> case fromException e of
                                Just (ServiceSomeInterrupt _) -> 5
                                _ -> 1
            ,case asyncExceptionFromException e of
                Just WorkerProcessIsExiting -> True
                _ -> False
            )
           )
{-# INLINE safeAsyncYYHandler #-}

fromHTTPHeaders :: HTTPHeaders -> L.ByteString
fromHTTPHeaders = L.fromChunks . foldr (\(z -> a, z -> b) -> ([a, b] ++)) []
    where z s | B.null s = B.singleton 0
              | otherwise = s

isIOError :: Errno -> IOError -> Bool
isIOError (Errno e) = (Just e ==) . ioe_errno
{-# INLINE isIOError #-}

isEINTR :: IOError -> Bool
isEINTR = isIOError eINTR
{-# INLINE isEINTR #-}

isEDEADLK :: IOError -> Bool
isEDEADLK = isIOError eDEADLK
{-# INLINE isEDEADLK #-}

sS :: SS -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
sS f x (I n) p pl =
    safeHandler p pl $ do
        PtrLen s l <- f <$> peekCStringLen (x, n)
                        >>= newCStringLen
        pokeCStringLen s l p pl
        return 0

sSS :: SSS -> CString -> CInt -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
sSS f x (I n) y (I m) p pl =
    safeHandler p pl $ do
        PtrLen s l <- f <$> peekCStringLen (x, n)
                        <*> peekCStringLen (y, m)
                        >>= newCStringLen
        pokeCStringLen s l p pl
        return 0

sLS :: SLS -> Ptr NgxStrType -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
sLS f x (I n) p pl =
    safeHandler p pl $ do
        PtrLen s l <- f <$> peekNgxStringArrayLenLS x n
                        >>= newCStringLen
        pokeCStringLen s l p pl
        return 0

yY :: YY -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
yY f x (I n) p pl spd = do
    (s, r) <- safeYYHandler $ do
        s <- f <$> B.unsafePackCStringLen (x, n)
        fmap (, 0) $ return $!! s
    pokeLazyByteString s p pl spd
    return r

ioyYCommon :: (CStringLen -> IO B.ByteString) ->
    IOYY -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
ioyYCommon pack f x (I n) p pl spd = do
    (s, r) <- safeYYHandler $ do
        s <- pack (x, n) >>= flip f False
        fmap (, 0) $ return $!! s
    pokeLazyByteString s p pl spd
    return r

ioyY :: IOYY -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
ioyY = ioyYCommon B.unsafePackCStringLen

ioyYWithFree :: IOYY -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
ioyYWithFree = ioyYCommon B.unsafePackMallocCStringLen

asyncIOFlag1b :: B.ByteString
asyncIOFlag1b = L.toStrict $ runPut $ putWord8 1

asyncIOFlag8b :: B.ByteString
asyncIOFlag8b = L.toStrict $ runPut $ putWord64host 1

asyncIOCommon :: IO (L.ByteString, Bool) ->
    CInt -> Bool -> Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))
asyncIOCommon a (I fd) efd p pl pr spd = mask_ $
    async
    (do
        (s, (r, exiting)) <- safeAsyncYYHandler $ do
            (s, exiting) <- a
            interruptible $ fmap (, (0, exiting)) $ return $!! s
        pokeLazyByteString s p pl spd
        poke pr r
        if exiting
            then unless efd closeChannel
            else uninterruptibleMask_ $
                    if efd
                        then writeFlag8b
                        else writeFlag1b >> closeChannel
    ) >>= newStablePtr
    where writeBufN n s =
              iterateUntilM (>= n)
              (\w -> (w +) <$>
                  fdWriteBuf fd (plusPtr s $ fromIntegral w) (n - w)
                  `catchIOError`
                  (\e -> return $ if isEINTR e
                                      then 0
                                      else n + 1
                  )
              ) 0 >>= \w -> when (w > n) cleanupOnWriteError
          writeFlag1b = B.unsafeUseAsCString asyncIOFlag1b $ writeBufN 1
          writeFlag8b = B.unsafeUseAsCString asyncIOFlag8b $ writeBufN 8
          closeChannel = closeFd fd `catchIOError` const (return ())
          -- FIXME: cleanupOnWriteError should free all previously allocated
          -- data and stable pointers. However, leaving this not implemented
          -- seems to be safe because Nginx won't close the event channel or
          -- delete the request object (for request-driven handlers)
          -- regardless of the Haskell handler's duration.
          cleanupOnWriteError = return ()

asyncIOYY :: IOYY -> CString -> CInt ->
    CInt -> CInt -> Ptr CUInt -> CUInt -> CUInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))
asyncIOYY f x (I n) fd (I fdlk) active (ToBool efd) (ToBool fstRun) =
    asyncIOCommon
    (do
        exiting <- if fstRun && fdlk /= -1
                       then getBestLockImpl fdlk >>= acquireLock fdlk
                       else return False
        if exiting
            then return (L.empty, True)
            else do
                when fstRun $ poke active 1
                x' <- B.unsafePackCStringLen (x, n)
                interruptible $ (, False) <$> f x' fstRun
    ) fd efd
    where acquireLock lk cmd = snd <$>
              iterateUntil fst
              (interruptible (safeWaitToSetLock lk cmd >> return (True, False))
               `catches`
               [E.Handler $ \e ->
                   if isEINTR e
                       then return (False, False)
                       else do
                           -- wait some time to avoid fastly repeated calls;
                           -- threadDelay is interruptible even in exception
                           -- handlers
                           exiting <- (threadDelay 500000 >> return False)
                               `catches`
                               [E.Handler $ return . (== WorkerProcessIsExiting)
                               ,E.Handler (const $ return False ::
                                              SomeException -> IO Bool
                                          )
                               ]
                           if exiting
                               then return (True, True)
                               else if isEDEADLK e
                                        then return (False, False)
                                        else throwIO $
                                            ServiceSomeInterrupt $ show e
               ,E.Handler $ return . (True, ) . (== WorkerProcessIsExiting)
               ,E.Handler (throwIO . ServiceSomeInterrupt . show ::
                              SomeException -> IO (Bool, Bool)
                          )
               ]
              )

asyncIOYYY :: IOYYY -> Ptr NgxStrType -> Ptr NgxStrType -> CInt ->
    CString -> CInt -> CInt -> CUInt -> Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))
asyncIOYYY f tmpf b (I m) x (I n) fd (ToBool efd) =
    asyncIOCommon
    (do
        b' <- peekRequestBodyChunks tmpf b m
        x' <- B.unsafePackCStringLen (x, n)
        interruptible $ (, False) <$> f b' x'
    ) fd efd

asyncHandler :: AsyncHandler -> CString -> CInt ->
    CInt -> CUInt ->
    Ptr CString -> Ptr CSize -> Ptr (StablePtr B.ByteString) -> Ptr CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr (StablePtr L.ByteString) ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))
asyncHandler f x (I n) fd (ToBool efd) pct plct spct pst
        prhs plrhs sprhs =
    asyncIOCommon
    (do
        x' <- B.unsafePackCStringLen (x, n)
        (s, ct, I st, rhs) <- interruptible $ do
            v <- f x'
            return $!! v
        pokeAsyncHandlerData ct pct plct spct pst st rhs prhs plrhs sprhs
        return (s, False)
    ) fd efd

asyncHandlerRB :: AsyncHandlerRB -> Ptr NgxStrType -> Ptr NgxStrType -> CInt ->
    CString -> CInt -> CInt -> CUInt ->
    Ptr CString -> Ptr CSize -> Ptr (StablePtr B.ByteString) -> Ptr CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr (StablePtr L.ByteString) ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr CUInt -> Ptr (StablePtr L.ByteString) -> IO (StablePtr (Async ()))
asyncHandlerRB f tmpf b (I m) x (I n) fd (ToBool efd) pct plct spct pst
        prhs plrhs sprhs =
    asyncIOCommon
    (do
        b' <- peekRequestBodyChunks tmpf b m
        x' <- B.unsafePackCStringLen (x, n)
        (s, ct, I st, rhs) <- interruptible $ do
            v <- f b' x'
            return $!! v
        pokeAsyncHandlerData ct pct plct spct pst st rhs prhs plrhs sprhs
        return (s, False)
    ) fd efd

bS :: BS -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bS f x (I n) p pl =
    safeHandler p pl $ do
        r <- fromBool . f <$> peekCStringLen (x, n)
        pokeCStringLen nullPtr 0 p pl
        return r

bSS :: BSS -> CString -> CInt -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bSS f x (I n) y (I m) p pl =
    safeHandler p pl $ do
        r <- (fromBool .) . f <$> peekCStringLen (x, n)
                              <*> peekCStringLen (y, m)
        pokeCStringLen nullPtr 0 p pl
        return r

bLS :: BLS -> Ptr NgxStrType -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bLS f x (I n) p pl =
    safeHandler p pl $ do
        r <- fromBool . f <$> peekNgxStringArrayLenLS x n
        pokeCStringLen nullPtr 0 p pl
        return r

bY :: BY -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bY f x (I n) p pl =
    safeHandler p pl $ do
        r <- fromBool . f <$> B.unsafePackCStringLen (x, n)
        pokeCStringLen nullPtr 0 p pl
        return r

handler :: Handler -> CString -> CInt -> Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr CString -> Ptr CSize -> Ptr (StablePtr B.ByteString) -> Ptr CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr (StablePtr L.ByteString) ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
handler f x (I n) p pl pct plct spct pst prhs plrhs sprhs spd =
    safeHandler pct pst $ do
        v@(s, ct, I st, rhs) <- f <$> B.unsafePackCStringLen (x, n)
        lct <- (return $!! v) >> pokeContentTypeAndStatus ct pct plct pst st
        (return $!! lct) >>
            pokeLazyByteString (fromHTTPHeaders rhs) prhs plrhs sprhs
        pokeLazyByteString s p pl spd
        when (lct > 0) $ newStablePtr ct >>= poke spct
        return 0

defHandler :: YY -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
defHandler f x (I n) p pl pe spd =
    safeHandler pe pl $ do
        s <- f <$> B.unsafePackCStringLen (x, n)
        pokeLazyByteString s p pl spd
        return 0

unsafeHandler :: UnsafeHandler -> CString -> CInt -> Ptr CString -> Ptr CSize ->
    Ptr CString -> Ptr CSize -> Ptr CInt -> IO CUInt
unsafeHandler f x (I n) p pl pct plct pst =
    safeHandler pct pst $ do
        v@(s, ct, I st) <- f <$> B.unsafePackCStringLen (x, n)
        (return $!! v) >> void (pokeContentTypeAndStatus ct pct plct pst st)
        PtrLen t l <- B.unsafeUseAsCStringLen s return
        pokeCStringLen t l p pl
        return 0

foreign export ccall ngxExportInstallSignalHandler :: IO ()
ngxExportInstallSignalHandler :: IO ()
ngxExportInstallSignalHandler = void $
    installHandler keyboardSignal Ignore Nothing

foreign export ccall ngxExportTerminateTask ::
    StablePtr (Async ()) -> IO ()
ngxExportTerminateTask ::
    StablePtr (Async ()) -> IO ()
ngxExportTerminateTask = deRefStablePtr >=>
    flip cancelWith WorkerProcessIsExiting

foreign export ccall ngxExportServiceHookInterrupt ::
    StablePtr (Async ()) -> IO ()
ngxExportServiceHookInterrupt ::
    StablePtr (Async ()) -> IO ()
ngxExportServiceHookInterrupt = deRefStablePtr >=>
    flip throwTo ServiceHookInterrupt . asyncThreadId

foreign export ccall ngxExportVersion ::
    Ptr CInt -> CInt -> IO CInt
ngxExportVersion ::
    Ptr CInt -> CInt -> IO CInt
ngxExportVersion x (I n) = fromIntegral <$>
    foldM (\k (I v) -> pokeElemOff x k v >> return (k + 1)) 0
        (take n $ versionBranch version)

-- | Returns an opaque pointer to the Nginx /cycle object/
--   for using it in C plugins.
--
-- The actual type of the returned pointer is
--
-- > ngx_cycle_t *
--
-- (the value of argument __/cycle/__ in the worker's initialization function).
ngxCyclePtr :: IO (Ptr ())
ngxCyclePtr = readIORef ngxCyclePtrStore

-- | Returns an opaque pointer to the Nginx /upstream main configuration/
--   for using it in C plugins.
--
-- The actual type of the returned pointer is
--
-- > ngx_http_upstream_main_conf_t *
--
-- (the value of expression
-- @ngx_http_cycle_get_module_main_conf(cycle, ngx_http_upstream_module)@ in
-- the worker's initialization function).
ngxUpstreamMainConfPtr :: IO (Ptr ())
ngxUpstreamMainConfPtr = readIORef ngxUpstreamMainConfPtrStore

-- | Returns an opaque pointer to the Nginx /cached time object/
--   for using it in C plugins.
--
-- The actual type of the returned pointer is
--
-- > volatile ngx_time_t **
--
-- (the /address/ of the Nginx global variable __/ngx_cached_time/__).
--
-- Be aware that time gotten from this pointer is not reliable in asynchronous
-- tasks and services as soon as it gets updated only when some event happens
-- inside the Nginx worker to which the task is bound and thus can be heavily
-- outdated.
ngxCachedTimePtr :: IO (Ptr (Ptr ()))
ngxCachedTimePtr = readIORef ngxCachedTimePtrStore

-- | Returns the /PID/ of the current worker process cached in Nginx.
--
-- @since 1.7.1
ngxCachedPid :: IO CPid
ngxCachedPid = readIORef ngxCachedPidStore

foreign export ccall ngxExportSetCyclePtr :: Ptr () -> IO ()
ngxExportSetCyclePtr :: Ptr () -> IO ()
ngxExportSetCyclePtr = writeIORef ngxCyclePtrStore

foreign export ccall ngxExportSetUpstreamMainConfPtr :: Ptr () -> IO ()
ngxExportSetUpstreamMainConfPtr :: Ptr () -> IO ()
ngxExportSetUpstreamMainConfPtr = writeIORef ngxUpstreamMainConfPtrStore

foreign export ccall ngxExportSetCachedTimePtr :: Ptr (Ptr ()) -> IO ()
ngxExportSetCachedTimePtr :: Ptr (Ptr ()) -> IO ()
ngxExportSetCachedTimePtr = writeIORef ngxCachedTimePtrStore

foreign export ccall ngxExportSetCachedPid :: CPid -> IO ()
ngxExportSetCachedPid :: CPid -> IO ()
ngxExportSetCachedPid = writeIORef ngxCachedPidStore

ngxCyclePtrStore :: IORef (Ptr ())
ngxCyclePtrStore = unsafePerformIO $ newIORef nullPtr
{-# NOINLINE ngxCyclePtrStore #-}

ngxUpstreamMainConfPtrStore :: IORef (Ptr ())
ngxUpstreamMainConfPtrStore = unsafePerformIO $ newIORef nullPtr
{-# NOINLINE ngxUpstreamMainConfPtrStore #-}

ngxCachedTimePtrStore :: IORef (Ptr (Ptr ()))
ngxCachedTimePtrStore = unsafePerformIO $ newIORef nullPtr
{-# NOINLINE ngxCachedTimePtrStore #-}

ngxCachedPidStore :: IORef CPid
ngxCachedPidStore = unsafePerformIO $ newIORef (-1)
{-# NOINLINE ngxCachedPidStore #-}

