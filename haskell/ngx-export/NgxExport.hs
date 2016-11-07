{-# LANGUAGE TemplateHaskell, ViewPatterns, PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport
-- Copyright   :  (c) Alexey Radkov 2016
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX)
--
-- Export regular haskell functions for using in directives of
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------

module NgxExport (
    -- * Exporters
                  ngxExportSS
                 ,ngxExportSSS
                 ,ngxExportSLS
                 ,ngxExportBS
                 ,ngxExportBSS
                 ,ngxExportBLS
                 ,ngxExportYY
                 ,ngxExportBY
                 ,ngxExportAsyncIOYY
                 ,ngxExportServiceIOYY
                 ,ngxExportHandler
                 ,ngxExportDefHandler
                 ,ngxExportUnsafeHandler
    -- * Re-exported data constructors from /"Foreign.C"/
    --   (for marshalling in foreign calls)
                 ,Foreign.C.CInt (..)
                 ,Foreign.C.CUInt (..)) where

import           Language.Haskell.TH
import           Foreign.C
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           System.IO.Error
import           System.Posix.IO
import           Control.Monad
import           Control.Exception hiding (Handler)
import           Control.Concurrent.Async
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L

pattern I l <- (fromIntegral -> l)
pattern PtrLen s l <- (s, I l)
pattern PtrLenFromMaybe s l <- (fromMaybe (nullPtr, -1) -> PtrLen s l)
pattern EmptyLBS <- (L.null -> True)

data NgxExport = SS            (String -> String)
               | SSS           (String -> String -> String)
               | SLS           ([String] -> String)
               | BS            (String -> Bool)
               | BSS           (String -> String -> Bool)
               | BLS           ([String] -> Bool)
               | YY            (B.ByteString -> L.ByteString)
               | BY            (B.ByteString -> Bool)
               | IOYY          (B.ByteString -> Bool -> IO L.ByteString)
               | Handler       (B.ByteString -> (L.ByteString, String, Int))
               | UnsafeHandler (B.ByteString ->
                                    (B.ByteString, B.ByteString, Int))

let name = mkName "exportType" in sequence
    [sigD name [t|NgxExport -> IO CInt|],
     funD name $
         map (\(c, i) -> clause [conP c [wildP]] (normalB [|return i|]) [])
             (zip ['SS, 'SSS, 'SLS, 'BS, 'BSS, 'BLS, 'YY, 'BY, 'IOYY,
                   'Handler, 'UnsafeHandler] [1 ..] :: [(Name, Int)])
    ]

ngxExport' :: (Name -> Q Exp) -> Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExport' m e h t f = sequence
    [funD nameFt $ body [|exportType $efVar|],
     ForeignD . ExportF CCall ftName nameFt <$> [t|IO CInt|],
     funD nameF $ body [|$hVar $efVar|],
     ForeignD . ExportF CCall fName nameF <$> t
    ]
    where hVar   = varE h
          efVar  = conE e `appE` m f
          fName  = "ngx_hs_" ++ nameBase f
          nameF  = mkName fName
          ftName = "type_" ++ fName
          nameFt = mkName ftName
          body b = [clause [] (normalB b) []]

ngxExport :: Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExport = ngxExport' varE

ngxExportC :: Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExportC = ngxExport' $ infixE (Just $ varE 'const) (varE '(.)) . Just . varE

-- | Exports a function of type
-- /'String' -> 'String'/
-- for using in directive /haskell_run/.
ngxExportSS =
    ngxExport 'SS 'sS
    [t|CString -> CInt -> Ptr CString -> IO CInt|]

-- | Exports a function of type
-- /'String' -> 'String' -> 'String'/
-- for using in directive /haskell_run/.
ngxExportSSS =
    ngxExport 'SSS 'sSS
    [t|CString -> CInt -> CString -> CInt -> Ptr CString -> IO CInt|]

-- | Exports a function of type
-- /['String'] -> 'String'/
-- for using in directive /haskell_run/.
ngxExportSLS =
    ngxExport 'SLS 'sLS
    [t|Ptr NgxStrType -> CInt -> Ptr CString -> IO CInt|]

-- | Exports a function of type
-- /'String' -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBS =
    ngxExport 'BS 'bS
    [t|CString -> CInt -> IO CUInt|]

-- | Exports a function of type
-- /'String' -> 'String' -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBSS =
    ngxExport 'BSS 'bSS
    [t|CString -> CInt -> CString -> CInt -> IO CUInt|]

-- | Exports a function of type
-- /['String'] -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBLS =
    ngxExport 'BLS 'bLS
    [t|Ptr NgxStrType -> CInt -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'L.ByteString'/
-- for using in directive /haskell_run/.
ngxExportYY =
    ngxExport 'YY 'yY
    [t|CString -> CInt -> Ptr CString -> IO CInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBY =
    ngxExport 'BY 'bY
    [t|CString -> CInt -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'IO' 'L.ByteString'/
-- for using in directive /haskell_run_async/.
ngxExportAsyncIOYY =
    ngxExportC 'IOYY 'ioyY
    [t|CString -> CInt ->
       CInt -> CUInt -> Ptr CString -> Ptr CSize -> Ptr CUInt -> IO ()|]

-- | Exports a function of type
-- /'B.ByteString' -> 'Bool' -> 'IO' 'L.ByteString'/
-- for using in directive /haskell_run_service/.
--
-- The boolean argument of the exported function marks that the service is
-- being run for the first time.
ngxExportServiceIOYY =
    ngxExport 'IOYY 'ioyY
    [t|CString -> CInt ->
       CInt -> CUInt -> Ptr CString -> Ptr CSize -> Ptr CUInt -> IO ()|]

-- | Exports a function of type
-- /'B.ByteString' -> ('L.ByteString', 'String', 'Int')/
-- for using in directives /haskell_content/ and /haskell_static_content/.
--
-- The first element in the returned /3-tuple/ of the exported function is
-- the /content/, the second is the /content type/, and the third is the
-- /HTTP status/.
ngxExportHandler =
    ngxExport 'Handler 'handler
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString -> Ptr CSize -> IO CInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'L.ByteString'/
-- for using in directives /haskell_content/ and /haskell_static_content/.
ngxExportDefHandler =
    ngxExport 'YY 'defHandler
    [t|CString -> CInt -> Ptr (Ptr NgxStrType) -> IO CInt|]

-- | Exports a function of type
-- /'B.ByteString' -> ('B.ByteString', 'B.ByteString', 'Int')/
-- for using in directive /haskell_unsafe_content/.
--
-- The first element in the returned /3-tuple/ of the exported function is
-- the /content/, the second is the /content type/, and the third is the
-- /HTTP status/. Both the content and the content type are supposed to be
-- referring to low-level string literals which do not need to be freed upon
-- the request termination and must not be garbage-collected in the Haskell RTS.
ngxExportUnsafeHandler =
    ngxExport 'UnsafeHandler 'unsafeHandler
    [t|CString -> CInt ->
       Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize -> IO CInt|]

data NgxStrType = NgxStrType CSize CString

instance Storable NgxStrType where
    alignment _ = max (alignment (undefined :: CSize))
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

catchAlloc :: IO (Ptr a) -> IO (Ptr a)
catchAlloc = (`catchIOError` const (return nullPtr))

peekNgxStringArrayLen :: Ptr NgxStrType -> Int -> IO [String]
peekNgxStringArrayLen x n = sequence $
    foldr (\k ->
              ((peekElemOff x k >>=
                  (\(NgxStrType (I m) y) ->
                      peekCStringLen (y, m))) :)) [] [0 .. n - 1]

pokeCStringLen :: CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()
pokeCStringLen x n p s = poke p x >> poke s n

toSingleBuffer :: L.ByteString -> IO (Maybe (CString, Int))
toSingleBuffer EmptyLBS =
    return $ Just (nullPtr, 0)
toSingleBuffer s = do
    let I l = L.length s
    t <- catchAlloc $ mallocBytes l
    if t /= nullPtr
        then do
            void $ L.foldlChunks
                (\a s -> do
                    off <- a
                    let l = B.length s
                    B.unsafeUseAsCString s $ flip (copyBytes $ plusPtr t off) l
                    return $ off + l
                ) (return 0) s
            return $ Just (t, l)
        else return Nothing

toBuffers :: L.ByteString -> IO (Maybe (Ptr NgxStrType, Int))
toBuffers EmptyLBS =
    return $ Just (nullPtr, 0)
toBuffers s = do
    t <- catchAlloc $ mallocBytes $
        L.foldlChunks (const . succ) 0 s * sizeOf (undefined :: NgxStrType)
    l <- L.foldlChunks
        (\a s -> do
            off <- a
            maybe (return Nothing)
                (\off -> do
                    let l = B.length s
                    -- l cannot be zero at this point because intermediate
                    -- chunks of a lazy ByteString cannot be empty which is
                    -- the consequence of Monoid laws applied when it grows
                    dst <- catchAlloc $ mallocBytes l
                    if dst /= nullPtr
                        then do
                            B.unsafeUseAsCString s $ flip (copyBytes dst) l
                            pokeElemOff t off $ NgxStrType (fromIntegral l) dst
                            return $ Just $ off + 1
                        else do
                            mapM_
                                (peekElemOff t >=> \(NgxStrType _ x) -> free x)
                                [0 .. off - 1]  -- [0 .. -1] makes [], so wise!
                            free t
                            return Nothing
                ) off
        ) (return $ if t /= nullPtr then Just 0 else Nothing) s
    return $ l >>= Just . (,) t

sS :: NgxExport -> CString -> CInt -> Ptr CString -> IO CInt
sS (SS f) x (I n) p = do
    PtrLen s l <- f <$> peekCStringLen (x, n)
                    >>= newCStringLen
    poke p s
    return l

sSS :: NgxExport -> CString -> CInt -> CString -> CInt -> Ptr CString -> IO CInt
sSS (SSS f) x (I n) y (I m) p = do
    PtrLen s l <- f <$> peekCStringLen (x, n)
                    <*> peekCStringLen (y, m)
                    >>= newCStringLen
    poke p s
    return l

sLS :: NgxExport -> Ptr NgxStrType -> CInt -> Ptr CString -> IO CInt
sLS (SLS f) x (I n) p = do
    PtrLen s l <- f <$> peekNgxStringArrayLen x n
                    >>= newCStringLen
    poke p s
    return l

yY :: NgxExport -> CString -> CInt -> Ptr CString -> IO CInt
yY (YY f) x (I n) p = do
    s <- f <$> B.unsafePackCStringLen (x, n)
    PtrLenFromMaybe t l <- toSingleBuffer s
    poke p t
    return l

ioyY :: NgxExport -> CString -> CInt ->
    CInt -> CUInt -> Ptr CString -> Ptr CSize -> Ptr CUInt -> IO ()
ioyY (IOYY f) x (I n) (I fd) ((/= 0) -> fstRun) p pl r =
    void . async $
    (do
    s <- (Right <$> (B.unsafePackCStringLen (x, n) >>= flip f fstRun))
        `catch` \e -> return $ Left $ show (e :: SomeException)
    either
        (\s -> do
            PtrLen x l <- newCStringLen s
            pokeCStringLen x l p pl
            poke r 1
        )
        (\s -> do
            PtrLenFromMaybe t l <- toSingleBuffer s
            pokeCStringLen t l p pl
            poke r 0
        ) s
    )
    `finally` ((fdWrite fd "0" >> closeFd fd) `catchIOError` const (return ()))

bS :: NgxExport -> CString -> CInt -> IO CUInt
bS (BS f) x (I n) =
    fromBool . f <$> peekCStringLen (x, n)

bSS :: NgxExport -> CString -> CInt -> CString -> CInt -> IO CUInt
bSS (BSS f) x (I n) y (I m) =
    (fromBool .) . f <$> peekCStringLen (x, n)
                     <*> peekCStringLen (y, m)

bLS :: NgxExport -> Ptr NgxStrType -> CInt -> IO CUInt
bLS (BLS f) x (I n) =
    fromBool . f <$> peekNgxStringArrayLen x n

bY :: NgxExport -> CString -> CInt -> IO CUInt
bY (BY f) x (I n) =
    fromBool . f <$> B.unsafePackCStringLen (x, n)

handler :: NgxExport -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString -> Ptr CSize -> IO CInt
handler (Handler f) x (I n) ps pls pt plt = do
    (s, mt, I st) <- f <$> B.unsafePackCStringLen (x, n)
    PtrLenFromMaybe t l <- toBuffers s
    poke ps t
    poke pls l
    PtrLen smt lmt <- newCStringLen mt
    pokeCStringLen smt lmt pt plt
    return st

defHandler :: NgxExport -> CString -> CInt -> Ptr (Ptr NgxStrType) -> IO CInt
defHandler (YY f) x (I n) ps = do
    s <- f <$> B.unsafePackCStringLen (x, n)
    PtrLenFromMaybe t l <- toBuffers s
    poke ps t
    return l

unsafeHandler :: NgxExport -> CString -> CInt ->
    Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize -> IO CInt
unsafeHandler (UnsafeHandler f) x (I n) ps pls pt plt = do
    (s, mt, I st) <- f <$> B.unsafePackCStringLen (x, n)
    PtrLen t lt <- B.unsafeUseAsCStringLen s return
    pokeCStringLen t lt ps pls
    PtrLen smt lmt <- B.unsafeUseAsCStringLen mt return
    pokeCStringLen smt lmt pt plt
    return st

