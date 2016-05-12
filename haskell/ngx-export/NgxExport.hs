{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module NgxExport (module Foreign.C
                 ,ngxExportSS
                 ,ngxExportSSS
                 ,ngxExportSLS
                 ,ngxExportBS
                 ,ngxExportBSS
                 ,ngxExportBLS
                 ,ngxExportYY
                 ,ngxExportBY
                 ,ngxExportHandler
                 ,ngxExportDefHandler
                 ,ngxExportUnsafeHandler) where

import           Language.Haskell.TH
import           Foreign.C
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           System.IO.Error
import           Control.Monad
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L

ngxExport :: Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExport e h t f = sequence
    [funD nameFt $ body [|exportType $efVar|],
     fmap (ForeignD . ExportF CCall ftName nameFt) [t|IO CInt|],
     funD nameF $ body [|$hVar $efVar|],
     fmap (ForeignD . ExportF CCall fName nameF) t
    ]
    where hVar   = varE h
          efVar  = conE e `appE` varE f
          fName  = "ngx_hs_" ++ nameBase f
          nameF  = mkName fName
          ftName = "type_" ++ fName
          nameFt = mkName ftName
          body b = [clause [] (normalB b) []]

ngxExportSS =
    ngxExport 'SS 'sS
    [t|CString -> CInt -> Ptr CString -> IO CInt|]
ngxExportSSS =
    ngxExport 'SSS 'sSS
    [t|CString -> CInt -> CString -> CInt -> Ptr CString -> IO CInt|]
ngxExportSLS =
    ngxExport 'SLS 'sLS
    [t|Ptr NgxStrType -> CInt -> Ptr CString -> IO CInt|]
ngxExportBS =
    ngxExport 'BS 'bS
    [t|CString -> CInt -> IO CUInt|]
ngxExportBSS =
    ngxExport 'BSS 'bSS
    [t|CString -> CInt -> CString -> CInt -> IO CUInt|]
ngxExportBLS =
    ngxExport 'BLS 'bLS
    [t|Ptr NgxStrType -> CInt -> IO CUInt|]
ngxExportYY =
    ngxExport 'YY 'yY
    [t|CString -> CInt -> Ptr CString -> IO CInt|]
ngxExportBY =
    ngxExport 'BY 'bY
    [t|CString -> CInt -> IO CUInt|]
ngxExportHandler =
    ngxExport 'Handler 'handler
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString -> Ptr CSize -> IO CInt|]
ngxExportDefHandler =
    ngxExport 'YY 'defHandler
    [t|CString -> CInt -> Ptr (Ptr NgxStrType) -> IO CInt|]
ngxExportUnsafeHandler =
    ngxExport 'UnsafeHandler 'unsafeHandler
    [t|CString -> CInt ->
       Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize -> IO CInt|]

data NgxExport = SS (String -> String)
               | SSS (String -> String -> String)
               | SLS ([String] -> String)
               | BS (String -> Bool)
               | BSS (String -> String -> Bool)
               | BLS ([String] -> Bool)
               | YY (B.ByteString -> L.ByteString)
               | BY (B.ByteString -> Bool)
               | Handler (B.ByteString -> (L.ByteString, String, Int))
               | UnsafeHandler (B.ByteString ->
                                    (B.ByteString, B.ByteString, Int))

instance Enum NgxExport where
    toEnum _ = SS id    -- not used
    fromEnum (SS _)            = 1
    fromEnum (SSS _)           = 2
    fromEnum (SLS _)           = 3
    fromEnum (BS _)            = 4
    fromEnum (BSS _)           = 5
    fromEnum (BLS _)           = 6
    fromEnum (YY _)            = 7
    fromEnum (BY _)            = 8
    fromEnum (Handler _)       = 9
    fromEnum (UnsafeHandler _) = 10

exportType :: NgxExport -> IO CInt
exportType = return . fromIntegral . fromEnum

data NgxStrType = NgxStrType CSize CString

instance Storable NgxStrType where
    alignment _ = max (alignment (undefined :: CSize))
                      (alignment (undefined :: CString))
    sizeOf = (* 2) . alignment  -- must always be correct for
                                -- aligned struct ngx_str_t
    peek p = do
        n <- peekByteOff p 0
        s <- peekByteOff p $ alignment (undefined :: NgxStrType)
        return $ NgxStrType n s
    poke p a@(NgxStrType n s) = do
        poke (castPtr p) n
        poke (plusPtr p $ alignment a) s

catchAlloc :: IO (Ptr a) -> IO (Ptr a)
catchAlloc = (`catchIOError` const (return nullPtr))

peekNgxStringArrayLen :: Ptr NgxStrType -> Int -> IO [String]
peekNgxStringArrayLen x n = sequence $
    foldr (\k ->
              ((peekElemOff x k >>=
                  (\(NgxStrType (fromIntegral -> m) y) ->
                      peekCStringLen (y, m))) :)) [] [0 .. n - 1]

pokeCStringLen :: CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()
pokeCStringLen x n p s = poke p x >> poke s n

toSingleBuffer :: L.ByteString -> IO (Maybe (CString, Int))
toSingleBuffer (L.uncons -> Nothing) =
    return $ Just (nullPtr, 0)
toSingleBuffer s = do
    let (fromIntegral -> l) = L.length s
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
toBuffers (L.uncons -> Nothing) =
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
                    -- l cannot be zero at this point
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
sS (SS f) x (fromIntegral -> n) p = do
    (s, fromIntegral -> l) <- f <$> peekCStringLen (x, n)
                                >>= newCStringLen
    poke p s
    return l

sSS :: NgxExport -> CString -> CInt -> CString -> CInt -> Ptr CString -> IO CInt
sSS (SSS f) x (fromIntegral -> n) y (fromIntegral -> m) p = do
    (s, fromIntegral -> l) <- f <$> peekCStringLen (x, n)
                                <*> peekCStringLen (y, m)
                                >>= newCStringLen
    poke p s
    return l

sLS :: NgxExport -> Ptr NgxStrType -> CInt -> Ptr CString -> IO CInt
sLS (SLS f) x (fromIntegral -> n) p = do
    (s, fromIntegral -> l) <- f <$> peekNgxStringArrayLen x n
                                >>= newCStringLen
    poke p s
    return l

yY :: NgxExport -> CString -> CInt -> Ptr CString -> IO CInt
yY (YY f) x (fromIntegral -> n) p = do
    s <- f <$> B.unsafePackCStringLen (x, n)
    (fromMaybe (nullPtr, -1) -> (t, fromIntegral -> l)) <- toSingleBuffer s
    poke p t
    return l

bS :: NgxExport -> CString -> CInt -> IO CUInt
bS (BS f) x (fromIntegral -> n) =
    fromBool . f <$> peekCStringLen (x, n)

bSS :: NgxExport -> CString -> CInt -> CString -> CInt -> IO CUInt
bSS (BSS f) x (fromIntegral -> n) y (fromIntegral -> m) =
    (fromBool .) . f <$> peekCStringLen (x, n)
                     <*> peekCStringLen (y, m)

bLS :: NgxExport -> Ptr NgxStrType -> CInt -> IO CUInt
bLS (BLS f) x (fromIntegral -> n) =
    fromBool . f <$> peekNgxStringArrayLen x n

bY :: NgxExport -> CString -> CInt -> IO CUInt
bY (BY f) x (fromIntegral -> n) =
    fromBool . f <$> B.unsafePackCStringLen (x, n)

handler :: NgxExport -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString -> Ptr CSize -> IO CInt
handler (Handler f) x (fromIntegral -> n) ps pls pt plt = do
    (s, mt, fromIntegral -> st) <- f <$> B.unsafePackCStringLen (x, n)
    (fromMaybe (nullPtr, -1) -> (t, fromIntegral -> l)) <- toBuffers s
    poke ps t
    poke pls l
    (smt, fromIntegral -> lmt) <- newCStringLen mt
    pokeCStringLen smt lmt pt plt
    return st

defHandler :: NgxExport -> CString -> CInt -> Ptr (Ptr NgxStrType) -> IO CInt
defHandler (YY f) x (fromIntegral -> n) ps = do
    s <- f <$> B.unsafePackCStringLen (x, n)
    (fromMaybe (nullPtr, -1) -> (t, fromIntegral -> l)) <- toBuffers s
    poke ps t
    return l

unsafeHandler :: NgxExport -> CString -> CInt ->
    Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize -> IO CInt
unsafeHandler (UnsafeHandler f) x (fromIntegral -> n) ps pls pt plt = do
    (s, mt, fromIntegral -> st) <- f <$> B.unsafePackCStringLen (x, n)
    (t, fromIntegral -> lt) <- B.unsafeUseAsCStringLen s return
    pokeCStringLen t lt ps pls
    (smt, fromIntegral -> lmt) <- B.unsafeUseAsCStringLen mt return
    pokeCStringLen smt lmt pt plt
    return st

