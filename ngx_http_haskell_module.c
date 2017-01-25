/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_module.c
 *
 *    Description:  nginx module for inlining haskell code
 *
 *        Version:  1.0
 *        Created:  23.12.2015 12:53:00
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>
#include <dlfcn.h>
#include <HsFFI.h>


static const ngx_str_t  haskell_module_handler_prefix =
ngx_string("ngx_hs_");
static const ngx_str_t  haskell_module_type_checker_prefix =
ngx_string("type_");
static const ngx_str_t  haskell_compile_cmd =
ngx_string("ghc -O2 -dynamic -shared -fPIC -o ");
static const ngx_str_t  template_haskell_option =
ngx_string(" -XTemplateHaskell");
static const ngx_str_t  ghc_rtslib =
ngx_string(" -lHSrts-ghc$(ghc --numeric-version)");
static const ngx_str_t  ghc_rtslib_thr =
ngx_string(" -lHSrts_thr-ghc$(ghc --numeric-version)");

static const ngx_str_t  haskell_module_code_head =
ngx_string(
"{-# LANGUAGE ForeignFunctionInterface, CPP #-}\n"
"{-# OPTIONS_GHC -pgmPcpphs -optP--cpp #-}\n\n"
"#define AUX_NGX_TYPECHECK(T, F, S) \\\n"
"type_ngx_hs_ ## F = aux_ngx_exportType $ T S; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_S_S(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_S_S, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_s_s $ AUX_NGX_S_S F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_S_SS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_S_SS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_s_ss $ AUX_NGX_S_SS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_S_LS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_S_LS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_s_ls $ AUX_NGX_S_LS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_B_S(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_S, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_s $ AUX_NGX_B_S F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"
"#define NGX_EXPORT_B_SS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_SS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_ss $ AUX_NGX_B_SS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"
"#define NGX_EXPORT_B_LS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_LS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_ls $ AUX_NGX_B_LS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"
"#define NGX_EXPORT_Y_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_Y_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_y_y $ AUX_NGX_Y_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_B_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_y $ AUX_NGX_B_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"
"#define NGX_EXPORT_IOY_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_IOY_Y, F, (const . F)) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_ioy_y $ AUX_NGX_IOY_Y (const . F); \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_ASYNC_IOY_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_IOY_Y, F, (const . F)) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_async_ioy_y $ AUX_NGX_IOY_Y (const . F); \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> AUX_NGX.CInt -> AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CUInt -> IO ();\n\n"
"#define NGX_EXPORT_SERVICE_IOY_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_IOY_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_async_ioy_y $ AUX_NGX_IOY_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> AUX_NGX.CInt -> AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CUInt -> IO ();\n\n"
"#define NGX_EXPORT_HANDLER(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_HANDLER, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_handler $ AUX_NGX_HANDLER F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> "
"AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_DEF_HANDLER(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_Y_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_def_handler $ AUX_NGX_Y_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> IO AUX_NGX.CInt;\n\n"
"#define NGX_EXPORT_UNSAFE_HANDLER(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_UNSAFE_HANDLER, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_unsafe_handler $ AUX_NGX_UNSAFE_HANDLER F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    IO AUX_NGX.CInt;\n\n"
"{-# LANGUAGE ViewPatterns #-}\n\n"
"module NgxHaskellUserRuntime where\n\n"
"import qualified Foreign.C as AUX_NGX\n"
"import qualified Foreign.Ptr as AUX_NGX\n"
"import qualified Foreign.Storable as AUX_NGX\n"
"import qualified Foreign.Marshal.Alloc as AUX_NGX\n"
"import qualified Foreign.Marshal.Utils as AUX_NGX\n"
"import qualified System.IO.Error as AUX_NGX\n"
"import qualified System.Posix.IO as AUX_NGX\n"
"import qualified Control.Monad as AUX_NGX\n"
"import qualified Control.Exception as AUX_NGX\n"
"import qualified Control.Concurrent.Async as AUX_NGX\n"
"import qualified Data.Maybe as AUX_NGX\n"
"import qualified Data.ByteString as AUX_NGX_BS\n"
"import qualified Data.ByteString.Unsafe as AUX_NGX_BS\n"
"import qualified Data.ByteString.Lazy as AUX_NGX_BSL\n\n"
"import qualified Data.ByteString.Lazy.Char8 as AUX_NGX_BSLC8\n\n"
"-- START OF USER HASKELL CODE\n"
);

static const ngx_str_t  haskell_module_code_tail =
ngx_string(
"\n-- END OF USER HASKELL CODE\n\n"
"data AUX_NGX_EXPORT = AUX_NGX_S_S (String -> String)\n"
"                    | AUX_NGX_S_SS (String -> String -> String)\n"
"                    | AUX_NGX_S_LS ([String] -> String)\n"
"                    | AUX_NGX_B_S (String -> Bool)\n"
"                    | AUX_NGX_B_SS (String -> String -> Bool)\n"
"                    | AUX_NGX_B_LS ([String] -> Bool)\n"
"                    | AUX_NGX_Y_Y (AUX_NGX_BS.ByteString ->\n"
"                          AUX_NGX_BSL.ByteString)\n"
"                    | AUX_NGX_B_Y (AUX_NGX_BS.ByteString -> Bool)\n"
"                    | AUX_NGX_IOY_Y (AUX_NGX_BS.ByteString -> Bool ->\n"
"                          IO AUX_NGX_BSL.ByteString)\n"
"                    | AUX_NGX_HANDLER (AUX_NGX_BS.ByteString ->\n"
"                          (AUX_NGX_BSL.ByteString, String, Int))\n"
"                    | AUX_NGX_UNSAFE_HANDLER (AUX_NGX_BS.ByteString ->\n"
"                          (AUX_NGX_BS.ByteString, AUX_NGX_BS.ByteString, Int))"
"\n\n"
"instance Enum AUX_NGX_EXPORT where\n"
"    toEnum _ = AUX_NGX_S_S id    -- not used\n"
"    fromEnum (AUX_NGX_S_S _)            = 1\n"
"    fromEnum (AUX_NGX_S_SS _)           = 2\n"
"    fromEnum (AUX_NGX_S_LS _)           = 3\n"
"    fromEnum (AUX_NGX_B_S _)            = 4\n"
"    fromEnum (AUX_NGX_B_SS _)           = 5\n"
"    fromEnum (AUX_NGX_B_LS _)           = 6\n"
"    fromEnum (AUX_NGX_Y_Y _)            = 7\n"
"    fromEnum (AUX_NGX_B_Y _)            = 8\n"
"    fromEnum (AUX_NGX_IOY_Y _)          = 9\n"
"    fromEnum (AUX_NGX_HANDLER _)        = 10\n"
"    fromEnum (AUX_NGX_UNSAFE_HANDLER _) = 11\n\n"
"aux_ngx_exportType :: AUX_NGX_EXPORT -> IO AUX_NGX.CInt\n"
"aux_ngx_exportType = return . fromIntegral . fromEnum\n\n"
"data AUX_NGX_STR_TYPE = AUX_NGX_STR_TYPE AUX_NGX.CSize AUX_NGX.CString\n\n"
"instance AUX_NGX.Storable AUX_NGX_STR_TYPE where\n"
"    alignment _ = max (AUX_NGX.alignment (undefined :: AUX_NGX.CSize))\n"
"                      (AUX_NGX.alignment (undefined :: AUX_NGX.CString))\n"
"    sizeOf = (2 *) . AUX_NGX.alignment   -- must always be correct for\n"
"                                         -- aligned struct ngx_str_t\n"
"    peek p = do\n"
"        n <- AUX_NGX.peekByteOff p 0\n"
"        s <- AUX_NGX.peekByteOff p $\n"
"            AUX_NGX.alignment (undefined :: AUX_NGX_STR_TYPE)\n"
"        return $ AUX_NGX_STR_TYPE n s\n"
"    poke p x@(AUX_NGX_STR_TYPE n s) = do\n"
"        AUX_NGX.poke (AUX_NGX.castPtr p) n\n"
"        AUX_NGX.poke (AUX_NGX.plusPtr p $ AUX_NGX.alignment x) s\n\n"
"aux_ngx_catchAlloc :: IO (AUX_NGX.Ptr a) -> IO (AUX_NGX.Ptr a)\n"
"aux_ngx_catchAlloc = (`AUX_NGX.catchIOError` const (return AUX_NGX.nullPtr))"
"\n\n"
"aux_ngx_peekNgxStringArrayLen :: AUX_NGX.Ptr AUX_NGX_STR_TYPE -> Int ->\n"
"    IO [String]\n"
"aux_ngx_peekNgxStringArrayLen x n = sequence $\n"
"    foldr (\\k ->\n"
"              ((AUX_NGX.peekElemOff x k >>=\n"
"                  (\\(AUX_NGX_STR_TYPE (fromIntegral -> m) y) ->\n"
"                      AUX_NGX.peekCStringLen (y, m))) :)) [] [0 .. n - 1]\n\n"
"aux_ngx_pokeCStringLen :: AUX_NGX.CString -> AUX_NGX.CSize ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> IO ()\n"
"aux_ngx_pokeCStringLen x n p s = AUX_NGX.poke p x >> AUX_NGX.poke s n\n\n"
"aux_ngx_pokeLazyByteString :: AUX_NGX_BSL.ByteString ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_pokeLazyByteString s p = do\n"
"    (AUX_NGX.fromMaybe (AUX_NGX.nullPtr, -1) -> (t, fromIntegral -> l)) <-\n"
"        aux_ngx_toSingleBuffer s\n"
"    AUX_NGX.poke p t\n"
"    return l\n\n"
"aux_ngx_toSingleBuffer :: AUX_NGX_BSL.ByteString ->\n"
"    IO (Maybe (AUX_NGX.CString, Int))\n"
"aux_ngx_toSingleBuffer (AUX_NGX_BSL.null -> True) =\n"
"    return $ Just (AUX_NGX.nullPtr, 0)\n"
"aux_ngx_toSingleBuffer s = do\n"
"    let (fromIntegral -> l) = AUX_NGX_BSL.length s\n"
"    t <- aux_ngx_catchAlloc $ AUX_NGX.mallocBytes l\n"
"    if t /= AUX_NGX.nullPtr\n"
"        then do\n"
"            AUX_NGX.void $ AUX_NGX_BSL.foldlChunks\n"
"                (\\a s -> do\n"
"                    off <- a\n"
"                    let l = AUX_NGX_BS.length s\n"
"                    AUX_NGX_BS.unsafeUseAsCString s $\n"
"                        flip (AUX_NGX.copyBytes $ AUX_NGX.plusPtr t off) l\n"
"                    return $ off + l\n"
"                ) (return 0) s\n"
"            return $ Just (t, l)\n"
"        else return Nothing\n\n"
"aux_ngx_toBuffers :: AUX_NGX_BSL.ByteString ->\n"
"    IO (Maybe (AUX_NGX.Ptr AUX_NGX_STR_TYPE, Int))\n"
"aux_ngx_toBuffers (AUX_NGX_BSL.null -> True) =\n"
"    return $ Just (AUX_NGX.nullPtr, 0)\n"
"aux_ngx_toBuffers s = do\n"
"    t <- aux_ngx_catchAlloc $ AUX_NGX.mallocBytes $\n"
"        AUX_NGX_BSL.foldlChunks (const . succ) 0 s *\n"
"        AUX_NGX.sizeOf (undefined :: AUX_NGX_STR_TYPE)\n"
"    l <- AUX_NGX_BSL.foldlChunks\n"
"        (\\a s -> do\n"
"            off <- a\n"
"            maybe (return Nothing)\n"
"                (\\off -> do\n"
"                    let l = AUX_NGX_BS.length s\n"
"                    -- l cannot be zero at this point because intermediate\n"
"                    -- chunks of a lazy ByteString cannot be empty which is\n"
"                    -- the consequence of Monoid laws applied when it grows\n"
"                    dst <- aux_ngx_catchAlloc $ AUX_NGX.mallocBytes l\n"
"                    if dst /= AUX_NGX.nullPtr\n"
"                        then do\n"
"                            AUX_NGX_BS.unsafeUseAsCString s $\n"
"                                flip (AUX_NGX.copyBytes dst) l\n"
"                            AUX_NGX.pokeElemOff t off $\n"
"                                AUX_NGX_STR_TYPE (fromIntegral l) dst\n"
"                            return $ Just $ off + 1\n"
"                        else do\n"
"                            mapM_ (AUX_NGX.peekElemOff t AUX_NGX.>=>\n"
"                                      \\(AUX_NGX_STR_TYPE _ x) -> "
"AUX_NGX.free x)\n"
"                                  [0 .. off - 1]    -- [0 .. -1] makes [],\n"
"                                                    -- so wise!\n"
"                            AUX_NGX.free t\n"
"                            return Nothing\n"
"                ) off\n"
"        ) (return $ if t /= AUX_NGX.nullPtr then Just 0 else Nothing) s\n"
"    return $ l >>= Just . (,) t\n\n"
"aux_ngx_hs_s_s :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_s_s (AUX_NGX_S_S f)\n"
"            x (fromIntegral -> n) p = do\n"
"    (s, fromIntegral -> l) <- f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                                >>= AUX_NGX.newCStringLen\n"
"    AUX_NGX.poke p s\n"
"    return l\n\n"
"aux_ngx_hs_s_ss :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_s_ss (AUX_NGX_S_SS f)\n"
"            x (fromIntegral -> n) y (fromIntegral -> m) p = do\n"
"    (s, fromIntegral -> l) <- f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                                <*> AUX_NGX.peekCStringLen (y, m)\n"
"                                >>= AUX_NGX.newCStringLen\n"
"    AUX_NGX.poke p s\n"
"    return l\n\n"
"aux_ngx_hs_s_ls :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_s_ls (AUX_NGX_S_LS f)\n"
"            x (fromIntegral -> n) p = do\n"
"    (s, fromIntegral -> l) <- f <$> aux_ngx_peekNgxStringArrayLen x n\n"
"                                >>= AUX_NGX.newCStringLen\n"
"    AUX_NGX.poke p s\n"
"    return l\n\n"
"aux_ngx_hs_y_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_y_y (AUX_NGX_Y_Y f)\n"
"            x (fromIntegral -> n) p = do\n"
"    s <- f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"    aux_ngx_pokeLazyByteString s p\n\n"
"aux_ngx_hs_ioy_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_ioy_y (AUX_NGX_IOY_Y f)\n"
"            x (fromIntegral -> n) p = do\n"
"    s <- (AUX_NGX_BS.unsafePackCStringLen (x, n) >>= flip f False)\n"
"        `AUX_NGX.catch` \\e -> return $ AUX_NGX_BSLC8.pack $\n"
"            show (e :: AUX_NGX.SomeException)\n"
"    aux_ngx_pokeLazyByteString s p\n\n"
"aux_ngx_hs_async_ioy_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> AUX_NGX.CInt -> AUX_NGX.CUInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->\n"
"    AUX_NGX.Ptr AUX_NGX.CUInt -> IO ()\n"
"aux_ngx_hs_async_ioy_y (AUX_NGX_IOY_Y f)\n"
"            x (fromIntegral -> n) (fromIntegral -> fd)\n"
"                    ((/= 0) -> fstRun) p pl r =\n"
"    AUX_NGX.void . AUX_NGX.async $\n"
"    (do\n"
"    s <- (Right <$> (AUX_NGX_BS.unsafePackCStringLen (x, n) >>= "
"flip f fstRun))\n"
"        `AUX_NGX.catch` \\e -> return $ Left $ "
"show (e :: AUX_NGX.SomeException)\n"
"    either\n"
"        (\\s -> do\n"
"            (x, fromIntegral -> l) <- AUX_NGX.newCStringLen s\n"
"            aux_ngx_pokeCStringLen x l p pl\n"
"            AUX_NGX.poke r 1\n"
"        )\n"
"        (\\s -> do\n"
"            (AUX_NGX.fromMaybe (AUX_NGX.nullPtr, -1) ->\n"
"                (t, fromIntegral -> l)) <- aux_ngx_toSingleBuffer s\n"
"            aux_ngx_pokeCStringLen t l p pl\n"
"            AUX_NGX.poke r 0\n"
"        ) s\n"
"    )\n"
"    `AUX_NGX.finally`\n"
"    ((AUX_NGX.fdWrite fd \"0\" >> AUX_NGX.closeFd fd)\n"
"        `AUX_NGX.catchIOError` const (return ()))\n\n"
"aux_ngx_hs_b_s :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_s (AUX_NGX_B_S f)\n"
"            x (fromIntegral -> n) =\n"
"    AUX_NGX.fromBool . f <$> AUX_NGX.peekCStringLen (x, n)\n\n"
"aux_ngx_hs_b_ss :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_ss (AUX_NGX_B_SS f)\n"
"            x (fromIntegral -> n) y (fromIntegral -> m) =\n"
"    (AUX_NGX.fromBool .) . f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                             <*> AUX_NGX.peekCStringLen (y, m)\n\n"
"aux_ngx_hs_b_ls :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_ls (AUX_NGX_B_LS f)\n"
"            x (fromIntegral -> n) =\n"
"    AUX_NGX.fromBool . f <$> aux_ngx_peekNgxStringArrayLen x n\n\n"
"aux_ngx_hs_b_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_y (AUX_NGX_B_Y f)\n"
"            x (fromIntegral -> n) =\n"
"    AUX_NGX.fromBool . f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n\n"
"aux_ngx_hs_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt ->"
"\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->\n"
"    IO AUX_NGX.CInt\n"
"aux_ngx_hs_handler (AUX_NGX_HANDLER f)\n"
"            x (fromIntegral -> n) ps pls pt plt = do\n"
"    (s, mt, fromIntegral -> st) <- f <$> AUX_NGX_BS.unsafePackCStringLen "
"(x, n)\n"
"    (AUX_NGX.fromMaybe (AUX_NGX.nullPtr, -1) -> (t, fromIntegral -> l)) <-\n"
"        aux_ngx_toBuffers s\n"
"    AUX_NGX.poke ps t\n"
"    AUX_NGX.poke pls l\n"
"    (smt, fromIntegral -> lmt) <- AUX_NGX.newCStringLen mt\n"
"    aux_ngx_pokeCStringLen smt lmt pt plt\n"
"    return st\n\n"
"aux_ngx_hs_def_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_def_handler (AUX_NGX_Y_Y f)\n"
"            x (fromIntegral -> n) ps = do\n"
"    s <- f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"    (AUX_NGX.fromMaybe (AUX_NGX.nullPtr, -1) -> (t, fromIntegral -> l)) <-\n"
"        aux_ngx_toBuffers s\n"
"    AUX_NGX.poke ps t\n"
"    return l\n\n"
"aux_ngx_hs_unsafe_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->\n"
"    IO AUX_NGX.CInt\n"
"aux_ngx_hs_unsafe_handler (AUX_NGX_UNSAFE_HANDLER f)\n"
"            x (fromIntegral -> n) ps pls pt plt = do\n"
"    (s, mt, fromIntegral -> st) <- f <$> AUX_NGX_BS.unsafePackCStringLen "
"(x, n)\n"
"    (t, fromIntegral -> lt) <- AUX_NGX_BS.unsafeUseAsCStringLen s return\n"
"    aux_ngx_pokeCStringLen t lt ps pls\n"
"    (smt, fromIntegral -> lmt) <- AUX_NGX_BS.unsafeUseAsCStringLen mt return\n"
"    aux_ngx_pokeCStringLen smt lmt pt plt\n"
"    return st\n"
);


typedef HsInt32 (*ngx_http_haskell_handler_s_s)
    (HsPtr, HsInt32, HsPtr);
typedef HsInt32 (*ngx_http_haskell_handler_s_ss)
    (HsPtr, HsInt32, HsPtr, HsInt32, HsPtr);
typedef HsInt32 (*ngx_http_haskell_handler_s_ls)
    (HsPtr, HsInt32, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_s)
    (HsPtr, HsInt32);
typedef HsWord32 (*ngx_http_haskell_handler_b_ss)
    (HsPtr, HsInt32, HsPtr, HsInt32);
typedef HsWord32 (*ngx_http_haskell_handler_b_ls)
    (HsPtr, HsInt32);
typedef HsInt32 (*ngx_http_haskell_handler_y_y)
    (HsPtr, HsInt32, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_y)
    (HsPtr, HsInt32);
typedef void (*ngx_http_haskell_handler_async_ioy_y)
    (HsPtr, HsInt32, HsInt32, HsWord32, HsPtr, HsPtr, HsPtr);
typedef HsInt32 (*ngx_http_haskell_handler_ch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsInt32 (*ngx_http_haskell_handler_dch)
    (HsPtr, HsInt32, HsPtr);
typedef HsInt32 (*ngx_http_haskell_handler_uch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr);

typedef enum {
    ngx_http_haskell_module_wrap_mode_modular = 0,
    ngx_http_haskell_module_wrap_mode_standalone
} ngx_http_haskell_module_wrap_mode_e;

typedef enum {
    ngx_http_haskell_handler_type_uninitialized = 0,
    ngx_http_haskell_handler_type_s_s,
    ngx_http_haskell_handler_type_s_ss,
    ngx_http_haskell_handler_type_s_ls,
    ngx_http_haskell_handler_type_b_s,
    ngx_http_haskell_handler_type_b_ss,
    ngx_http_haskell_handler_type_b_ls,
    ngx_http_haskell_handler_type_y_y,
    ngx_http_haskell_handler_type_b_y,
    ngx_http_haskell_handler_type_ioy_y,
    ngx_http_haskell_handler_type_ch,
    ngx_http_haskell_handler_type_uch
} ngx_http_haskell_handler_type_e;


typedef enum {
    ngx_http_haskell_handler_role_uninitialized = 0,
    ngx_http_haskell_handler_role_variable,
    ngx_http_haskell_handler_role_async_variable,
    ngx_http_haskell_handler_role_service_variable,
    ngx_http_haskell_handler_role_content_handler
} ngx_http_haskell_handler_role_e;


typedef enum {
    ngx_http_haskell_compile_mode_threaded = 0,
    ngx_http_haskell_compile_mode_no_threaded,
    ngx_http_haskell_compile_mode_load_existing
} ngx_http_haskell_compile_mode_e;


typedef struct {
    ngx_http_haskell_module_wrap_mode_e        wrap_mode;
    ngx_str_t                                  ghc_extra_options;
    ngx_array_t                                rts_options;
    ngx_array_t                                program_options;
    ngx_str_t                                  lib_path;
    ngx_array_t                                handlers;
    void                                      *dl_handle;
    void                                     (*hs_init)(int *, char ***);
    void                                     (*hs_exit)(void);
    void                                     (*hs_add_root)(void (*)(void));
    void                                     (*init_HsModule)(void);
    ngx_http_haskell_compile_mode_e            compile_mode;
    ngx_array_t                                service_code_vars;
    ngx_array_t                                var_nocacheable;
    ngx_array_t                                var_compensate_uri_changes;
    ngx_array_t                                service_var_ignore_empty;
    ngx_uint_t                                 code_loaded:1;
    ngx_uint_t                                 has_async_tasks:1;
} ngx_http_haskell_main_conf_t;


typedef struct {
    ngx_int_t                                  handler;
    ngx_http_complex_value_t                  *args;
} ngx_http_haskell_content_handler_t;


typedef struct {
    ngx_str_t                                 *bufs;
    ngx_int_t                                  size;
    ngx_str_t                                  content_type;
    ngx_int_t                                  status;
} ngx_http_haskell_content_handler_data_t;


typedef struct {
    ngx_array_t                                code_vars;
    ngx_pool_t                                *pool;
    ngx_http_haskell_content_handler_t        *content_handler;
    ngx_http_haskell_content_handler_data_t   *content_handler_data;
    ngx_uint_t                                 static_content;
} ngx_http_haskell_loc_conf_t;


typedef struct {
    void                                      *self;
    ngx_http_haskell_handler_type_e            type;
    ngx_http_haskell_handler_role_e            role;
    ngx_str_t                                  name;
    ngx_uint_t                                 n_args[3];
    ngx_uint_t                                 unsafe;
} ngx_http_haskell_handler_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_int_t                                  handler;
    ngx_array_t                                args;
} ngx_http_haskell_code_var_data_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_str_t                                  result;
    ngx_uint_t                                 error;
} ngx_http_haskell_async_data_t;


typedef struct {
    ngx_http_haskell_code_var_data_t          *data;
    ngx_http_haskell_async_data_t              future_async_data;
    ngx_http_haskell_async_data_t              async_data;
    ngx_event_t                               *event;
    ngx_fd_t                                   fd;
} ngx_http_haskell_service_code_var_data_t;


typedef struct {
    ngx_array_t                                async_data;
    ngx_array_t                                var_nocacheable_cache;
} ngx_http_haskell_ctx_t;


typedef struct {
    /* ngx_connection_t stub to allow use c->fd as event ident */
    void                                      *data;
    ngx_event_t                               *read;
    ngx_event_t                               *write;
    ngx_fd_t                                   fd;
} ngx_http_haskell_async_event_stub_t;


typedef struct {
    ngx_http_haskell_async_event_stub_t        s;
    ngx_http_request_t                        *r;
} ngx_http_haskell_async_event_t;


typedef struct {
    ngx_http_haskell_async_event_stub_t        s;
    ngx_cycle_t                               *cycle;
    ngx_http_haskell_service_code_var_data_t  *service_code_var;
} ngx_http_haskell_service_async_event_t;


typedef struct {
    ngx_str_t                                  name;
    ngx_int_t                                  index;
} ngx_http_haskell_var_handle_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_str_t                                  value;
    ngx_uint_t                                 checked;
} ngx_http_haskell_var_cache_t;


static char *ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static char *ngx_http_haskell_write_code(ngx_conf_t *cf, void *conf,
    ngx_str_t source_name, ngx_str_t fragment);
static char *ngx_http_haskell_compile(ngx_conf_t *cf, void *conf,
    ngx_str_t source_name);
static ngx_int_t ngx_http_haskell_load(ngx_cycle_t *cycle);
static void ngx_http_haskell_unload(ngx_cycle_t *cycle);
static ngx_int_t ngx_http_haskell_init_services(ngx_cycle_t *cycle);
static void ngx_http_haskell_stop_services(ngx_cycle_t *cycle);
static ngx_int_t ngx_http_haskell_run_service(ngx_cycle_t *cycle,
                    ngx_http_haskell_service_code_var_data_t *service_code_var,
                    ngx_uint_t service_first_run);
static char *ngx_http_haskell_run(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static char *ngx_http_haskell_content(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static char *ngx_http_haskell_var_configure(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static ngx_int_t ngx_http_haskell_init(ngx_conf_t *cf);
static void *ngx_http_haskell_create_main_conf(ngx_conf_t *cf);
static void *ngx_http_haskell_create_loc_conf(ngx_conf_t *cf);
static char *ngx_http_haskell_merge_loc_conf(ngx_conf_t *cf, void *parent,
    void *child);
static ngx_int_t ngx_http_haskell_rewrite_phase_handler(ngx_http_request_t *r);
static void ngx_http_service_async_event(ngx_event_t *ev);
static ngx_int_t ngx_http_haskell_init_worker(ngx_cycle_t *cycle);
static void ngx_http_haskell_exit_worker(ngx_cycle_t *cycle);
static ngx_int_t ngx_http_haskell_run_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
static ngx_int_t ngx_http_haskell_run_async_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
static ngx_int_t ngx_http_haskell_run_service_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
static ngx_int_t ngx_http_haskell_content_handler(ngx_http_request_t *r);
static void ngx_http_haskell_async_event(ngx_event_t *ev);
static void ngx_http_haskell_service_async_event(ngx_event_t *ev);
static void ngx_http_haskell_variable_cleanup(void *data);
static void ngx_http_haskell_content_handler_cleanup(void *data);
static void close_pipe(ngx_log_t *log, ngx_fd_t *fd);


static ngx_command_t  ngx_http_haskell_module_commands[] = {

    { ngx_string("haskell"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_2MORE,
      ngx_http_haskell,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_run"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_ANY,
      ngx_http_haskell_run,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_run_async"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_ANY,
      ngx_http_haskell_run,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_run_service"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_TAKE23,
      ngx_http_haskell_run,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_content"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_static_content"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_unsafe_content"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_var_nocacheable"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_var_compensate_uri_changes"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_service_var_ignore_empty"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },

      ngx_null_command
};


static ngx_http_module_t  ngx_http_haskell_module_ctx = {
    NULL,                                    /* preconfiguration */
    ngx_http_haskell_init,                   /* postconfiguration */

    ngx_http_haskell_create_main_conf,       /* create main configuration */
    NULL,                                    /* init main configuration */

    NULL,                                    /* create server configuration */
    NULL,                                    /* merge server configuration */

    ngx_http_haskell_create_loc_conf,        /* create location configuration */
    ngx_http_haskell_merge_loc_conf          /* merge location configuration */
};


ngx_module_t  ngx_http_haskell_module = {
    NGX_MODULE_V1,
    &ngx_http_haskell_module_ctx,            /* module context */
    ngx_http_haskell_module_commands,        /* module directives */
    NGX_HTTP_MODULE,                         /* module type */
    NULL,                                    /* init master */
    NULL,                                    /* init module */
    ngx_http_haskell_init_worker,            /* init process */
    NULL,                                    /* init thread */
    NULL,                                    /* exit thread */
    ngx_http_haskell_exit_worker,            /* exit process */
    NULL,                                    /* exit master */
    NGX_MODULE_V1_PADDING
};


static ngx_int_t
ngx_http_haskell_init(ngx_conf_t *cf)
{
    ngx_uint_t                     i;
    ngx_http_haskell_main_conf_t  *mcf;
    ngx_http_core_main_conf_t     *cmcf;
    ngx_array_t                   *hs;
    ngx_http_handler_pt           *h, *hs_elts;

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded
        || !(mcf->has_async_tasks || mcf->var_nocacheable.nelts > 0))
    {
        return NGX_OK;
    }

    cmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_core_module);
    hs = &cmcf->phases[NGX_HTTP_REWRITE_PHASE].handlers;

    h = ngx_array_push_n(hs, 2);
    if (h == NULL) {
        return NGX_ERROR;
    }

    hs_elts = hs->elts;
    for (i = hs->nelts - 2 ; i > 0; i--) {
        hs_elts[i] = hs_elts[i - 1];
    }

    *++h = ngx_http_haskell_rewrite_phase_handler;
    hs_elts[0] = ngx_http_haskell_rewrite_phase_handler;

    return NGX_OK;
}


static void *
ngx_http_haskell_create_main_conf(ngx_conf_t *cf)
{
    ngx_http_haskell_main_conf_t  *mcf;

    mcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_haskell_main_conf_t));
    if (mcf == NULL) {
        return NULL;
    }

    if (ngx_array_init(&mcf->handlers, cf->pool, 1,
                       sizeof(ngx_http_haskell_handler_t)) != NGX_OK)
    {
        return NULL;
    }

    if (ngx_array_init(&mcf->service_code_vars, cf->pool, 1,
                       sizeof(ngx_http_haskell_service_code_var_data_t))
        != NGX_OK)
    {
        return NULL;
    }

    return mcf;
}


static void *
ngx_http_haskell_create_loc_conf(ngx_conf_t *cf)
{
    ngx_http_haskell_loc_conf_t  *lcf;

    lcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_haskell_loc_conf_t));
    if (lcf == NULL) {
        return NULL;
    }

    if (ngx_array_init(&lcf->code_vars, cf->pool, 1,
                       sizeof(ngx_http_haskell_code_var_data_t)) != NGX_OK)
    {
        return NULL;
    }

    return lcf;
}


static char *
ngx_http_haskell_merge_loc_conf(ngx_conf_t *cf, void *parent, void *child)
{
    ngx_http_haskell_loc_conf_t  *prev = parent;
    ngx_http_haskell_loc_conf_t  *conf = child;

    ngx_uint_t                    i;

    for (i = 0; i < prev->code_vars.nelts; i++) {
        ngx_http_haskell_code_var_data_t  *elem;

        elem = ngx_array_push(&conf->code_vars);
        if (elem == NULL) {
            return NGX_CONF_ERROR;
        }

        *elem = ((ngx_http_haskell_code_var_data_t *) prev->code_vars.elts)[i];
    }

    return NGX_CONF_OK;
}


static ngx_int_t
ngx_http_haskell_rewrite_phase_handler(ngx_http_request_t *r)
{
    ngx_uint_t                         i, j;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_http_haskell_loc_conf_t       *lcf;
    ngx_http_haskell_handler_t        *handlers;
    ngx_http_haskell_code_var_data_t  *code_vars;
    ngx_http_haskell_ctx_t            *ctx;
    ngx_http_haskell_async_data_t     *async_data, *async_data_elts;
    ngx_http_haskell_var_handle_t     *var_nocacheable;
    ngx_http_haskell_var_cache_t      *var_nocacheable_cache;
    ngx_int_t                          found_idx;
    ngx_event_t                       *event;
    ngx_http_haskell_async_event_t    *hev;
    ngx_http_complex_value_t          *args;
    ngx_str_t                          arg1;
    ngx_fd_t                           fd[2];
    ngx_pool_cleanup_t                *cln;

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;
    code_vars = lcf->code_vars.elts;

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (mcf->var_nocacheable.nelts > 0 && ctx == NULL) {
        ctx = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_ctx_t));
        if (ctx == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                "failed to create request context, declining phase handler");
            return NGX_DECLINED;
        }
        if (ngx_array_init(&ctx->var_nocacheable_cache, r->pool,
                           mcf->var_nocacheable.nelts,
                           sizeof(ngx_http_haskell_var_cache_t)) != NGX_OK
            || ngx_array_push_n(&ctx->var_nocacheable_cache,
                                mcf->var_nocacheable.nelts) == NULL)
        {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                "failed to create variable cache, declining phase handler");
            return NGX_DECLINED;
        }
        var_nocacheable = mcf->var_nocacheable.elts;
        var_nocacheable_cache = ctx->var_nocacheable_cache.elts;
        for (i = 0; i < ctx->var_nocacheable_cache.nelts; i++) {
            var_nocacheable_cache[i].index = var_nocacheable[i].index;
            var_nocacheable_cache[i].checked = 0;
            ngx_str_null(&var_nocacheable_cache[i].value);
        }
        ngx_http_set_ctx(r, ctx, ngx_http_haskell_module);
    }

    for (i = 0; i < lcf->code_vars.nelts; i++) {
        if (handlers[code_vars[i].handler].role !=
            ngx_http_haskell_handler_role_async_variable)
        {
            continue;
        }
        if (ctx == NULL) {
            ctx = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_ctx_t));
            if (ctx == NULL) {
                goto decline_phase_handler;
            }
            ngx_http_set_ctx(r, ctx, ngx_http_haskell_module);
        }
        if (ctx->async_data.nalloc == 0
            && ngx_array_init(&ctx->async_data, r->pool, 1,
                              sizeof(ngx_http_haskell_async_data_t)) != NGX_OK)
        {
            goto decline_phase_handler;
        }

        found_idx = NGX_ERROR;
        async_data_elts = ctx->async_data.elts;
        for (j = 0; j < ctx->async_data.nelts; j++) {
            if (async_data_elts[j].index == code_vars[i].index) {
                found_idx = code_vars[i].index;
                break;
            }
        }
        if (found_idx != NGX_ERROR) {
            continue;
        }

        async_data = ngx_array_push(&ctx->async_data);
        if (async_data == NULL) {
            goto decline_phase_handler;
        }
        async_data->index = code_vars[i].index;
        ngx_str_null(&async_data->result);
        async_data->error = 0;

        args = code_vars[i].args.elts;
        if (ngx_http_complex_value(r, &args[0], &arg1) != NGX_OK) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to compile complex value for "
                          "future async result, skipping IO task");
            continue;
        }

        if (pipe2(fd, O_NONBLOCK) == -1) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, ngx_errno,
                          "failed to create pipe for "
                          "future async result, skipping IO task");
            continue;
        }

        hev = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_async_event_t));
        if (hev == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to allocate memory for "
                          "future async result, skipping IO task");
            close_pipe(r->connection->log, fd);
            continue;
        }
        hev->s.fd = fd[0];
        hev->r = r;

        event = ngx_pcalloc(r->pool, sizeof(ngx_event_t));
        if (event== NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to allocate memory for "
                          "future async result, skipping IO task");
            close_pipe(r->connection->log, fd);
            continue;
        }
        event->data = hev;
        event->handler = ngx_http_haskell_async_event;
        event->log = r->connection->log;
        hev->s.read = event;
        hev->s.write = event;   /* to make ngx_add_event() happy */
        if (ngx_add_event(event, NGX_READ_EVENT, 0) != NGX_OK) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to add event for "
                          "future async result, skipping IO task");
            close_pipe(r->connection->log, fd);
            continue;
        }

        ((ngx_http_haskell_handler_async_ioy_y)
         handlers[code_vars[i].handler].self)
                (arg1.data, arg1.len, fd[1], 0, &async_data->result.data,
                 &async_data->result.len, &async_data->error);

        cln = ngx_pool_cleanup_add(r->pool, 0);
        if (cln == NULL) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "failed to register future async result in request "
                          "cleanup handler");
            return NGX_DONE;
        }
        cln->handler = ngx_http_haskell_variable_cleanup;
        cln->data = async_data->result.data;

        return NGX_DONE;
    }

    return NGX_DECLINED;

decline_phase_handler:

    ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                  "failed to create an async task, declining phase handler");

    return NGX_DECLINED;
}


static ngx_int_t
ngx_http_haskell_init_worker(ngx_cycle_t *cycle)
{
    ngx_uint_t                         i, j;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_http_core_main_conf_t         *cmcf;
    ngx_http_haskell_var_handle_t     *vars;
    ngx_http_variable_t               *cmvars;
    ngx_uint_t                         found;

    if (ngx_process == NGX_PROCESS_HELPER) {
        return NGX_OK;
    }

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return NGX_OK;
    }

    cmcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_core_module);
    cmvars = cmcf->variables.elts;

    vars = mcf->var_nocacheable.elts;
    for (i = 0; i < mcf->var_nocacheable.nelts; i++) {
        found = 0;
        for (j = 0; j < cmcf->variables.nelts; j++) {
            if (vars[i].name.len == cmvars[j].name.len
                && ngx_strncmp(vars[i].name.data, cmvars[j].name.data,
                               vars[i].name.len) == 0)
            {
                vars[i].index = cmvars[j].index;
                /* variables with any get handler are allowed here! */
                cmvars[j].flags |= NGX_HTTP_VAR_NOCACHEABLE;
                found = 1;
                break;
            }
        }
        if (found == 0) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" was not declared", &vars[i]);
        }
    }

    vars = mcf->var_compensate_uri_changes.elts;
    for (i = 0; i < mcf->var_compensate_uri_changes.nelts; i++) {
        found = 0;
        for (j = 0; j < cmcf->variables.nelts; j++) {
            if (vars[i].name.len == cmvars[j].name.len
                && ngx_strncmp(vars[i].name.data, cmvars[j].name.data,
                               vars[i].name.len) == 0)
            {
                if (cmvars[j].get_handler != ngx_http_haskell_run_handler) {
                    ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                                  "variable \"%V\" has incompatible "
                                  "get handler", &vars[i].name);
                } else {
                    vars[i].index = cmvars[j].index;
                }
                found = 1;
                break;
            }
        }
        if (found == 0) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" was not declared", &vars[i].name);
        }
    }

    vars = mcf->service_var_ignore_empty.elts;
    for (i = 0; i < mcf->service_var_ignore_empty.nelts; i++) {
        found = 0;
        for (j = 0; j < cmcf->variables.nelts; j++) {
            if (vars[i].name.len == cmvars[j].name.len
                && ngx_strncmp(vars[i].name.data, cmvars[j].name.data,
                               vars[i].name.len) == 0)
            {
                if (cmvars[j].get_handler
                    != ngx_http_haskell_run_service_handler)
                {
                    ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                                  "variable \"%V\" has incompatible "
                                  "get handler", &vars[i].name);
                } else {
                    vars[i].index = cmvars[j].index;
                }
                found = 1;
                break;
            }
        }
        if (found == 0) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" was not declared", &vars[i].name);
        }
    }

    if (ngx_http_haskell_load(cycle) != NGX_OK) {
        return NGX_ERROR;
    }

    return ngx_http_haskell_init_services(cycle);
}


static void
ngx_http_haskell_exit_worker(ngx_cycle_t *cycle)
{
    if (ngx_process == NGX_PROCESS_HELPER) {
        return;
    }

    ngx_http_haskell_unload(cycle);

    ngx_http_haskell_stop_services(cycle);
}


static char *
ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_main_conf_t   *mcf = conf;

    ngx_int_t                       i;
    ngx_str_t                      *value, base_name;
    ngx_file_info_t                 lib_info;
    ngx_int_t                       idx;
    ngx_uint_t                      load = 0, load_existing = 0;
    ngx_uint_t                      base_name_start = 0;
    ngx_uint_t                      has_wrap_mode = 0, has_threaded = 0;
    char                          **options;
    ngx_int_t                       len;

    value = cf->args->elts;

    if (value[1].len == 7
        && ngx_strncmp(value[1].data, "compile", 7) == 0)
    {
        if (cf->args->nelts < 4) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell compile requires 2 parameters");
            return NGX_CONF_ERROR;
        }

    } else if (value[1].len == 4
               && ngx_strncmp(value[1].data, "load", 4) == 0)
    {
        load = 1;

    } else if (value[1].len == 17
               && ngx_strncmp(value[1].data, "ghc_extra_options", 17) == 0)
    {
        if (mcf->code_loaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell ghc_extra_options must precede "
                    "directives haskell compile / load");
            return NGX_CONF_ERROR;
        }
        if (mcf->ghc_extra_options.len > 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell ghc_extra_options was already set");
            return NGX_CONF_ERROR;
        }
        len = cf->args->nelts - 2;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            len += value[i].len;
        }
        mcf->ghc_extra_options.len = len;
        mcf->ghc_extra_options.data = ngx_pnalloc(cf->pool, len);
        if (mcf->ghc_extra_options.data == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to allocate memory for ghc extra options");
            return NGX_CONF_ERROR;
        }
        len = 0;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            ngx_memcpy(mcf->ghc_extra_options.data + len++, " ", 1);
            ngx_memcpy(mcf->ghc_extra_options.data + len, value[i].data,
                       value[i].len);
            len += value[i].len;
        }

        return NGX_CONF_OK;

    } else if (value[1].len == 11
               && ngx_strncmp(value[1].data, "rts_options", 11) == 0)
    {
        if (mcf->rts_options.nelts > 1) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell rts_options was already set");
            return NGX_CONF_ERROR;
        }
        if (ngx_array_init(&mcf->rts_options, cf->pool, cf->args->nelts - 2,
                           sizeof(char *)) != NGX_OK
            || ngx_array_push_n(&mcf->rts_options, cf->args->nelts - 2)
            == NULL)
        {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to allocate memory for ghc RTS options");
            return NGX_CONF_ERROR;
        }
        options = mcf->rts_options.elts;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            options[i - 2] = (char *) value[i].data;
        }

        return NGX_CONF_OK;

    } else if (value[1].len == 15
               && ngx_strncmp(value[1].data, "program_options", 15) == 0)
    {
        if (mcf->program_options.nelts > 1) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell program_options was already set");
            return NGX_CONF_ERROR;
        }
        if (ngx_array_init(&mcf->program_options, cf->pool, cf->args->nelts - 2,
                           sizeof(char *)) != NGX_OK
            || ngx_array_push_n(&mcf->program_options, cf->args->nelts - 2)
            == NULL)
        {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to allocate memory for program options");
            return NGX_CONF_ERROR;
        }
        options = mcf->program_options.elts;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            options[i - 2] = (char *) value[i].data;
        }

        return NGX_CONF_OK;

    } else {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "unknown haskell directive \"%V\"", &value[1]);
        return NGX_CONF_ERROR;
    }

    if (mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "only one haskell source code block is allowed");
        return NGX_CONF_ERROR;
    }

    if (cf->args->nelts > 6) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directives haskell compile / load do not accept "
                    "more than 5 parameters");
        return NGX_CONF_ERROR;
    }

    if (value[2].len == 8 && ngx_strncmp(value[2].data, "threaded", 8) == 0) {
        has_threaded = 1;
    }

    idx = 2 + has_threaded;

    if (value[idx].len == 10
        && ngx_strncmp(value[idx].data, "standalone", 10) == 0)
    {
        has_wrap_mode = 1;
        mcf->wrap_mode = ngx_http_haskell_module_wrap_mode_standalone;
    } else if (value[idx].len == 7
               && ngx_strncmp(value[idx].data, "modular", 10) == 0)
    {
        has_wrap_mode = 1;
    }

    if (has_wrap_mode) {
        if (cf->args->nelts < 4 + has_threaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "directives haskell compile / load require "
                        "at least 2 parameters when wrap mode is specified");
            return NGX_CONF_ERROR;
        }
        if (!load && cf->args->nelts < 5 + has_threaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "directive haskell compile requires 3 parameters "
                        "when wrap mode is specified");
            return NGX_CONF_ERROR;
        }
    }

    if (load) {
        load_existing =
                cf->args->nelts < 4 + has_threaded + has_wrap_mode ? 1 : 0;
    }
    idx += has_wrap_mode;

    if (value[idx].len < 3
        || !(ngx_strncmp(value[idx].data + value[idx].len - 3, ".hs", 3) == 0
             || (load_existing
                 && ngx_strncmp(value[idx].data + value[idx].len - 3, ".so", 3)
                    == 0)))
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file must have extension \".hs\"");
        return NGX_CONF_ERROR;
    }
    if (!ngx_path_separator(value[idx].data[0])) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file path must be absolute");
        return NGX_CONF_ERROR;
    }
    for (i = value[idx].len - 4; i >= 0; i--) {
        if (ngx_path_separator(value[idx].data[i])) {
            base_name_start = i;
            break;
        }
    }
    base_name.len = value[idx].len - 4 - base_name_start;
    if (base_name.len == 0) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file base name is empty");
        return NGX_CONF_ERROR;
    }
    base_name.data = value[idx].data + base_name_start;

    mcf->lib_path.len = value[idx].len;
    mcf->lib_path.data = ngx_pnalloc(cf->pool, mcf->lib_path.len + 1);
    if (mcf->lib_path.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(mcf->lib_path.data, value[idx].data, value[idx].len - 3);
    ngx_memcpy(mcf->lib_path.data + value[idx].len - 3, ".so", 4);

    if (load) {
        if (ngx_file_info(mcf->lib_path.data, &lib_info) == NGX_FILE_ERROR) {
            if (load_existing) {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                        "haskell library cannot be loaded nor compiled");
                return NGX_CONF_ERROR;
            }
            load = 0;
        } else {
            if (has_threaded) {
                ngx_conf_log_error(NGX_LOG_NOTICE, cf, 0,
                        "haskell library exist but asked to be compiled as "
                        "threaded, please make sure that it was indeed "
                        "compiled as threaded, otherwise async tasks may "
                        "stall in runtime");
                mcf->compile_mode = ngx_http_haskell_compile_mode_load_existing;
            }
        }
    }

    if (!load) {
        if (ngx_http_haskell_write_code(cf, conf, value[idx], value[idx + 1])
            != NGX_CONF_OK)
        {
            return NGX_CONF_ERROR;
        }

        mcf->compile_mode = has_threaded ?
                ngx_http_haskell_compile_mode_threaded :
                ngx_http_haskell_compile_mode_no_threaded;
        if (ngx_http_haskell_compile(cf, conf, value[idx]) != NGX_CONF_OK) {
            return NGX_CONF_ERROR;
        }

        if (ngx_file_info(mcf->lib_path.data, &lib_info) == NGX_FILE_ERROR) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                               "haskell library cannot be loaded");
            return NGX_CONF_ERROR;
        }

        lib_info.st_mode |= S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH;
        if (chmod((const char *) mcf->lib_path.data, lib_info.st_mode) == -1) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                               "chmod() \"%V\" failed", &mcf->lib_path);
        }
    }

    mcf->code_loaded = 1;

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_write_code(ngx_conf_t *cf, void *conf, ngx_str_t source_name,
                            ngx_str_t fragment)
{
    ngx_http_haskell_main_conf_t  *mcf = conf;

    ngx_file_t                     out;
    ngx_str_t                      code;
    ngx_uint_t                     code_head_len = 0, code_tail_len = 0;

    if (mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_standalone) {
        code_head_len = haskell_module_code_head.len;
        code_tail_len = haskell_module_code_tail.len;
    }

    code.len = code_head_len + fragment.len + code_tail_len;
    code.data = ngx_pnalloc(cf->pool, code.len);
    if (code.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(code.data,
               haskell_module_code_head.data, code_head_len);
    ngx_memcpy(code.data + code_head_len,
               fragment.data, fragment.len);
    ngx_memcpy(code.data + code_head_len + fragment.len,
               haskell_module_code_tail.data, code_tail_len);

    ngx_memzero(&out, sizeof(ngx_file_t));

    out.name.data = ngx_pnalloc(cf->pool, source_name.len + 1);
    if (out.name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(out.name.data, source_name.data, source_name.len);
    out.name.data[source_name.len] = '\0';
    out.name.len = source_name.len;

    out.fd = ngx_open_file(out.name.data, NGX_FILE_WRONLY, NGX_FILE_TRUNCATE,
                           NGX_FILE_DEFAULT_ACCESS);
    if (out.fd == NGX_INVALID_FILE
        || ngx_write_file(&out, code.data, code.len, 0) == NGX_ERROR)
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                           "failed to write haskell source code file");
        return NGX_CONF_ERROR;
    }

    if (ngx_close_file(out.fd) == NGX_FILE_ERROR) {
        ngx_conf_log_error(NGX_LOG_ALERT, cf, ngx_errno,
                           "failed to close haskell source code file handle");
    }

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_compile(ngx_conf_t *cf, void *conf, ngx_str_t source_name)
{
    ngx_http_haskell_main_conf_t  *mcf = conf;

    char                          *compile_cmd;
    ngx_str_t                      rtslib;
    ngx_uint_t                     extra_len = 0, th_len = 0;
    ngx_uint_t                     passed_len, full_len;
    ngx_uint_t                     compile_cmd_len;

    compile_cmd_len = haskell_compile_cmd.len;

    rtslib = mcf->compile_mode == ngx_http_haskell_compile_mode_threaded ?
            ghc_rtslib_thr : ghc_rtslib;
    if (mcf->ghc_extra_options.len > 0) {
        extra_len = mcf->ghc_extra_options.len;
    }
    if (mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_modular) {
        th_len = template_haskell_option.len;
    }
    full_len = compile_cmd_len + mcf->lib_path.len + source_name.len +
            rtslib.len + extra_len + th_len + 2;

    compile_cmd = ngx_pnalloc(cf->pool, full_len);
    if (compile_cmd == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(compile_cmd,
               haskell_compile_cmd.data, compile_cmd_len);
    ngx_memcpy(compile_cmd + compile_cmd_len, mcf->lib_path.data,
               mcf->lib_path.len);
    ngx_memcpy(compile_cmd + compile_cmd_len + mcf->lib_path.len,
               rtslib.data, rtslib.len);
    ngx_memcpy(compile_cmd + compile_cmd_len + mcf->lib_path.len + rtslib.len,
               template_haskell_option.data, th_len);
    passed_len = compile_cmd_len + mcf->lib_path.len + rtslib.len + th_len;
    if (extra_len > 0) {
        ngx_memcpy(compile_cmd + passed_len,
                   mcf->ghc_extra_options.data, mcf->ghc_extra_options.len);
        passed_len += extra_len;
    }
    ngx_memcpy(compile_cmd + passed_len, " ", 1);
    ngx_memcpy(compile_cmd + passed_len + 1, source_name.data, source_name.len);
    compile_cmd[full_len - 1] = '\0';

    if (system(compile_cmd) != 0) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to compile haskell source code file");
        return NGX_CONF_ERROR;
    }

    return NGX_CONF_OK;
}


static ngx_int_t
ngx_http_haskell_load(ngx_cycle_t *cycle)
{
    ngx_uint_t                      i;
    ngx_http_haskell_main_conf_t   *mcf;
    ngx_http_haskell_handler_t     *handlers;
    char                           *dl_error;
    char                          **argv = NULL;
    int                             argc;
    char                           *hs_init = "hs_init";

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return NGX_OK;
    }

    if (mcf->dl_handle != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "haskell library has been unexpectedly loaded");
        return NGX_ERROR;
    }

    mcf->dl_handle = dlopen((char *) mcf->lib_path.data, RTLD_LAZY);
    dl_error = dlerror();
    if (mcf->dl_handle == NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load compiled haskell library: %s", dl_error);
        return NGX_ERROR;
    }

    if (mcf->rts_options.nelts > 0) {
        hs_init = "hs_init_with_rtsopts";
    } else {
        for (i = 0; i < mcf->program_options.nelts; i++) {
            if (ngx_strcmp(((char **) mcf->program_options.elts)[i], "+RTS")
                == 0)
            {
                hs_init = "hs_init_with_rtsopts";
                break;
            }
        }
    }
    mcf->hs_init = (void (*)(int *, char ***)) dlsym(mcf->dl_handle, hs_init);
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"%s\": %s", hs_init, dl_error);
        return NGX_ERROR;
    }

    mcf->hs_exit = (void (*)(void)) dlsym(mcf->dl_handle, "hs_exit");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_exit\": %s", dl_error);
        return NGX_ERROR;
    }

    mcf->hs_add_root = (void (*)(void (*)(void))) dlsym(mcf->dl_handle,
                                                        "hs_add_root");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_add_root\": %s", dl_error);
        return NGX_ERROR;
    }

    mcf->init_HsModule = (void (*)(void)) dlsym(mcf->dl_handle,
                                            "__stginit_NgxHaskellUserRuntime");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function "
                      "\"__stginit_NgxHaskellUserRuntime\": %s", dl_error);
        return NGX_ERROR;
    }

    argc = mcf->program_options.nelts + 1;
    if (mcf->rts_options.nelts > 0) {
        argc += mcf->rts_options.nelts + 1;
    }
    argv = ngx_alloc(argc * sizeof(char *), cycle->log);
    if (argv == NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to allocate artifacts for haskell init options");
        return NGX_ERROR;
    }
    argv[0] = "NgxHaskellUserRuntime";
    for (i = 0; i < mcf->program_options.nelts; i++) {
        argv[i + 1] = ((char **) mcf->program_options.elts)[i];
    }
    if (mcf->rts_options.nelts > 0) {
        argv[mcf->program_options.nelts + 1] = "+RTS";
        for (i = 0; i < mcf->rts_options.nelts; i++) {
            argv[mcf->program_options.nelts + 2 + i] =
                    ((char **) mcf->rts_options.elts)[i];
        }
    }
    mcf->hs_init(&argc, &argv);
    ngx_free(argv);
    mcf->hs_add_root(mcf->init_HsModule);

    handlers = mcf->handlers.elts;

    for (i = 0; i < mcf->handlers.nelts; i++) {
        ngx_str_t     handler_name;
        int         (*type_checker)(void);
        char         *type_checker_name = NULL;
        ngx_uint_t    wrong_n_args = 0;

        if (handlers[i].name.len <= haskell_module_handler_prefix.len
            || ngx_strncmp(handlers[i].name.data,
                           haskell_module_handler_prefix.data,
                           haskell_module_handler_prefix.len) != 0)
        {
            continue;
        }

        handler_name.len = handlers[i].name.len -
                haskell_module_handler_prefix.len;
        handler_name.data = handlers[i].name.data +
                haskell_module_handler_prefix.len;

        handlers[i].self = dlsym(mcf->dl_handle,
                                 (char *) handlers[i].name.data);
        dl_error = dlerror();
        if (dl_error != NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to load haskell handler \"%V\": %s",
                          &handler_name, dl_error);
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        type_checker_name = ngx_alloc(
            haskell_module_type_checker_prefix.len + handlers[i].name.len + 1,
            cycle->log);
        if (type_checker_name == NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to allocate artifacts for type checker");
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        ngx_memcpy(type_checker_name,
                   haskell_module_type_checker_prefix.data,
                   haskell_module_type_checker_prefix.len);
        ngx_memcpy(type_checker_name + haskell_module_type_checker_prefix.len,
                   handlers[i].name.data, handlers[i].name.len + 1);

        type_checker = dlsym(mcf->dl_handle, type_checker_name);
        dl_error = dlerror();
        ngx_free(type_checker_name);
        if (dl_error != NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to load haskell handler type checker \"%V\": "
                          "%s", &handler_name, dl_error);
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        handlers[i].type = type_checker();

        if ((handlers[i].role == ngx_http_haskell_handler_role_content_handler
             && (handlers[i].type != ngx_http_haskell_handler_type_ch
                && handlers[i].type != ngx_http_haskell_handler_type_uch
                && handlers[i].type != ngx_http_haskell_handler_type_y_y))
            ||
            (handlers[i].role == ngx_http_haskell_handler_role_variable
             && (handlers[i].type == ngx_http_haskell_handler_type_ch
                 || handlers[i].type == ngx_http_haskell_handler_type_uch))
            ||
            ((handlers[i].role == ngx_http_haskell_handler_role_async_variable
              || handlers[i].role ==
                    ngx_http_haskell_handler_role_service_variable)
             && handlers[i].type != ngx_http_haskell_handler_type_ioy_y))
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "haskell handler \"%V\" role and type mismatch",
                          &handler_name);
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        if (handlers[i].role == ngx_http_haskell_handler_role_content_handler
            && ((handlers[i].type == ngx_http_haskell_handler_type_ch
                 && handlers[i].unsafe)
                || (handlers[i].type == ngx_http_haskell_handler_type_uch
                    && !handlers[i].unsafe)))
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "haskell handler \"%V\" unsafety attribute mismatch",
                          &handler_name);
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        switch (handlers[i].type) {
        case ngx_http_haskell_handler_type_s_s:
        case ngx_http_haskell_handler_type_b_s:
        case ngx_http_haskell_handler_type_y_y:
        case ngx_http_haskell_handler_type_b_y:
        case ngx_http_haskell_handler_type_ioy_y:
        case ngx_http_haskell_handler_type_ch:
        case ngx_http_haskell_handler_type_uch:
            wrong_n_args = handlers[i].n_args[0] == 0
                        || handlers[i].n_args[1] > 0
                        || handlers[i].n_args[2] > 0 ? 1 : 0;
            break;
        case ngx_http_haskell_handler_type_s_ss:
        case ngx_http_haskell_handler_type_b_ss:
            wrong_n_args = handlers[i].n_args[0] > 0
                        || handlers[i].n_args[1] == 0
                        || handlers[i].n_args[2] > 0 ? 1 : 0;
            break;
        case ngx_http_haskell_handler_type_s_ls:
        case ngx_http_haskell_handler_type_b_ls:
            break;
        default:
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        if (wrong_n_args) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "actual type of haskell handler \"%V\" "
                          "does not match call samples", &handler_name);
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }
    }

    return NGX_OK;
}


static void
ngx_http_haskell_unload(ngx_cycle_t *cycle)
{
    ngx_http_haskell_main_conf_t    *mcf;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return;
    }

    if (mcf->dl_handle != NULL) {
        mcf->hs_exit();
        dlclose(mcf->dl_handle);
        mcf->dl_handle = NULL;
    }
}


static ngx_int_t
ngx_http_haskell_init_services(ngx_cycle_t *cycle)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return NGX_OK;
    }

    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (ngx_http_haskell_run_service(cycle, &service_code_vars[i], 1)
            != NGX_OK)
        {
            return NGX_ERROR;
        }
    }

    return NGX_OK;
}


static void
ngx_http_haskell_stop_services(ngx_cycle_t *cycle)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return;
    }

    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (service_code_vars[i].event != NULL) {
            if (ngx_del_event(service_code_vars[i].event, NGX_READ_EVENT, 0)
                != NGX_OK)
            {
                ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                              "failed to delete event while stopping service");
            }
            if (close(service_code_vars[i].fd) == -1) {
                ngx_log_error(NGX_LOG_CRIT, cycle->log, ngx_errno,
                              "failed to close reading end of pipe while "
                              "stopping service");
            }
            ngx_free(service_code_vars[i].async_data.result.data);
        }
    }
}


static ngx_int_t
ngx_http_haskell_run_service(ngx_cycle_t *cycle,
                    ngx_http_haskell_service_code_var_data_t *service_code_var,
                    ngx_uint_t service_first_run)
{
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_handler_t                *handlers;
    ngx_event_t                               *event;
    ngx_http_haskell_service_async_event_t    *hev;
    ngx_http_complex_value_t                  *args;
    ngx_str_t                                  arg1;
    ngx_fd_t                                   fd[2];

    service_code_var->event = NULL;

    if (pipe2(fd, O_NONBLOCK) == -1) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, ngx_errno,
                      "failed to create pipe for "
                      "future async result, skipping IO task");
        return NGX_ERROR;
    }

    hev = ngx_pcalloc(cycle->pool,
                      sizeof(ngx_http_haskell_service_async_event_t));
    if (hev == NULL) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to allocate memory for "
                      "future async result, skipping IO task");
        close_pipe(cycle->log, fd);
        return NGX_ERROR;
    }
    hev->s.fd = fd[0];
    hev->service_code_var = service_code_var;
    hev->cycle = cycle;

    event = ngx_pcalloc(cycle->pool, sizeof(ngx_event_t));
    if (event== NULL) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to allocate memory for "
                      "future async result, skipping IO task");
        close_pipe(cycle->log, fd);
        return NGX_ERROR;
    }
    event->data = hev;
    event->handler = ngx_http_haskell_service_async_event;
    event->log = cycle->log;
    hev->s.read = event;
    hev->s.write = event;   /* to make ngx_add_event() happy */
    if (ngx_add_event(event, NGX_READ_EVENT, 0) != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to add event for "
                      "future async result, skipping IO task");
        close_pipe(cycle->log, fd);
        return NGX_ERROR;
    }
    service_code_var->event = event;
    service_code_var->fd = fd[0];

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;

    args = service_code_var->data->args.elts;
    arg1 = args[0].value;
    ((ngx_http_haskell_handler_async_ioy_y)
     handlers[service_code_var->data->handler].self)
            (arg1.data, arg1.len, fd[1], service_first_run,
             &service_code_var->future_async_data.result.data,
             &service_code_var->future_async_data.result.len,
             &service_code_var->future_async_data.error);

    return NGX_OK;
}


static char *
ngx_http_haskell_run(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_loc_conf_t               *lcf = conf;

    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_str_t                                 *value;
    ngx_uint_t                                 n_args, n_size;
    ngx_str_t                                  handler_name;
    ngx_http_haskell_handler_t                *handlers;
    ngx_http_compile_complex_value_t           ccv;
    ngx_http_complex_value_t                  *args;
    ngx_http_variable_t                       *v;
    ngx_http_haskell_code_var_data_t          *code_var_data;
    ngx_http_haskell_service_code_var_data_t  *service_code_var_data;
    ngx_int_t                                  v_idx;
    ngx_uint_t                                *v_idx_ptr;
    ngx_uint_t                                 async, service;

    value = cf->args->elts;

    service = value[0].len == 19
            && ngx_strncmp(value[0].data, "haskell_run_service", 19) == 0;

    mcf = service ? conf :
            ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);

    if (!mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "haskell code was not loaded");
        return NGX_CONF_ERROR;
    }

    if ((!service && cf->args->nelts < 4) || cf->args->nelts < 3) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "too few arguments");
        return NGX_CONF_ERROR;
    }
    n_args = cf->args->nelts - 3;
    n_size = n_args == 0 ? 1 : n_args;

    if (value[2].len < 2 || value[2].data[0] != '$') {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid variable name \"%V\"", &value[2]);
        return NGX_CONF_ERROR;
    }
    value[2].len--;
    value[2].data++;

    handler_name.len = value[1].len + haskell_module_handler_prefix.len;
    handler_name.data = ngx_pnalloc(cf->pool, handler_name.len + 1);
    if (handler_name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(handler_name.data,
               haskell_module_handler_prefix.data,
               haskell_module_handler_prefix.len);
    ngx_memcpy(handler_name.data + haskell_module_handler_prefix.len,
               value[1].data, value[1].len);
    handler_name.data[handler_name.len] ='\0';

    if (service) {
        service_code_var_data = ngx_array_push(&mcf->service_code_vars);
        if (service_code_var_data == NULL) {
            return NGX_CONF_ERROR;
        }
        code_var_data = ngx_palloc(cf->pool,
                                   sizeof(ngx_http_haskell_code_var_data_t));
        if (code_var_data == NULL) {
            return NGX_CONF_ERROR;
        }
        ngx_memzero(service_code_var_data,
                    sizeof(ngx_http_haskell_service_code_var_data_t));
        service_code_var_data->data = code_var_data;
    } else {
        code_var_data = ngx_array_push(&lcf->code_vars);
        if (code_var_data == NULL) {
            return NGX_CONF_ERROR;
        }
    }
    if (ngx_array_init(&code_var_data->args, cf->pool, n_size,
                       sizeof(ngx_http_complex_value_t)) != NGX_OK)
    {
        return NGX_CONF_ERROR;
    }

    code_var_data->handler = NGX_ERROR;

    async = ngx_strncmp(value[0].data, "haskell_run_async", 17) == 0;
    mcf->has_async_tasks = mcf->has_async_tasks ? 1 : async;

    async = async ? 1 : service;
    if (async) {
        if (mcf->compile_mode == ngx_http_haskell_compile_mode_no_threaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "haskell module was compiled without thread "
                               "support, using async tasks will inevitably "
                               "cause stalls of requests in runtime");
            return NGX_CONF_ERROR;
        }
        if (mcf->compile_mode == ngx_http_haskell_compile_mode_no_threaded) {
            ngx_conf_log_error(NGX_LOG_NOTICE, cf, 0,
                               "haskell module was loaded from existing "
                               "library, please make sure that it was compiled "
                               "as threaded, otherwise async tasks may stall "
                               "in runtime");
        }
    }

    handlers = mcf->handlers.elts;
    for (i = 0; i < mcf->handlers.nelts; i++) {
        if (handler_name.len == handlers[i].name.len
            && ngx_strncmp(handler_name.data, handlers[i].name.data,
                           handler_name.len) == 0)
        {
            if (handlers[i].role ==
                ngx_http_haskell_handler_role_content_handler)
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared as content handler", &value[1]);
                return NGX_CONF_ERROR;
            }
            if ((handlers[i].role ==
                 ngx_http_haskell_handler_role_variable && async)
                || ((handlers[i].role ==
                    ngx_http_haskell_handler_role_async_variable
                   || handlers[i].role ==
                        ngx_http_haskell_handler_role_service_variable)
                        && !async))
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" async attribute "
                                   "mismatch", &value[1]);
                return NGX_CONF_ERROR;
            }
            code_var_data->handler = i;
            break;
        }
    }
    if (code_var_data->handler == NGX_ERROR) {
        ngx_http_haskell_handler_t  *handler;

        handler = ngx_array_push(&mcf->handlers);
        if (handler == NULL) {
            return NGX_CONF_ERROR;
        }

        handler->self = NULL;
        handler->type = ngx_http_haskell_handler_type_uninitialized;
        handler->name = handler_name;
        ngx_memzero(&handler->n_args, sizeof(handler->n_args));
        handler->role = service ?
                ngx_http_haskell_handler_role_service_variable :
                (async ? ngx_http_haskell_handler_role_async_variable :
                    ngx_http_haskell_handler_role_variable);

        handlers = mcf->handlers.elts;
        code_var_data->handler = mcf->handlers.nelts - 1;
    }

    ++handlers[code_var_data->handler].n_args[n_args > 2 ? 2 : n_size - 1];

    v = ngx_http_add_variable(cf, &value[2], NGX_HTTP_VAR_CHANGEABLE);
    if (v == NULL) {
        return NGX_CONF_ERROR;
    }

    v_idx = ngx_http_get_variable_index(cf, &value[2]);
    if (v_idx == NGX_ERROR) {
        return NGX_CONF_ERROR;
    }

    v_idx_ptr = ngx_palloc(cf->pool, sizeof(ngx_uint_t));
    if (v_idx_ptr == NULL) {
        return NGX_CONF_ERROR;
    }

    code_var_data->index = v_idx;
    *v_idx_ptr = v_idx;

    v->data = (uintptr_t) v_idx_ptr;
    v->get_handler = service ? ngx_http_haskell_run_service_handler :
            (async ? ngx_http_haskell_run_async_handler :
             ngx_http_haskell_run_handler);

    if (ngx_array_push_n(&code_var_data->args, n_size) == NULL) {
        return NGX_CONF_ERROR;
    }
    args = code_var_data->args.elts;

    if (service) {
        ngx_str_null(&args[0].value);
        for (i = 0; i < n_args; i++) {
            args[i].value = value[3 + i];
        }
    } else {
        ngx_memzero(&ccv, sizeof(ngx_http_compile_complex_value_t));
        ccv.cf = cf;

        for (i = 0; i < n_args; i++) {
            ccv.value = &value[3 + i];
            ccv.complex_value = &args[i];

            if (ngx_http_compile_complex_value(&ccv) != NGX_OK) {
                return NGX_CONF_ERROR;
            }
        }
    }

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_content(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_loc_conf_t       *lcf = conf;

    ngx_uint_t                         i;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_str_t                         *value;
    ngx_str_t                          handler_name;
    ngx_http_haskell_handler_t        *handlers;
    ngx_http_core_loc_conf_t          *clcf;
    ngx_uint_t                         unsafe = 0;

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);

    if (!mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "haskell code was not loaded");
        return NGX_CONF_ERROR;
    }

    if (lcf->content_handler != NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "haskell handler was already set");
        return NGX_CONF_ERROR;
    }

    value = cf->args->elts;

    if (value[0].len == 22
        && ngx_strncmp(value[0].data, "haskell_static_content", 22) == 0)
    {
        lcf->static_content = 1;
        lcf->pool = cf->pool;
    } else if (value[0].len == 22
        && ngx_strncmp(value[0].data, "haskell_unsafe_content", 22) == 0)
    {
        unsafe = 1;
    }

    handler_name.len = value[1].len + haskell_module_handler_prefix.len;
    handler_name.data = ngx_pnalloc(cf->pool, handler_name.len + 1);
    if (handler_name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(handler_name.data,
               haskell_module_handler_prefix.data,
               haskell_module_handler_prefix.len);
    ngx_memcpy(handler_name.data + haskell_module_handler_prefix.len,
               value[1].data, value[1].len);
    handler_name.data[handler_name.len] ='\0';

    lcf->content_handler = ngx_pcalloc(cf->pool,
                                    sizeof(ngx_http_haskell_content_handler_t));

    if (lcf->content_handler == NULL) {
        return NGX_CONF_ERROR;
    }

    lcf->content_handler->handler = NGX_ERROR;
    handlers = mcf->handlers.elts;

    for (i = 0; i < mcf->handlers.nelts; i++) {
        if (handler_name.len == handlers[i].name.len
            && ngx_strncmp(handler_name.data, handlers[i].name.data,
                           handler_name.len) == 0)
        {
            if (handlers[i].role == ngx_http_haskell_handler_role_variable
                || handlers[i].role ==
                        ngx_http_haskell_handler_role_async_variable
                || handlers[i].role ==
                        ngx_http_haskell_handler_role_service_variable)
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared as a variable handler", &value[1]);
                return NGX_CONF_ERROR;
            }
            if (handlers[i].unsafe != unsafe) {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared with a different unsafety "
                                   "attribute", &value[1]);
                return NGX_CONF_ERROR;
            }
            lcf->content_handler->handler = i;
            break;
        }
    }
    if (lcf->content_handler->handler == NGX_ERROR) {
        ngx_http_haskell_handler_t  *handler;

        handler = ngx_array_push(&mcf->handlers);
        if (handler == NULL) {
            return NGX_CONF_ERROR;
        }

        handler->self = NULL;
        handler->type = ngx_http_haskell_handler_type_uninitialized;
        handler->name = handler_name;
        ngx_memzero(&handler->n_args, sizeof(handler->n_args));
        handler->role = ngx_http_haskell_handler_role_content_handler;
        handler->unsafe = unsafe;

        handlers = mcf->handlers.elts;
        lcf->content_handler->handler = mcf->handlers.nelts - 1;
    }

    ++handlers[lcf->content_handler->handler].n_args[0];

    if (cf->args->nelts == 3) {
        ngx_http_compile_complex_value_t   ccv;

        lcf->content_handler->args = ngx_pcalloc(cf->pool,
                                            sizeof(ngx_http_complex_value_t));

        if (lcf->content_handler->args == NULL) {
            return NGX_CONF_ERROR;
        }

        ngx_memzero(&ccv, sizeof(ngx_http_compile_complex_value_t));
        ccv.cf = cf;

        ccv.value = &value[2];
        ccv.complex_value = lcf->content_handler->args;

        if (ngx_http_compile_complex_value(&ccv) != NGX_OK) {
            return NGX_CONF_ERROR;
        }
    }

    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_haskell_content_handler;

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_var_configure(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_main_conf_t      *mcf = conf;

    ngx_uint_t                         i;
    ngx_str_t                         *value;
    ngx_array_t                       *data;
    ngx_http_haskell_var_handle_t     *vars;
    ngx_uint_t                         n_vars;

    value = cf->args->elts;

    if (value[0].len == 23
        && ngx_strncmp(value[0].data, "haskell_var_nocacheable", 23)
        == 0)
    {
        data = &mcf->var_nocacheable;
    } else if (value[0].len == 34
        && ngx_strncmp(value[0].data, "haskell_var_compensate_uri_changes", 34)
        == 0)
    {
        data = &mcf->var_compensate_uri_changes;
    } else if (value[0].len == 32
        && ngx_strncmp(value[0].data, "haskell_service_var_ignore_empty", 32)
        == 0)
    {
        data = &mcf->service_var_ignore_empty;
    } else {
        return NGX_CONF_ERROR;
    }

    if (data->nalloc > 0) {
        return "is duplicate";
    }

    n_vars = cf->args->nelts - 1;

    if (ngx_array_init(data, cf->pool, n_vars,
                       sizeof(ngx_http_haskell_var_handle_t)) != NGX_OK
        || ngx_array_push_n(data, n_vars) == NULL)
    {
        return NGX_CONF_ERROR;
    }

    vars = data->elts;

    for (i = 0; i < n_vars; i++) {
        if (value[i + 1].len < 2 || value[i + 1].data[0] != '$') {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "invalid variable name \"%V\"", &value[i + 1]);
            return NGX_CONF_ERROR;
        }
        value[i + 1].len--;
        value[i + 1].data++;
        vars[i].name = value[i + 1];
        vars[i].index = -1;
    }

    return NGX_CONF_OK;
}


static ngx_int_t
ngx_http_haskell_run_handler(ngx_http_request_t *r,
                             ngx_http_variable_value_t *v, uintptr_t  data)
{
    ngx_uint_t                         i;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_http_haskell_loc_conf_t       *lcf;
    ngx_http_core_main_conf_t         *cmcf;
    ngx_http_haskell_ctx_t            *ctx;
    ngx_int_t                         *index = (ngx_int_t *) data;
    ngx_int_t                          found_idx = NGX_ERROR;
    ngx_http_variable_t               *vars;
    ngx_http_haskell_var_handle_t     *vars_comp;
    ngx_http_haskell_var_cache_t      *var_nocacheable_cache;
    ngx_http_haskell_handler_t        *handlers;
    ngx_http_haskell_code_var_data_t  *code_vars;
    ngx_http_complex_value_t          *args;
    ngx_str_t                          arg1, arg2, *argn = NULL;
    char                              *res = NULL;
    ngx_pool_cleanup_t                *cln;
    ngx_int_t                          len;

    if (index == NULL) {
        return NGX_ERROR;
    }

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (ctx) {
        var_nocacheable_cache = ctx->var_nocacheable_cache.elts;
        for (i = 0; i < ctx->var_nocacheable_cache.nelts; i++) {
            if (var_nocacheable_cache[i].index == *index
                && var_nocacheable_cache[i].checked)
            {
                v->len = var_nocacheable_cache[i].value.len;
                v->data = var_nocacheable_cache[i].value.data;
                v->valid = 1;
                v->no_cacheable = 0;
                v->not_found = 0;

                return NGX_OK;
            }
        }
    }

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    code_vars = lcf->code_vars.elts;

    for (i = 0; i < lcf->code_vars.nelts; i++) {
        if (*index == code_vars[i].index) {
            found_idx = i;
            break;
        }
    }
    if (found_idx == NGX_ERROR) {
        return NGX_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;
    args = code_vars[found_idx].args.elts;

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_s_ss:
    case ngx_http_haskell_handler_type_b_ss:
        if (ngx_http_complex_value(r, &args[1], &arg2) != NGX_OK) {
            return NGX_ERROR;
        }
    case ngx_http_haskell_handler_type_s_s:
    case ngx_http_haskell_handler_type_b_s:
    case ngx_http_haskell_handler_type_y_y:
    case ngx_http_haskell_handler_type_b_y:
    case ngx_http_haskell_handler_type_ioy_y:
        if (ngx_http_complex_value(r, &args[0], &arg1) != NGX_OK) {
            return NGX_ERROR;
        }
        break;
    case ngx_http_haskell_handler_type_s_ls:
    case ngx_http_haskell_handler_type_b_ls:
        argn = ngx_palloc(r->pool,
                          sizeof(ngx_str_t) * code_vars[found_idx].args.nelts);
        if (argn == NULL) {
            return NGX_ERROR;
        }
        for (i = 0; i < code_vars[found_idx].args.nelts; i++) {
            if (ngx_http_complex_value(r, &args[i], &argn[i]) != NGX_OK) {
                return NGX_ERROR;
            }
        }
        break;
    default:
        return NGX_ERROR;
    }

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_s_s:
        len = ((ngx_http_haskell_handler_s_s)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res);
        break;
    case ngx_http_haskell_handler_type_s_ss:
        len = ((ngx_http_haskell_handler_s_ss)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, arg2.data, arg2.len, &res);
        break;
    case ngx_http_haskell_handler_type_s_ls:
        len = ((ngx_http_haskell_handler_s_ls)
               handlers[code_vars[found_idx].handler].self)
                    (argn, code_vars[found_idx].args.nelts, &res);
        break;
    case ngx_http_haskell_handler_type_b_s:
        len = ((ngx_http_haskell_handler_b_s)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len);
        break;
    case ngx_http_haskell_handler_type_b_ss:
        len = ((ngx_http_haskell_handler_b_ss)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, arg2.data, arg2.len);
        break;
    case ngx_http_haskell_handler_type_b_ls:
        len = ((ngx_http_haskell_handler_b_ls)
               handlers[code_vars[found_idx].handler].self)
                    (argn, code_vars[found_idx].args.nelts);
        break;
    case ngx_http_haskell_handler_type_y_y:
    case ngx_http_haskell_handler_type_ioy_y:
        len = ((ngx_http_haskell_handler_y_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res);
        break;
    case ngx_http_haskell_handler_type_b_y:
        len = ((ngx_http_haskell_handler_b_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len);
        break;
    default:
        return NGX_ERROR;
    }

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    vars = cmcf->variables.elts;

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_y_y:
    case ngx_http_haskell_handler_type_ioy_y:
        if (len == -1) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "memory allocation error while running "
                          "haskell handler");
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "memory allocation error while getting "
                          "value of variable \"%V\"",
                          &vars[*index].name);
            return NGX_ERROR;
        }
    case ngx_http_haskell_handler_type_s_s:
    case ngx_http_haskell_handler_type_s_ss:
    case ngx_http_haskell_handler_type_s_ls:
        if (res == NULL) {
            if (len == 0) {
                res = "";
            } else {
                ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                              "impossible branch while running "
                              "haskell handler");
                return NGX_ERROR;
            }
        } else {
            cln = ngx_pool_cleanup_add(r->pool, 0);
            if (cln == NULL) {
                ngx_free(res);
                return NGX_ERROR;
            }
            cln->handler = ngx_http_haskell_variable_cleanup;
            cln->data = res;
        }
        break;
    case ngx_http_haskell_handler_type_b_s:
    case ngx_http_haskell_handler_type_b_ss:
    case ngx_http_haskell_handler_type_b_ls:
    case ngx_http_haskell_handler_type_b_y:
        res = len == 1 ? "1" : "0";
        len = 1;
        break;
    default:
        return NGX_ERROR;
    }

    if (r->internal) {
        vars_comp = mcf->var_compensate_uri_changes.elts;
        for (i = 0; i < mcf->var_compensate_uri_changes.nelts; i++) {
            if (vars_comp[i].index == *index
                && r->uri_changes < NGX_HTTP_MAX_URI_CHANGES + 1)
            {
                ++r->uri_changes;
                break;
            }
        }
    }

    if (ctx) {
        var_nocacheable_cache = ctx->var_nocacheable_cache.elts;
        for (i = 0; i < ctx->var_nocacheable_cache.nelts; i++) {
            if (var_nocacheable_cache[i].index == *index) {
                var_nocacheable_cache[i].checked = 1;
                var_nocacheable_cache[i].value.len = len;
                var_nocacheable_cache[i].value.data = (u_char *) res;
                break;
            }
        }
    }

    v->len = len;
    v->data = (u_char *) res;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}


static ngx_int_t
ngx_http_haskell_run_async_handler(ngx_http_request_t *r,
                                   ngx_http_variable_value_t *v,
                                   uintptr_t  data)
{
    ngx_uint_t                         i;
    ngx_http_core_main_conf_t         *cmcf;
    ngx_http_haskell_ctx_t            *ctx;
    ngx_http_haskell_async_data_t     *async_data_elts;
    ngx_int_t                         *index = (ngx_int_t *) data;
    ngx_int_t                          found_idx = NGX_ERROR;
    ngx_http_variable_t               *vars;

    if (index == NULL) {
        return NGX_ERROR;
    }

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (ctx == NULL) {
        return NGX_ERROR;
    }
    async_data_elts = ctx->async_data.elts;

    for (i = 0; i < ctx->async_data.nelts; i++) {
        if (*index == async_data_elts[i].index) {
            found_idx = i;
            break;
        }
    }
    if (found_idx == NGX_ERROR) {
        return NGX_ERROR;
    }

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    vars = cmcf->variables.elts;
    if (async_data_elts[found_idx].result.len == (ngx_uint_t) -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while getting "
                      "value of variable \"%V\" asynchronously",
                      &vars[*index].name);
        return NGX_ERROR;
    }

    if (async_data_elts[found_idx].error) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "an exception was caught while getting "
                      "value of variable \"%V\" asynchronously: \"%V\"",
                      &vars[*index].name,
                      &async_data_elts[found_idx].result);
    }

    v->len = async_data_elts[found_idx].result.len;
    v->data = async_data_elts[found_idx].result.data;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}


static ngx_int_t
ngx_http_haskell_run_service_handler(ngx_http_request_t *r,
                                     ngx_http_variable_value_t *v,
                                     uintptr_t  data)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_core_main_conf_t                 *cmcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_int_t                                 *index = (ngx_int_t *) data;
    ngx_int_t                                  found_idx = NGX_ERROR;
    ngx_http_variable_t                       *vars;

    if (index == NULL) {
        return NGX_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (*index == service_code_vars[i].data->index) {
            found_idx = i;
            break;
        }
    }
    if (found_idx == NGX_ERROR) {
        return NGX_ERROR;
    }

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    vars = cmcf->variables.elts;
    if (service_code_vars[found_idx].async_data.result.len == (ngx_uint_t) -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while getting "
                      "value of variable \"%V\" asynchronously",
                      &vars[*index].name);
        return NGX_ERROR;
    }

    if (service_code_vars[found_idx].async_data.error) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "an exception was caught while getting "
                      "value of variable \"%V\" asynchronously: \"%V\"",
                      &vars[*index].name,
                      &service_code_vars[found_idx].async_data.result);
    }

    v->len = service_code_vars[found_idx].async_data.result.len;
    v->data = service_code_vars[found_idx].async_data.result.data;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}


static ngx_int_t
ngx_http_haskell_content_handler(ngx_http_request_t *r)
{
    ngx_int_t                                 i;
    ngx_http_haskell_main_conf_t             *mcf;
    ngx_http_haskell_loc_conf_t              *lcf;
    ngx_http_haskell_handler_t               *handlers;
    ngx_http_complex_value_t                 *args;
    ngx_str_t                                 arg = ngx_null_string;
    ngx_str_t                                 ct = ngx_string("text/plain");
    ngx_int_t                                 len = 0, st = NGX_HTTP_OK;
    size_t                                    slen;
    ngx_str_t                                *res = NULL;
    u_char                                   *sres = NULL;
    ngx_chain_t                              *out, *out_cur;
    ngx_buf_t                                *b;
    ngx_pool_cleanup_t                       *cln;
    ngx_http_haskell_content_handler_data_t  *clnd;
    ngx_pool_t                               *pool;
    ngx_uint_t                                def_handler;
    ngx_int_t                                 rc;

    if (ngx_http_discard_request_body(r) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;

    def_handler = handlers[lcf->content_handler->handler].type ==
                                ngx_http_haskell_handler_type_y_y ? 1 : 0;

    if (lcf->static_content && lcf->content_handler_data != NULL) {
        res = lcf->content_handler_data->bufs;
        len = lcf->content_handler_data->size;
        if (!def_handler) {
            ct = lcf->content_handler_data->content_type;
        }
        st = lcf->content_handler_data->status;
        goto send_response;
    }

    args = lcf->content_handler->args;

    if (args && ngx_http_complex_value(r, args, &arg) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    switch (handlers[lcf->content_handler->handler].type) {
    case ngx_http_haskell_handler_type_y_y:
        len = ((ngx_http_haskell_handler_dch)
               handlers[lcf->content_handler->handler].self)
                    (arg.data, arg.len, &res);
        break;
    case ngx_http_haskell_handler_type_ch:
        st = ((ngx_http_haskell_handler_ch)
              handlers[lcf->content_handler->handler].self)
                   (arg.data, arg.len, &res, &len, &ct.data, &ct.len);
        break;
    case ngx_http_haskell_handler_type_uch:
        st = ((ngx_http_haskell_handler_uch)
              handlers[lcf->content_handler->handler].self)
                   (arg.data, arg.len, &sres, &slen, &ct.data, &ct.len);
        len = slen;
        goto send_response;
    default:
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (len == -1 || ct.data == NULL) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while running "
                      "haskell content handler");
        goto cleanup;
    }

    if (res == NULL && len != 0) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "impossible branch while running "
                      "haskell content handler");
        goto cleanup;
    }

    pool = lcf->static_content ? lcf->pool : r->pool;

    if (!lcf->static_content || lcf->content_handler_data == NULL) {
        cln = ngx_pool_cleanup_add(pool, 0);
        if (cln == NULL) {
            goto cleanup;
        }
        clnd = ngx_palloc(pool,
                          sizeof(ngx_http_haskell_content_handler_data_t));
        if (clnd == NULL) {
            goto cleanup;
        }
        clnd->bufs = res;
        clnd->size = len;
        clnd->content_type.len = def_handler ? 0 : ct.len;
        clnd->content_type.data = def_handler ? NULL : ct.data;
        clnd->status = st;
        cln->handler = ngx_http_haskell_content_handler_cleanup;
        cln->data = clnd;
    }

    if (lcf->static_content && lcf->content_handler_data == NULL) {
        lcf->content_handler_data = clnd;
    }

send_response:

    r->headers_out.content_type_len = ct.len;
    r->headers_out.content_type = ct;
    r->headers_out.content_type_lowcase = NULL;
    r->headers_out.status = st;
    r->headers_out.content_length_n = 0;

    out = NULL;

    if (len > 0) {
        if (handlers[lcf->content_handler->handler].type ==
            ngx_http_haskell_handler_type_uch)
        {
            out = ngx_pcalloc(r->pool, sizeof(ngx_chain_t));
            if (out == NULL) {
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }
            b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
            if (b == NULL) {
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }

            b->pos = b->start = sres;
            b->last = b->end = sres + len;
            b->memory = 1;
            b->last_buf = (r == r->main) ? 1 : 0;
            b->last_in_chain = 1;
            r->headers_out.content_length_n = len;

            out->buf = b;
            out->next = NULL;
        } else {
            for (i = len - 1; i >= 0; i--) {
                out_cur = out;
                out = ngx_pcalloc(r->pool, sizeof(ngx_chain_t));
                if (out == NULL) {
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
                if (b == NULL) {
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }

                b->pos = b->start = res[i].data;
                b->last = b->end = res[i].data + res[i].len;
                b->memory = 1;
                if (out_cur == NULL) {
                    b->last_buf = (r == r->main) ? 1 : 0;
                    b->last_in_chain = 1;
                }
                r->headers_out.content_length_n += res[i].len;

                out->buf = b;
                out->next = out_cur;
            }
        }
    }

    r->header_only = r->headers_out.content_length_n == 0 ? 1 : 0;

    rc = ngx_http_send_header(r);

    if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
        return rc;
    }

    return ngx_http_output_filter(r, out);

cleanup:

    if (res != NULL) {
        for (i = 0; i < len; i++) {
            ngx_free(res[i].data);
        }
        ngx_free(res);
    }
    if (!def_handler) {
        ngx_free(ct.data);
    }

    return NGX_HTTP_INTERNAL_SERVER_ERROR;
}


static void
ngx_http_haskell_async_event(ngx_event_t *ev)
{
    ngx_http_haskell_async_event_t    *hev = ev->data;

    if (close(hev->s.fd) == -1) {
        ngx_log_error(NGX_LOG_CRIT, hev->r->connection->log, ngx_errno,
                      "failed to close reading end of pipe after async task "
                      "was finished");
    }

    ngx_http_core_run_phases(hev->r);
}


static void
ngx_http_haskell_service_async_event(ngx_event_t *ev)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_service_async_event_t    *hev = ev->data;

    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_var;
    ngx_http_haskell_var_handle_t             *service_var_ignore_empty;
    ngx_uint_t                                 ignore_empty = 0;

    service_code_var = hev->service_code_var;
    if (close(hev->s.fd) == -1) {
        ngx_log_error(NGX_LOG_CRIT, hev->cycle->log, ngx_errno,
                      "failed to close reading end of pipe after service task "
                      "was finished");
    }

    mcf = ngx_http_cycle_get_module_main_conf(hev->cycle,
                                              ngx_http_haskell_module);
    service_var_ignore_empty = mcf->service_var_ignore_empty.elts;

    for (i = 0; i < mcf->service_var_ignore_empty.nelts; i++) {
        if (service_var_ignore_empty[i].index == service_code_var->data->index)
        {
            ignore_empty = 1;
            break;
        }
    }

    if (ignore_empty && service_code_var->future_async_data.result.len == 0) {
        /* newCStringLen allocates memory even for zero size! */
        ngx_free(service_code_var->future_async_data.result.data);
    } else {
        ngx_free(service_code_var->async_data.result.data);
        service_code_var->async_data = service_code_var->future_async_data;
    }

    ngx_http_haskell_run_service(hev->cycle, service_code_var, 0);
}


static void
ngx_http_haskell_variable_cleanup(void *data)
{
    ngx_free(data);
}


static void
ngx_http_haskell_content_handler_cleanup(void *data)
{
    ngx_int_t                                 i;
    ngx_http_haskell_content_handler_data_t  *clnd = data;

    if (clnd->bufs != NULL) {
        for (i = 0; i < clnd->size; i++) {
            ngx_free(clnd->bufs[i].data);
        }
        ngx_free(clnd->bufs);
    }
    ngx_free(clnd->content_type.data);
}


static void
close_pipe(ngx_log_t *log, ngx_fd_t *fd)
{
    ngx_int_t  i;

    for (i = 0; i < 2; i++) {
        if (close(fd[i]) == -1) {
            ngx_log_error(NGX_LOG_CRIT, log, ngx_errno,
                          "failed to close file descriptor of pipe");
        }
    }
}

