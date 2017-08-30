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
#include <ghcversion.h>


static const ngx_str_t  haskell_module_handler_prefix =
ngx_string("ngx_hs_");
static const ngx_str_t  haskell_module_type_checker_prefix =
ngx_string("type_");
static const ngx_str_t  haskell_compile_cmd =
ngx_string("ghc -O2 -dynamic -shared -fPIC -o ");
static const ngx_str_t  template_haskell_option =
ngx_string(" -XTemplateHaskell");
static const ngx_str_t  ghc_rtslib =
ngx_string(
" -L$(ghc --print-libdir)/rts -lHSrts-ghc$(ghc --numeric-version)"
);
static const ngx_str_t  ghc_rtslib_thr =
ngx_string(
" -L$(ghc --print-libdir)/rts -lHSrts_thr-ghc$(ghc --numeric-version)"
);
static const ngx_int_t haskell_module_ngx_export_api_version_major = 0;
static const ngx_int_t haskell_module_ngx_export_api_version_minor = 7;

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
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_S_SS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_S_SS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_s_ss $ AUX_NGX_S_SS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_S_LS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_S_LS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_s_ls $ AUX_NGX_S_LS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_B_S(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_S, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_s $ AUX_NGX_B_S F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_B_SS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_SS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_ss $ AUX_NGX_B_SS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_B_LS(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_LS, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_ls $ AUX_NGX_B_LS F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_Y_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_Y_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_y_y $ AUX_NGX_Y_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt -> "
"\\\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_B_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_B_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_b_y $ AUX_NGX_B_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_IOY_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_IOY_Y, F, (const . F)) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_ioy_y $ AUX_NGX_IOY_Y (const . F); \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt -> "
"\\\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_ASYNC_IOY_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_IOY_Y, F, (const . F)) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_async_ioy_y $ AUX_NGX_IOY_Y (const . F); \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.CUInt -> AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt -> "
"\\\n"
"    AUX_NGX.Ptr AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> IO ();\n\n"

"#define NGX_EXPORT_ASYNC_ON_REQ_BODY(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_IOY_YY, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_async_ioy_yy $ AUX_NGX_IOY_YY F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> AUX_NGX.CInt -> AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt -> "
"\\\n"
"    AUX_NGX.Ptr AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> IO ();\n\n"

"#define NGX_EXPORT_SERVICE_IOY_Y(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_IOY_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_async_ioy_y $ AUX_NGX_IOY_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.CUInt -> AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt -> "
"\\\n"
"    AUX_NGX.Ptr AUX_NGX.CUInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> IO ();\n\n"

"#define NGX_EXPORT_HANDLER(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_HANDLER, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_handler $ AUX_NGX_HANDLER F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> "
"AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_DEF_HANDLER(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_Y_Y, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_def_handler $ AUX_NGX_Y_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> AUX_NGX.Ptr AUX_NGX.CString -> \\\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> \\\n"
"    IO AUX_NGX.CUInt;\n\n"

"#define NGX_EXPORT_UNSAFE_HANDLER(F) \\\n"
"AUX_NGX_TYPECHECK(AUX_NGX_UNSAFE_HANDLER, F, F) \\\n"
"ngx_hs_ ## F = aux_ngx_hs_unsafe_handler $ AUX_NGX_UNSAFE_HANDLER F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize -> \\\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> IO AUX_NGX.CUInt;\n\n"

"{-# LANGUAGE ViewPatterns #-}\n\n"

"module NgxHaskellUserRuntime where\n\n"

"-- requires packages base, cpphs, bytestring, async, binary and unix\n\n"

"import qualified Foreign.C as AUX_NGX\n"
"import qualified Foreign.Ptr as AUX_NGX\n"
"import qualified Foreign.StablePtr as AUX_NGX\n"
"import qualified Foreign.Storable as AUX_NGX\n"
"import qualified Foreign.Marshal.Alloc as AUX_NGX\n"
"import qualified Foreign.Marshal.Utils as AUX_NGX\n"
"import qualified System.IO.Error as AUX_NGX\n"
"import qualified System.Posix.IO as AUX_NGX\n"
"import qualified Control.Monad as AUX_NGX\n"
"import qualified Control.Exception as AUX_NGX\n"
"import qualified GHC.IO.Exception as AUX_NGX\n"
"import qualified Control.Concurrent.Async as AUX_NGX\n"
"import qualified Data.ByteString as AUX_NGX_BS\n"
"import qualified Data.ByteString.Unsafe as AUX_NGX_BS\n"
"import qualified Data.ByteString.Lazy as AUX_NGX_BSL\n"
"import qualified Data.ByteString.Lazy.Char8 as AUX_NGX_BSLC8\n"
"import qualified Data.Binary.Put as AUX_NGX\n\n"

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
"                    | AUX_NGX_IOY_YY (AUX_NGX_BSL.ByteString ->\n"
"                          AUX_NGX_BS.ByteString -> IO AUX_NGX_BSL.ByteString)"
"\n"
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
"    fromEnum (AUX_NGX_IOY_YY _)         = 10\n"
"    fromEnum (AUX_NGX_HANDLER _)        = 11\n"
"    fromEnum (AUX_NGX_UNSAFE_HANDLER _) = 12\n\n"

"aux_ngx_exportType :: AUX_NGX_EXPORT -> IO AUX_NGX.CInt\n"
"aux_ngx_exportType = return . fromIntegral . fromEnum\n\n"

"data AUX_NGX_STR_TYPE = AUX_NGX_STR_TYPE AUX_NGX.CSize AUX_NGX.CString\n\n"

"instance AUX_NGX.Storable AUX_NGX_STR_TYPE where\n"
"    alignment = const $ max (AUX_NGX.alignment (undefined :: AUX_NGX.CSize))\n"
"                            (AUX_NGX.alignment (undefined :: AUX_NGX.CString))"
"\n"
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

"aux_ngx_safeMallocBytes :: Int -> IO (AUX_NGX.Ptr a)\n"
"aux_ngx_safeMallocBytes =\n"
"    flip AUX_NGX.catchIOError (const $ return AUX_NGX.nullPtr) .\n"
"        AUX_NGX.mallocBytes\n"
"{-# INLINE aux_ngx_safeMallocBytes #-}\n\n"

"aux_ngx_safeNewCStringLen :: String -> IO AUX_NGX.CStringLen\n"
"aux_ngx_safeNewCStringLen =\n"
"    flip AUX_NGX.catchIOError (const $ return (AUX_NGX.nullPtr, -1)) .\n"
"        AUX_NGX.newCStringLen\n"
"{-# INLINE aux_ngx_safeNewCStringLen #-}\n\n"

"aux_ngx_peekNgxStringArrayLen :: AUX_NGX.Ptr AUX_NGX_STR_TYPE -> Int ->\n"
"    IO [String]\n"
"aux_ngx_peekNgxStringArrayLen x n = sequence $\n"
"    foldr (\\k ->\n"
"              ((AUX_NGX.peekElemOff x k >>=\n"
"                  (\\(AUX_NGX_STR_TYPE (fromIntegral -> m) y) ->\n"
"                      AUX_NGX.peekCStringLen (y, m))) :)) [] [0 .. n - 1]\n\n"

"aux_ngx_peekNgxStringArrayLenY :: AUX_NGX.Ptr AUX_NGX_STR_TYPE -> Int ->\n"
"    IO AUX_NGX_BSL.ByteString\n"
"aux_ngx_peekNgxStringArrayLenY x n = AUX_NGX_BSL.fromChunks <$> sequence\n"
"    (foldr (\\k ->\n"
"              ((AUX_NGX.peekElemOff x k >>=\n"
"                  (\\(AUX_NGX_STR_TYPE (fromIntegral -> m) y) ->\n"
"                      AUX_NGX_BS.unsafePackCStringLen (y, m))) :)) []\n"
"                           [0 .. n - 1])\n\n"

"aux_ngx_pokeCStringLen :: AUX_NGX.Storable a =>\n"
"    AUX_NGX.CString -> a -> AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr a ->\n"
"    IO ()\n"
"aux_ngx_pokeCStringLen x n p s = AUX_NGX.poke p x >> AUX_NGX.poke s n\n"
"{-# SPECIALIZE INLINE\n"
"    aux_ngx_pokeCStringLen :: AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"        AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->IO () #-}\n"
"{-# SPECIALIZE INLINE\n"
"    aux_ngx_pokeCStringLen :: AUX_NGX.CString -> AUX_NGX.CSize ->\n"
"        AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->IO () #-}"
"\n\n"

"aux_ngx_toBuffers :: AUX_NGX_BSL.ByteString -> AUX_NGX.Ptr AUX_NGX_STR_TYPE ->"
"\n"
"    IO (AUX_NGX.Ptr AUX_NGX_STR_TYPE, Int)\n"
"aux_ngx_toBuffers (AUX_NGX_BSL.null -> True) _ =\n"
"    return (AUX_NGX.nullPtr, 0)\n"
"aux_ngx_toBuffers s p = do\n"
"    let n = AUX_NGX_BSL.foldlChunks (const . succ) 0 s\n"
"    if n == 1\n"
"        then do\n"
"            AUX_NGX_BS.unsafeUseAsCStringLen (head $ AUX_NGX_BSL.toChunks s) $"
"\n"
"                \\(x, fromIntegral -> l) ->\n"
"                    AUX_NGX.poke p $ AUX_NGX_STR_TYPE l x\n"
"            return (p, 1)\n"
"        else do\n"
"            t <- aux_ngx_safeMallocBytes $\n"
"                n * AUX_NGX.sizeOf (undefined :: AUX_NGX_STR_TYPE)\n"
"            if t == AUX_NGX.nullPtr\n"
"                then return (AUX_NGX.nullPtr, -1)\n"
"                else (,) t <$>\n"
"                        AUX_NGX_BSL.foldlChunks\n"
"                            (\\a c -> do\n"
"                                off <- a\n"
"                                AUX_NGX_BS.unsafeUseAsCStringLen c $\n"
"                                    \\(x, fromIntegral -> l) ->\n"
"                                        AUX_NGX.pokeElemOff t off $\n"
"                                            AUX_NGX_STR_TYPE l x\n"
"                                return $ off + 1\n"
"                            ) (return 0) s\n\n"

"aux_ngx_pokeLazyByteString :: AUX_NGX_BSL.ByteString ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt ->"
"\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> IO ()\n"
"aux_ngx_pokeLazyByteString s p pl spd = do\n"
"    (t, fromIntegral -> l) <- AUX_NGX.peek p >>= aux_ngx_toBuffers s\n"
"    AUX_NGX.when (l /= 1) (AUX_NGX.poke p t) >> AUX_NGX.poke pl l\n"
"    AUX_NGX.when (t /= AUX_NGX.nullPtr) $\n"
"        AUX_NGX.newStablePtr s >>= AUX_NGX.poke spd\n\n"

"aux_ngx_safeHandler :: AUX_NGX.Ptr AUX_NGX.CString ->\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> IO AUX_NGX.CUInt -> IO AUX_NGX.CUInt\n"
"aux_ngx_safeHandler p pl = AUX_NGX.handle $ \\e -> do\n"
"    (x, fromIntegral -> l) <- aux_ngx_safeNewCStringLen $\n"
"        show (e :: AUX_NGX.SomeException)\n"
"    aux_ngx_pokeCStringLen x l p pl\n"
"    return 1\n\n"

"aux_ngx_hs_s_s :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_s_s (AUX_NGX_S_S f)\n"
"            x (fromIntegral -> n) p pl =\n"
"    aux_ngx_safeHandler p pl $ do\n"
"        (s, fromIntegral -> l) <- f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                                    >>= AUX_NGX.newCStringLen\n"
"        aux_ngx_pokeCStringLen s l p pl\n"
"        return 0\n\n"

"aux_ngx_hs_s_ss :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_s_ss (AUX_NGX_S_SS f)\n"
"            x (fromIntegral -> n) y (fromIntegral -> m) p pl =\n"
"    aux_ngx_safeHandler p pl $ do\n"
"        (s, fromIntegral -> l) <- f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                                    <*> AUX_NGX.peekCStringLen (y, m)\n"
"                                    >>= AUX_NGX.newCStringLen\n"
"        aux_ngx_pokeCStringLen s l p pl\n"
"        return 0\n\n"

"aux_ngx_hs_s_ls :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_s_ls (AUX_NGX_S_LS f)\n"
"            x (fromIntegral -> n) p pl =\n"
"    aux_ngx_safeHandler p pl $ do\n"
"        (s, fromIntegral -> l) <- f <$> aux_ngx_peekNgxStringArrayLen x n\n"
"                                    >>= AUX_NGX.newCStringLen\n"
"        aux_ngx_pokeCStringLen s l p pl\n"
"        return 0\n\n"

"aux_ngx_hs_y_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt ->"
"\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_y_y (AUX_NGX_Y_Y f)\n"
"            x (fromIntegral -> n) p pl spd = do\n"
"    (s, r) <- (flip (,) 0 . f <$> AUX_NGX_BS.unsafePackCStringLen (x, n))\n"
"                `AUX_NGX.catch`\n"
"                     \\e -> return (AUX_NGX_BSLC8.pack $\n"
"                                       show (e :: AUX_NGX.SomeException), 1)\n"
"    aux_ngx_pokeLazyByteString s p pl spd\n"
"    return r\n\n"

"aux_ngx_hs_ioy_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt ->"
"\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_ioy_y (AUX_NGX_IOY_Y f)\n"
"            x (fromIntegral -> n) p pl spd = do\n"
"    (s, r) <- (do\n"
"                  s <- AUX_NGX_BS.unsafePackCStringLen (x, n) >>= flip f False"
"\n"
"                  fmap (flip (,) 0) $ return $! s\n"
"              ) `AUX_NGX.catch`\n"
"                     \\e -> return (AUX_NGX_BSLC8.pack $\n"
"                                       show (e :: AUX_NGX.SomeException), 1)\n"
"    aux_ngx_pokeLazyByteString s p pl spd\n"
"    return r\n\n"

"aux_ngx_asyncIOFlag1b :: AUX_NGX_BS.ByteString\n"
"aux_ngx_asyncIOFlag1b =\n"
"    AUX_NGX_BSL.toStrict $ AUX_NGX.runPut $ AUX_NGX.putInt8 1\n\n"

"aux_ngx_asyncIOFlag8b :: AUX_NGX_BS.ByteString\n"
"aux_ngx_asyncIOFlag8b =\n"
"    AUX_NGX_BSL.toStrict $ AUX_NGX.runPut $ AUX_NGX.putInt64host 1\n\n"

"aux_ngx_asyncIOCommon :: IO AUX_NGX_BSL.ByteString ->\n"
"    AUX_NGX.CInt -> Bool -> AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) ->\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> AUX_NGX.Ptr AUX_NGX.CUInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> IO ()\n"
"aux_ngx_asyncIOCommon a (fromIntegral -> fd) efd p pl r spd =\n"
"    AUX_NGX.void . AUX_NGX.async $ do\n"
"    (do {s <- a; fmap Right $ return $! s})\n"
"        `AUX_NGX.catch`\n"
"            (\\e -> return $ Left $ show (e :: AUX_NGX.SomeException)) >>=\n"
"                either (pokeAll 1 . AUX_NGX_BSLC8.pack) (pokeAll 0)\n"
"    AUX_NGX.uninterruptibleMask_ $\n"
"        if efd\n"
"            then writeFlag8b\n"
"            else writeFlag1b >> AUX_NGX.closeFd fd `AUX_NGX.catchIOError`\n"
"                     const (return ())\n"
"    where pokeAll e s = aux_ngx_pokeLazyByteString s p pl spd >>\n"
"              AUX_NGX.poke r e\n"
"          writeBufN n s w\n"
"              | w < n = (w +) <$>\n"
"                  AUX_NGX.fdWriteBuf fd (AUX_NGX.plusPtr s $ fromIntegral w)\n"
"                      (n - w)\n"
"                  `AUX_NGX.catchIOError`\n"
"                  (\\e -> return $\n"
"                       if AUX_NGX.ioe_errno e ==\n"
"                              Just ((\\(AUX_NGX.Errno i) -> i) AUX_NGX.eINTR)"
"\n"
"                           then 0\n"
"                           else n\n"
"                  ) >>= writeBufN n s\n"
"              | otherwise = return w\n"
"          writeFlag1b = AUX_NGX.void $\n"
"              AUX_NGX_BS.unsafeUseAsCString aux_ngx_asyncIOFlag1b $\n"
"                  flip (writeBufN 1) 0\n"
"          writeFlag8b = AUX_NGX.void $\n"
"              AUX_NGX_BS.unsafeUseAsCString aux_ngx_asyncIOFlag8b $\n"
"                  flip (writeBufN 8) 0\n"

"aux_ngx_hs_async_ioy_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt -> AUX_NGX.CInt -> AUX_NGX.CUInt ->\n"
"    AUX_NGX.CUInt -> AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) ->\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> AUX_NGX.Ptr AUX_NGX.CUInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> IO ()\n"
"aux_ngx_hs_async_ioy_y (AUX_NGX_IOY_Y f)\n"
"            x (fromIntegral -> n) fd (AUX_NGX.toBool -> efd)\n"
"            (AUX_NGX.toBool -> fstRun) =\n"
"    aux_ngx_asyncIOCommon\n"
"        (AUX_NGX_BS.unsafePackCStringLen (x, n) >>= flip f fstRun) fd efd\n\n"

"aux_ngx_hs_async_ioy_yy :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE ->\n"
"    AUX_NGX.CInt -> AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.CInt -> AUX_NGX.CUInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) ->\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> AUX_NGX.Ptr AUX_NGX.CUInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) -> IO ()\n"
"aux_ngx_hs_async_ioy_yy (AUX_NGX_IOY_YY f)\n"
"            b (fromIntegral -> m) x (fromIntegral -> n)\n"
"            fd (AUX_NGX.toBool -> efd) =\n"
"    aux_ngx_asyncIOCommon\n"
"    (do\n"
"        b' <- aux_ngx_peekNgxStringArrayLenY b m\n"
"        x' <- AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"        f b' x'\n"
"    ) fd efd\n\n"

"aux_ngx_hs_b_s :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_s (AUX_NGX_B_S f)\n"
"            x (fromIntegral -> n) p pl =\n"
"    aux_ngx_safeHandler p pl $ do\n"
"        r <- AUX_NGX.fromBool . f <$> AUX_NGX.peekCStringLen (x, n)\n"
"        aux_ngx_pokeCStringLen AUX_NGX.nullPtr 0 p pl\n"
"        return r\n\n"

"aux_ngx_hs_b_ss :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_ss (AUX_NGX_B_SS f)\n"
"            x (fromIntegral -> n) y (fromIntegral -> m) p pl =\n"
"    aux_ngx_safeHandler p pl $ do\n"
"        r <- (AUX_NGX.fromBool .) . f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                                      <*> AUX_NGX.peekCStringLen (y, m)\n"
"        aux_ngx_pokeCStringLen AUX_NGX.nullPtr 0 p pl\n"
"        return r\n\n"

"aux_ngx_hs_b_ls :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_ls (AUX_NGX_B_LS f)\n"
"            x (fromIntegral -> n) p pl =\n"
"    aux_ngx_safeHandler p pl $ do\n"
"        r <- AUX_NGX.fromBool . f <$> aux_ngx_peekNgxStringArrayLen x n\n"
"        aux_ngx_pokeCStringLen AUX_NGX.nullPtr 0 p pl\n"
"        return r\n\n"

"aux_ngx_hs_b_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_y (AUX_NGX_B_Y f)\n"
"            x (fromIntegral -> n) p pl =\n"
"    aux_ngx_safeHandler p pl $ do\n"
"        r <- AUX_NGX.fromBool . f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"        aux_ngx_pokeCStringLen AUX_NGX.nullPtr 0 p pl\n"
"        return r\n\n"

"aux_ngx_hs_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt ->"
"\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->\n"
"    AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_handler (AUX_NGX_HANDLER f)\n"
"            x (fromIntegral -> n) p pl pct plct pst spd =\n"
"    aux_ngx_safeHandler pct pst $ do\n"
"        (s, ct, fromIntegral -> st) <-\n"
"            f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"        (t, fromIntegral -> l) <- AUX_NGX.peek p >>= aux_ngx_toBuffers s\n"
"        AUX_NGX.when (l /= 1) (AUX_NGX.poke p t) >> AUX_NGX.poke pl l\n"
"        (sct, fromIntegral -> lct) <- AUX_NGX.newCStringLen ct\n"
"        aux_ngx_pokeCStringLen sct lct pct plct >> AUX_NGX.poke pst st\n"
"        AUX_NGX.when (t /= AUX_NGX.nullPtr) $\n"
"            AUX_NGX.newStablePtr s >>= AUX_NGX.poke spd\n"
"        return 0\n\n"

"aux_ngx_hs_def_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt ->"
"\n"
"    AUX_NGX.Ptr AUX_NGX.CString ->\n"
"    AUX_NGX.Ptr (AUX_NGX.StablePtr AUX_NGX_BSL.ByteString) ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_def_handler (AUX_NGX_Y_Y f)\n"
"            x (fromIntegral -> n) p pl pe spd =\n"
"    aux_ngx_safeHandler pe pl $ do\n"
"        s <- f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"        aux_ngx_pokeLazyByteString s p pl spd\n"
"        return 0\n\n"

"aux_ngx_hs_unsafe_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CSize ->\n"
"    AUX_NGX.Ptr AUX_NGX.CInt -> IO AUX_NGX.CUInt\n"
"aux_ngx_hs_unsafe_handler (AUX_NGX_UNSAFE_HANDLER f)\n"
"            x (fromIntegral -> n) p pl pct plct pst =\n"
"    aux_ngx_safeHandler pct pst $ do\n"
"        (s, ct, fromIntegral -> st) <-\n"
"            f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"        (t, fromIntegral -> l) <- AUX_NGX_BS.unsafeUseAsCStringLen s return\n"
"        aux_ngx_pokeCStringLen t l p pl\n"
"        (sct, fromIntegral -> lct) <-\n"
"            AUX_NGX_BS.unsafeUseAsCStringLen ct return\n"
"        aux_ngx_pokeCStringLen sct lct pct plct >> AUX_NGX.poke pst st\n"
"        return 0\n\n"

"foreign export ccall ngxExportReleaseLockedByteString ::\n"
"    AUX_NGX.StablePtr AUX_NGX_BSL.ByteString -> IO ()\n\n"

"ngxExportReleaseLockedByteString ::\n"
"    AUX_NGX.StablePtr AUX_NGX_BSL.ByteString -> IO ()\n"
"ngxExportReleaseLockedByteString = AUX_NGX.freeStablePtr\n\n"
);

static const ngx_uint_t use_eventfd_channel =
#if (NGX_HAVE_EVENTFD)
    1;
#else
    0;
#endif


typedef HsWord32 (*ngx_http_haskell_handler_s_s)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_s_ss)
    (HsPtr, HsInt32, HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_s_ls)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_s)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_ss)
    (HsPtr, HsInt32, HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_ls)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_y_y)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_y)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_ioy_y)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr);
typedef void (*ngx_http_haskell_handler_async_ioy_y)
    (HsPtr, HsInt32, HsInt32, HsWord32, HsWord32, HsPtr, HsPtr, HsPtr,
     HsPtr);
typedef void (*ngx_http_haskell_handler_async_ioy_yy)
    (HsPtr, HsInt32, HsPtr, HsInt32, HsInt32, HsWord32, HsPtr, HsPtr, HsPtr,
     HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_ch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_dch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_uch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr);

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
    ngx_http_haskell_handler_type_ioy_yy,
    ngx_http_haskell_handler_type_ch,
    ngx_http_haskell_handler_type_uch
} ngx_http_haskell_handler_type_e;


typedef enum {
    ngx_http_haskell_handler_role_uninitialized = 0,
    ngx_http_haskell_handler_role_variable,
    ngx_http_haskell_handler_role_async_variable,
    ngx_http_haskell_handler_role_async_variable_rb,
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
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 702
    void                                     (*hs_add_root)(void (*)(void));
    void                                     (*init_HsModule)(void);
#endif
    void                                     (*release_locked_bytestring)
                                                                (HsStablePtr);
    ngx_http_haskell_compile_mode_e            compile_mode;
    ngx_array_t                                service_code_vars;
    ngx_array_t                                var_nocacheable;
    ngx_array_t                                var_compensate_uri_changes;
    ngx_array_t                                service_var_ignore_empty;
    ngx_array_t                                service_var_in_shm;
    ngx_shm_zone_t                            *shm_zone;
    ngx_uint_t                                 code_loaded:1;
    ngx_uint_t                                 has_async_tasks:1;
} ngx_http_haskell_main_conf_t;


typedef struct {
    ngx_int_t                                  handler;
    ngx_http_complex_value_t                  *args;
} ngx_http_haskell_content_handler_t;


typedef struct {
    ngx_str_t                                 *bufs;
    HsInt32                                    n_bufs;
    void                                     (*release_locked_bytestring)
                                                                (HsStablePtr);
    HsStablePtr                                locked_bytestring;
} ngx_http_haskell_yy_cleanup_data_t;


typedef struct {
    ngx_http_haskell_yy_cleanup_data_t         yy_cleanup_data;
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
    ngx_str_t                                  data;
    ngx_uint_t                                 complete;
} ngx_http_haskell_async_result_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_http_haskell_yy_cleanup_data_t         yy_cleanup_data;
    HsWord32                                   error;
    ngx_http_haskell_async_result_t            result;
    ngx_uint_t                                 ref_count;
    ngx_uint_t                                 complete;
} ngx_http_haskell_async_data_t;


typedef struct {
    ngx_array_t                                async_data;
    ngx_array_t                                var_nocacheable_cache;
    ngx_array_t                                request_body;
    ngx_uint_t                                 waiting_more_request_body:1;
    ngx_uint_t                                 read_request_body_error:1;
    ngx_uint_t                                 no_request_body:1;
} ngx_http_haskell_ctx_t;


typedef struct {
    ngx_str_t                                  name;
    ngx_int_t                                  index;
} ngx_http_haskell_var_handle_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_str_t                                  value;
    ngx_uint_t                                 checked;
} ngx_http_haskell_var_cache_t;


typedef struct {
    ngx_http_haskell_var_handle_t              handle;
    ngx_http_haskell_async_data_t              data;
    time_t                                     modified;
} ngx_http_haskell_shm_var_handle_t;


typedef struct {
    /* ngx_connection_t stub to allow use c->fd as event ident */
    void                                             *data;
    ngx_event_t                                      *read;
    ngx_event_t                                      *write;
    ngx_fd_t                                          fd;
} ngx_http_haskell_async_event_stub_t;


typedef struct {
    ngx_http_haskell_async_event_stub_t               s;
    ngx_http_request_t                               *r;
    ngx_uint_t                                       *complete;
} ngx_http_haskell_async_event_t;


typedef struct {
    ngx_http_haskell_async_event_stub_t               s;
    ngx_cycle_t                                      *cycle;
    struct ngx_http_haskell_service_code_var_data_s  *service_code_var;
    ngx_uint_t                                        first_run;
} ngx_http_haskell_service_async_event_t;


struct ngx_http_haskell_service_code_var_data_s {
    ngx_http_haskell_code_var_data_t                 *data;
    ngx_http_haskell_async_data_t                     future_async_data;
    ngx_http_haskell_async_data_t                    *async_data;
    ngx_event_t                                       event;
    ngx_http_haskell_service_async_event_t            hev;
    ngx_uint_t                                        cb:1;
    ngx_uint_t                                        noarg:1;
    ngx_uint_t                                        running:1;
};

typedef struct ngx_http_haskell_service_code_var_data_s
    ngx_http_haskell_service_code_var_data_t;


static char *ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static char *ngx_http_haskell_write_code(ngx_conf_t *cf, void *conf,
    ngx_str_t source_name, ngx_str_t fragment);
static char *ngx_http_haskell_compile(ngx_conf_t *cf, void *conf,
    ngx_str_t source_name);
static ngx_int_t ngx_http_haskell_load(ngx_cycle_t *cycle);
static void ngx_http_haskell_unload(ngx_cycle_t *cycle, ngx_uint_t exiting);
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
static char *ngx_http_haskell_service_var_in_shm(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);
static ngx_int_t ngx_http_haskell_init(ngx_conf_t *cf);
static void *ngx_http_haskell_create_main_conf(ngx_conf_t *cf);
static void *ngx_http_haskell_create_loc_conf(ngx_conf_t *cf);
static char *ngx_http_haskell_merge_loc_conf(ngx_conf_t *cf, void *parent,
    void *child);
static ngx_int_t ngx_http_haskell_rewrite_phase_handler(ngx_http_request_t *r);
static ngx_int_t ngx_http_haskell_create_async_task(ngx_http_request_t *r,
    ngx_fd_t fd[2], ngx_http_haskell_async_data_t *async_data);
static void ngx_http_haskell_post_handler(ngx_http_request_t *r);
static ngx_int_t ngx_http_haskell_init_worker(ngx_cycle_t *cycle);
static void ngx_http_haskell_exit_worker(ngx_cycle_t *cycle);
static void ngx_http_haskell_var_init(ngx_log_t *log, ngx_array_t *cmvar,
    ngx_array_t *var, ngx_http_get_variable_pt get_handler);
static ngx_int_t ngx_http_haskell_run_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
static ngx_int_t ngx_http_haskell_run_async_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
static ngx_int_t ngx_http_haskell_run_service_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
static ngx_int_t ngx_http_haskell_content_handler(ngx_http_request_t *r);
static ngx_int_t ngx_http_haskell_service_var_init_zone(
    ngx_shm_zone_t *shm_zone, void *data);
static void ngx_http_haskell_async_event(ngx_event_t *ev);
static void ngx_http_haskell_service_async_event(ngx_event_t *ev);
static ngx_int_t ngx_http_haskell_yy_handler_result(ngx_log_t *log,
    ngx_pool_t *pool, ngx_str_t *bufs, HsInt32 n_bufs, ngx_str_t *res,
    void (*release_locked_bytestring)(HsStablePtr),
    HsStablePtr locked_bytestring, ngx_http_variable_t *var,
    ngx_uint_t cleanup, ngx_uint_t service);
static void ngx_http_haskell_yy_handler_cleanup(void *data);
static void ngx_http_haskell_content_handler_cleanup(void *data);
static void ngx_http_haskell_service_handler_cleanup(void *data);
static ngx_int_t ngx_http_haskell_open_async_event_channel(ngx_fd_t fd[2]);
static void ngx_http_haskell_close_async_event_channel(ngx_log_t *log,
    ngx_fd_t fd[2]);


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
    { ngx_string("haskell_run_async_on_request_body"),
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
    { ngx_string("haskell_service_var_update_callback"),
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
    { ngx_string("haskell_service_var_in_shm"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_2MORE,
      ngx_http_haskell_service_var_in_shm,
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
    ngx_uint_t                           i, j;
    ngx_http_haskell_main_conf_t        *mcf;
    ngx_http_haskell_loc_conf_t         *lcf;
    ngx_http_haskell_handler_t          *handlers;
    ngx_http_haskell_code_var_data_t    *code_vars;
    ngx_http_haskell_ctx_t              *ctx;
    ngx_http_haskell_async_data_t       *async_data, *async_data_elts;
    ngx_http_haskell_var_handle_t       *var_nocacheable;
    ngx_http_haskell_var_cache_t        *var_nocacheable_cache;
    ngx_int_t                            found_idx;
    ngx_uint_t                           task_complete;
    ngx_http_complex_value_t            *args;
    ngx_str_t                            arg1;
    ngx_fd_t                             fd[2];
    ngx_pool_cleanup_t                  *cln;
    ngx_http_haskell_yy_cleanup_data_t  *clnd;
    ngx_uint_t                           rb, rb_skip, rb_done;
    ngx_uint_t                           r_main_count;
    ngx_int_t                            rc;

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;
    code_vars = lcf->code_vars.elts;

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (mcf->var_nocacheable.nelts > 0 && ctx == NULL) {
        ctx = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_ctx_t));
        if (ctx == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to create request context, "
                          "declining phase handler");
            return NGX_DECLINED;
        }
        if (ngx_array_init(&ctx->var_nocacheable_cache, r->pool,
                           mcf->var_nocacheable.nelts,
                           sizeof(ngx_http_haskell_var_cache_t)) != NGX_OK
            || ngx_array_push_n(&ctx->var_nocacheable_cache,
                                mcf->var_nocacheable.nelts) == NULL)
        {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to create variable cache, "
                          "declining phase handler");
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
        rb = handlers[code_vars[i].handler].role
                == ngx_http_haskell_handler_role_async_variable_rb;
        if (!rb
            && handlers[code_vars[i].handler].role
            != ngx_http_haskell_handler_role_async_variable)
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

        if (ctx->waiting_more_request_body) {
            return NGX_DONE;
        }

        rb_skip = ctx->read_request_body_error || ctx->no_request_body;
        rb_done = rb_skip || ctx->request_body.nalloc > 0;

        if (rb && !rb_done) {
            r_main_count = r->main->count;
            rc = ngx_http_read_client_request_body(r,
                                                ngx_http_haskell_post_handler);
            r->main->count = r_main_count;

            if (rc == NGX_ERROR || rc >= NGX_HTTP_SPECIAL_RESPONSE) {
                ctx->read_request_body_error = 1;
            }

            if (rc == NGX_AGAIN) {
                ctx->waiting_more_request_body = 1;
                return NGX_DONE;
            }
        }

        found_idx = NGX_ERROR;
        task_complete = 0;
        async_data_elts = ctx->async_data.elts;
        for (j = 0; j < ctx->async_data.nelts; j++) {
            if (async_data_elts[j].index == code_vars[i].index) {
                found_idx = code_vars[i].index;
                if (rb && rb_skip) {
                    async_data_elts[j].complete = 1;
                }
                task_complete = async_data_elts[j].complete;
                break;
            }
        }
        if (found_idx != NGX_ERROR) {
            if (!task_complete) {
                return NGX_DONE;
            }
            continue;
        }

        async_data = ngx_array_push(&ctx->async_data);
        if (async_data == NULL) {
            goto decline_phase_handler;
        }
        async_data->index = code_vars[i].index;
        ngx_str_null(&async_data->result.data);
        async_data->result.complete = 0;
        async_data->yy_cleanup_data.bufs = &async_data->result.data;
        async_data->yy_cleanup_data.n_bufs = 0;
        async_data->yy_cleanup_data.release_locked_bytestring =
                                        mcf->release_locked_bytestring;
        async_data->yy_cleanup_data.locked_bytestring = NULL;
        async_data->error = 0;
        async_data->ref_count = 0;
        async_data->complete = 0;

        args = code_vars[i].args.elts;
        if (ngx_http_complex_value(r, &args[0], &arg1) != NGX_OK) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to compile complex value for "
                          "future async result, skipping IO task");
            async_data->complete = 1;
            continue;
        }

        if (ngx_http_haskell_create_async_task(r, fd, async_data) != NGX_OK) {
            async_data->complete = 1;
            continue;
        }

        cln = ngx_pool_cleanup_add(r->pool, 0);
        clnd = ngx_palloc(r->pool, sizeof(ngx_http_haskell_yy_cleanup_data_t));
        if (cln == NULL || clnd == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to register cleanup handler for future async "
                          "result, skipping IO task");
            async_data->complete = 1;
            continue;
        }

        if (rb) {
            ((ngx_http_haskell_handler_async_ioy_yy)
             handlers[code_vars[i].handler].self)
                    (ctx->request_body.elts, ctx->request_body.nelts,
                     arg1.data, arg1.len, fd[1], use_eventfd_channel,
                     &async_data->yy_cleanup_data.bufs,
                     &async_data->yy_cleanup_data.n_bufs, &async_data->error,
                     &async_data->yy_cleanup_data.locked_bytestring);
        } else {
            ((ngx_http_haskell_handler_async_ioy_y)
             handlers[code_vars[i].handler].self)
                    (arg1.data, arg1.len, fd[1], use_eventfd_channel, 0,
                     &async_data->yy_cleanup_data.bufs,
                     &async_data->yy_cleanup_data.n_bufs, &async_data->error,
                     &async_data->yy_cleanup_data.locked_bytestring);
        }

        clnd->bufs = async_data->yy_cleanup_data.bufs;
        clnd->n_bufs = async_data->yy_cleanup_data.n_bufs;
        clnd->release_locked_bytestring =
                        async_data->yy_cleanup_data.release_locked_bytestring;
        clnd->locked_bytestring = async_data->yy_cleanup_data.locked_bytestring;
        cln->handler = ngx_http_haskell_yy_handler_cleanup;
        cln->data = clnd;

        return NGX_DONE;
    }

    return NGX_DECLINED;

decline_phase_handler:

    ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                  "failed to create an async task, declining phase handler");

    return NGX_DECLINED;
}


static ngx_int_t
ngx_http_haskell_create_async_task(ngx_http_request_t *r, ngx_fd_t fd[2],
                                   ngx_http_haskell_async_data_t *async_data)
{
    ngx_http_haskell_async_event_t    *hev;
    ngx_event_t                       *event;

    if (ngx_http_haskell_open_async_event_channel(fd) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, ngx_errno,
                      "failed to create async event channel for future "
                      "async result, skipping IO task");
        return NGX_ERROR;
    }

    hev = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_async_event_t));
    if (hev == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to allocate memory for future async result, "
                      "skipping IO task");
        ngx_http_haskell_close_async_event_channel(r->connection->log, fd);
        return NGX_ERROR;
    }

    hev->s.fd = fd[0];
    hev->r = r;
    hev->complete = &async_data->complete;

    event = ngx_pcalloc(r->pool, sizeof(ngx_event_t));
    if (event== NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to allocate memory for future async result, "
                      "skipping IO task");
        ngx_http_haskell_close_async_event_channel(r->connection->log, fd);
        return NGX_ERROR;
    }
    event->data = hev;
    event->handler = ngx_http_haskell_async_event;
    event->log = r->connection->log;
    hev->s.read = event;
    hev->s.write = event;   /* to make ngx_add_event() happy */
    if (ngx_add_event(event, NGX_READ_EVENT, 0) != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to add event for future async result, "
                      "skipping IO task");
        ngx_http_haskell_close_async_event_channel(r->connection->log, fd);
        return NGX_ERROR;
    }

    return NGX_OK;
}


static void
ngx_http_haskell_post_handler(ngx_http_request_t *r)
{
    ngx_http_haskell_ctx_t            *ctx;
    ngx_chain_t                       *cl;
    ngx_str_t                         *rb;
    ngx_uint_t                         n = 0;

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (ctx == NULL) {
        return;
    }

    if (r->request_body == NULL || r->request_body->bufs == NULL
        || r->request_body->temp_file)
    {
        ctx->no_request_body = 1;
        if (r->request_body != NULL && r->request_body->temp_file) {
            ngx_log_error(NGX_LOG_ALERT, r->connection->log, 0,
                          "request body was saved in a temporary file, "
                          "exiting from haskell POST handler");
        }
        goto request_body_done;
    }

    if (ctx->request_body.nalloc != 0 || ctx->no_request_body) {
        goto request_body_done;
    }

    for (cl = r->request_body->bufs; cl != NULL; cl = cl->next) {
        n++;
    }

    if (ngx_array_init(&ctx->request_body, r->pool, n, sizeof(ngx_str_t))
        != NGX_OK || ngx_array_push_n(&ctx->request_body, n) == NULL)
    {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to allocate memory for haskell POST handler");
        ctx->no_request_body = 1;
        goto request_body_done;
    }

    rb = ctx->request_body.elts;

    n = 0;
    for (cl = r->request_body->bufs; cl != NULL; cl = cl->next) {
        rb[n].len = cl->buf == NULL ? 0 : cl->buf->last - cl->buf->pos;
        rb[n].data = cl->buf == NULL ? NULL : cl->buf->pos;
        n++;
    }

request_body_done:

    if (ctx->waiting_more_request_body) {
        ctx->waiting_more_request_body = 0;
        ngx_http_core_run_phases(r);
    }
}


static ngx_int_t
ngx_http_haskell_init_worker(ngx_cycle_t *cycle)
{
    ngx_uint_t                                 i, j;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_core_main_conf_t                 *cmcf;
    ngx_http_haskell_var_handle_t             *vars;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_http_variable_t                       *cmvars;
    ngx_uint_t                                 found;
    ngx_int_t                                  index;

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

    ngx_http_haskell_var_init(cycle->log, &cmcf->variables,
                              &mcf->var_compensate_uri_changes,
                              ngx_http_haskell_run_handler);
    ngx_http_haskell_var_init(cycle->log, &cmcf->variables,
                              &mcf->service_var_ignore_empty,
                              ngx_http_haskell_run_service_handler);
    ngx_http_haskell_var_init(cycle->log, &cmcf->variables,
                              &mcf->service_var_in_shm,
                              ngx_http_haskell_run_service_handler);

    vars = mcf->service_var_in_shm.elts;
    service_code_vars = mcf->service_code_vars.elts;
    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (!service_code_vars[i].cb) {
            continue;
        }
        index = service_code_vars[i].data->index;
        if (cmvars[index].get_handler != ngx_http_haskell_run_service_handler) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" has incompatible get handler",
                          &cmvars[index].name);
            service_code_vars[i].data->index = NGX_ERROR;
            continue;
        }
        found = 0;
        for (j = 0; j < mcf->service_var_in_shm.nelts; j++) {
            if (index == vars[j].index) {
                found = 1;
                break;
            }
        }
        if (found == 0) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" is not in shm", &cmvars[index].name);
            service_code_vars[i].data->index = NGX_ERROR;
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

    ngx_http_haskell_unload(cycle, 1);

    ngx_http_haskell_stop_services(cycle);
}


static void
ngx_http_haskell_var_init(ngx_log_t *log, ngx_array_t *cmvar, ngx_array_t *var,
                          ngx_http_get_variable_pt get_handler)
{
    ngx_uint_t                                 i, j;
    ngx_http_haskell_var_handle_t             *vars;
    ngx_http_variable_t                       *cmvars;
    ngx_uint_t                                 found;

    cmvars = cmvar->elts;

    vars = var->elts;
    for (i = 0; i < var->nelts; i++) {
        found = 0;
        for (j = 0; j < cmvar->nelts; j++) {
            if (vars[i].name.len == cmvars[j].name.len
                && ngx_strncmp(vars[i].name.data, cmvars[j].name.data,
                               vars[i].name.len) == 0)
            {
                if (cmvars[j].get_handler != get_handler) {
                    ngx_log_error(NGX_LOG_ERR, log, 0,
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
            ngx_log_error(NGX_LOG_ERR, log, 0,
                          "variable \"%V\" was not declared", &vars[i].name);
        }
    }
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
    typedef HsInt32               (*version_f_t)(HsInt32 *, HsInt32);
    typedef HsInt32               (*type_checker_t)(void);

    ngx_uint_t                      i;
    ngx_http_haskell_main_conf_t   *mcf;
    ngx_http_haskell_handler_t     *handlers;
    char                           *dl_error;
    char                          **argv = NULL;
    int                             argc;
    char                           *hs_init = "hs_init";
    version_f_t                     version_f = NULL;
    HsInt32                         version[4], version_len;

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

    if (mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_modular) {
        version_f = (version_f_t) dlsym(mcf->dl_handle, "ngxExportVersion");
        dl_error = dlerror();
        if (version_f == NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to get API version of haskell library: %s",
                          dl_error);
            goto dlclose_and_exit;
        }
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
        goto dlclose_and_exit;
    }

    mcf->hs_exit = (void (*)(void)) dlsym(mcf->dl_handle, "hs_exit");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_exit\": %s", dl_error);
        goto dlclose_and_exit;
    }

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 702
    mcf->hs_add_root = (void (*)(void (*)(void))) dlsym(mcf->dl_handle,
                                                        "hs_add_root");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_add_root\": %s", dl_error);
        goto dlclose_and_exit;
    }

    mcf->init_HsModule = (void (*)(void)) dlsym(mcf->dl_handle,
                                            "__stginit_NgxHaskellUserRuntime");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function "
                      "\"__stginit_NgxHaskellUserRuntime\": %s", dl_error);
        goto dlclose_and_exit;
    }
#endif

    mcf->release_locked_bytestring = (void (*)(HsStablePtr))
            dlsym(mcf->dl_handle, "ngxExportReleaseLockedByteString");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"release_locked_bytestring\": "
                      "%s", dl_error);
        goto dlclose_and_exit;
    }

    argc = mcf->program_options.nelts + 1;
    if (mcf->rts_options.nelts > 0) {
        argc += mcf->rts_options.nelts + 1;
    }
    argv = ngx_palloc(cycle->pool, argc * sizeof(char *));
    if (argv == NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to allocate artifacts for haskell init options");
        goto dlclose_and_exit;
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
    ngx_pfree(cycle->pool, argv);

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 702
    mcf->hs_add_root(mcf->init_HsModule);
#endif

    if (mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_modular) {
        version_len = version_f(version, sizeof(version) / sizeof(version[0]));
        if (version_len < 2) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "bad API version of haskell library");
            goto unload_and_exit;
        }
        if (version[0] != haskell_module_ngx_export_api_version_major
            || version[1] != haskell_module_ngx_export_api_version_minor)
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "bad API version of haskell library: %d.%d "
                          "(expected %d.%d)", version[0], version[1],
                          haskell_module_ngx_export_api_version_major,
                          haskell_module_ngx_export_api_version_minor);
            goto unload_and_exit;
        }
    }

    handlers = mcf->handlers.elts;

    for (i = 0; i < mcf->handlers.nelts; i++) {
        ngx_str_t        handler_name;
        type_checker_t   type_checker;
        char            *type_checker_name = NULL;
        ngx_uint_t       wrong_n_args = 0;

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
            goto unload_and_exit;
        }

        type_checker_name = ngx_palloc(cycle->pool,
            haskell_module_type_checker_prefix.len + handlers[i].name.len + 1);
        if (type_checker_name == NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to allocate artifacts for type checker");
            goto unload_and_exit;
        }

        ngx_memcpy(type_checker_name,
                   haskell_module_type_checker_prefix.data,
                   haskell_module_type_checker_prefix.len);
        ngx_memcpy(type_checker_name + haskell_module_type_checker_prefix.len,
                   handlers[i].name.data, handlers[i].name.len + 1);

        type_checker = (type_checker_t) dlsym(mcf->dl_handle,
                                              type_checker_name);
        dl_error = dlerror();
        ngx_pfree(cycle->pool, type_checker_name);
        if (dl_error != NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to load haskell handler type checker \"%V\": "
                          "%s", &handler_name, dl_error);
            goto unload_and_exit;
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
              || handlers[i].role
              == ngx_http_haskell_handler_role_service_variable)
             && handlers[i].type != ngx_http_haskell_handler_type_ioy_y)
            ||
            (handlers[i].role == ngx_http_haskell_handler_role_async_variable_rb
             && handlers[i].type != ngx_http_haskell_handler_type_ioy_yy))
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "haskell handler \"%V\" role and type mismatch",
                          &handler_name);
            goto unload_and_exit;
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
            goto unload_and_exit;
        }

        switch (handlers[i].type) {
        case ngx_http_haskell_handler_type_s_s:
        case ngx_http_haskell_handler_type_b_s:
        case ngx_http_haskell_handler_type_y_y:
        case ngx_http_haskell_handler_type_b_y:
        case ngx_http_haskell_handler_type_ioy_y:
        case ngx_http_haskell_handler_type_ioy_yy:
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
            goto unload_and_exit;
        }

        if (wrong_n_args) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "actual type of haskell handler \"%V\" "
                          "does not match call samples", &handler_name);
            goto unload_and_exit;
        }
    }

    return NGX_OK;

dlclose_and_exit:

    dlclose(mcf->dl_handle);

    return NGX_ERROR;

unload_and_exit:

    ngx_http_haskell_unload(cycle, 0);

    return NGX_ERROR;
}


static void
ngx_http_haskell_unload(ngx_cycle_t *cycle, ngx_uint_t exiting)
{
    ngx_http_haskell_main_conf_t    *mcf;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return;
    }

    if (mcf->dl_handle != NULL) {
        mcf->hs_exit();
        /* dlclose() may cause sigsegv when a haskell service wakes up
         * during or after munmap() but before the worker exits */
        if (!exiting) {
            dlclose(mcf->dl_handle);
        }
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
        if (!service_code_vars[i].cb
            && ngx_http_haskell_run_service(cycle, &service_code_vars[i], 1)
            != NGX_OK)
        {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to start haskell services");
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
        if (!service_code_vars[i].running) {
            continue;
        }
        if (service_code_vars[i].async_data != NULL
            && --service_code_vars[i].async_data->ref_count == 0)
        {
            if (service_code_vars[i].async_data->result.complete == 2) {
                ngx_free(service_code_vars[i].async_data->result.data.data);
            }
            ngx_free(service_code_vars[i].async_data);
            service_code_vars[i].async_data = NULL;
        }
        if (service_code_vars[i].hev.s.fd == NGX_INVALID_FILE) {
            if (service_code_vars[i].event.timer_set) {
                ngx_del_timer(&service_code_vars[i].event);
            }
            continue;
        }
        service_code_vars[i].event.active = 0;
        if (ngx_del_event(&service_code_vars[i].event, NGX_READ_EVENT, 0)
            == NGX_ERROR)
        {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to delete event while stopping service");
        }
        if (close(service_code_vars[i].hev.s.fd) == -1) {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, ngx_errno,
                          "failed to close async event channel "
                          "while stopping service");
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

    if (ngx_terminate || ngx_exiting) {
        return NGX_OK;
    }

    service_code_var->running = 1;

    event = &service_code_var->event;
    hev = &service_code_var->hev;

    ngx_memzero(event, sizeof(ngx_event_t));
    event->data = hev;
    event->handler = ngx_http_haskell_service_async_event;
    event->log = cycle->log;

    ngx_memzero(hev, sizeof(ngx_http_haskell_service_async_event_t));
    hev->service_code_var = service_code_var;
    hev->s.read = event;
    hev->s.write = event;   /* to make ngx_add_event() happy: */

    hev->s.fd = NGX_INVALID_FILE;
    hev->cycle = cycle;
    hev->first_run = service_first_run;

    if (ngx_http_haskell_open_async_event_channel(fd) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, ngx_errno,
                      "failed to create async event channel for future "
                      "async result, postponing IO task for 0.5 sec");
        ngx_add_timer(event, 500);
        return NGX_OK;
    }

    hev->s.fd = fd[0];

    if (ngx_add_event(event, NGX_READ_EVENT, 0) != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to add event for future async result, "
                      "postponing IO task for 2 sec");
        ngx_http_haskell_close_async_event_channel(cycle->log, fd);
        ngx_add_timer(event, 2000);
        return NGX_OK;
    }

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;

    service_code_var->future_async_data.yy_cleanup_data.bufs =
            &service_code_var->future_async_data.result.data;
    args = service_code_var->data->args.elts;
    arg1 = args[0].value;
    ((ngx_http_haskell_handler_async_ioy_y)
     handlers[service_code_var->data->handler].self)
            (arg1.data, arg1.len, fd[1], use_eventfd_channel, service_first_run,
             &service_code_var->future_async_data.yy_cleanup_data.bufs,
             &service_code_var->future_async_data.yy_cleanup_data.n_bufs,
             &service_code_var->future_async_data.error,
             &service_code_var->
                        future_async_data.yy_cleanup_data.locked_bytestring);
    service_code_var->
            future_async_data.yy_cleanup_data.release_locked_bytestring =
                                                mcf->release_locked_bytestring;

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
    ngx_http_get_variable_pt                   get_handler;
    ngx_uint_t                                 async, rb, service, service_cb;

    value = cf->args->elts;

    service_cb = value[0].len == 35
            && ngx_strncmp(value[0].data,
                           "haskell_service_var_update_callback", 35) == 0;
    service = service_cb
            || (value[0].len == 19
                && ngx_strncmp(value[0].data, "haskell_run_service", 19) == 0);

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
        service_code_var_data->cb = service_cb ? 1 : 0;
        service_code_var_data->noarg = n_args > 0 ? 0 : 1;
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

    rb = ngx_strncmp(value[0].data,
                     "haskell_run_async_on_request_body", 33) == 0;
    async = rb ? 1 : ngx_strncmp(value[0].data, "haskell_run_async", 17) == 0;
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
            if (handlers[i].role
                == ngx_http_haskell_handler_role_content_handler)
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared as content handler", &value[1]);
                return NGX_CONF_ERROR;
            }
            if ((handlers[i].role
                 == ngx_http_haskell_handler_role_variable && async)
                || ((handlers[i].role
                     == ngx_http_haskell_handler_role_async_variable
                     || handlers[i].role
                     == ngx_http_haskell_handler_role_async_variable_rb
                     || handlers[i].role
                     == ngx_http_haskell_handler_role_service_variable)
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
                (rb ? ngx_http_haskell_handler_role_async_variable_rb :
                 (async ? ngx_http_haskell_handler_role_async_variable :
                  ngx_http_haskell_handler_role_variable));

        handlers = mcf->handlers.elts;
        code_var_data->handler = mcf->handlers.nelts - 1;
    }

    ++handlers[code_var_data->handler].n_args[n_args > 2 ? 2 : n_size - 1];

    if (service_cb) {
        v_idx = ngx_http_get_variable_index(cf, &value[2]);
        if (v_idx == NGX_ERROR) {
            return NGX_CONF_ERROR;
        }
    } else {
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
        *v_idx_ptr = v_idx;

        v->data = (uintptr_t) v_idx_ptr;

        get_handler = v->get_handler;
        v->get_handler = service ? ngx_http_haskell_run_service_handler :
                (async ? ngx_http_haskell_run_async_handler :
                 ngx_http_haskell_run_handler);

        if (get_handler != NULL && get_handler != v->get_handler) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "variable \"%V\" has been already defined with "
                               "another variable handler", &value[2]);
            return NGX_CONF_ERROR;
        }
    }

    code_var_data->index = v_idx;

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
                || handlers[i].role
                == ngx_http_haskell_handler_role_async_variable
                || handlers[i].role
                == ngx_http_haskell_handler_role_async_variable_rb
                || handlers[i].role
                == ngx_http_haskell_handler_role_service_variable)
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

    ngx_uint_t                         i, j;
    ngx_str_t                         *value;
    ngx_array_t                       *data;
    ngx_http_haskell_var_handle_t     *vars;
    ngx_uint_t                         n_vars;
    ngx_int_t                          idx = 0;

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
    } else if (value[0].len == 26
        && ngx_strncmp(value[0].data, "haskell_service_var_in_shm", 26)
        == 0)
    {
        data = &mcf->service_var_in_shm;
        idx = 2;
    } else {
        return NGX_CONF_ERROR;
    }

    if (data->nalloc > 0) {
        return "is duplicate";
    }

    n_vars = cf->args->nelts - idx - 1;

    if (ngx_array_init(data, cf->pool, n_vars,
                       sizeof(ngx_http_haskell_var_handle_t)) != NGX_OK
        || ngx_array_push_n(data, n_vars) == NULL)
    {
        return NGX_CONF_ERROR;
    }

    vars = data->elts;

    for (i = 0, j = idx + 1; i < n_vars; i++, j++) {
        if (value[j].len < 2 || value[j].data[0] != '$') {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "invalid variable name \"%V\"", &value[j]);
            return NGX_CONF_ERROR;
        }
        value[j].len--;
        value[j].data++;
        ngx_strlow(value[j].data, value[j].data, value[j].len);
        vars[i].name = value[j];
        vars[i].index = -1;
    }

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_service_var_in_shm(ngx_conf_t *cf, ngx_command_t *cmd,
                                    void *conf)
{
    ngx_http_haskell_main_conf_t      *mcf = conf;

    ngx_str_t                         *value;
    ssize_t                            shm_size;

    value = cf->args->elts;

    if (mcf->shm_zone != NULL) {
        return "is duplicate";
    }

    if (cf->args->nelts < 4) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "too few arguments");
        return NGX_CONF_ERROR;
    }

    shm_size = ngx_parse_size(&value[2]);

    if (shm_size == NGX_ERROR) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid zone size \"%V\"", &value[2]);
        return NGX_CONF_ERROR;
    }

    if (shm_size < (ssize_t) (8 * ngx_pagesize)) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "zone \"%V\" is too small", &value[1]);
        return NGX_CONF_ERROR;
    }

    mcf->shm_zone = ngx_shared_memory_add(cf, &value[1], shm_size,
                                          &ngx_http_haskell_module);
    if (mcf->shm_zone == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to add memory for zone \"%V\"", &value[1]);
        return NGX_CONF_ERROR;
    }

    mcf->shm_zone->init = ngx_http_haskell_service_var_init_zone;
    mcf->shm_zone->data = &mcf->service_var_in_shm;

    mcf->shm_zone->noreuse = 1;

    return ngx_http_haskell_var_configure(cf, cmd, conf);
}


static ngx_int_t
ngx_http_haskell_run_handler(ngx_http_request_t *r,
                             ngx_http_variable_value_t *v, uintptr_t  data)
{
    ngx_uint_t                           i;
    ngx_http_haskell_main_conf_t        *mcf;
    ngx_http_haskell_loc_conf_t         *lcf;
    ngx_http_core_main_conf_t           *cmcf;
    ngx_http_haskell_ctx_t              *ctx;
    ngx_int_t                           *index = (ngx_int_t *) data;
    ngx_int_t                            found_idx = NGX_ERROR;
    ngx_http_variable_t                 *vars;
    ngx_http_haskell_var_handle_t       *vars_comp;
    ngx_http_haskell_var_cache_t        *var_nocacheable_cache;
    ngx_http_haskell_handler_t          *handlers;
    ngx_http_haskell_code_var_data_t    *code_vars;
    ngx_http_complex_value_t            *args;
    ngx_str_t                            arg1, arg2, *argn = NULL;
    char                                *res = NULL;
    ngx_str_t                           *res_yy, buf_yy;
    HsStablePtr                          locked_bytestring;
    HsInt32                              len;
    HsWord32                             err;
    ngx_str_t                            reslen;
    ngx_pool_cleanup_t                  *cln;

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
    case ngx_http_haskell_handler_type_y_y:
    case ngx_http_haskell_handler_type_ioy_y:
        res_yy = &buf_yy;
    case ngx_http_haskell_handler_type_s_s:
    case ngx_http_haskell_handler_type_b_s:
    case ngx_http_haskell_handler_type_b_y:
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
        err = ((ngx_http_haskell_handler_s_s)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_s_ss:
        err = ((ngx_http_haskell_handler_s_ss)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, arg2.data, arg2.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_s_ls:
        err = ((ngx_http_haskell_handler_s_ls)
               handlers[code_vars[found_idx].handler].self)
                    (argn, code_vars[found_idx].args.nelts, &res, &len);
        break;
    case ngx_http_haskell_handler_type_b_s:
        err = ((ngx_http_haskell_handler_b_s)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_b_ss:
        err = ((ngx_http_haskell_handler_b_ss)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, arg2.data, arg2.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_b_ls:
        err = ((ngx_http_haskell_handler_b_ls)
               handlers[code_vars[found_idx].handler].self)
                    (argn, code_vars[found_idx].args.nelts, &res, &len);
        break;
    case ngx_http_haskell_handler_type_y_y:
        err = ((ngx_http_haskell_handler_y_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res_yy, &len, &locked_bytestring);
        break;
    case ngx_http_haskell_handler_type_b_y:
        err = ((ngx_http_haskell_handler_b_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_ioy_y:
        err = ((ngx_http_haskell_handler_ioy_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res_yy, &len, &locked_bytestring);
        break;
    default:
        return NGX_ERROR;
    }

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    vars = cmcf->variables.elts;

    if (len == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while running haskell handler");
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while getting value of "
                      "variable \"%V\"", &vars[*index].name);
        return NGX_ERROR;
    }

    reslen.len = len;
    reslen.data = (u_char *) res;

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_s_s:
    case ngx_http_haskell_handler_type_s_ss:
    case ngx_http_haskell_handler_type_s_ls:
        if (err) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while getting value of "
                          "variable \"%V\": \"%V\"",
                          &vars[*index].name, &reslen);
            ngx_free(res);
            return NGX_ERROR;
        }
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
            cln->handler = ngx_free;
            cln->data = res;
        }
        break;
    case ngx_http_haskell_handler_type_y_y:
    case ngx_http_haskell_handler_type_ioy_y:
        if (ngx_http_haskell_yy_handler_result(r->connection->log, r->pool,
                                               res_yy, len, &reslen,
                                               mcf->release_locked_bytestring,
                                               locked_bytestring,
                                               &vars[*index], 1, 0)
            == NGX_ERROR)
        {
            return NGX_ERROR;
        }
        len = reslen.len;
        res = (char *) reslen.data;
        if (err) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while getting value of "
                          "variable \"%V\": \"%V\"",
                          &vars[*index].name, &reslen);
            if (handlers[code_vars[found_idx].handler].type
                != ngx_http_haskell_handler_type_ioy_y)
            {
                return NGX_ERROR;
            }
        }
        break;
    case ngx_http_haskell_handler_type_b_s:
    case ngx_http_haskell_handler_type_b_ss:
    case ngx_http_haskell_handler_type_b_ls:
    case ngx_http_haskell_handler_type_b_y:
        if (res != NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while getting value of "
                          "variable \"%V\": \"%V\"",
                          &vars[*index].name, &reslen);
            ngx_free(res);
            return NGX_ERROR;
        }
        res = err ? "1" : "0";
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
    ngx_int_t                          rc;

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
    if (async_data_elts[found_idx].yy_cleanup_data.n_bufs == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while getting "
                      "value of variable \"%V\" asynchronously",
                      &vars[*index].name);
        return NGX_ERROR;
    }

    if (!async_data_elts[found_idx].result.complete) {
        rc = ngx_http_haskell_yy_handler_result(r->connection->log, r->pool,
                async_data_elts[found_idx].yy_cleanup_data.bufs,
                async_data_elts[found_idx].yy_cleanup_data.n_bufs,
                &async_data_elts[found_idx].result.data,
                async_data_elts[found_idx].yy_cleanup_data.
                                                release_locked_bytestring,
                async_data_elts[found_idx].yy_cleanup_data.locked_bytestring,
                &vars[*index], 0, 0);
        async_data_elts[found_idx].result.complete = 1;
        if (rc == NGX_ERROR) {
            return NGX_ERROR;
        }
    }

    if (async_data_elts[found_idx].error) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "an exception was caught while getting "
                      "value of variable \"%V\" asynchronously: \"%V\"",
                      &vars[*index].name,
                      &async_data_elts[found_idx].result);
        /* BEWARE: return value of the exception */
    }

    v->len = async_data_elts[found_idx].result.data.len;
    v->data = async_data_elts[found_idx].result.data.data;
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
    ngx_slab_pool_t                           *shpool;
    ngx_http_haskell_shm_var_handle_t         *shm_vars;
    ngx_http_variable_t                       *vars;
    ngx_str_t                                  res = ngx_null_string;
    ngx_http_haskell_async_data_t             *service_data = NULL;
    ngx_pool_cleanup_t                        *cln;

    if (index == NULL) {
        return NGX_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    service_code_vars = mcf->service_code_vars.elts;

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    vars = cmcf->variables.elts;

    for (i = 0; i < mcf->service_var_in_shm.nelts; i++) {
        if (*index == service_code_vars[i].data->index) {
            found_idx = i;
            break;
        }
    }

    if (found_idx != NGX_ERROR) {
        shpool = (ngx_slab_pool_t *) mcf->shm_zone->shm.addr;
        shm_vars = shpool->data;

        ngx_shmtx_lock(&shpool->mutex);

        if (shm_vars[found_idx].data.yy_cleanup_data.n_bufs == -1) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "memory allocation error while getting "
                          "value of variable \"%V\" asynchronously",
                          &vars[*index].name);
            ngx_shmtx_unlock(&shpool->mutex);
            return NGX_ERROR;
        }

        if (shm_vars[found_idx].data.error) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while getting "
                          "value of variable \"%V\" asynchronously: \"%V\"",
                          &vars[*index].name,
                          &shm_vars[found_idx].data.result.data);
            ngx_shmtx_unlock(&shpool->mutex);
            /* BEWARE: do not return value of the exception */
            return NGX_ERROR;
        }

        if (shm_vars[found_idx].data.result.data.len == 0) {
            ngx_shmtx_unlock(&shpool->mutex);
            goto update_var;
        }

        /* BEWARE: there is no cache for res, normally it must be OK because
         * this handler must be called only once in normal case when the
         * associated variable is not cacheable */
        res.len = shm_vars[found_idx].data.result.data.len;
        res.data = ngx_pnalloc(r->pool, res.len);
        if (res.data == NULL) {
            ngx_shmtx_unlock(&shpool->mutex);
            return NGX_ERROR;
        }

        ngx_memcpy(res.data, shm_vars[found_idx].data.result.data.data,
                   res.len);

        ngx_shmtx_unlock(&shpool->mutex);

        goto update_var;
    }

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (*index == service_code_vars[i].data->index) {
            found_idx = i;
            break;
        }
    }
    if (found_idx == NGX_ERROR) {
        return NGX_ERROR;
    }

    for (cln = r->pool->cleanup; cln != NULL; cln = cln->next) {
        if (cln->handler == ngx_http_haskell_service_handler_cleanup) {
            service_data = cln->data;
            break;
        }
    }

    if (service_data == NULL) {
        service_data = service_code_vars[found_idx].async_data;
        if (service_data == NULL) {
            return NGX_ERROR;
        }
        cln = ngx_pool_cleanup_add(r->pool, 0);
        if (cln == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to register cleanup handler for "
                          "service data");
            return NGX_ERROR;
        }
        ++service_data->ref_count;
        cln->handler = ngx_http_haskell_service_handler_cleanup;
        cln->data = service_data;
    }

    if (service_data->yy_cleanup_data.n_bufs == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while getting "
                      "value of variable \"%V\" asynchronously",
                      &vars[*index].name);
        return NGX_ERROR;
    }

    if (service_data->error) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "an exception was caught while getting "
                      "value of variable \"%V\" asynchronously: \"%V\"",
                      &vars[*index].name, &service_data->result.data);
        /* BEWARE: do not return value of the exception */
        return NGX_ERROR;
    }

    res = service_data->result.data;

update_var:

    v->len = res.len;
    v->data = res.data;
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
    HsInt32                                   len = 0, st = NGX_HTTP_OK;
    HsWord32                                  err;
    size_t                                    slen;
    ngx_str_t                                *res, buf;
    u_char                                   *sres = NULL;
    char                                     *eres;
    ngx_int_t                                 elen;
    ngx_str_t                                 ereslen;
    ngx_chain_t                              *out, *out_cur;
    ngx_buf_t                                *b;
    ngx_pool_cleanup_t                       *cln;
    ngx_http_haskell_content_handler_data_t  *clnd;
    ngx_pool_t                               *pool;
    HsStablePtr                               locked_bytestring;
    ngx_uint_t                                def_handler;
    ngx_int_t                                 rc;

    if (ngx_http_discard_request_body(r) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;

    def_handler = handlers[lcf->content_handler->handler].type
                                == ngx_http_haskell_handler_type_y_y ? 1 : 0;

    if (lcf->static_content && lcf->content_handler_data != NULL) {
        res = lcf->content_handler_data->yy_cleanup_data.bufs;
        len = lcf->content_handler_data->yy_cleanup_data.n_bufs;
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

    res = &buf;

    switch (handlers[lcf->content_handler->handler].type) {
    case ngx_http_haskell_handler_type_y_y:
        err = ((ngx_http_haskell_handler_dch)
               handlers[lcf->content_handler->handler].self)
                    (arg.data, arg.len, &res, &len, &eres,
                     &locked_bytestring);
        elen = len;
        break;
    case ngx_http_haskell_handler_type_ch:
        err = ((ngx_http_haskell_handler_ch)
               handlers[lcf->content_handler->handler].self)
                    (arg.data, arg.len, &res, &len, &ct.data, &ct.len, &st,
                     &locked_bytestring);
        elen = st;
        eres = (char *) ct.data;
        break;
    case ngx_http_haskell_handler_type_uch:
        err = ((ngx_http_haskell_handler_uch)
               handlers[lcf->content_handler->handler].self)
                    (arg.data, arg.len, &sres, &slen, &ct.data, &ct.len, &st);
        len = slen;
        elen = st;
        eres = (char *) ct.data;
        break;
    default:
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (elen == -1 || len == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while running "
                      "haskell content handler");
        if (handlers[lcf->content_handler->handler].type
            == ngx_http_haskell_handler_type_uch)
        {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        goto cleanup;
    }

    if (err) {
        if (eres == NULL) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "impossible branch while running "
                          "haskell content handler");
        } else {
            ereslen.len = elen;
            ereslen.data = (u_char *) eres;
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while running "
                          "haskell content handler: \"%V\"", &ereslen);
            if (handlers[lcf->content_handler->handler].type
                == ngx_http_haskell_handler_type_y_y)
            {
                ngx_free(eres);
            }
        }
        if (handlers[lcf->content_handler->handler].type
            == ngx_http_haskell_handler_type_uch)
        {
            ngx_free(eres);
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        goto cleanup;
    }

    if (handlers[lcf->content_handler->handler].type
        == ngx_http_haskell_handler_type_uch)
    {
        goto send_response;
    }

    if (res == NULL && len != 0) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "impossible branch while running "
                      "haskell content handler");
        goto cleanup;
    }

    if (!lcf->static_content || lcf->content_handler_data == NULL) {
        pool = lcf->static_content ? lcf->pool : r->pool;
        cln = ngx_pool_cleanup_add(pool, 0);
        clnd = ngx_palloc(pool,
                          sizeof(ngx_http_haskell_content_handler_data_t));
        if (cln == NULL || clnd == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to register cleanup handler for "
                          "content handler data");
            mcf->release_locked_bytestring(locked_bytestring);
            goto cleanup;
        }
        /* do not let release_locked_bytestring() after hs_exit()
         * for static content! */
        clnd->yy_cleanup_data.release_locked_bytestring =
                lcf->static_content ? NULL : mcf->release_locked_bytestring;
        clnd->yy_cleanup_data.locked_bytestring =
                lcf->static_content ? NULL : locked_bytestring;
        clnd->yy_cleanup_data.bufs = len > 1 ? res : NULL;
        clnd->yy_cleanup_data.n_bufs = len;
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
        if (handlers[lcf->content_handler->handler].type
            == ngx_http_haskell_handler_type_uch)
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

    if (len > 1) {
        ngx_free(res);
    }

    if (!def_handler) {
        ngx_free(ct.data);
    }

    return NGX_HTTP_INTERNAL_SERVER_ERROR;
}


static ngx_int_t
ngx_http_haskell_service_var_init_zone(ngx_shm_zone_t *shm_zone, void *data)
{
    ngx_uint_t                          i;
    ngx_slab_pool_t                    *shpool;
    ngx_array_t                        *vars;
    ngx_http_haskell_var_handle_t      *vars_elts;
    ngx_http_haskell_shm_var_handle_t  *shm_vars;

    if (shm_zone->shm.exists) {
        return NGX_OK;
    }

    shpool = (ngx_slab_pool_t *) shm_zone->shm.addr;
    vars = shm_zone->data;
    vars_elts = vars->elts;

    ngx_shmtx_lock(&shpool->mutex);

    shm_vars = ngx_slab_alloc_locked(shpool,
                    sizeof(ngx_http_haskell_shm_var_handle_t) * vars->nelts);
    if (shm_vars == NULL) {
        ngx_shmtx_unlock(&shpool->mutex);
        return NGX_ERROR;
    }

    for (i = 0; i < vars->nelts; i++) {
        shm_vars[i].handle = vars_elts[i];
        ngx_str_null(&shm_vars[i].data.result.data);
        shm_vars[i].data.result.complete = 0;
        shm_vars[i].data.index = vars_elts[i].index;
        shm_vars[i].data.error = 0;
        shm_vars[i].modified = 0;
    }

    ngx_shmtx_unlock(&shpool->mutex);

    shpool->data = shm_vars;
    shm_zone->data = shm_vars;

    return NGX_OK;
}


static void
ngx_http_haskell_async_event(ngx_event_t *ev)
{
    ngx_http_haskell_async_event_t    *hev = ev->data;

    /* FIXME: can events outlast request data? In this case we must not use r!
     * Tests have shown that request does persist when the client side closes
     * connection. Is this still correct for posted events? */

    ev->active = 0;
    if (ngx_del_event(ev, NGX_READ_EVENT, 0) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, hev->r->connection->log, 0,
                      "failed to delete event after async task was finished");
    }

    if (close(hev->s.fd) == -1) {
        ngx_log_error(NGX_LOG_CRIT, hev->r->connection->log, ngx_errno,
                      "failed to close async event channel "
                      "after async task was finished");
    }

    *hev->complete = 1;

    ngx_http_core_run_phases(hev->r);
}


static void
ngx_http_haskell_service_async_event(ngx_event_t *ev)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_service_async_event_t    *hev = ev->data;
    ngx_cycle_t                               *cycle = hev->cycle;

    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_core_main_conf_t                 *cmcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_var;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_http_haskell_var_handle_t             *vars;
    ngx_http_variable_t                       *cmvars;
    ngx_slab_pool_t                           *shpool;
    ngx_http_haskell_shm_var_handle_t         *shm_vars;
    ngx_str_t                                 *var;
    u_char                                    *var_data;
    ngx_http_complex_value_t                  *args;
    ngx_uint_t                                 ignore_empty = 0;
    ngx_int_t                                  found_idx = NGX_ERROR;
    ngx_int_t                                  rc;

    service_code_var = hev->service_code_var;
    service_code_var->running = 0;

    if (hev->s.fd == NGX_INVALID_FILE) {
        ngx_http_haskell_run_service(cycle, service_code_var, hev->first_run);
        return;
    }

    ev->active = 0;
    if (ngx_del_event(ev, NGX_READ_EVENT, 0) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to delete event after service task was finished");
    }

    if (close(hev->s.fd) == -1) {
        ngx_log_error(NGX_LOG_CRIT, cycle->log, ngx_errno,
                      "failed to close async event channel "
                      "after service task was finished");
    }

    cmcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_core_module);
    cmvars = cmcf->variables.elts;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);

    rc = ngx_http_haskell_yy_handler_result(cycle->log, NULL,
                    service_code_var->future_async_data.yy_cleanup_data.bufs,
                    service_code_var->future_async_data.yy_cleanup_data.n_bufs,
                    &service_code_var->future_async_data.result.data,
                    service_code_var->future_async_data.yy_cleanup_data.
                                                    release_locked_bytestring,
                    service_code_var->future_async_data.yy_cleanup_data.
                                                    locked_bytestring,
                    &cmvars[service_code_var->data->index], 1, 1);
    service_code_var->future_async_data.result.complete =
            rc == NGX_OK ? 2 : (rc == NGX_DONE ? 1 : 0);

    if (service_code_var->cb) {
        if (service_code_var->noarg) {
            args = service_code_var->data->args.elts;
            ngx_free(args[0].value.data);
            args[0].value.len = 0;
        }
        if (service_code_var->future_async_data.result.complete == 2) {
            ngx_free(service_code_var->future_async_data.result.data.data);
        } else if (service_code_var->future_async_data.result.complete == 1) {
            mcf->release_locked_bytestring(
                        service_code_var->future_async_data.yy_cleanup_data.
                                                            locked_bytestring);
        }
        return;
    }

    vars = mcf->service_var_ignore_empty.elts;
    for (i = 0; i < mcf->service_var_ignore_empty.nelts; i++) {
        if (vars[i].index == service_code_var->data->index) {
            ignore_empty = 1;
            break;
        }
    }

    if (ignore_empty && service_code_var->future_async_data.result.data.len
        == 0)
    {
        if (service_code_var->future_async_data.yy_cleanup_data.bufs != NULL) {
            ngx_free(service_code_var->future_async_data.yy_cleanup_data.bufs);
            mcf->release_locked_bytestring(
                        service_code_var->future_async_data.yy_cleanup_data.
                                                            locked_bytestring);
        }
    } else {
        if (service_code_var->async_data != NULL) {
            ngx_http_haskell_service_handler_cleanup(
                                                service_code_var->async_data);
        }
        service_code_var->async_data =
                ngx_alloc(sizeof(ngx_http_haskell_async_data_t), cycle->log);

        if (service_code_var->async_data == NULL) {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to allocate memory for service async data, "
                          "using old data");
            ngx_free(service_code_var->future_async_data.yy_cleanup_data.bufs);
            mcf->release_locked_bytestring(
                        service_code_var->future_async_data.yy_cleanup_data.
                                                            locked_bytestring);
            goto run_service;
        } else {
            *service_code_var->async_data = service_code_var->future_async_data;
            service_code_var->async_data->ref_count = 1;
        }

        vars = mcf->service_var_in_shm.elts;
        for (i = 0; i < mcf->service_var_in_shm.nelts; i++) {
            if (vars[i].index == service_code_var->data->index) {
                found_idx = i;
                break;
            }
        }
    }

    if (found_idx == NGX_ERROR) {
        goto run_service;
    }

    shpool = (ngx_slab_pool_t *) mcf->shm_zone->shm.addr;
    shm_vars = shpool->data;

    ngx_shmtx_lock(&shpool->mutex);

    var = &shm_vars[found_idx].data.result.data;

    shm_vars[found_idx].data.error = service_code_var->async_data->error;

    if (service_code_var->async_data->result.data.len == (ngx_uint_t) -1) {
        if (var->data != NULL) {
            ngx_slab_free_locked(shpool, var->data);
        }
        var->len = -1;
        var->data = NULL;
        goto unlock_and_run_service;
    }

    if (var->len == service_code_var->async_data->result.data.len
        && ngx_memcmp(var->data, service_code_var->async_data->result.data.data,
                      var->len) == 0)
    {
        goto unlock_and_run_service;
    }

    if (service_code_var->async_data->result.data.len == 0) {
        if (var->data != NULL) {
            ngx_slab_free_locked(shpool, var->data);
        }
        ngx_str_null(var);
        goto cb_unlock_and_run_service;
    }

    var_data = ngx_slab_alloc_locked(shpool,
                                service_code_var->async_data->result.data.len);
    if (var_data == NULL) {
        ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                      "failed to allocate memory to store variable \"%V\"",
                      &shm_vars[found_idx].handle.name);
        goto unlock_and_run_service;
    }

    ngx_memcpy(var_data, service_code_var->async_data->result.data.data,
               service_code_var->async_data->result.data.len);
    shm_vars[found_idx].data.error = 0;
    /* TODO: update field modified with current time value */

    if (var->data != NULL) {
        ngx_slab_free_locked(shpool, var->data);
    }
    var->len = service_code_var->async_data->result.data.len;
    var->data = var_data;

cb_unlock_and_run_service:

    service_code_vars = mcf->service_code_vars.elts;
    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (service_code_vars[i].data->index == service_code_var->data->index
            && service_code_vars[i].cb && !service_code_vars[i].running)
        {
            if (service_code_vars[i].noarg) {
                args = service_code_vars[i].data->args.elts;
                args[0].value.len = var->len;
                args[0].value.data = NULL;
                if (var->len > 0) {
                     args[0].value.data = ngx_alloc(var->len, cycle->log);
                     if (args[0].value.data == NULL) {
                         ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                                       "failed to allocate memory for haskell "
                                       "callback argument");
                         args[0].value.len = 0;
                         continue;
                     }
                     ngx_memcpy(args[0].value.data, var->data, var->len);
                }
            }
            ngx_http_haskell_run_service(cycle, &service_code_vars[i],
                                         hev->first_run);
        }
    }

unlock_and_run_service:

    ngx_shmtx_unlock(&shpool->mutex);

run_service:

    ngx_http_haskell_run_service(cycle, service_code_var, 0);
}


static ngx_int_t
ngx_http_haskell_yy_handler_result(ngx_log_t *log, ngx_pool_t *pool,
                                   ngx_str_t *bufs, HsInt32 n_bufs,
                                   ngx_str_t *res,
                                   void (*release_locked_bytestring)
                                                                (HsStablePtr),
                                   HsStablePtr locked_bytestring,
                                   ngx_http_variable_t *var,
                                   ngx_uint_t cleanup, ngx_uint_t service)
{
    ngx_int_t                            i;
    ngx_pool_cleanup_t                  *cln = NULL;
    ngx_http_haskell_yy_cleanup_data_t  *clnd = NULL;
    ngx_int_t                            written;
    ngx_int_t                            rc;

    rc = service ? NGX_DONE : NGX_OK;

    if (n_bufs == -1) {
        return NGX_ERROR;
    }

    if (bufs == NULL) {
        rc = service ? NGX_ERROR : NGX_OK;
        if (n_bufs == 0) {
            res->len = 0;
            res->data = (u_char *) "";
        } else {
            ngx_log_error(NGX_LOG_CRIT, log, 0,
                          "impossible branch while running "
                          "haskell handler");
            rc = NGX_ERROR;
        }
        return rc;
    }

    if (n_bufs == 0) {
        ngx_log_error(NGX_LOG_CRIT, log, 0,
                      "impossible branch while running "
                      "haskell handler");
        return NGX_ERROR;       /* cleanup is dangerous here! */
    } else if (n_bufs == 1) {
        if (cleanup && !service /* any synchronous handler */) {
            cln = ngx_pool_cleanup_add(pool, 0);
            clnd = ngx_palloc(pool, sizeof(ngx_http_haskell_yy_cleanup_data_t));
            if (cln == NULL || clnd == NULL) {
                ngx_log_error(NGX_LOG_ERR, log, 0,
                              "failed to allocate cleanup handler "
                              "for variable \"%V\"", var->name);
                goto cleanup;
            }
            clnd->bufs = NULL;
            clnd->n_bufs = 1;
            clnd->release_locked_bytestring = release_locked_bytestring;
            clnd->locked_bytestring = locked_bytestring;
            cln->handler = ngx_http_haskell_yy_handler_cleanup;
            cln->data = clnd;
            res->len = bufs->len;
            res->data = bufs->data;
        }
        return rc;
    } else {
        res->len = 0;
        for (i = 0; i < n_bufs; i++) {
            res->len += bufs[i].len;
        }
        res->data = service ? ngx_alloc(res->len, log) :
                                            ngx_pnalloc(pool, res->len);
        if (res->data == NULL) {
            ngx_log_error(NGX_LOG_ERR, log, 0,
                        "failed to allocate contiguous memory block "
                        "for variable \"%V\"", var->name);
            goto cleanup;
        }
        written = 0;
        for (i = 0; i < n_bufs; i++) {
            ngx_memcpy(res->data + written, bufs[i].data, bufs[i].len);
            written += bufs[i].len;
        }
        if (cleanup && bufs != NULL) {
            ngx_free(bufs);
            release_locked_bytestring(locked_bytestring);
        }
    }

    return NGX_OK;

cleanup:

    if (cleanup && bufs != NULL) {
        if (n_bufs > 1) {
            ngx_free(bufs);
        }
        release_locked_bytestring(locked_bytestring);
    }

    return NGX_ERROR;
}


static void
ngx_http_haskell_yy_handler_cleanup(void *data)
{
    ngx_http_haskell_yy_cleanup_data_t       *clnd = data;

    if (clnd->n_bufs > 0) {
        if (clnd->n_bufs > 1) {
            ngx_free(clnd->bufs);
        }
        if (clnd->release_locked_bytestring != NULL) {
            clnd->release_locked_bytestring(clnd->locked_bytestring);
        }
    }
}


static void
ngx_http_haskell_content_handler_cleanup(void *data)
{
    ngx_http_haskell_content_handler_data_t  *clnd = data;

    ngx_free(clnd->content_type.data);
    ngx_http_haskell_yy_handler_cleanup(&clnd->yy_cleanup_data);
}


static void
ngx_http_haskell_service_handler_cleanup(void *data)
{
    ngx_http_haskell_async_data_t            *async_data = data;

    if (--async_data->ref_count != 0) {
        return;
    }

    if (async_data->result.complete == 2) {
        ngx_free(async_data->result.data.data);
    } else if (async_data->result.complete == 1) {
        async_data->yy_cleanup_data.release_locked_bytestring(
                                async_data->yy_cleanup_data.locked_bytestring);
    }

    ngx_free(async_data);
}


static ngx_int_t
ngx_http_haskell_open_async_event_channel(ngx_fd_t fd[2])
{
#if (NGX_HAVE_EVENTFD)
#if (NGX_HAVE_SYS_EVENTFD_H)
    fd[0] = fd[1] = eventfd(0, EFD_NONBLOCK);
#else
    fd[0] = fd[1] = syscall(323, O_NONBLOCK);
#endif
    return fd[0] == NGX_INVALID_FILE ? NGX_ERROR : NGX_OK;
#else
    return pipe2(fd, O_NONBLOCK) == -1 ? NGX_ERROR : NGX_OK;
#endif
}


static void
ngx_http_haskell_close_async_event_channel(ngx_log_t *log, ngx_fd_t fd[2])
{
    ngx_int_t  i;

    for (i = 0; i < (fd[0] == fd[1] ? 1 : 2); i++) {
        if (close(fd[i]) == -1) {
            ngx_log_error(NGX_LOG_CRIT, log, ngx_errno,
                          "failed to close file descriptor of "
                          "async event channel");
        }
    }
}

