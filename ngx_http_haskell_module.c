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

#define STRLEN(X) (sizeof(X) - 1)


static const char  haskell_module_handler_prefix[] = "ngx_hs_";
static const char  haskell_module_type_checker_prefix[] = "type_";
static const size_t  haskell_module_handler_prefix_len =
    STRLEN(haskell_module_handler_prefix);
static const size_t  haskell_module_type_checker_prefix_len =
    STRLEN(haskell_module_type_checker_prefix);

static const char  haskell_module_code_head[] =
"{-# LANGUAGE ForeignFunctionInterface, CPP, ViewPatterns #-}""\n\n"
"#define NGX_EXPORT_S_S(F) ngx_hs_ ## F = aux_ngx_hs_s_s $ AUX_NGX_S_S F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> \\\n    AUX_NGX.Ptr AUX_NGX.CString -> "
"IO AUX_NGX.CInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_S_S F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_S_SS(F) ngx_hs_ ## F = aux_ngx_hs_s_ss $ AUX_NGX_S_SS F; "
"\\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> \\\n    AUX_NGX.CString -> AUX_NGX.CInt -> "
"\\\n    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_S_SS F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_S_LS(F) ngx_hs_ ## F = aux_ngx_hs_s_ls $ AUX_NGX_S_LS F; "
"\\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt -> "
"\\\n    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_S_LS F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_B_S(F) ngx_hs_ ## F = aux_ngx_hs_b_s $ AUX_NGX_B_S F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> \\\n    IO AUX_NGX.CUInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_B_S F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_B_SS(F) ngx_hs_ ## F = aux_ngx_hs_b_ss $ AUX_NGX_B_SS F; "
"\\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> \\\n    AUX_NGX.CString -> AUX_NGX.CInt -> "
"\\\n    IO AUX_NGX.CUInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_B_SS F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_B_LS(F) ngx_hs_ ## F = aux_ngx_hs_b_ls $ AUX_NGX_B_LS F; "
"\\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt -> \\\n    IO AUX_NGX.CUInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_B_LS F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_Y_Y(F) ngx_hs_ ## F = aux_ngx_hs_y_y $ AUX_NGX_Y_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> \\\n    AUX_NGX.Ptr AUX_NGX.CString -> "
"IO AUX_NGX.CInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_Y_Y F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_B_Y(F) ngx_hs_ ## F = aux_ngx_hs_b_y $ AUX_NGX_B_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> \\\n    IO AUX_NGX.CUInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_B_Y F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_HANDLER(F) ngx_hs_ ## F = aux_ngx_hs_handler $ "
"AUX_NGX_HANDLER F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> "
"\\\n    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> "
"AUX_NGX.Ptr AUX_NGX.CInt -> \\\n    AUX_NGX.Ptr AUX_NGX.CString -> "
"AUX_NGX.Ptr AUX_NGX.CInt -> \\\n    IO AUX_NGX.CInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_HANDLER F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"#define NGX_EXPORT_DEF_HANDLER(F) ngx_hs_ ## F = aux_ngx_hs_def_handler $ "
"AUX_NGX_Y_Y F; \\\n"
"foreign export ccall ngx_hs_ ## F :: \\\n    "
"AUX_NGX.CString -> AUX_NGX.CInt -> "
"\\\n    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> IO AUX_NGX.CInt; \\\n"
"type_ngx_hs_ ## F = return $ fromIntegral $ fromEnum $ AUX_NGX_Y_Y F; \\\n"
"foreign export ccall type_ngx_hs_ ## F :: IO AUX_NGX.CInt\n\n"
"module NgxHaskellUserRuntime where\n\n"
"import qualified Foreign.C as AUX_NGX\n"
"import qualified Foreign.Ptr as AUX_NGX\n"
"import qualified Foreign.Storable as AUX_NGX\n"
"import qualified Foreign.Marshal.Alloc as AUX_NGX\n"
"import qualified Foreign.Marshal.Utils as AUX_NGX\n"
"import qualified System.IO.Error as AUX_NGX\n"
"import qualified Data.ByteString as AUX_NGX_BS\n"
"import qualified Data.ByteString.Unsafe as AUX_NGX_BS\n"
"import qualified Data.ByteString.Lazy as AUX_NGX_BSL\n\n"
"-- START OF USER HASKELL CODE\n";

static const char  haskell_module_code_tail[] =
"\n-- END OF USER HASKELL CODE\n\n"
"data AUX_NGX_EXPORT = AUX_NGX_S_S (String -> String)\n"
"                    | AUX_NGX_S_SS (String -> String -> String)\n"
"                    | AUX_NGX_S_LS ([String] -> String)\n"
"                    | AUX_NGX_B_S (String -> Bool)\n"
"                    | AUX_NGX_B_SS (String -> String -> Bool)\n"
"                    | AUX_NGX_B_LS ([String] -> Bool)\n"
"                    | AUX_NGX_Y_Y (AUX_NGX_BS.ByteString -> "
"AUX_NGX_BSL.ByteString)\n"
"                    | AUX_NGX_B_Y (AUX_NGX_BS.ByteString -> Bool)\n"
"                    | AUX_NGX_HANDLER (AUX_NGX_BS.ByteString -> "
"(AUX_NGX_BSL.ByteString, String, Int))\n\n"
"instance Enum AUX_NGX_EXPORT where\n"
"    toEnum _ = AUX_NGX_S_S id            -- not used\n"
"    fromEnum (AUX_NGX_S_S _)     = 1\n"
"    fromEnum (AUX_NGX_S_SS _)    = 2\n"
"    fromEnum (AUX_NGX_S_LS _)    = 3\n"
"    fromEnum (AUX_NGX_B_S _)     = 4\n"
"    fromEnum (AUX_NGX_B_SS _)    = 5\n"
"    fromEnum (AUX_NGX_B_LS _)    = 6\n"
"    fromEnum (AUX_NGX_Y_Y _)     = 7\n"
"    fromEnum (AUX_NGX_B_Y _)     = 8\n"
"    fromEnum (AUX_NGX_HANDLER _) = 9\n\n"
"data AUX_NGX_STR_TYPE = AUX_NGX_STR_TYPE AUX_NGX.CSize "
"(AUX_NGX.Ptr AUX_NGX.CChar)\n"
"instance AUX_NGX.Storable AUX_NGX_STR_TYPE where\n"
"    alignment _ = max (AUX_NGX.alignment (undefined :: AUX_NGX.CSize)) $\n"
"                       AUX_NGX.alignment "
"(undefined :: AUX_NGX.Ptr AUX_NGX.CChar)\n"
"    sizeOf = (* 2) . AUX_NGX.alignment   -- must always be correct for "
"aligned struct ngx_str_t\n"
"    peek p = do\n"
"        n <- AUX_NGX.peekByteOff p 0\n"
"        s <- AUX_NGX.peekByteOff p $ AUX_NGX.alignment "
"(undefined :: AUX_NGX_STR_TYPE)\n"
"        return $ AUX_NGX_STR_TYPE n s\n"
"    poke p a@(AUX_NGX_STR_TYPE n s) = do\n"
"        AUX_NGX.poke (AUX_NGX.castPtr p) n\n"
"        AUX_NGX.poke (AUX_NGX.plusPtr p $ AUX_NGX.alignment a) s\n\n"
"aux_ngx_catchAlloc :: IO (AUX_NGX.Ptr a) -> IO (AUX_NGX.Ptr a)\n"
"aux_ngx_catchAlloc = (`AUX_NGX.catchIOError` const (return AUX_NGX.nullPtr))"
"\n\n"
"aux_ngx_peekNgxStringArrayLen :: AUX_NGX.Ptr AUX_NGX_STR_TYPE -> Int -> "
"IO [String]\n"
"aux_ngx_peekNgxStringArrayLen x n = sequence $\n"
"    foldr (\\k ->\n"
"              ((AUX_NGX.peekElemOff x k >>=\n"
"                  (\\(AUX_NGX_STR_TYPE m y) ->\n"
"                      AUX_NGX.peekCStringLen (y, fromIntegral m))) :))\n"
"          [] [0 .. n - 1]\n\n"
"aux_ngx_toSingleBuffer :: AUX_NGX_BSL.ByteString -> "
"IO (Maybe (AUX_NGX.CString, Int))\n"
"aux_ngx_toSingleBuffer (AUX_NGX_BSL.uncons -> Nothing) = "
"return $ Just (AUX_NGX.nullPtr, 0)\n"
"aux_ngx_toSingleBuffer s = do\n"
"    let (fromIntegral -> l) = AUX_NGX_BSL.length s\n"
"    t <- aux_ngx_catchAlloc $ AUX_NGX.mallocBytes l\n"
"    if t /= AUX_NGX.nullPtr\n"
"        then do\n"
"            AUX_NGX_BSL.foldlChunks\n"
"                (\\a s -> do\n"
"                    off <- a\n"
"                    let l = AUX_NGX_BS.length s\n"
"                    AUX_NGX_BS.unsafeUseAsCString s $\n"
"                        flip (AUX_NGX.copyBytes $ AUX_NGX.plusPtr t off) l\n"
"                    return $ off + l\n"
"                ) (return 0) s\n"
"            return $ Just (t, l)\n"
"        else return Nothing\n\n"
"aux_ngx_toBuffers :: AUX_NGX_BSL.ByteString -> "
"IO (Maybe (AUX_NGX.Ptr AUX_NGX_STR_TYPE, Int))\n"
"aux_ngx_toBuffers (AUX_NGX_BSL.uncons -> Nothing) = "
"return $ Just (AUX_NGX.nullPtr, 0)\n"
"aux_ngx_toBuffers s = do\n"
"    t <- aux_ngx_catchAlloc $ AUX_NGX.mallocBytes $\n"
"        AUX_NGX_BSL.foldlChunks (const . succ) 0 s *\n"
"        AUX_NGX.sizeOf (undefined :: AUX_NGX_STR_TYPE)\n"
"    (fromIntegral . fst -> l) <- AUX_NGX_BSL.foldlChunks\n"
"        (\\a s -> do\n"
"            (off, mallocSuccess) <- a\n"
"            if mallocSuccess\n"
"                then do\n"
"                    let l = AUX_NGX_BS.length s\n"
"                    -- l cannot be zero at this point\n"
"                    dst <- aux_ngx_catchAlloc $ AUX_NGX.mallocBytes l\n"
"                    if dst /= AUX_NGX.nullPtr\n"
"                        then do\n"
"                            AUX_NGX_BS.unsafeUseAsCString s $ "
"flip (AUX_NGX.copyBytes dst) l\n"
"                            AUX_NGX.pokeElemOff t off $ "
"AUX_NGX_STR_TYPE (fromIntegral l) dst\n"
"                            return (off + 1, True)\n"
"                        else do\n"
"                            mapM_ (AUX_NGX.peekElemOff t >=>\n"
"                                      \\(AUX_NGX_STR_TYPE n x) -> "
"AUX_NGX.free x)\n"
"                                  [0 .. off - 1]    "
"-- [0 .. -1] makes [], so wise!\n"
"                            AUX_NGX.free t\n"
"                            return (0, False)\n"
"                else return (0, False)\n"
"        ) (return (0, t /= AUX_NGX.nullPtr)) s\n"
"    return $ if l > 0 then Just (t, l) else Nothing\n\n"
"aux_ngx_hs_s_s :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_s_s (AUX_NGX_S_S f) x (fromIntegral -> n) p = do\n"
"    (s, fromIntegral -> l) <- f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                                >>= AUX_NGX.newCStringLen\n"
"    AUX_NGX.poke p s\n"
"    return l\n\n"
"aux_ngx_hs_s_ss :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_s_ss (AUX_NGX_S_SS f) x (fromIntegral -> n) y (fromIntegral -> m) "
"p = do\n"
"    (s, fromIntegral -> l) <- f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                                <*> AUX_NGX.peekCStringLen (y, m)\n"
"                                >>= AUX_NGX.newCStringLen\n"
"    AUX_NGX.poke p s\n"
"    return l\n\n"
"aux_ngx_hs_s_ls :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_s_ls (AUX_NGX_S_LS f) x (fromIntegral -> n) p = do\n"
"    (s, fromIntegral -> l) <- f <$> aux_ngx_peekNgxStringArrayLen x n\n"
"                                >>= AUX_NGX.newCStringLen\n"
"    AUX_NGX.poke p s\n"
"    return l\n\n"
"aux_ngx_hs_y_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_y_y (AUX_NGX_Y_Y f) x (fromIntegral -> n) p = do\n"
"    s <- f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"    (maybe (AUX_NGX.nullPtr, -1) id -> (t, l)) <- aux_ngx_toSingleBuffer s\n"
"    AUX_NGX.poke p t\n"
"    return $ fromIntegral l\n\n"
"aux_ngx_hs_b_s :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_s (AUX_NGX_B_S f) x (fromIntegral -> n) =\n"
"    AUX_NGX.fromBool . f <$> AUX_NGX.peekCStringLen (x, n)\n\n"
"aux_ngx_hs_b_ss :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_ss (AUX_NGX_B_SS f) x (fromIntegral -> n) y "
"(fromIntegral -> m) =\n"
"    (AUX_NGX.fromBool .) . f <$> AUX_NGX.peekCStringLen (x, n)\n"
"                             <*> AUX_NGX.peekCStringLen (y, m)\n\n"
"aux_ngx_hs_b_ls :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.Ptr AUX_NGX_STR_TYPE -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_ls (AUX_NGX_B_LS f) x (fromIntegral -> n) =\n"
"    AUX_NGX.fromBool . f <$> aux_ngx_peekNgxStringArrayLen x n\n\n"
"aux_ngx_hs_b_y :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CUInt\n"
"aux_ngx_hs_b_y (AUX_NGX_B_Y f) x (fromIntegral -> n) =\n"
"    AUX_NGX.fromBool . f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n\n"
"aux_ngx_hs_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> AUX_NGX.Ptr AUX_NGX.CInt ->"
"\n"
"    AUX_NGX.Ptr AUX_NGX.CString -> AUX_NGX.Ptr AUX_NGX.CInt ->\n"
"    IO AUX_NGX.CInt\n"
"aux_ngx_hs_handler (AUX_NGX_HANDLER f) x (fromIntegral -> n) ps pls pt plt = "
"do\n"
"    (s, mt, fromIntegral -> st) <- f <$> AUX_NGX_BS.unsafePackCStringLen "
"(x, n)\n"
"    (maybe (AUX_NGX.nullPtr, -1) id -> (t, l)) <- aux_ngx_toBuffers s\n"
"    AUX_NGX.poke ps t\n"
"    AUX_NGX.poke pls $ fromIntegral l\n"
"    (smt, fromIntegral -> lmt) <- AUX_NGX.newCStringLen mt\n"
"    AUX_NGX.poke pt smt\n"
"    AUX_NGX.poke plt lmt\n"
"    return st\n\n"
"aux_ngx_hs_def_handler :: AUX_NGX_EXPORT ->\n"
"    AUX_NGX.CString -> AUX_NGX.CInt ->\n"
"    AUX_NGX.Ptr (AUX_NGX.Ptr AUX_NGX_STR_TYPE) -> IO AUX_NGX.CInt\n"
"aux_ngx_hs_def_handler (AUX_NGX_Y_Y f) x (fromIntegral -> n) ps = do\n"
"    s <- f <$> AUX_NGX_BS.unsafePackCStringLen (x, n)\n"
"    (maybe (AUX_NGX.nullPtr, -1) id -> (t, l)) <- aux_ngx_toBuffers s\n"
"    AUX_NGX.poke ps t\n"
"    return $ fromIntegral l\n\n";

static const char  haskell_compile_cmd[] =
    "ghc --make -O2 -shared -dynamic -no-hs-main -pgmP cpp"
    " -optl-Wl,-rpath,$(ghc --print-libdir)/rts"
    " -lHSrts-ghc$(ghc --numeric-version) -o ";


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
typedef HsInt32 (*ngx_http_haskell_handler_ch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsInt32 (*ngx_http_haskell_handler_dch)
    (HsPtr, HsInt32, HsPtr);

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
    ngx_http_haskell_handler_type_ch
} ngx_http_haskell_handler_type_e;


typedef enum {
    ngx_http_haskell_handler_role_uninitialized = 0,
    ngx_http_haskell_handler_role_variable,
    ngx_http_haskell_handler_role_content_handler
} ngx_http_haskell_handler_role_e;


typedef struct {
    ngx_uint_t                            code_loaded;
    ngx_str_t                             ghc_extra_flags;
    ngx_str_t                             lib_path;
    ngx_array_t                           handlers;
    void                                 *dl_handle;
    void                                (*hs_init)(int *, char ***);
    void                                (*hs_exit)(void);
    void                                (*hs_add_root)(void (*)(void));
    void                                (*init_HsModule)(void);
} ngx_http_haskell_main_conf_t;


typedef struct {
    ngx_int_t                             handler;
    ngx_http_complex_value_t             *args;
} ngx_http_haskell_content_handler_t;


typedef struct {
    ngx_array_t                           code_vars;
    ngx_http_haskell_content_handler_t   *content_handler;
} ngx_http_haskell_loc_conf_t;


typedef struct {
    void                                 *self;
    ngx_http_haskell_handler_type_e       type;
    ngx_http_haskell_handler_role_e       role;
    ngx_str_t                             name;
    ngx_uint_t                            n_args[3];
} ngx_http_haskell_handler_t;


typedef struct {
    ngx_int_t                             index;
    ngx_int_t                             handler;
    ngx_array_t                           args;
} ngx_http_haskell_code_var_data_t;


static char *ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static char *ngx_http_haskell_write_code(ngx_conf_t *cf, ngx_str_t source_name,
    ngx_str_t fragment);
static char *ngx_http_haskell_compile(ngx_conf_t *cf, void *conf,
    ngx_str_t source_name);
static ngx_int_t ngx_http_haskell_load(ngx_cycle_t *cycle);
static void ngx_http_haskell_unload(ngx_cycle_t *cycle);
static char *ngx_http_haskell_run(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static char *ngx_http_haskell_content(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static void *ngx_http_haskell_create_main_conf(ngx_conf_t *cf);
static void *ngx_http_haskell_create_loc_conf(ngx_conf_t *cf);
static char *ngx_http_haskell_merge_loc_conf(ngx_conf_t *cf, void *parent,
    void *child);
static ngx_int_t ngx_http_haskell_init(ngx_cycle_t *cycle);
static void ngx_http_haskell_exit(ngx_cycle_t *cycle);
static ngx_int_t ngx_http_haskell_run_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
static ngx_int_t ngx_http_haskell_content_handler(ngx_http_request_t *r);
static void ngx_http_haskell_request_cleanup(void *data);


static ngx_command_t  ngx_http_haskell_module_commands[] = {

    { ngx_string("haskell"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_TAKE23,
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
    { ngx_string("haskell_content"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },

      ngx_null_command
};


static ngx_http_module_t  ngx_http_haskell_module_ctx = {
    NULL,                                    /* preconfiguration */
    NULL,                                    /* postconfiguration */

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
    ngx_http_haskell_init,                   /* init process */
    NULL,                                    /* init thread */
    NULL,                                    /* exit thread */
    ngx_http_haskell_exit,                   /* exit process */
    NULL,                                    /* exit master */
    NGX_MODULE_V1_PADDING
};


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
ngx_http_haskell_init(ngx_cycle_t *cycle)
{
    return ngx_http_haskell_load(cycle);
}


static void
ngx_http_haskell_exit(ngx_cycle_t *cycle)
{
    ngx_http_haskell_unload(cycle);
}


static char *
ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_main_conf_t  *mcf = conf;

    ngx_int_t                      i;
    ngx_str_t                     *value, base_name;
    ngx_file_info_t                lib_info;
    ngx_uint_t                     load = 0, load_without_code = 0;
    ngx_uint_t                     base_name_start = 0;

    if (mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "only one haskell source code block is allowed");
        return NGX_CONF_ERROR;
    }

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
        load_without_code = cf->args->nelts < 4 ? 1 : 0;
    } else if (value[1].len == 15
               && ngx_strncmp(value[1].data, "ghc_extra_flags", 15) == 0)
    {
        if (cf->args->nelts != 3) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell ghc_extra_flags requires 1 parameter");
            return NGX_CONF_ERROR;
        }
        if (mcf->ghc_extra_flags.len > 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell ghc_extra_flags was already set");
            return NGX_CONF_ERROR;
        }
        mcf->ghc_extra_flags = value[2];
        return NGX_CONF_OK;
    } else
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "unknown haskell directive \"%V\"", &value[1]);
        return NGX_CONF_ERROR;
    }

    if (value[2].len < 3
        || !(ngx_strncmp(value[2].data + value[2].len - 3, ".hs", 3) == 0
             || (load_without_code
                 && ngx_strncmp(value[2].data + value[2].len - 3, ".so", 3)
                    == 0)))
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file must have extension \".hs\"");
        return NGX_CONF_ERROR;
    }
    if (!ngx_path_separator(value[2].data[0])) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file path must be absolute");
        return NGX_CONF_ERROR;
    }
    for (i = value[2].len - 4; i >= 0; i--) {
        if (ngx_path_separator(value[2].data[i])) {
            base_name_start = i;
            break;
        }
    }
    base_name.len = value[2].len - 4 - base_name_start;
    base_name.data = value[2].data + base_name_start;
    if (base_name.len == 0) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file base name is empty");
        return NGX_CONF_ERROR;
    }

    mcf->lib_path.len = value[2].len;
    mcf->lib_path.data = ngx_pnalloc(cf->pool, mcf->lib_path.len + 1);
    if (mcf->lib_path.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(mcf->lib_path.data, value[2].data, value[2].len - 3);
    ngx_memcpy(mcf->lib_path.data + value[2].len - 3, ".so", 3);
    mcf->lib_path.data[value[2].len] = '\0';

    if (load) {
        if (ngx_file_info(mcf->lib_path.data, &lib_info) == NGX_FILE_ERROR) {
            if (load_without_code) {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell library cannot be loaded nor compiled");
                return NGX_CONF_ERROR;
            }
            load = 0;
        }
    }

    if (!load) {
        if (ngx_http_haskell_write_code(cf, value[2], value[3])
            != NGX_CONF_OK)
        {
            return NGX_CONF_ERROR;
        }

        if (ngx_http_haskell_compile(cf, conf, value[2])
            != NGX_CONF_OK)
        {
            return NGX_CONF_ERROR;
        }
    }

    mcf->code_loaded = 1;

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_write_code(ngx_conf_t *cf, ngx_str_t source_name,
                            ngx_str_t fragment)
{
    ngx_file_t  out;
    ngx_str_t   code;

    code.len = STRLEN(haskell_module_code_head) + fragment.len +
            STRLEN(haskell_module_code_tail);
    code.data = ngx_pnalloc(cf->pool, code.len);
    if (code.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(code.data,
               haskell_module_code_head, STRLEN(haskell_module_code_head));
    ngx_memcpy(code.data + STRLEN(haskell_module_code_head),
               fragment.data, fragment.len);
    ngx_memcpy(code.data + STRLEN(haskell_module_code_head) + fragment.len,
               haskell_module_code_tail, STRLEN(haskell_module_code_tail));

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
    if (out.fd == NGX_INVALID_FILE) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                           "failed to write haskell source code file");
        return NGX_CONF_ERROR;
    }

    (void) ngx_write_file(&out, code.data, code.len, 0);

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
    ngx_uint_t                     extra_len = 0, passed_len, full_len;

    if (mcf->ghc_extra_flags.len > 0) {
        extra_len = mcf->ghc_extra_flags.len + 1;
    }
    full_len = STRLEN(haskell_compile_cmd) + mcf->lib_path.len +
            source_name.len + extra_len + 2;

    compile_cmd = ngx_pnalloc(cf->pool, full_len);
    if (compile_cmd == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(compile_cmd,
               haskell_compile_cmd, STRLEN(haskell_compile_cmd));
    ngx_memcpy(compile_cmd + STRLEN(haskell_compile_cmd), mcf->lib_path.data,
               mcf->lib_path.len);
    ngx_memcpy(compile_cmd + STRLEN(haskell_compile_cmd) + mcf->lib_path.len,
               " ", 1);
    passed_len = STRLEN(haskell_compile_cmd) + mcf->lib_path.len + 1;
    if (extra_len > 0) {
        ngx_memcpy(compile_cmd + passed_len,
                   mcf->ghc_extra_flags.data, mcf->ghc_extra_flags.len);
        ngx_memcpy(compile_cmd + passed_len + mcf->ghc_extra_flags.len,
                   " ", 1);
        passed_len += extra_len;
    }
    ngx_memcpy(compile_cmd + passed_len, source_name.data, source_name.len);
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
    ngx_uint_t                     i;
    ngx_http_haskell_main_conf_t  *mcf;
    ngx_http_haskell_handler_t    *handlers;
    char                          *dl_error;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);

    if (!mcf->code_loaded) {
        return NGX_OK;
    }

    if (mcf->dl_handle != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "haskell library has been unexpectedly loaded");
        return NGX_ERROR;
    }

    mcf->dl_handle = dlopen((char *) mcf->lib_path.data, RTLD_LAZY);
    if (mcf->dl_handle == NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load compiled haskell library");
        return NGX_ERROR;
    }
    dlerror();

    mcf->hs_init = (void (*)(int *, char ***)) dlsym(mcf->dl_handle, "hs_init");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_init\": %s", dl_error);
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

    mcf->hs_init(&ngx_argc, &ngx_argv);
    mcf->hs_add_root(mcf->init_HsModule);

    handlers = mcf->handlers.elts;

    for (i = 0; i < mcf->handlers.nelts; i++) {
        ngx_str_t     handler_name;
        int         (*type_checker)(void);
        char         *type_checker_name = NULL;
        ngx_uint_t    wrong_n_args = 0;

        if (handlers[i].name.len <= haskell_module_handler_prefix_len
            || ngx_strncmp(handlers[i].name.data,
                           haskell_module_handler_prefix,
                           haskell_module_handler_prefix_len) != 0)
        {
            continue;
        }

        handler_name.len = handlers[i].name.len -
                haskell_module_handler_prefix_len;
        handler_name.data = handlers[i].name.data +
                haskell_module_handler_prefix_len;

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
            haskell_module_type_checker_prefix_len + handlers[i].name.len + 1,
            cycle->log);
        if (type_checker_name == NULL) {
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        ngx_memcpy(type_checker_name,
                   haskell_module_type_checker_prefix,
                   haskell_module_type_checker_prefix_len);
        ngx_memcpy(type_checker_name + haskell_module_type_checker_prefix_len,
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
                && handlers[i].type != ngx_http_haskell_handler_type_y_y))
            ||
            (handlers[i].role == ngx_http_haskell_handler_role_variable
             && handlers[i].type == ngx_http_haskell_handler_type_ch))
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "haskell handler \"%V\" role and type mismatch",
                          &handler_name);
            ngx_http_haskell_unload(cycle);
            return NGX_ERROR;
        }

        switch (handlers[i].type) {
        case ngx_http_haskell_handler_type_s_s:
        case ngx_http_haskell_handler_type_b_s:
        case ngx_http_haskell_handler_type_y_y:
        case ngx_http_haskell_handler_type_b_y:
        case ngx_http_haskell_handler_type_ch:
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

    if (!mcf->code_loaded) {
        return;
    }

    if (mcf->dl_handle != NULL) {
        mcf->hs_exit();
        dlclose(mcf->dl_handle);
        mcf->dl_handle = NULL;
    }
}


static char *
ngx_http_haskell_run(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_loc_conf_t       *lcf = conf;

    ngx_uint_t                         i;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_str_t                         *value;
    ngx_uint_t                         n_args;
    ngx_str_t                          handler_name;
    ngx_http_haskell_handler_t        *handlers;
    ngx_http_compile_complex_value_t   ccv;
    ngx_http_complex_value_t          *args;
    ngx_http_variable_t               *v;
    ngx_http_haskell_code_var_data_t  *code_var_data;
    ngx_int_t                          v_idx;
    ngx_uint_t                        *v_idx_ptr;

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);

    if (!mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "haskell code was not loaded");
        return NGX_CONF_ERROR;
    }

    value = cf->args->elts;

    if (cf->args->nelts < 4) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "too few arguments");
        return NGX_CONF_ERROR;
    }
    n_args = cf->args->nelts - 3;

    if (value[2].len < 2 || value[2].data[0] != '$') {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid variable name \"%V\"", &value[2]);
        return NGX_CONF_ERROR;
    }
    value[2].len--;
    value[2].data++;

    handler_name.len = value[1].len + haskell_module_handler_prefix_len;
    handler_name.data = ngx_pnalloc(cf->pool, handler_name.len + 1);
    if (handler_name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(handler_name.data,
               haskell_module_handler_prefix,
               haskell_module_handler_prefix_len);
    ngx_memcpy(handler_name.data + haskell_module_handler_prefix_len,
               value[1].data, value[1].len);
    handler_name.data[handler_name.len] ='\0';

    code_var_data = ngx_array_push(&lcf->code_vars);
    if (code_var_data == NULL) {
        return NGX_CONF_ERROR;
    }
    if (ngx_array_init(&code_var_data->args, cf->pool, 1,
                       sizeof(ngx_http_complex_value_t)) != NGX_OK)
    {
        return NGX_CONF_ERROR;
    }

    code_var_data->handler = NGX_ERROR;

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
        handler->role = ngx_http_haskell_handler_role_variable;

        handlers = mcf->handlers.elts;
        code_var_data->handler = mcf->handlers.nelts - 1;
    }

    ++handlers[code_var_data->handler].n_args[n_args > 2 ? 2 : n_args - 1];

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
    v->get_handler = ngx_http_haskell_run_handler;

    if (ngx_array_push_n(&code_var_data->args, n_args) == NULL) {
        return NGX_CONF_ERROR;
    }
    args = code_var_data->args.elts;

    ngx_memzero(&ccv, sizeof(ngx_http_compile_complex_value_t));
    ccv.cf = cf;

    for (i = 0; i < n_args; i++) {
        ccv.value = &value[3 + i];
        ccv.complex_value = &args[i];

        if (ngx_http_compile_complex_value(&ccv) != NGX_OK) {
            return NGX_CONF_ERROR;
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

    handler_name.len = value[1].len + haskell_module_handler_prefix_len;
    handler_name.data = ngx_pnalloc(cf->pool, handler_name.len + 1);
    if (handler_name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(handler_name.data,
               haskell_module_handler_prefix,
               haskell_module_handler_prefix_len);
    ngx_memcpy(handler_name.data + haskell_module_handler_prefix_len,
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
            if (handlers[i].role ==
                ngx_http_haskell_handler_role_variable)
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared as a vaiable handler", &value[1]);
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


static ngx_int_t
ngx_http_haskell_run_handler(ngx_http_request_t *r,
                             ngx_http_variable_value_t *v, uintptr_t  data)
{
    ngx_uint_t                         i;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_http_haskell_loc_conf_t       *lcf;
    ngx_int_t                         *index = (ngx_int_t *) data;
    ngx_int_t                          found_idx = NGX_ERROR;
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

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    code_vars = lcf->code_vars.elts;

    for (i = 0; i < lcf->code_vars.nelts; i++) {
        if (*index != code_vars[i].index) {
            continue;
        }
        found_idx = i;
        break;
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
        if (ngx_http_complex_value(r, &args[0], &arg1) != NGX_OK) {
            return NGX_ERROR;
        }
        break;
    case ngx_http_haskell_handler_type_s_ls:
    case ngx_http_haskell_handler_type_b_ls:
        argn = ngx_pnalloc(r->pool,
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

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_y_y:
        if (len == -1) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "memory allocation error while running "
                          "haskell handler");
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
            cln->handler = ngx_http_haskell_request_cleanup;
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

    v->len = len;
    v->data = (u_char *) res;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}


static ngx_int_t
ngx_http_haskell_content_handler(ngx_http_request_t *r)
{
    ngx_int_t                          i;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_http_haskell_loc_conf_t       *lcf;
    ngx_http_haskell_handler_t        *handlers;
    ngx_http_complex_value_t          *args;
    ngx_str_t                          arg = ngx_string("");
    ngx_str_t                         *res = NULL;
    ngx_str_t                          ct = { 10, (u_char *) "text/plain" };
    ngx_int_t                          len = 0, st = NGX_HTTP_OK;
    ngx_chain_t                       *out, *out_cur;
    ngx_buf_t                         *b;
    ngx_int_t                          rc;

    if (ngx_http_discard_request_body(r) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

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
    default:
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (len == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while running "
                      "haskell content handler");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (res == NULL) {
        if (len == 0) {
            r->header_only = 1;
        } else {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "impossible branch while running "
                          "haskell content handler");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
    }

    r->headers_out.content_type_len = ct.len;
    r->headers_out.content_type = ct;
    r->headers_out.content_type_lowcase = NULL;
    r->headers_out.status = st;
    r->headers_out.content_length_n = 0;

    out = NULL;
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

    rc = ngx_http_send_header(r);

    if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
        return rc;
    }

    return ngx_http_output_filter(r, out);
}

static void ngx_http_haskell_request_cleanup(void *data)
{
    ngx_free(data);
}

