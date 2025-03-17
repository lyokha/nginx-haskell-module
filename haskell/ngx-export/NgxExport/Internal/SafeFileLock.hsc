{-# LANGUAGE CPP, ForeignFunctionInterface, InterruptibleFFI #-}

module NgxExport.Internal.SafeFileLock (safeWaitToSetLock
                                       ,getBestLockImpl
                                       ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.Posix.IO
import System.Posix.Types
import System.Posix.Internals
import GHC.IO.Device

#include <HsFFI.h>

#ifndef HAVE_FCNTL_H
#error HsFFI claims that required C header file fcntl.h is missing
#endif

#include <fcntl.h>

fcntlOfdSetlkw :: CInt
#ifdef F_OFD_SETLKW
fcntlOfdSetlkw = #const F_OFD_SETLKW
#else
fcntlOfdSetlkw = 38
#endif

fcntlOfdGetlk :: CInt
#ifdef F_OFD_SETLKW
fcntlOfdGetlk = #const F_OFD_GETLK
#else
fcntlOfdGetlk = 36
#endif

fcntlSetlkw :: CInt
fcntlSetlkw = #const F_SETLKW

-- functions below were mostly adopted from System.Posix.IO.Common

mode2CShort :: SeekMode -> CShort
mode2CShort AbsoluteSeek = #const SEEK_SET
mode2CShort RelativeSeek = #const SEEK_CUR
mode2CShort SeekFromEnd  = #const SEEK_END

lockReq2CShort :: LockRequest -> CShort
lockReq2CShort ReadLock  = #const F_RDLCK
lockReq2CShort WriteLock = #const F_WRLCK
lockReq2CShort Unlock    = #const F_UNLCK

allocaLock :: FileLock -> (Ptr CFLock -> IO a) -> IO a
allocaLock (lockreq, mode, start, len) io =
    allocaBytes (#size struct flock) $ \p -> do
        (#poke struct flock, l_type)   p $ lockReq2CShort lockreq
        (#poke struct flock, l_whence) p $ mode2CShort mode
        (#poke struct flock, l_start)  p start
        (#poke struct flock, l_len)    p len
        io p

writeLock :: FileLock
writeLock = (WriteLock, AbsoluteSeek, 0, 0)

foreign import ccall interruptible "HsBase.h fcntl"
    safe_c_fcntl_lock :: CInt -> CInt -> Ptr CFLock -> IO CInt

-- interruptible version of waitToSetLock as defined in System.Posix.IO
safeWaitToSetLock :: Fd -> CInt -> IO ()
safeWaitToSetLock (Fd fd) cmd = allocaLock writeLock $
    throwErrnoIfMinus1_ "safeWaitToSetLock" . safe_c_fcntl_lock fd cmd

-- returns fcntlOfdSetlkw if OFD locks are available, or fcntlSetlkw otherwise
getBestLockImpl :: Fd -> IO CInt
getBestLockImpl (Fd fd) = allocaLock writeLock $ \pFlock -> do
    res <- c_fcntl_lock fd fcntlOfdGetlk pFlock
    if res == -1
        then do
            errno <- getErrno
            return $ if errno == eINVAL
                         then fcntlSetlkw
                         else fcntlOfdSetlkw
        else return fcntlOfdSetlkw

