{-# LANGUAGE ForeignFunctionInterface, InterruptibleFFI #-}

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
fcntlOfdSetlkw = (#const F_OFD_SETLKW)
#else
fcntlOfdSetlkw = 38
#endif

fcntlOfdGetlk :: CInt
#ifdef F_OFD_SETLKW
fcntlOfdGetlk = (#const F_OFD_GETLK)
#else
fcntlOfdGetlk = 36
#endif

fcntlSetlkw :: CInt
fcntlSetlkw = (#const F_SETLKW)

-- functions below were mostly adopted from System.Posix.IO.Common

mode2Int :: SeekMode -> CInt
mode2Int AbsoluteSeek = (#const SEEK_SET)
mode2Int RelativeSeek = (#const SEEK_CUR)
mode2Int SeekFromEnd  = (#const SEEK_END)

lockReq2Int :: LockRequest -> CShort
lockReq2Int ReadLock  = (#const F_RDLCK)
lockReq2Int WriteLock = (#const F_WRLCK)
lockReq2Int Unlock    = (#const F_UNLCK)

allocaLock :: FileLock -> (Ptr CFLock -> IO a) -> IO a
allocaLock (lockreq, mode, start, len) io =
  allocaBytes (#const sizeof(struct flock)) $ \p -> do
    (#poke struct flock, l_type)   p (lockReq2Int lockreq :: CShort)
    (#poke struct flock, l_whence) p (fromIntegral (mode2Int mode) :: CShort)
    (#poke struct flock, l_start)  p start
    (#poke struct flock, l_len)    p len
    (#poke struct flock, l_pid)    p (0 :: CPid)
    io p

writeLock :: FileLock
writeLock = (WriteLock, AbsoluteSeek, 0, 0)

foreign import ccall interruptible "HsBase.h fcntl"
    safe_c_fcntl_lock :: CInt -> CInt -> Ptr CFLock -> IO CInt

-- interruptible version of waitToSetLock as defined in System.Posix.IO
safeWaitToSetLock :: Fd -> CInt -> IO ()
safeWaitToSetLock (Fd fd) cmd = allocaLock writeLock $ \p_flock ->
    throwErrnoIfMinus1_ "safeWaitToSetLock" $ safe_c_fcntl_lock fd cmd p_flock

-- returns fcntlOfdSetlkw if OFD locks are available, or fcntlSetlkw otherwise
getBestLockImpl :: Fd -> IO CInt
getBestLockImpl (Fd fd) = allocaLock writeLock $ \p_flock -> do
    res <- c_fcntl_lock fd fcntlOfdGetlk p_flock
    if res == -1
        then do
            errno <- getErrno
            return $ if errno == eINVAL
                         then fcntlSetlkw
                         else fcntlOfdSetlkw
        else return fcntlOfdSetlkw

