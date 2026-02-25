-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Types
-- Copyright   :  (c) Alexey Radkov 2023
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------


module NgxExport.Tools.Types (
    -- * Exported types
                              NgxExportService
                             ) where

import           Data.ByteString.Lazy (LazyByteString)

-- | Allows writing fancier declarations of services.
--
-- For example, service /signalUpconf/ in
--
-- @
-- type Upconf = [Text]
--
-- signalUpconf :: Upconf -> Bool -> IO LazyByteString
-- signalUpconf = 'NgxExport.Tools.Combinators.voidHandler'' . mapConcurrently_ getUrl
--
-- 'NgxExport.Tools.SimpleService.ngxExportSimpleServiceTyped' \'signalUpconf \'\'Upconf $
--     'NgxExport.Tools.SimpleService.PersistentService' Nothing
-- @
--
-- can be rewritten in a fancier way:
--
-- @
-- signalUpconf :: Upconf -> __/NgxExportService/__
-- signalUpconf = 'NgxExport.Tools.Combinators.voidHandler'' . mapConcurrently_ getUrl
-- @
--
-- @since 1.2.2
type NgxExportService = Bool               -- ^ First-run flag
                     -> IO LazyByteString

