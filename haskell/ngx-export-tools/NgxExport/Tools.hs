-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools
-- Copyright   :  (c) Alexey Radkov 2018-2023
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires POSIX and Template Haskell)
--
-- Extra tools for using in custom Haskell code with
-- <https://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools (
    -- *** Combinators of effectful actions (including split services)
                        module NgxExport.Tools.Combinators
    -- *** Reading custom types from /ByteStrings/
                       ,module NgxExport.Tools.Read
    -- *** Exporters of simple services
                       ,module NgxExport.Tools.SimpleService
    -- *** Split services
                       ,module NgxExport.Tools.SplitService
    -- *** Various functions to access low-level Nginx API
                       ,module NgxExport.Tools.System
    -- *** A simple implementation of time intervals
                       ,module NgxExport.Tools.TimeInterval
    -- *** Various type declarations
                       ,module NgxExport.Tools.Types
                       ) where

import           NgxExport.Tools.Combinators
import           NgxExport.Tools.Read
import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.SplitService
import           NgxExport.Tools.System
import           NgxExport.Tools.TimeInterval
import           NgxExport.Tools.Types

