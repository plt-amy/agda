{-# OPTIONS_GHC -Wunused-imports #-}

module Agda.Compiler.Backend
  ( module Agda.Syntax.Treeless
  , Backend
  , activeBackendMayEraseType
  , lookupBackend
  )
  where

import Agda.Compiler.Backend.Base

import Agda.Syntax.Abstract.Name (QName)
import Agda.Syntax.Common (BackendName)

-- Explicitly adding the Agda.Syntax.Treeless import to the .hs-boot file
-- so that the `Args` symbol can be hidden by the `SOURCE` import in
-- TypeChecking.Monad.Base.
--
-- Without exporting it here, a `hiding` clause there causes a compilation
-- error. But without hiding it there, the name conflicts with the one
-- imported from Agda.Syntax.Internal.
--
-- This is only a problem with ghci, which will load a fully-compiled module if
-- available; but that module will contain more symbols than just the few in
-- the .hs-boot
import Agda.Syntax.Treeless (TTerm, Args)

import {-# SOURCE #-} Agda.TypeChecking.Monad.Base (TCM)

type Backend = Backend_boot TCM

activeBackendMayEraseType :: QName -> TCM Bool
lookupBackend :: BackendName -> TCM (Maybe Backend)
