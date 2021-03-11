{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Lib where

import           Common
import           PlcTestUtils

import           Language.Haskell.TH

import qualified Language.PlutusTx.Builtins   as Builtins
import           Language.PlutusTx.Code
import           Language.PlutusTx.Evaluation
import           Language.PlutusTx.TH

import qualified PlutusCore                   as PLC
import           PlutusCore.Pretty            (PrettyConst)
import qualified PlutusCore.Universe          as PLC
import qualified UntypedPlutusCore            as UPLC
import qualified UntypedPlutusCore.DeBruijn   as UPLC

import           Data.Text.Prettyprint.Doc
import           Flat                         (Flat)

instance (PLC.Closed uni, uni `PLC.Everywhere` Flat, Flat fun) =>
            ToUPlc (CompiledCodeIn uni fun a) uni fun where
    toUPlc v = do
        v' <- catchAll $ getPlc v
        toUPlc v'

goldenPir
    :: (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PrettyConst, uni `PLC.Everywhere` Flat, Pretty fun, Flat fun)
    => String -> CompiledCodeIn uni fun a -> TestNested
goldenPir name value = nestedGoldenVsDoc name $ pretty $ getPir value
