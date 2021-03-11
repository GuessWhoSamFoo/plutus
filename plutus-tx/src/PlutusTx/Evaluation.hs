{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module PlutusTx.Evaluation
    ( evaluateCek
    , unsafeEvaluateCek
    , evaluateCekTrace
    , ErrorWithCause(..)
    , EvaluationError(..)
    , CekExTally
    , CekEvaluationException
    , Plain
    )
where

import           PlutusCore.Builtins
import           PlutusCore.Evaluation.Machine.ExBudgeting
import           PlutusCore.Evaluation.Machine.ExMemory
import           PlutusCore.Name
import           PlutusCore.Universe

import           UntypedPlutusCore
import           UntypedPlutusCore.Evaluation.Machine.Cek  hiding (evaluateCek, unsafeEvaluateCek)
import qualified UntypedPlutusCore.Evaluation.Machine.Cek  as UPLC

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins.
evaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> Either (CekEvaluationException uni fun) (Term Name uni fun ())
evaluateCek (Program _ _ t) = UPLC.evaluateCekNoEmit defBuiltinsRuntime t

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins. May throw.
unsafeEvaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> EvaluationResult (Term Name uni fun ())
unsafeEvaluateCek (Program _ _ t) = UPLC.unsafeEvaluateCekNoEmit defBuiltinsRuntime t

-- TODO: pretty sure we shouldn't need the unsafePerformIOs here, we should expose a pure interface even if it has IO hacks under the hood

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins and tracing, additionally
-- returning the trace output.
evaluateCekTrace
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun ()
    -> ([String], CekExTally fun, Either (CekEvaluationException uni fun) (Term Name uni fun ()))
evaluateCekTrace (Program _ _ t) =
    case runCek defBuiltinsRuntime Counting True t of
        (errOrRes, st, logs) -> (logs, toTally st, errOrRes)