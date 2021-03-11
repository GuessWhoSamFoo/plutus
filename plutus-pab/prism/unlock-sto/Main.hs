{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Main where

import           Data.Bifunctor                                          (first)
import           Data.Proxy                                              (Proxy (..))
import           Data.Text.Extras                                        (tshow)
import           Plutus.Contract                                         (BlockchainActions, type (.\/))
import           Plutus.Contract.Effects.RPC                             (RPCClient)
import           Plutus.PAB.ContractCLI                                  (commandLineApp')
import           PlutusTx.Coordination.Contracts.Prism.CredentialManager (CredentialManager)
import           PlutusTx.Coordination.Contracts.Prism.Unlock            as Prism

main :: IO ()
main =
    commandLineApp'
        (Proxy @(BlockchainActions .\/ RPCClient CredentialManager))
        $ first tshow
        $ Prism.subscribeSTO @() @Prism.STOSubscriberSchema
