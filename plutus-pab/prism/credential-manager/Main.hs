{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Main where

import           Data.Bifunctor                                          (first)
import           Data.Proxy                                              (Proxy (..))
import           Data.Text.Extras                                        (tshow)
import           Language.Plutus.Contract                                (BlockchainActions, type (.\/))
import           Language.Plutus.Contract.Effects.RPC                    (RPCServer)
import           Plutus.PAB.ContractCLI                                  (commandLineApp')
import           PlutusTx.Coordination.Contracts.Prism.CredentialManager (CredentialManager, CredentialManagerSchema,
                                                                          credentialManager)

main :: IO ()
main =
    commandLineApp'
        (Proxy @(BlockchainActions .\/ RPCServer CredentialManager))
        $ first tshow
        $ credentialManager @() @CredentialManagerSchema
