module Main
    ( main
    ) where

import           Plutus.PAB.ContractCLI               (commandLineApp)
import           PlutusTx.Coordination.Contracts.Game (game)

main :: IO ()
main = commandLineApp game
