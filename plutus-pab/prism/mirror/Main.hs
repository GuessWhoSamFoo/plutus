{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Main where

import           Data.Bifunctor                               (first)
import           Data.Text.Extras                             (tshow)
import           Plutus.PAB.ContractCLI                       (commandLineApp)
import           PlutusTx.Coordination.Contracts.Prism.Mirror (MirrorSchema, mirror)

main :: IO ()
main =
    commandLineApp
        $ first tshow
        $ mirror @MirrorSchema @()
