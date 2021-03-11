module Main
    ( main
    ) where

import           Control.Monad                            (void)
import           Data.Bifunctor                           (first)
import qualified Data.Text                                as T
import           Plutus.PAB.ContractCLI                   (commandLineApp)
import           PlutusTx.Coordination.Contracts.Currency (forgeCurrency)

main :: IO ()
main = commandLineApp $ first (T.pack . show) $ void forgeCurrency
