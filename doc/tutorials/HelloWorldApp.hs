{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module HelloWorldApp where

import qualified Data.Text                as T
import           Language.Plutus.Contract hiding (when)
import           Playground.Contract
import           PlutusTx.Prelude


-- BLOCK1
-- | A 'Contract' that logs a message.
hello :: Contract () BlockchainActions T.Text ()
hello = logInfo @String "Hello, world"
-- BLOCK2

endpoints :: Contract () BlockchainActions T.Text ()
endpoints = hello

mkSchemaDefinitions ''BlockchainActions

$(mkKnownCurrencies [])
