{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Batch64SignedMinting where

-- On Chain 
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
-- Ledger Types
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
-- Off-chain Contract Monad
import           Plutus.Contract        as Contract
-- Simulation - Playground
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
-- Simulation - Trace
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
-- Haskell
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)

--ON-CHAIN

{-# INLINABLE signedMintingPolicy #-}
signedMintingPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
signedMintingPolicy pkh () sContext = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkh

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
             $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . signedMintingPolicy ||])
             `PlutusTx.applyCode`
             PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

--OFF-CHAIN

