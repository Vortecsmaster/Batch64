{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module CustomTypedValidator where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)
import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)  

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

newtype MyWonderfullRedeemer = MWR Integer
newtype MyWonderfullDatum = MWD Integer

--PlutusTx.unstableMakeIsData ''MyWonderfullRedeemer
--PlutusTx.unstableMakeIsData ''MyWonderfullDatum

PlutusTx.makeIsDataIndexed ''MyWonderfullRedeemer [('MWR,0)]
PlutusTx.makeIsDataIndexed ''MyWonderfullDatum [('MWD,0)]

customTypedValidator :: MyWonderfullDatum -> MyWonderfullRedeemer -> ScriptContext -> Bool
customTypedValidator (MWD datum) (MWR redeemer) _ = traceIfFalse "Wrong redeemer!"  (redeemer == datum)

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = MyWonderfullDatum
    type instance RedeemerType Typed = MyWonderfullRedeemer

tValidator :: Scripts.TypedValidator Typed
tValidator = Scripts.mkTypedValidator @Typed 
          $$(PlutusTx.compile [|| customTypedValidator ||])
          $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MyWonderfullDatum @MyWonderfullRedeemer

validator :: Validator
validator = Scripts.validatorScript tValidator

scrAddress :: Address
scrAddress = scriptAddress validator    


--OFF-CHAIN RELATED

type GiftSchema =
            Endpoint "give" GiveParams  
        .\/ Endpoint "grab" Integer

data GiveParams = GP {
                       gpAmount :: Integer
                     , gpDatum :: Integer 
                     } deriving (Generic, FromJSON, ToJSON, ToSchema)

give :: AsContractError e => GiveParams -> Contract w s e ()
give gparams = do
     let tx = mustPayToTheScript (MWD $ gpDatum gparams)  $ Ada.lovelaceValueOf $ gpAmount gparams 
     ledgerTx <- submitTxConstraints tValidator tx
     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                  
     logInfo @String $ printf "Send some value %d lovelace to the contract." $ gpAmount gparams    
    
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                                     
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (MWR n) | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "collected gifts"                                                              -- Log information 

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab                                                                    -- block until grab
                                                              

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that
mkKnownCurrencies [] 
