{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names

module JustRedeemer where

--On Chain PlutusCore related
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
--Ledger Types, Fn and Typeclaes related
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts               -- Low Level Typed Validator 
import           Ledger.Ada          as Ada
--Plutus Off-Chain related - Contract Monad and Playground
import           Plutus.Contract
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Text.Printf         (printf)
--Haskell related
import           Prelude             (IO, Semigroup (..), String)
import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)          

--ON-CHAIN RELATED CODE

{-# INLINABLE actualValidator #-}
actualValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
actualValidator datum redeemer _ 
                | redeemer == datum  = ()
                | otherwise          = traceError "Wrong redeemer!"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| actualValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  

scrAddress :: Address
scrAddress = scriptAddress validator 


--THE OFFCHAIN RELATED CODE
data GiveParams = GP { gpAmount :: Integer, gpDatum :: Integer } deriving (Generic, FromJSON, ToJSON, ToSchema)

type GiftSchema =
            Endpoint "give" GiveParams  
        .\/ Endpoint "grab" Integer

give :: AsContractError e => GiveParams -> Contract w s e ()
give gparams = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI $ gpDatum gparams) $ Ada.lovelaceValueOf $ gpAmount gparams      --This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so is created and the ammount of lovelaces
    ledgerTx <- submitTx tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    logInfo @String $ printf "made a gift of %d lovelace" $ gpAmount gparams                                     --This line log info,usable on the PP(Plutus Playground)


grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                                     
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "collected gifts"                                                              -- Log information 

-- Alternative route solution (line 78)
-- (mustSpendScriptOutput oref1 $ Redeemer $ Builtins.mkI 19) <> (mustSpendScriptOutput oref2 $ Redeemer $ Builtins.mkI 7)

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab                                                            -- block until grab

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that
mkKnownCurrencies []  