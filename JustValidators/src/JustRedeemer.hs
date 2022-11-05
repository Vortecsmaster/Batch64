{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names
{-# LANGUAGE OverloadedStrings   #-}

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
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Text.Printf         (printf)
--Haskell related
import           Prelude             (IO, Semigroup (..), String)
import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)

--ON-CHAIN RELATED CODE

{-# INLINABLE actualValidator #-}
actualValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
actualValidator datum redeemer _ 
                | redeemer == datum  = ()
                | otherwise          = traceError "Wrong redeemer!"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| actualValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  -- The hash of the validators

scrAddress :: Address
scrAddress = scriptAddress validator 


--THE OFFCHAIN RELATED CODE

type GiftSchema =
            Endpoint "give" Integer  --An Integer Parameter
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 19) $ Ada.lovelaceValueOf amount      --This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so is created and the ammount of lovelaces
    ledgerTx <- submitTx tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    logInfo @String $ printf "made a gift of %d lovelace" amount                                     --This line log info,usable on the PP(Plutus Playground)
--CLI VERSION
--alonzo-era \
-- $TESTNET \
  --tx-in 4a5d4091600a8ea3eac1c3cde32d99c4514541888b071007a69ccdcf6dfab639#1 \
  --tx-out $(cat validator.addr)+5100000 \
  --tx-out-datum-hash-file 19datum.json \
  --change-address $nami \
  --out-file tx.unsigned

--cardano-cli transaction sign  \
  --tx-body-file tx.unsigned  \
  --signing-key-file Adr07.skey  \
  -- $TESTNET  \
  --out-file tx.signed

-- cardano-cli transaction submit  \
  -- $TESTNET \
  --tx-file tx.signed



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

{-
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 1097911063 \
  --tx-in fa3af564fff04bd5b77a9a39f0e6055acf7eeb7358516754855b30fcbb4524d5#1 \
  --tx-in-script-file ./vesting.plutus \
  --tx-in-datum-file ./unit.json  \
  --tx-in-redeemer-file ./unit.json \
  --required-signer-hash 1dcdf420c1488ba345730d41f72a846428ba814c2bd639462eaf5a07 \
  --tx-in-collateral 67557dbe63d46e252276c729ebf75afe615c9903b644a37c11f6f0ac22fa8aff#0 \
  --change-address $nami \
  --invalid-before 61967066 \
  --protocol-params-file ./protocol.params \
  --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file payment2.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
-}


endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab                                                            -- block until grab

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that
mkKnownCurrencies []  