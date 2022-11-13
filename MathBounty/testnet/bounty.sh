cardano-cli transaction build \
  --babbage-era \
  $PREPROD \
  --tx-in 26563357f4f511d38727ce6a491f564fc79825a3fe98c30e4947adf2bd44c939#0 \
  --tx-out $(cat mathBounty.addr)+500000000 \
  --tx-out-datum-hash-file datum.json \
  --change-address $Adr01 \
  --out-file tx.unsigned

cardano-cli transaction sign \
  --tx-body-file tx.unsigned \
  --signing-key-file Adr10.skey \
  $PREPROD \
  --out-file tx.signed

cardano-cli transaction submit \
  $PREPROD \
  --tx-file tx.signed