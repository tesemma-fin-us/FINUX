(define-namespace "free" (read-keyset "finux-module-admin") (read-keyset "finux-operations-admin"))
(namespace "free")

(define-keyset "free.finux-module-admin" (read-keyset "finux-module-admin"))
(define-keyset "free.finux-operations-admin" (read-keyset "finux-operations-admin"))

(module finux GOVERNANCE
    @doc "finux is a utility token dedicated to supporting honest citizen journalism \
    \ and a free and informative prediction market, all on finulab: the front-page of finance"
    @model [
        (defproperty conserves-mass (amount:decimal)
            (= (column-delta token-table 'balance) 0.0)
        )
        (defproperty valid-account-id (account-id:string)
            (and
                (>= (length account-id) 3)
                (<= (length account-id) 256)
            )
        )
    ]

    ;; -----------------------------------------------------------------------------------------------------
    ;;  schemas
    (defschema token-schema
        @doc "schema designated to account and maintain token distribution ledger for all accounts"
        @model [
          (invariant (>= balance 0.0))
        ]
        guard:guard
        balance:decimal
    )
    (defschema token-initialization-schema
        @doc "schema designated to ensuring that this contract is only initialized once--see initialize-contract function below"
        initialized:bool
    )

    (defschema crosschain-schema
        @doc "schema designated to facilitate cross-chain transfers; see transfer-crosschain function below"
        @model [
            (invariant (>= amount 0.0))
            (invariant (!= "" receiver))
        ]
        receiver:string
        receiver-guard:guard
        amount:decimal
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  tables
    (deftable token-table:{token-schema})
    (deftable token-initialization-table:{token-initialization-schema})

    ;; -----------------------------------------------------------------------------------------------------
    ;;  constants
    (defconst finulab-bank:string "finulab-bank"
        @doc "treasury account established to initially hold all the tokens \
        \ allocated to the users of finulab: 66.67% (i.e., 100M / 150M) of the supply. \
        \ for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-marketing-bank:string "finulab-marketing-bank"
        @doc "treasury account established to initially hold all the tokens \
        \ allocated to marketing finulab to the public: 5% (i.e., 7.5M / 150M) of the supply. \
        \ for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-competition-bank:string "finulab-competition-bank"
        @doc "treasury account established to initially hold all the tokens \
        \ allocated to finulab supported competitions for users: 5% (i.e., 7.5M / 150M) of the supply. \
        \ for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-account-creation-bonus-bank:string "finulab-account-creation-bonus-bank"
        @doc "treasury account established to initially hold all the tokens \
        \ allocated to the first users to create a finulab account: 16.67% (i.e., 25M / 150M) of the supply. \
        \ for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-prediction-market-liquidity-pool:string "finulab-prediction-market-liquidity-pool"
        @doc "pool account established to support \
        \ finulab's liquidity-sensitive LMSR based automated market maker \
        \ for prediction markets: 6.63% (i.e., 10M / 150M) of the supply. \
        \ for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-prediction-market-escrow-account:string "finulab-prediction-market-escrow-account"
        @doc "account established to manage all the balances put into finulab's prediction market"
    )
    (defconst finulab-prediction-market-collateral-account:string "finulab-prediction-market-collateral-account"
        @doc "account established to manage all the collateral posted to conduct sales in finulab's prediction market"
    )
    (defconst finulab-prediction-market-fees:string "finulab-prediction-market-fees"
        @doc "account established to manage a portion of the fees related to purchases on finulab's prediction market"
    )
    (defconst finulab-monetization-vault:string "finulab-monetization-vault"
        @doc "account established to hold and retain locked finux, so that monetized account on finulab can earn on every engagement"
    )
    (defconst finulab-subscriptions:string "finulab-subscriptions"
        @doc "account established to receive subscription fees paid on finulab for privileged accounts"
    )
    (defconst pi:decimal 3.1415926535
        @doc "pi to the 10th decimal; utilized in the delta-function scoped into the initialization of this contract. see initialize-contract function below"
    )
    (defconst token-supply:decimal 150000000.0
        @doc "initial and max supply of the finux token; for more on our tokenomics, visit: finux.xyz"
    )
    
    ;; -----------------------------------------------------------------------------------------------------
    ;;  interfaces
    (implements fungible-v2)
    (implements fungible-xchain-v1)

    ;; -----------------------------------------------------------------------------------------------------
    ;;  capabilities
    (defcap GOVERNANCE ()
        (enforce-keyset "free.finux-module-admin")
    )
    (defcap DEBIT (sender:string)
        @doc "capability to ensure that the sender is appropriately validated"
        (enforce (!= "" sender) "sender must be specified in order to perform a debit")
        (with-read token-table sender {"guard":=sender-guard}
            (if (contains sender [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
                (and
                    (enforce-guard sender-guard)
                    (enforce-keyset "free.finux-operations-admin")
                )
                (enforce-guard sender-guard)
            )
        )
    )
    (defcap CREDIT (receiver:string)
        @doc "capability to ensure that the receiver is appropriately validated"
        (enforce (!= "" receiver) "receiver must be specified in order to perform a credit")
    )
    (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
        @doc "capability to ensure that the transfer operation is appropriately validated"
        @managed amount TRANSFER-mgr
        
        (enforce (> amount 0.0) "transfer amount must be greater than 0.0")
        (enforce-unit amount)
        (enforce (!= sender receiver) "sender cannot match the receiver in a transfer operation")
        (compose-capability (DEBIT sender))
        (compose-capability (CREDIT receiver))
    )
    (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
        @doc "managed capability to control the transfer amount"
        (let ((new-balance (- managed requested)))
            (enforce (>= new-balance 0.0)
                (format "requested transfer amount has exceeded {}, i.e., amount approved for the transfer" [managed])
            )
            new-balance
        )
    )

    (defcap TRANSFER_XCHAIN:bool (sender:string receiver:string amount:decimal target-chain:string)
        @managed amount TRANSFER_XCHAIN-mgr
        (enforce-unit amount)
        (enforce (> amount 0.0) "Cross-chain transfers require a positive amount")
        (compose-capability (DEBIT sender))
    )
    (defun TRANSFER_XCHAIN-mgr:decimal (managed:decimal requested:decimal)
        (enforce (>= managed requested)
          (format "TRANSFER_XCHAIN exceeded for balance {}" [managed])
        )
        0.0
    )
    (defcap TRANSFER_XCHAIN_RECD:bool (sender:string receiver:string amount:decimal source-chain:string)
        @event true
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  functionality
    (defun validate-account-id (account-id:string)
        @doc "enforce an account name to meet the required format"
        (enforce (!= "" account-id) "account name must be specified, i.e., account name cannot be an empty string")
        (enforce (is-charset CHARSET_LATIN1 account-id) "account name must conform to the character set allowed under ISO-8859-1")
        (let ((account-id-length (length account-id)))
            (and
                (enforce (>= account-id-length 3) "account name must be at least 3 characters long")
                (enforce (<= account-id-length 256) "account name cannot be over 256 characters long")
            )
        )
    )
    (defun validate-account-for-credit-root (account-id:string account-guard:guard)
        @doc "ensure that a root account's guard is never modified in a credit operation"
        (enforce 
            (contains account-id [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            "account name is invalid for the utilization of this function"
        )
        (if (= account-id finulab-bank)
            (enforce
                (=
                    (format "{}" [account-guard])
                    (format "KeySet {keys: [{}],pred: keys-all}" ["8a6d0f7488699d061fa8f6eb87cca6de9994d867435269987aa68bef88e038c9"])
                )
                "in order to credit a root account, the appropriate guard must be specified"
            )
            (if (= account-id finulab-marketing-bank)
                (enforce
                    (=
                        (format "{}" [account-guard])
                        (format "KeySet {keys: [{}],pred: keys-all}" ["c34808f8a13425eb5e21c0b4a14eadd89dae5558fd2a22affec24ccce7444586"])
                    )
                    "in order to credit a root account, the appropriate guard must be specified"
                )
                (if (= account-id finulab-competition-bank)
                    (enforce
                        (=
                            (format "{}" [account-guard])
                            (format "KeySet {keys: [{}],pred: keys-all}" ["4463abf6b912a677f941a46bad70c988dfc5801452ebb50e29cbc31bb61ac28c"])
                        )
                        "in order to credit a root account, the appropriate guard must be specified"
                    )
                    (if (= account-id finulab-account-creation-bonus-bank)
                        (enforce
                            (=
                                (format "{}" [account-guard])
                                (format "KeySet {keys: [{}],pred: keys-all}" ["e871fa64713c80d2a746942a4bd76c8ace913c3b156e5eea92e8bd50c6b21a54"])
                            )
                            "in order to credit a root account, the appropriate guard must be specified"
                        )
                        (if (= account-id finulab-prediction-market-liquidity-pool)
                            (enforce
                                (=
                                    (format "{}" [account-guard])
                                    (format "KeySet {keys: [{}],pred: keys-all}" ["37a6038bd13db5a53d96e0db0fc180dd93753e9e60f08761bbc97702399e0c95"])
                                )
                                "in order to credit a root account, the appropriate guard must be specified"
                            )
                            (if (= account-id finulab-prediction-market-escrow-account)
                                (enforce
                                    (=
                                        (format "{}" [account-guard])
                                        (format "KeySet {keys: [{}],pred: keys-all}" ["714a2a357b1b417fba468e05ee16fcfe3c2d6200ecb33673e5793b097130f711"])
                                    )
                                    "in order to credit a root account, the appropriate guard must be specified"
                                )
                                (if (= account-id finulab-prediction-market-collateral-account)
                                    (enforce
                                        (=
                                            (format "{}" [account-guard])
                                            (format "KeySet {keys: [{}],pred: keys-all}" ["44818c010cb7075bedd8e32adc61c3c7afe6f7e1da3df6aae07db005b787f159"])
                                        )
                                        "in order to credit a root account, the appropriate guard must be specified"
                                    )
                                    (if (= account-id finulab-prediction-market-fees)
                                        (enforce
                                            (=
                                                (format "{}" [account-guard])
                                                (format "KeySet {keys: [{}],pred: keys-all}" ["33693ab467270fcb86ec63e6313e04a6e403deac0244c45986491b71ae01a702"])
                                            )
                                            "in order to credit a root account, the appropriate guard must be specified"
                                        )
                                        (if (= account-id finulab-monetization-vault)
                                            (enforce
                                                (=
                                                    (format "{}" [account-guard])
                                                    (format "KeySet {keys: [{}],pred: keys-all}" ["d3eab7eb65171d827dc9a3667ee0ac86bba107d7a5331d79b180039541180fb8"])
                                                )
                                                "in order to credit a root account, the appropriate guard must be specified"
                                            )
                                            (enforce
                                                (=
                                                    (format "{}" [account-guard])
                                                    (format "KeySet {keys: [{}],pred: keys-all}" ["b43f3ce060c7b6b25225e01c281b24acb6772a6233b6c2486dd663f8fd5b7de5"])
                                                )
                                                "in order to credit a root account, the appropriate guard must be specified"
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ) 
        )
    )
    (defun validate-account-for-credit-non-root (account-id:string account-guard:guard)
        @doc "ensure that a non-root account's guard is never modified in a credit operation"
        (if (= (take 2 account-id) "k:")
            (let ((afx (drop 2 account-id)))
                (let ((publicKey (drop 15 (drop (- 17) (format "{}" [account-guard])))))
                    (enforce (not (contains "," publicKey)) 
                        "finux token contract does not support multiple keys, an account can only have one key"
                    )
                    (enforce (= afx publicKey)
                        "an account's name must start off with a 'k:', which must then be followed by the account's public key"
                    )
                )
            )
            (enforce (= 1 1) "matrix is broken")
        )
    )
    (defun enforce-unit:bool (amount:decimal)
        @doc "enforce minimum precision allowed for transactions"
        (enforce (= (floor amount 12) amount)
            "transfer amount's fractional part cannot exceed the maximum allowed decimal precision of 12"
        )
    )

    (defun create-account:string (account:string guard:guard)
        @doc "utility to create a new account"
        @model [
            (property (valid-account-id account))
        ]
        
        (validate-account-id account)
        (if (contains account [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            (validate-account-for-credit-root account guard)
            (validate-account-for-credit-non-root account guard)
        )
        (insert token-table account {"guard":guard, "balance":0.0})
    )
    (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
        @doc "utility to transfer an amount to another account, creating the account if it does not exist"
        @model [
            (property (> amount 0.0))
            (property (!= "" sender))
            (property (!= "" receiver))
            (property (!= sender receiver))
            
            (property (conserves-mass amount))
            (property (valid-account-id sender))
            (property (valid-account-id receiver))
        ]
        
        ;; Validation of amount, sender and receiver's account-ids.
        ;; Further, ensure that the guard for the receiver is appropriate via the validate-account-for-x functions.
        (enforce (> amount 0.0) "transfer amount must be greater than 0.0")
        (enforce-unit amount)
        (validate-account-id sender)
        (validate-account-id receiver)
        (enforce (!= sender receiver) "sender and receiver cannot match in a transfer operation")
        (if (contains receiver [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            (validate-account-for-credit-root receiver receiver-guard)
            (validate-account-for-credit-non-root receiver receiver-guard)
        )
        
        ;; Transfer FINUX on same chain,
        ;; utilizing with-default-read so that the account is created if it doesn't exist
        (with-capability (TRANSFER sender receiver amount)
            (require-capability (DEBIT sender))
            (with-read token-table sender {"guard":=sender-guard, "balance":=balance}
                (enforce-guard sender-guard)
                (enforce (>= balance amount) "insufficient balance to complete the transfer")
                (update token-table sender {"balance":(- balance amount)})
            )
            
            (require-capability (CREDIT receiver))
            (with-default-read token-table receiver 
                {"guard":receiver-guard, "balance":-1.0}
                {"guard":=retg, "balance":=balance}
                (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                
                (let ((is-new (if (= -1.0 balance) true false)))
                    (write token-table receiver
                        {"guard":retg, "balance":(if is-new amount (+ balance amount))}
                    )
                )
            )
        )
    )
    (defun transfer:string (sender:string receiver:string amount:decimal)
        @doc "utility to transfer an amount to another account, failing if the account does not exist"
        @model [
            (property (> amount 0.0))
            (property (!= "" sender))
            (property (!= "" receiver))
            (property (!= sender receiver))
            
            (property (conserves-mass amount))
            (property (valid-account-id sender))
            (property (valid-account-id receiver))
        ]
        
        ;; Transfer FINUX on same chain
        ;; works only if sender and receiver exist, otherwise it will fail
        (with-read token-table receiver {"guard":=receiver-guard}
            (transfer-create sender receiver receiver-guard amount)
        )
    )
    (defpact transfer-crosschain:string (sender:string receiver:string receiver-guard:guard target-chain:string amount:decimal)
        @doc "utility to transfer an amount to another account cross-chain"
        @model [
            (property (> amount 0.0))
            (property (!= "" sender))
            (property (!= "" receiver))
            
            (property (valid-account-id sender))
            (property (valid-account-id receiver))
        ]
        
        ;; Transfer FINUX across different chains, 
        ;; step 1, debit sender on current chain, validating amount, sender, receiver, and their guards
        (step 
            (with-capability (DEBIT sender)
                (enforce (> amount 0.0) "transfer amount must be greater than 0.0")
                (enforce-unit amount)
                (validate-account-id sender)
                (validate-account-id receiver)
                (enforce 
                    (contains target-chain ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"])
                    "specified target-chain is invalid, please select a valid chain id (i.e. 0 to 19)"
                )
                (enforce (!= (at 'chain-id (chain-data)) target-chain) "cannot run cross-chain transfers to the same chain")
                (if (contains receiver [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
                    (validate-account-for-credit-root receiver receiver-guard)
                    (validate-account-for-credit-non-root receiver receiver-guard)
                )
                
                ;; step 1: debit sender account on the current chain
                (with-read token-table sender {"guard":=sender-guard, "balance":=balance}
                    (enforce-guard sender-guard)
                    (enforce (>= balance amount) "insufficient balance to complete the transfer")
                    (update token-table sender {"balance":(- balance amount)})
                )
                
                (let 
                    ((crosschain-details:object{crosschain-schema}
                        {
                            "receiver":receiver,
                            "receiver-guard":receiver-guard,
                            "amount":amount
                        }
                    ))
                    (yield crosschain-details target-chain)
                )
            )
        )
        
        ;; step 2, credit receiver on target chain
        ;; utilizing with-default-read so that the account is created if it doesn't exist
        (step
            (resume
                {
                    "receiver":=receiver,
                    "receiver-guard":=receiver-guard,
                    "amount":=amount
                }
                
                ;; step 2: credit receiver account on target-chain
                (with-capability (CREDIT receiver)
                    (with-default-read token-table receiver
                        {"guard":receiver-guard, "balance":-1.0}
                        {"guard":=retg, "balance":=balance}
                        (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                        
                        (let ((is-new (if (= -1.0 balance) true false)))
                            (write token-table receiver
                                {"guard":retg, "balance":(if is-new amount (+ balance amount))}
                            )
                        )
                    )
                )
            )
        )
    )
    
    (defun precision:integer ()
        @doc "returns the maximum allowed decimal precision"
        12
    )
    (defun get-balance:decimal (account:string)
        @doc "get balance for an account, failing if the account does not exist"
        (with-read token-table account {"balance":=balance}
            balance
        )
    )
    (defun details:object{fungible-v2.account-details} (account:string)
        @doc "utility to pull an account's general detail"
        (with-read token-table account {"guard":=account-guard, "balance":=balance}
            {
                "account":account,
                "balance":balance,
                "guard":account-guard
            }
        )
    )
    (defun rotate:string (account:string new-guard:guard)
        @doc "utility to rotate an entire account based on the new provided guard; \
        \ note that this function will transition entire balance to the newly created account \
        \ after enforcing the old guard. the new account's format will be of the form: current account's protocol ('k:') followed by the new public key. \
        \ note that the finux token contract does not support multiple keys, an account can only have one key"
        (enforce
            (not (contains account [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions]))
            (format "{} is not authorized to call this function" [account])
        )
        (with-read token-table account {"guard":=old-guard, "balance":=balance}
            (enforce-guard old-guard)
            (enforce-guard new-guard)
            (let ((account-protocol (take 1 account)))
                (let ((publicKey (drop 15 (drop (- 17) (format "{}" [new-guard])))))
                    (let ((new-account (format "{}:{}" [account-protocol publicKey])))
                        (create-account new-account new-guard)
                        (update token-table new-account {"balance":balance})
                        (update token-table account {"balance":0.0})
                    )
                )
            )
        )
    )
    (defun rotate-root:string (account:string new-guard:guard)
        @doc "utility to rotate the keyset of a root account"
        (enforce-keyset "free.finux-operations-admin")
        (enforce 
            (contains account [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            "account name is invalid for the utilization of this function"
        )
        (with-read token-table account {"guard":=old-guard, "balance":=balance}
            (enforce-guard old-guard)
            (enforce-guard new-guard)
            (update token-table account {"guard":new-guard, "balance":balance})
        )
    )

    (defun initialize-contract:string ()
        @doc "function to initialize token contract \
        \ balance is distributed utilizing the delta function \
        \ detal function equal 1 for chain 3 and 0 for all other chains, so that the full balance is only distributed on chain 3"
        (with-default-read token-initialization-table "contract" {"initialized":false} {"initialized":=initialized-val}
            (enforce (!= initialized-val true) "initialize-contract function has expired")
            
            (with-capability (GOVERNANCE)
                (create-account finulab-bank (read-keyset "finulab-bank"))
                
                (let ((chain-val (str-to-int (at 'chain-id (chain-data)))))
                    (let ((delta-function-val (floor (exp (- (^ (* (sqrt pi) (- chain-val 3)) 2))))))
                        (update token-table finulab-bank 
                            {"balance":(* delta-function-val token-supply)}
                        )
                    )
                )
                (write token-initialization-table "contract" {"initialized":true})
            )
        )
    )
)

(create-table token-table)
(create-table token-initialization-table)
