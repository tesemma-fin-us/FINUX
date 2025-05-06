(define-namespace "free" (read-keyset "finux-module-admin") (read-keyset "finux-operations-admin"))
(namespace "free")

(define-keyset "free.finux-module-admin" (read-keyset "finux-module-admin"))
(define-keyset "free.finux-operations-admin" (read-keyset "finux-operations-admin"))

(module finux-y-n-prediction_market GOVERNANCE
    @doc "Yes or No based prediction market that runs off FINUX. \
    \ This module supports the creation of market arounds events like: \
    \ 'Will it rain tomorrow?' or 'F1 Drivers Champion, Max Verstappen?'. \
    \ Events that have one of two resolutions: yes or no."

    ;; -----------------------------------------------------------------------------------------------------
    ;;  schemas
    (defschema y-n-market-schema
        @doc "schema designated to account and maintain finulab's yes-or-no based prediction market"
        @model [
            (invariant (>= quantityYes 0.0))
            (invariant (>= quantityNo 0.0))
        ]
        predictiveQuestion:string
        outcome:string
        quantityYes:decimal
        quantityNo:decimal
        continuous:bool
        endDate:integer
        status:string
        resolved:bool
        resolutionOutcome:string
    )
    (defschema portfolio-schema
        @doc "schema designated to account and maintain each finux account's portfolio by market for yes-or-no based predictions"
        @model [
          (invariant (>= yesQuantity 0.0))
          (invariant (>= noQuantity 0.0))
        ]
        yesQuantity:decimal
        noQuantity:decimal
    )
    (defschema collateralized-portfolio-schema
        @doc "schema designated to account and maintain each finux account's sale portfolio by market for yes-or-no based predictions"
        @model [
          (invariant (>= soldYesQuantity 0.0))
          (invariant (>= soldNoQuantity 0.0))
        ]
        soldYesQuantity:decimal
        soldYesCollateral:decimal
        soldNoQuantity:decimal
        soldNoCollateral:decimal
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  tables
    (deftable y-n-market-table:{y-n-market-schema})
    (deftable portfolio-table:{portfolio-schema})
    (deftable collateralized-portfolio-table:{collateralized-portfolio-schema})

    ;; -----------------------------------------------------------------------------------------------------
    ;;  capabilities
    (defcap GOVERNANCE ()
        (enforce-keyset "free.finux-module-admin")
    )
    
    ;; -----------------------------------------------------------------------------------------------------
    ;;  Utilize Modules
    (use free.finux)

    ;; -----------------------------------------------------------------------------------------------------
    ;;  functionality
    (defun market-tx-validation (tx-desc:object)
        @doc "Validation to ensure all market related transactions are correctly structured. \
        \ In all transactions, quantity and cost must be greater than 0. "

        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)

        (enforce (> (at "cost" tx-desc) 0.0) "tx cost must be a decimal greater than 0.0")
        (enforce-unit (at "cost" tx-desc))

        (enforce (> (at "quantity" tx-desc) 0.0) "quantity must be a decimal greater than 0.0")
        (enforce-unit (at "quantity" tx-desc))
    )

    (defun y-n-create-market (account:string market-id:string predictive-question:string outcome:string continuous:bool end-date:integer market-desc:object)
        @doc "Functionality to create a yes or no market related to a prediction. \
        \ Note that any account can create the prediction; however, operations-admin \
        \ must sign-off to ensure that there is no conflicts, and the prediction is resolvable (i.e., \
        \ having a definitive outcome of yes or no)."

        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)

        ;; In market creation, quantities of Yes and No shares created must be >= 0,
        ;; and just as importantly, liquidity locked must be greater than 0.
        (enforce (> (at "cost" market-desc) 0.0) "tx cost must be greater than 0.0")
        (enforce-unit (at "cost" market-desc))

        (enforce-unit (at "quantity-no" market-desc))
        (enforce-unit (at "quantity-yes" market-desc))
        (enforce (>= (at "quantity-no" market-desc) 0.0) "quantity-no must be a decimal greater than or equal to 0.0")
        (enforce (>= (at "quantity-yes" market-desc) 0.0) "quantity-yes must be a decimal greater than or equal to 0.0")
        (enforce (> (+ (at "quantity-yes" market-desc) (at "quantity-no" market-desc)) 0.0) "sum of quantity-yes and quantity-no must be greater than 0.0")

        ;; On creation, the locked liquidity is transfered to the root escrow account,
        ;; where the funds will be held, either until the user sells or the market resolves.
        ;; Furter, in addition to the market's creation per the unique market-id, user's 
        ;; balance is added to portfolio-table based on a unique portfolio-id.
        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" market-desc))
        )
        (insert y-n-market-table market-id {"predictiveQuestion":predictive-question, "outcome":outcome, "quantityYes":(at "quantity-yes" market-desc), "quantityNo":(at "quantity-no" market-desc), "continuous":continuous, "endDate":end-date, "status":"live", "resolved":false, "resolutionOutcome":""})
        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (insert portfolio-table portfolio-id {"yesQuantity":(at "quantity-yes" market-desc), "noQuantity":(at "quantity-no" market-desc)})
        )
    )
    
    (defun y-n-portfolio-credit (account:string market-id:string type:string tx-desc:object supportive-desc:object)
        @doc "functionality to purchase yes or no outcomes"
        (market-tx-validation tx-desc)

        ;; On all purchases, there is a fee. That fee is broken out into 2:
        ;; one paid to the creator of the prediction and the other to Finulab.
        ;; After validating the fee is greater than 0.0, we transfer the fee amount to the creator of the market and Finulab's fees account.
        (enforce (>= (at "fee" supportive-desc) 0.0) "tx fee must be greater than or equal to 0.0")
        (enforce-unit (at "fee" supportive-desc))
        (if (> (at "fee" supportive-desc) 0.0)
            (with-read token-table finulab-prediction-market-fees {"guard":=fees-guard}
                (transfer-create account finulab-prediction-market-fees fees-guard (at "fee" supportive-desc))
            )
            (enforce (= 1 1) "matrix is broken")
        )

        (enforce (>= (at "creator-fee" supportive-desc) 0.0) "creator's portion of fee must be greater than or equal to 0.0")
        (enforce-unit (at "creator-fee" supportive-desc))
        (if (> (at "creator-fee" supportive-desc) 0.0)
            (with-read token-table (at "creator-address" supportive-desc) {"guard":=creator-guard}
                (transfer-create account (at "creator-address" supportive-desc) creator-guard (at "creator-fee" supportive-desc))
            )
            (enforce (= 1 1) "matrix is broken")
        )

        ;; After the payment of fees, we send the locked FINUX amount to the escrow account.
        ;; Then, in the y-n-market-table we adjust the shares distribution, and then move on to
        ;; credit the user's account with the shares purchased in the portfolio-table. Note that we use with-default-read
        ;; so that this operation will not fail if the market-id or portfolio-id doesn't exist.
        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" tx-desc))
        )

        (if (= type "yes")
            (with-default-read y-n-market-table market-id
                {"predictiveQuestion":"", "outcome":"", "quantityYes":-1.0, "quantityNo":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                {"predictiveQuestion":=p-q, "outcome":=o, "quantityYes":=q-y, "quantityNo":=q-n, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                (let ((is-new (if (= -1.0 q-y) true false)))
                    (if is-new
                        (write y-n-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcome":(at "outcome" supportive-desc), "quantityYes":(at "quantity" tx-desc), "quantityNo":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                        (write y-n-market-table market-id {"predictiveQuestion":p-q, "outcome":o, "quantityYes":(+ q-y (at "quantity" tx-desc)), "quantityNo":q-n, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                    )
                )
            )
            (with-default-read y-n-market-table market-id
                {"predictiveQuestion":"", "outcome":"", "quantityYes":-1.0, "quantityNo":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                {"predictiveQuestion":=p-q, "outcome":=o, "quantityYes":=q-y, "quantityNo":=q-n, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                (let ((is-new (if (= -1.0 q-n) true false)))
                    (if is-new
                        (write y-n-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcome":(at "outcome" supportive-desc), "quantityYes":0.0, "quantityNo":(at "quantity" tx-desc), "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                        (write y-n-market-table market-id {"predictiveQuestion":p-q, "outcome":o, "quantityYes":q-y, "quantityNo":(+ q-n (at "quantity" tx-desc)), "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                    )
                )
            )
        )
        
        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "yes")
                (with-default-read portfolio-table portfolio-id
                    {"yesQuantity":-1.0, "noQuantity":-1.0}
                    {"yesQuantity":=y-q, "noQuantity":=n-q}
                    (let ((is-account-new (if (= -1.0 y-q) true false)))
                        (write portfolio-table portfolio-id {"yesQuantity":(if is-account-new (at "quantity" tx-desc) (+ y-q (at "quantity" tx-desc))), "noQuantity":(if is-account-new 0.0 n-q)})
                    )
                )
                (with-default-read portfolio-table portfolio-id
                    {"yesQuantity":-1.0, "noQuantity":-1.0}
                    {"yesQuantity":=y-q, "noQuantity":=n-q}
                    (let ((is-account-new (if (= -1.0 n-q) true false)))
                        (write portfolio-table portfolio-id {"yesQuantity":(if is-account-new 0.0 y-q), "noQuantity":(if is-account-new (at "quantity" tx-desc) (+ n-q (at "quantity" tx-desc)))})
                    )
                )
            )
        )
    )

    (defun y-n-portfolio-debit (account:string market-id:string type:string tx-desc:object)
        @doc "functionality to sell yes or no outcomes"
        (market-tx-validation tx-desc)

        ;; On the selling process, validate that the user's portfolio and the general market,
        ;; possess enough shares to commit the sale. Then, we unlocked the FINUX amount from the escrow account.
        ;; and send it to the user. After, in the y-n-market-table, we adjust the shares distribution, and then move on to
        ;; debit the user's account with the shares purchased in the portfolio-table. Note that we use with-read
        ;; so that this operation will fail if the market-id or portfolio-id doesn't exist.
        (with-read y-n-market-table market-id {"quantityYes":=q-y, "quantityNo":=q-n, "status":=status}
            (enforce (= status "live") "market is closed")
            (if (= type "yes")
                (enforce (>= (- q-y (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                (enforce (>= (- q-n (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
            )
        )

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "yes")
                (with-read portfolio-table portfolio-id {"yesQuantity":=y-q}
                    (enforce (>= y-q (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                    (with-read token-table account {"guard":=account-guard}
                        (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                    )
                    (update portfolio-table portfolio-id {"yesQuantity":(- y-q (at "quantity" tx-desc))})
                )
                (with-read portfolio-table portfolio-id {"noQuantity":=n-q}
                    (enforce (>= n-q (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                    (with-read token-table account {"guard":=account-guard}
                        (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                    )
                    (update portfolio-table portfolio-id {"noQuantity":(- n-q (at "quantity" tx-desc))})
                )
            )
        )

        (if (= type "yes")
            (with-read y-n-market-table market-id {"quantityYes":=q-y}
                (update y-n-market-table market-id {"quantityYes":(- q-y (at "quantity" tx-desc))})
            )
            (with-read y-n-market-table market-id {"quantityNo":=q-n}
                (update y-n-market-table market-id {"quantityNo":(- q-n (at "quantity" tx-desc))})
            )
        )
    )

    (defun y-n-close-market (market-id:string)
        @doc "functionality to close a yes or no market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)

        (with-read y-n-market-table market-id {"continuous":=continuous, "status":=status}
            (enforce (= status "live") (format "cannot close a {} market" [status]))
            (enforce (not continuous) "cannot close a continuous market")
            (update y-n-market-table market-id {"status":"ended"})
        )
    )
    
    (defun y-n-resolve-market (market-id:string resolution-outcome:string)
        @doc "functionality to resolve a yes or no market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)
        (enforce
            (contains resolution-outcome ["yes" "no"])
            "resolution-outcome is invalid for the utilization of this function"
        )

        (with-read y-n-market-table market-id {"status":=status}
            (enforce (= status "ended") (format "cannot resolve a {} market" [status]))
            (update y-n-market-table market-id {"status":"resolved", "resolved":true, "resolutionOutcome":resolution-outcome})
        )
    )
    
    (defun y-n-resolve-portfolio (account:string market-id:string)
        @doc "functionality to resolve an account's portfolio related to a yes or no market"
        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)

        ;; After a market is resolved, we run this function for all user portfolios related to the market
        ;; with a balance of yes or no shares greater than 0. This function validates that the market is resolved,
        ;; and then reviews the user's portfolio in the portfolio-table. Based on the validated outcome (i.e, yes or no),
        ;; the user will receive 1 FINUX per each yes share or no share. If the validated outcome is yes, the user will 
        ;; receive 1 FINUX per each yes share and nothing for each no share. If the validated outcome is no, then inverse
        ;; will take place. But on each resolution, the y-n-market-table's quantityYes and quantityNo balances are adjusted 
        ;; and decreased by the amount of shares possessed by the user. So that on the resolution of all accounts, the 
        ;; balance of shares across the market for this market-id is 0--it is 0 since FINUX has been sent to each user on resolution,
        ;; for the right outcome. Further, all the shares of the other outcome are valueless.
        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (with-read portfolio-table portfolio-id {"yesQuantity":=y-q, "noQuantity":=n-q}
                (with-read y-n-market-table market-id {"quantityYes":=q-y, "quantityNo":=q-n, "resolved":=rslvd, "resolutionOutcome":=r-o}
                    (enforce rslvd "market has not yet resolved")

                    (if (= r-o "yes")
                        (if (> y-q 0.0)
                            (with-read token-table account {"guard":=account-guard}
                                (enforce (>= q-y y-q) "insufficient market balance to complete the tx")
                                (transfer-create finulab-prediction-market-escrow-account account account-guard y-q)
                                (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                            )
                            (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                        )
                        (if (> n-q 0.0)
                            (with-read token-table account {"guard":=account-guard}
                                (enforce (>= q-n n-q) "insufficient market balance to complete the tx")
                                (transfer-create finulab-prediction-market-escrow-account account account-guard n-q)
                                (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                            )
                            (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                        )
                    )

                    (update y-n-market-table market-id {"quantityYes":(- q-y y-q), "quantityNo":(- q-n n-q)})
                )
            )
        )
    )
    
    (defun y-n-read-market (market-id:string)
        @doc "utility to pull a yes or no market's details"
        (with-read y-n-market-table market-id {"predictiveQuestion":=p-q, "outcome":=outcome, "quantityYes":=q-y, "quantityNo":=q-n, "continuous":=cont, "endDate":=e-d, "status":=status, "resolved":=rslvd, "resolutionOutcome":=rslvd-o}
            {
                "predictiveQuestion":p-q,
                "outcome":outcome,
                "quantityYes":q-y,
                "quantityNo":q-n,
                "continuous":cont,
                "endDate":e-d,
                "status":status,
                "resolved":rslvd,
                "resolutionOutcome":rslvd-o
            }
        )
    )

    (defun y-n-read-portfolio (account:string market-id:string)
        @doc "utility to pull a portfolio's details related to a yes or no market"

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (with-read portfolio-table portfolio-id {"yesQuantity":=y-q, "noQuantity":=n-q}
                {
                    "yesQuantity":y-q, 
                    "noQuantity":n-q
                }
            )
        )
    )
)

(create-table y-n-market-table)
(create-table portfolio-table)
(create-table collateralized-portfolio-table)
