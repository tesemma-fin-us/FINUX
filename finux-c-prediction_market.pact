(define-namespace "free" (read-keyset "finux-module-admin") (read-keyset "finux-operations-admin"))
(namespace "free")

(define-keyset "free.finux-module-admin" (read-keyset "finux-module-admin"))
(define-keyset "free.finux-operations-admin" (read-keyset "finux-operations-admin"))

(module finux-c-prediction_market GOVERNANCE
    @doc "Categorical based prediction market that runs off FINUX. \
    \ This module supports the creation of markets arounds events like: \
    \ 'Who will win the Super Bowl?' (Cheifs or Eagles) or \
    \ 'Who will win the NBA Championship? (Mavericks or Celtics)'."

    ;; -----------------------------------------------------------------------------------------------------
    ;;  schemas
    (defschema c-market-schema
        @doc "schema designated to account and maintain finulab's categorical based prediction market"
        @model [
            (invariant (>= quantityOne 0.0))
            (invariant (>= quantityTwo 0.0))
            (invariant (>= quantityThree 0.0))
            (invariant (>= quantityFour 0.0))
        ]
        predictiveQuestion:string
        outcomes:object
        quantityOne:decimal
        quantityTwo:decimal
        quantityThree:decimal
        quantityFour:decimal
        continuous:bool
        endDate:integer
        status:string
        resolved:bool
        resolutionOutcome:string
    )
    (defschema c-portfolio-schema
        @doc "schema designated to account and maintain each finux account's portfolio by market for categorical based predictions"
        @model [
            (invariant (>= quantityOne 0.0))
            (invariant (>= quantityTwo 0.0))
            (invariant (>= quantityThree 0.0))
            (invariant (>= quantityFour 0.0))
        ]
        quantityOne:decimal
        quantityTwo:decimal
        quantityThree:decimal
        quantityFour:decimal
    )
    (defschema c-collateralized-portfolio-schema
        @doc "schema designated to account and maintain each finux account's sale portfolio by market for categorical based predictions"
        @model [
            (invariant (>= soldQuantityOne 0.0))
            (invariant (>= soldQuantityTwo 0.0))
            (invariant (>= soldQuantityThree 0.0))
            (invariant (>= soldQuantityFour 0.0))
        ]
        soldQuantityOne:decimal
        soldCollateralOne:decimal
        soldQuantityTwo:decimal
        soldCollateralTwo:decimal
        soldQuantityThree:decimal
        soldCollateralThree:decimal
        soldQuantityFour:decimal
        soldCollateralFour:decimal
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  tables
    (deftable c-market-table:{c-market-schema})
    (deftable c-portfolio-table:{c-portfolio-schema})
    (deftable c-collateralized-portfolio-table:{c-collateralized-portfolio-schema})

    ;; -----------------------------------------------------------------------------------------------------
    ;;  capabilities
    (defcap GOVERNANCE ()
        (enforce-keyset "free.finux-module-admin")
    )
    
    ;; -----------------------------------------------------------------------------------------------------
    ;;  Utilize Modules
    (use free.finux)
    (use free.finux-y-n-prediction_market)

    ;; -----------------------------------------------------------------------------------------------------
    ;;  functionality
    (defun c-create-market (account:string market-id:string predictive-question:string outcomes:object continuous:bool end-date:integer market-desc:object)
        @doc "Functionality to create a categorical market related to a prediction. \
        \ Note that any account can create the prediction; however, operations-admin \
        \ must sign-off to ensure that there is no conflicts, and the prediction is resolvable (i.e., \
        \ having a definitive and comprehensive list of outcomes)."

        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)

        ;; In market creation, note that the maximum length of outcomes
        ;; supported by this contract is 4. Furthermore, the quantities tied each
        ;; outcome (i.e., outcomeOne to outcomeFour) must be greater than or equal to 0.0.
        ;; And just as importantly, the liquidity locked to create the market must be greater than 0.0.
        (enforce (> (at "cost" market-desc) 0.0) "tx cost must be greater than 0.0")
        (enforce-unit (at "cost" market-desc))

        (enforce (>= (length outcomes) 2) "there must be at least 2 outcomes per a categorical market")
        (enforce (<= (length outcomes) 4) "at most there can only be 4 outcomes per a categorical market")

        (let ((outcomes-length (length outcomes)))
            (if (= outcomes-length 2)
                (and
                    (enforce (contains "outcomeOne" outcomes) "outcomes object must be properly structured")
                    (enforce (contains "outcomeTwo" outcomes) "outcomes object must be properly structured")
                )
                (if (= outcomes-length 3)
                    (and
                        (enforce (contains "outcomeOne" outcomes) "outcomes object must be properly structured")
                        (and
                            (enforce (contains "outcomeTwo" outcomes) "outcomes object must be properly structured")
                            (enforce (contains "outcomeThree" outcomes) "outcomes object must be properly structured")
                        )
                    )
                    (and
                        (enforce (contains "outcomeOne" outcomes) "outcomes object must be properly structured")
                        (and
                            (enforce (contains "outcomeTwo" outcomes) "outcomes object must be properly structured")
                            (and
                                (enforce (contains "outcomeThree" outcomes) "outcomes object must be properly structured")
                                (enforce (contains "outcomeFour" outcomes) "outcomes object must be properly structured")
                            )
                        )
                    )
                )
            )

            (if (= outcomes-length 2)
                (and
                    (enforce (>= (at "quantityOne" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                    (and
                        (enforce (>= (at "quantityTwo" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                        (enforce (> (+ (at "quantityOne" market-desc) (at "quantityTwo" market-desc)) 0.0) "sum of all outcome quantities must be greater than 0.0")
                    )
                )
                (if (= outcomes-length 3)
                    (and
                        (enforce (>= (at "quantityOne" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                        (and
                            (enforce (>= (at "quantityTwo" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                            (and
                                (enforce (>= (at "quantityThree" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                                (enforce (> (+ (at "quantityOne" market-desc) (+ (at "quantityTwo" market-desc) (at "quantityThree" market-desc))) 0.0) "sum of all outcome quantities must be greater than 0.0")
                            )
                        )
                    )
                    (and
                        (enforce (>= (at "quantityOne" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                        (and
                            (enforce (>= (at "quantityTwo" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                            (and
                                (enforce (>= (at "quantityThree" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                                (and
                                    (enforce (>= (at "quantityFour" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                                    (enforce (> (+ (at "quantityOne" market-desc) (+ (at "quantityTwo" market-desc) (+ (at "quantityThree" market-desc) (at "quantityFour" market-desc)))) 0.0) "sum of all outcome quantities must be greater than 0.0")
                                )
                            )
                        )
                    )
                )
            )

            (if (= outcomes-length 2)
                (and
                    (enforce-unit (at "quantityOne" market-desc))
                    (enforce-unit (at "quantityTwo" market-desc))
                )
                (if (= outcomes-length 3)
                    (and
                        (enforce-unit (at "quantityOne" market-desc))
                        (and
                            (enforce-unit (at "quantityTwo" market-desc))
                            (enforce-unit (at "quantityThree" market-desc))
                        )
                    )
                    (and
                        (enforce-unit (at "quantityOne" market-desc))
                        (and
                            (enforce-unit (at "quantityTwo" market-desc))
                            (and
                                (enforce-unit (at "quantityThree" market-desc))
                                (enforce-unit (at "quantityFour" market-desc))
                            )
                        )
                    )
                )
            )
        )

        ;; On creation, the locked liquidity is transfered to the root escrow account,
        ;; where the funds will be held, either until the user sells or the market resolves.
        ;; Furter, in addition to the market's creation for the unique market-id, user's 
        ;; balance is added to c-portfolio-table based on a unique portfolio-id.
        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" market-desc))
        )
        (insert c-market-table market-id {"predictiveQuestion":predictive-question, "outcomes":outcomes, "quantityOne":(at "quantityOne" market-desc), "quantityTwo":(at "quantityTwo" market-desc), "quantityThree":(at "quantityThree" market-desc), "quantityFour":(at "quantityFour" market-desc), "continuous":continuous, "endDate":end-date, "status":"live", "resolved":false, "resolutionOutcome":""})
        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (insert c-portfolio-table portfolio-id {"quantityOne":(at "quantityOne" market-desc), "quantityTwo":(at "quantityTwo" market-desc), "quantityThree":(at "quantityThree" market-desc), "quantityFour":(at "quantityFour" market-desc)})
        )
    )

    (defun c-portfolio-credit (account:string market-id:string type:string tx-desc:object supportive-desc:object)
        @doc "functionality to purchase categorical outcomes"
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

        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" tx-desc))
        )

        ;; After the payment of fees, we send the locked FINUX amount to the escrow account.
        ;; Then, in the c-market-table we adjust the shares distribution, and then move on to
        ;; credit the user's account with the shares purchased in the c-portfolio-table. Note that we use with-default-read
        ;; so that this operation will not fail if the market-id or portfolio-id doesn't exist.
        (if (= type "outcomeOne")
            (with-default-read c-market-table market-id
                {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                (let ((is-new (if (= -1.0 q-o) true false)))
                    (if is-new
                        (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":(at "quantity" tx-desc), "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                        (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":(+ q-o (at "quantity" tx-desc)), "quantityTwo":q-tw, "quantityThree":q-th, "quantityFour":q-f, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                    )
                )
            )
            (if (= type "outcomeTwo")
                (with-default-read c-market-table market-id
                    {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                    {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                    (let ((is-new (if (= -1.0 q-tw) true false)))
                        (if is-new
                            (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":0.0, "quantityTwo":(at "quantity" tx-desc), "quantityThree":0.0, "quantityFour":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                            (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":q-o, "quantityTwo":(+ q-tw (at "quantity" tx-desc)), "quantityThree":q-th, "quantityFour":q-f, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                        )
                    )
                )
                (if (= type "outcomeThree")
                    (with-default-read c-market-table market-id
                        {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                        {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                        (let ((is-new (if (= -1.0 q-th) true false)))
                            (if is-new
                                (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":0.0, "quantityTwo":0.0, "quantityThree":(at "quantity" tx-desc), "quantityFour":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                                (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":q-o, "quantityTwo":q-tw, "quantityThree":(+ q-th (at "quantity" tx-desc)), "quantityFour":q-f, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                            )
                        )
                    )
                    (with-default-read c-market-table market-id
                        {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                        {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                        (let ((is-new (if (= -1.0 q-f) true false)))
                            (if is-new
                                (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":(at "quantity" tx-desc), "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                                (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":q-o, "quantityTwo":q-tw, "quantityThree":q-th, "quantityFour":(+ q-f (at "quantity" tx-desc)), "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                            )
                        )
                    )
                )
            )
        )

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "outcomeOne")
                (with-default-read c-portfolio-table portfolio-id
                    {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                    {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                    (let ((is-account-new (if (= -1.0 q-o) true false)))
                        (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new (at "quantity" tx-desc) (+ q-o (at "quantity" tx-desc))), "quantityTwo":(if is-account-new 0.0 q-tw), "quantityThree":(if is-account-new 0.0 q-th), "quantityFour":(if is-account-new 0.0 q-f)})
                    )
                )
                (if (= type "outcomeTwo")
                    (with-default-read c-portfolio-table portfolio-id
                        {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                        {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                        (let ((is-account-new (if (= -1.0 q-tw) true false)))
                            (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new 0.0 q-o), "quantityTwo":(if is-account-new (at "quantity" tx-desc) (+ q-tw (at "quantity" tx-desc))), "quantityThree":(if is-account-new 0.0 q-th), "quantityFour":(if is-account-new 0.0 q-f)})
                        )
                    )
                    (if (= type "outcomeThree")
                        (with-default-read c-portfolio-table portfolio-id
                            {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                            {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                            (let ((is-account-new (if (= -1.0 q-th) true false)))
                                (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new 0.0 q-o), "quantityTwo":(if is-account-new 0.0 q-tw), "quantityThree":(if is-account-new (at "quantity" tx-desc) (+ q-th (at "quantity" tx-desc))), "quantityFour":(if is-account-new 0.0 q-f)})
                            )
                        )
                        (with-default-read c-portfolio-table portfolio-id
                            {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                            {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                            (let ((is-account-new (if (= -1.0 q-f) true false)))
                                (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new 0.0 q-o), "quantityTwo":(if is-account-new 0.0 q-tw), "quantityThree":(if is-account-new 0.0 q-th), "quantityFour":(if is-account-new (at "quantity" tx-desc) (+ q-f (at "quantity" tx-desc)))})
                            )
                        )
                    )
                )
            )
        )
    )

    (defun c-portfolio-debit (account:string market-id:string type:string tx-desc:object)
        @doc "functionality to sell categorical outcomes"
        (market-tx-validation tx-desc)

        ;; On the selling process, validate that the user's portfolio and the general market,
        ;; possess enough shares to commit the sale. Then, we unlocked the FINUX amount from the escrow account
        ;; and send it to the user. After, in the c-market-table we adjust the shares distribution, and then move on to
        ;; debit the user's account with the shares purchased in the c-portfolio-table. Note that we use with-read
        ;; so that this operation will fail if the market-id or portfolio-id doesn't exist.
        (with-read c-market-table market-id {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "status":=status}
            (enforce (= status "live") "market is closed")
            (if (= type "outcomeOne")
                (enforce (>= (- q-o (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                (if (= type "outcomeTwo")
                    (enforce (>= (- q-tw (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                    (if (= type "outcomeThree")
                        (enforce (>= (- q-th (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                        (enforce (>= (- q-f (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                    )
                )
            )
        )

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "outcomeOne")
                (with-read c-portfolio-table portfolio-id {"quantityOne":=q-o}
                    (enforce (>= q-o (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                    (with-read token-table account {"guard":=account-guard}
                        (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                    )
                    (update c-portfolio-table portfolio-id {"quantityOne":(- q-o (at "quantity" tx-desc))})
                )
                (if (= type "outcomeTwo")
                    (with-read c-portfolio-table portfolio-id {"quantityTwo":=q-tw}
                        (enforce (>= q-tw (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                        (with-read token-table account {"guard":=account-guard}
                            (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                        )
                        (update c-portfolio-table portfolio-id {"quantityTwo":(- q-tw (at "quantity" tx-desc))})
                    )
                    (if (= type "outcomeThree")
                        (with-read c-portfolio-table portfolio-id {"quantityThree":=q-th}
                            (enforce (>= q-th (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                            (with-read token-table account {"guard":=account-guard}
                                (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                            )
                            (update c-portfolio-table portfolio-id {"quantityThree":(- q-th (at "quantity" tx-desc))})
                        )
                        (with-read c-portfolio-table portfolio-id {"quantityFour":=q-f}
                            (enforce (>= q-f (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                            (with-read token-table account {"guard":=account-guard}
                                (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                            )
                            (update c-portfolio-table portfolio-id {"quantityFour":(- q-f (at "quantity" tx-desc))})
                        )
                    )
                )
            )
        )

        (if (= type "outcomeOne")
            (with-read c-market-table market-id {"quantityOne":=q-o}
                (update c-market-table market-id {"quantityOne":(- q-o (at "quantity" tx-desc))})
            )
            (if (= type "outcomeTwo")
                (with-read c-market-table market-id {"quantityTwo":=q-tw}
                    (update c-market-table market-id {"quantityTwo":(- q-tw (at "quantity" tx-desc))})
                )
                (if (= type "outcomeThree")
                    (with-read c-market-table market-id {"quantityThree":=q-th}
                        (update c-market-table market-id {"quantityThree":(- q-th (at "quantity" tx-desc))})
                    )
                    (with-read c-market-table market-id {"quantityFour":=q-f}
                        (update c-market-table market-id {"quantityFour":(- q-f (at "quantity" tx-desc))})
                    )
                )
            )
        )
    )

    (defun c-close-market (market-id:string)
        @doc "functionality to close a categorical market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)
        
        (with-read c-market-table market-id {"continuous":=continuous, "status":=status}
            (enforce (= status "live") (format "cannot close a {} market" [status]))
            (enforce (not continuous) "cannot close a continuous market")
            (update c-market-table market-id {"status":"ended"})
        )
    )

    (defun c-resolve-market (market-id:string resolution-outcome:string)
        @doc "functionality to resolve a categorical market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)

        (with-read c-market-table market-id {"outcomes":=outcomes, "status":=status}
            (enforce (= status "ended") (format "cannot resolve a {} market" [status]))
            (enforce
                (contains resolution-outcome outcomes)
                "resolution-outcome is invalid for the utilization of this function"
            )
            (update c-market-table market-id {"status":"resolved", "resolved":true, "resolutionOutcome":resolution-outcome})
        )
    )

    (defun c-resolve-portfolio (account:string market-id:string)
        @doc "functionality to resolve an account's portfolio related to a categorical market"
        (enforce-keyset "free.finux-operations-admin")
        (acquire-module-admin free.finux)

        ;; After a market is resolved, we run this function for all user portfolios related to the market
        ;; with a balance of shares across all outcomes greater than 0. This function validated that the market is resolved,
        ;; and then reviews the user's portfolio in the c-portfolio-table. Based on the validated outcome,
        ;; the user will receive 1 FINUX per share they possess of that outcome. On each resolution, the c-market-table 
        ;; quantityOne to quantityFour balances are adjusted and decreased by the amount of shares possessed by the user.
        ;; So that on the resolution of all account, the balance of shares across the market for this market-id is 0--it is 0 
        ;; since FINUX has been sent to each user on resolution, for the right outcome. Further, all the shares of the other outcome are valueless.

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (with-read c-portfolio-table portfolio-id {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                (with-read c-market-table market-id {"quantityOne":=mq-o, "quantityTwo":=mq-tw, "quantityThree":=mq-th, "quantityFour":=mq-f, "resolved":=rslvd, "resolutionOutcome":=r-o}
                    (enforce rslvd "market has not yet resolved")

                    (if (= r-o "outcomeOne")
                        (if (> q-o 0.0)
                            (with-read token-table account {"guard":=account-guard}
                                (enforce (>= mq-o q-o) "insufficient market balance to complete the tx")
                                (transfer-create finulab-prediction-market-escrow-account account account-guard q-o)
                                (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                            )
                            (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                        )
                        (if (= r-o "outcomeTwo")
                            (if (> q-tw 0.0)
                                (with-read token-table account {"guard":=account-guard}
                                    (enforce (>= mq-tw q-tw) "insufficient market balance to complete the tx")
                                    (transfer-create finulab-prediction-market-escrow-account account account-guard q-tw)
                                    (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                )
                                (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                            )
                            (if (= r-o "outcomeThree")
                                (if (> q-th 0.0)
                                    (with-read token-table account {"guard":=account-guard}
                                        (enforce (>= mq-th q-th) "insufficient market balance to complete the tx")
                                        (transfer-create finulab-prediction-market-escrow-account account account-guard q-th)
                                        (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                    )
                                    (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                )
                                (if (> q-f 0.0)
                                    (with-read token-table account {"guard":=account-guard}
                                        (enforce (>= mq-f q-f) "insufficient market balance to complete the tx")
                                        (transfer-create finulab-prediction-market-escrow-account account account-guard q-f)
                                        (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                    )
                                    (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                )
                            )
                        )
                    )

                    (update c-market-table market-id {"quantityOne":(- mq-o q-o), "quantityTwo":(- mq-tw q-tw), "quantityThree":(- mq-th q-th), "quantityFour":(- mq-f q-f)})
                )
            )
        )
    )

    (defun c-read-market (market-id:string)
        @doc "utility to pull a categorical market's details"
        (with-read c-market-table market-id {"predictiveQuestion":=p-q, "outcomes":=outcomes, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=cont, "endDate":=e-d, "status":=status, "resolved":=rslvd, "resolutionOutcome":=rslvd-o}
            (let ((outcomes-length (length outcomes)))
                (if (= outcomes-length 2)
                    {
                        "predictiveQuestion":p-q,
                        "outcomes":[(at "outcomeOne" outcomes) (at "outcomeTwo" outcomes)],
                        "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw]],
                        "continuous":cont,
                        "endDate":e-d,
                        "status":status,
                        "resolved":rslvd,
                        "resolutionOutcome":rslvd-o
                    }
                    (if (= outcomes-length 3)
                        {
                            "predictiveQuestion":p-q,
                            "outcomes":[(at "outcomeOne" outcomes) (at "outcomeTwo" outcomes) (at "outcomeThree" outcomes)],
                            "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th]],
                            "continuous":cont,
                            "endDate":e-d,
                            "status":status,
                            "resolved":rslvd,
                            "resolutionOutcome":rslvd-o
                        }
                        {
                            "predictiveQuestion":p-q,
                            "outcomes":[(at "outcomeOne" outcomes) (at "outcomeTwo" outcomes) (at "outcomeThree" outcomes) (at "outcomeFour" outcomes)],
                            "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th] [(at "outcomeFour" outcomes) q-f]],
                            "continuous":cont,
                            "endDate":e-d,
                            "status":status,
                            "resolved":rslvd,
                            "resolutionOutcome":rslvd-o
                        }
                    )
                )
            )
        )
    )

    (defun c-read-portfolio (account:string market-id:string)
        @doc "utility to pull a portfolio's details related to a categorical market"

        (with-read c-market-table market-id {"outcomes":=outcomes}
            (let ((outcomes-length (length outcomes)))
                (let ((portfolio-id (format "{}-{}" [account market-id])))
                    (with-read c-portfolio-table portfolio-id {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                        (if (= outcomes-length 2)
                            {
                                "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw]]
                            }
                            (if (= outcomes-length 3)
                                {
                                    "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th]]
                                }
                                {
                                    "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th] [(at "outcomeFour" outcomes) q-f]]
                                }
                            )
                        )
                    )
                )
            )
        )
    )
)

(create-table c-market-table)
(create-table c-portfolio-table)
(create-table c-collateralized-portfolio-table)
