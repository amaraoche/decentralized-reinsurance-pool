;; reinsurance-pool
;; Community-owned reinsurance platform with parametric triggers and automated payouts

;; constants
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_INSUFFICIENT_FUNDS (err u2))
(define-constant ERR_INVALID_AMOUNT (err u3))
(define-constant ERR_POOL_NOT_FOUND (err u4))
(define-constant ERR_CLAIM_NOT_FOUND (err u5))
(define-constant CONTRACT_OWNER tx-sender)
(define-constant MIN_POOL_CONTRIBUTION u10000)
(define-constant PAYOUT_THRESHOLD u80) ;; 80% confidence threshold

;; data maps and vars
(define-map risk-pools
    { pool-id: uint }
    {
        pool-name: (string-ascii 64),
        risk-type: (string-ascii 32),
        total-capital: uint,
        participants: uint,
        coverage-limit: uint,
        status: (string-ascii 20)
    }
)

(define-map participant-stakes
    { pool-id: uint, participant: principal }
    {
        stake-amount: uint,
        join-date: uint,
        claims-paid: uint,
        rewards-earned: uint
    }
)

(define-map parametric-claims
    { claim-id: uint }
    {
        pool-id: uint,
        claimant: principal,
        trigger-value: uint,
        payout-amount: uint,
        verification-score: uint,
        status: (string-ascii 20),
        claim-date: uint
    }
)

(define-data-var next-pool-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var total-pools uint u0)
(define-data-var total-capital-pooled uint u0)

;; private functions
(define-private (calculate-payout (trigger-value uint) (threshold uint))
    (if (>= trigger-value threshold)
        (* trigger-value u100) ;; Simple payout calculation
        u0
    )
)

;; public functions
(define-public (create-risk-pool (pool-name (string-ascii 64)) (risk-type (string-ascii 32)) (coverage-limit uint))
    (let (
        (pool-id (var-get next-pool-id))
    )
    (asserts! (> coverage-limit u0) ERR_INVALID_AMOUNT)
    
    (map-set risk-pools { pool-id: pool-id }
        {
            pool-name: pool-name,
            risk-type: risk-type,
            total-capital: u0,
            participants: u0,
            coverage-limit: coverage-limit,
            status: "active"
        }
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (var-set total-pools (+ (var-get total-pools) u1))
    (ok pool-id)
    )
)

(define-public (contribute-to-pool (pool-id uint) (amount uint))
    (let (
        (pool-data (unwrap! (map-get? risk-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
        (existing-stake (default-to
            { stake-amount: u0, join-date: u0, claims-paid: u0, rewards-earned: u0 }
            (map-get? participant-stakes { pool-id: pool-id, participant: tx-sender })
        ))
    )
    (asserts! (>= amount MIN_POOL_CONTRIBUTION) ERR_INVALID_AMOUNT)
    
    ;; Update participant stake
    (map-set participant-stakes { pool-id: pool-id, participant: tx-sender }
        {
            stake-amount: (+ (get stake-amount existing-stake) amount),
            join-date: (if (is-eq (get join-date existing-stake) u0) block-height (get join-date existing-stake)),
            claims-paid: (get claims-paid existing-stake),
            rewards-earned: (get rewards-earned existing-stake)
        }
    )
    
    ;; Update pool data
    (map-set risk-pools { pool-id: pool-id }
        {
            pool-name: (get pool-name pool-data),
            risk-type: (get risk-type pool-data),
            total-capital: (+ (get total-capital pool-data) amount),
            participants: (if (is-eq (get stake-amount existing-stake) u0) 
                         (+ (get participants pool-data) u1) 
                         (get participants pool-data)),
            coverage-limit: (get coverage-limit pool-data),
            status: (get status pool-data)
        }
    )
    
    (var-set total-capital-pooled (+ (var-get total-capital-pooled) amount))
    (ok amount)
    )
)

(define-public (submit-parametric-claim (pool-id uint) (trigger-value uint))
    (let (
        (pool-data (unwrap! (map-get? risk-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
        (claim-id (var-get next-claim-id))
        (payout-amount (calculate-payout trigger-value PAYOUT_THRESHOLD))
    )
    (asserts! (> trigger-value u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq (get status pool-data) "active") ERR_POOL_NOT_FOUND)
    
    (map-set parametric-claims { claim-id: claim-id }
        {
            pool-id: pool-id,
            claimant: tx-sender,
            trigger-value: trigger-value,
            payout-amount: payout-amount,
            verification-score: u90, ;; Simulated verification
            status: (if (>= trigger-value PAYOUT_THRESHOLD) "approved" "pending"),
            claim-date: block-height
        }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
    )
)

;; read-only functions
(define-read-only (get-pool-info (pool-id uint))
    (map-get? risk-pools { pool-id: pool-id })
)

(define-read-only (get-participant-stake (pool-id uint) (participant principal))
    (map-get? participant-stakes { pool-id: pool-id, participant: participant })
)

(define-read-only (get-claim-details (claim-id uint))
    (map-get? parametric-claims { claim-id: claim-id })
)

(define-read-only (get-platform-stats)
    {
        total-pools: (var-get total-pools),
        total-capital: (var-get total-capital-pooled),
        next-pool-id: (var-get next-pool-id)
    }
)
