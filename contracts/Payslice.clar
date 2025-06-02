(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PERCENTAGE (err u101))
(define-constant ERR-NO-PROFILE (err u102))
(define-constant ERR-INVALID-RECIPIENT (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))

(define-data-var contract-owner principal tx-sender)

(define-map PaymentProfiles
  { owner: principal }
  {
    savings-address: principal,
    savings-percentage: uint,
    loan-address: principal,
    loan-percentage: uint,
    investment-address: principal,
    investment-percentage: uint,
    main-address: principal
  }
)

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))))

(define-public (create-profile 
    (savings-addr principal)
    (savings-pct uint)
    (loan-addr principal)
    (loan-pct uint)
    (investment-addr principal)
    (investment-pct uint)
    (main-addr principal))
  (begin
    (asserts! (<= (+ savings-pct loan-pct investment-pct) u100) ERR-INVALID-PERCENTAGE)
    (ok (map-set PaymentProfiles
      { owner: tx-sender }
      {
        savings-address: savings-addr,
        savings-percentage: savings-pct,
        loan-address: loan-addr,
        loan-percentage: loan-pct,
        investment-address: investment-addr,
        investment-percentage: investment-pct,
        main-address: main-addr
      }))))

(define-public (update-profile
    (savings-addr principal)
    (savings-pct uint)
    (loan-addr principal)
    (loan-pct uint)
    (investment-addr principal)
    (investment-pct uint)
    (main-addr principal))
  (begin
    ;; (asserts! (map-get? PaymentProfiles { owner: tx-sender }) ERR-NO-PROFILE)
    (asserts! (<= (+ savings-pct loan-pct investment-pct) u100) ERR-INVALID-PERCENTAGE)
    (ok (map-set PaymentProfiles
      { owner: tx-sender }
      {
        savings-address: savings-addr,
        savings-percentage: savings-pct,
        loan-address: loan-addr,
        loan-percentage: loan-pct,
        investment-address: investment-addr,
        investment-percentage: investment-pct,
        main-address: main-addr
      }))))

(define-public (delete-profile)
  (begin
    ;; (asserts! (map-get? PaymentProfiles { owner: tx-sender }) ERR-NO-PROFILE)
    (ok (map-delete PaymentProfiles { owner: tx-sender }))))

(define-read-only (get-profile (owner principal))
  (map-get? PaymentProfiles { owner: owner }))

(define-public (receive-payment (amount uint))
  (let (
    (profile (unwrap! (get-profile tx-sender) ERR-NO-PROFILE))
    (savings-amount (/ (* amount (get savings-percentage profile)) u100))
    (loan-amount (/ (* amount (get loan-percentage profile)) u100))
    (investment-amount (/ (* amount (get investment-percentage profile)) u100))
    (remaining-amount (- amount (+ savings-amount (+ loan-amount investment-amount))))
  )
  (begin
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
    
    (if (> savings-amount u0)
      (unwrap! (stx-transfer? savings-amount tx-sender (get savings-address profile)) ERR-INVALID-RECIPIENT)
      true)
    
    (if (> loan-amount u0)
      (unwrap! (stx-transfer? loan-amount tx-sender (get loan-address profile)) ERR-INVALID-RECIPIENT)
      true)
    
    (if (> investment-amount u0)
      (unwrap! (stx-transfer? investment-amount tx-sender (get investment-address profile)) ERR-INVALID-RECIPIENT)
      true)
    
    (if (> remaining-amount u0)
      (unwrap! (stx-transfer? remaining-amount tx-sender (get main-address profile)) ERR-INVALID-RECIPIENT)
      true)
    
    (ok true))))

(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (stx-transfer? amount (as-contract tx-sender) (var-get contract-owner))))