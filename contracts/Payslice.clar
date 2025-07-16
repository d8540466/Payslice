(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PERCENTAGE (err u101))
(define-constant ERR-NO-PROFILE (err u102))
(define-constant ERR-INVALID-RECIPIENT (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-TEMPLATE-NOT-FOUND (err u105))
(define-constant ERR-INVALID-FREQUENCY (err u106))
(define-constant ERR-TEMPLATE-EXISTS (err u107))
(define-constant ERR-PAYMENT-NOT-DUE (err u108))
(define-constant ERR-INVALID-AMOUNT (err u109))

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

(define-map PaymentTemplates
  { owner: principal, template-id: uint }
  {
    name: (string-ascii 50),
    savings-address: principal,
    savings-percentage: uint,
    loan-address: principal,
    loan-percentage: uint,
    investment-address: principal,
    investment-percentage: uint,
    main-address: principal,
    expected-amount: uint,
    frequency-days: uint,
    last-payment-height: uint,
    next-payment-height: uint,
    is-active: bool,
    auto-execute: bool
  }
)

(define-map UserTemplateCounters
  { owner: principal }
  { counter: uint }
)

(define-map ScheduledPayments
  { template-owner: principal, template-id: uint, execution-height: uint }
  {
    amount: uint,
    executed: bool,
    execution-timestamp: uint
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

(define-private (get-next-template-id (owner principal))
  (let ((current-counter (default-to u0 (get counter (map-get? UserTemplateCounters { owner: owner })))))
    (+ current-counter u1)))

(define-private (increment-template-counter (owner principal))
  (let ((current-counter (default-to u0 (get counter (map-get? UserTemplateCounters { owner: owner })))))
    (map-set UserTemplateCounters
      { owner: owner }
      { counter: (+ current-counter u1) })))

(define-public (create-payment-template
    (name (string-ascii 50))
    (savings-addr principal)
    (savings-pct uint)
    (loan-addr principal)
    (loan-pct uint)
    (investment-addr principal)
    (investment-pct uint)
    (main-addr principal)
    (expected-amount uint)
    (frequency-days uint)
    (auto-execute bool))
  (let ((template-id (get-next-template-id tx-sender)))
    (begin
      (asserts! (<= (+ savings-pct loan-pct investment-pct) u100) ERR-INVALID-PERCENTAGE)
      (asserts! (> frequency-days u0) ERR-INVALID-FREQUENCY)
      (asserts! (> expected-amount u0) ERR-INVALID-AMOUNT)
      (asserts! (is-none (map-get? PaymentTemplates { owner: tx-sender, template-id: template-id })) ERR-TEMPLATE-EXISTS)
      
      (map-set PaymentTemplates
        { owner: tx-sender, template-id: template-id }
        {
          name: name,
          savings-address: savings-addr,
          savings-percentage: savings-pct,
          loan-address: loan-addr,
          loan-percentage: loan-pct,
          investment-address: investment-addr,
          investment-percentage: investment-pct,
          main-address: main-addr,
          expected-amount: expected-amount,
          frequency-days: frequency-days,
          last-payment-height: u0,
          next-payment-height: (+ burn-block-height (* frequency-days u144)),
          is-active: true,
          auto-execute: auto-execute
        })
      
      (increment-template-counter tx-sender)
      (ok template-id))))

(define-public (update-payment-template
    (template-id uint)
    (name (string-ascii 50))
    (savings-addr principal)
    (savings-pct uint)
    (loan-addr principal)
    (loan-pct uint)
    (investment-addr principal)
    (investment-pct uint)
    (main-addr principal)
    (expected-amount uint)
    (frequency-days uint)
    (auto-execute bool))
  (let ((template (unwrap! (map-get? PaymentTemplates { owner: tx-sender, template-id: template-id }) ERR-TEMPLATE-NOT-FOUND)))
    (begin
      (asserts! (<= (+ savings-pct loan-pct investment-pct) u100) ERR-INVALID-PERCENTAGE)
      (asserts! (> frequency-days u0) ERR-INVALID-FREQUENCY)
      (asserts! (> expected-amount u0) ERR-INVALID-AMOUNT)
      
      (map-set PaymentTemplates
        { owner: tx-sender, template-id: template-id }
        {
          name: name,
          savings-address: savings-addr,
          savings-percentage: savings-pct,
          loan-address: loan-addr,
          loan-percentage: loan-pct,
          investment-address: investment-addr,
          investment-percentage: investment-pct,
          main-address: main-addr,
          expected-amount: expected-amount,
          frequency-days: frequency-days,
          last-payment-height: (get last-payment-height template),
          next-payment-height: (if (> (get last-payment-height template) u0)
                                 (+ (get last-payment-height template) (* frequency-days u144))
                                 (+ burn-block-height (* frequency-days u144))),
          is-active: (get is-active template),
          auto-execute: auto-execute
        })
      (ok true))))

(define-public (toggle-template-status (template-id uint))
  (let ((template (unwrap! (map-get? PaymentTemplates { owner: tx-sender, template-id: template-id }) ERR-TEMPLATE-NOT-FOUND)))
    (begin
      (map-set PaymentTemplates
        { owner: tx-sender, template-id: template-id }
        (merge template { is-active: (not (get is-active template)) }))
      (ok (not (get is-active template))))))

(define-public (delete-payment-template (template-id uint))
  (let ((template (unwrap! (map-get? PaymentTemplates { owner: tx-sender, template-id: template-id }) ERR-TEMPLATE-NOT-FOUND)))
    (begin
      (map-delete PaymentTemplates { owner: tx-sender, template-id: template-id })
      (ok true))))

(define-public (execute-template-payment (template-id uint) (amount uint))
  (let ((template (unwrap! (map-get? PaymentTemplates { owner: tx-sender, template-id: template-id }) ERR-TEMPLATE-NOT-FOUND)))
    (begin
      (asserts! (get is-active template) ERR-TEMPLATE-NOT-FOUND)
      (asserts! (>= burn-block-height (get next-payment-height template)) ERR-PAYMENT-NOT-DUE)
      (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
      
      (let (
        (savings-amount (/ (* amount (get savings-percentage template)) u100))
        (loan-amount (/ (* amount (get loan-percentage template)) u100))
        (investment-amount (/ (* amount (get investment-percentage template)) u100))
        (remaining-amount (- amount (+ savings-amount (+ loan-amount investment-amount))))
      )
      (begin
        (if (> savings-amount u0)
          (unwrap! (stx-transfer? savings-amount tx-sender (get savings-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (if (> loan-amount u0)
          (unwrap! (stx-transfer? loan-amount tx-sender (get loan-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (if (> investment-amount u0)
          (unwrap! (stx-transfer? investment-amount tx-sender (get investment-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (if (> remaining-amount u0)
          (unwrap! (stx-transfer? remaining-amount tx-sender (get main-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (map-set PaymentTemplates
          { owner: tx-sender, template-id: template-id }
          (merge template {
            last-payment-height: burn-block-height,
            next-payment-height: (+ burn-block-height (* (get frequency-days template) u144))
          }))
        
        (map-set ScheduledPayments
          { template-owner: tx-sender, template-id: template-id, execution-height: burn-block-height }
          {
            amount: amount,
            executed: true,
            execution-timestamp: burn-block-height
          })
        
        (ok true))))))

(define-public (schedule-payment (template-id uint) (amount uint) (execution-height uint))
  (let ((template (unwrap! (map-get? PaymentTemplates { owner: tx-sender, template-id: template-id }) ERR-TEMPLATE-NOT-FOUND)))
    (begin
      (asserts! (get is-active template) ERR-TEMPLATE-NOT-FOUND)
      (asserts! (> execution-height burn-block-height) ERR-INVALID-FREQUENCY)
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      
      (map-set ScheduledPayments
        { template-owner: tx-sender, template-id: template-id, execution-height: execution-height }
        {
          amount: amount,
          executed: false,
          execution-timestamp: u0
        })
      (ok true))))

(define-public (execute-scheduled-payment (template-owner principal) (template-id uint) (execution-height uint))
  (let (
    (scheduled-payment (unwrap! (map-get? ScheduledPayments { template-owner: template-owner, template-id: template-id, execution-height: execution-height }) ERR-TEMPLATE-NOT-FOUND))
    (template (unwrap! (map-get? PaymentTemplates { owner: template-owner, template-id: template-id }) ERR-TEMPLATE-NOT-FOUND)))
    (begin
      (asserts! (not (get executed scheduled-payment)) ERR-TEMPLATE-NOT-FOUND)
      (asserts! (>= burn-block-height execution-height) ERR-PAYMENT-NOT-DUE)
      (asserts! (get auto-execute template) ERR-NOT-AUTHORIZED)
      (asserts! (>= (stx-get-balance template-owner) (get amount scheduled-payment)) ERR-INSUFFICIENT-FUNDS)
      
      (let (
        (amount (get amount scheduled-payment))
        (savings-amount (/ (* amount (get savings-percentage template)) u100))
        (loan-amount (/ (* amount (get loan-percentage template)) u100))
        (investment-amount (/ (* amount (get investment-percentage template)) u100))
        (remaining-amount (- amount (+ savings-amount (+ loan-amount investment-amount))))
      )
      (begin
        (if (> savings-amount u0)
          (unwrap! (stx-transfer? savings-amount template-owner (get savings-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (if (> loan-amount u0)
          (unwrap! (stx-transfer? loan-amount template-owner (get loan-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (if (> investment-amount u0)
          (unwrap! (stx-transfer? investment-amount template-owner (get investment-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (if (> remaining-amount u0)
          (unwrap! (stx-transfer? remaining-amount template-owner (get main-address template)) ERR-INVALID-RECIPIENT)
          true)
        
        (map-set ScheduledPayments
          { template-owner: template-owner, template-id: template-id, execution-height: execution-height }
          (merge scheduled-payment {
            executed: true,
            execution-timestamp: burn-block-height
          }))
        
        (ok true))))))

(define-read-only (get-payment-template (owner principal) (template-id uint))
  (map-get? PaymentTemplates { owner: owner, template-id: template-id }))

(define-read-only (get-user-template-count (owner principal))
  (default-to u0 (get counter (map-get? UserTemplateCounters { owner: owner }))))

(define-read-only (get-scheduled-payment (template-owner principal) (template-id uint) (execution-height uint))
  (map-get? ScheduledPayments { template-owner: template-owner, template-id: template-id, execution-height: execution-height }))

(define-read-only (is-payment-due (owner principal) (template-id uint))
  (match (map-get? PaymentTemplates { owner: owner, template-id: template-id })
    template (>= burn-block-height (get next-payment-height template))
    false))

(define-read-only (get-template-next-payment-info (owner principal) (template-id uint))
  (match (map-get? PaymentTemplates { owner: owner, template-id: template-id })
    template (some {
      next-payment-height: (get next-payment-height template),
      blocks-remaining: (if (>= burn-block-height (get next-payment-height template))
                         u0
                         (- (get next-payment-height template) burn-block-height)),
      is-due: (>= burn-block-height (get next-payment-height template))
    })
    none))