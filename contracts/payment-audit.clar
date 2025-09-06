;; Payment Audit and History Tracking System
;; Provides comprehensive transaction history and audit capabilities for Payslice

;; Error constants
(define-constant err-unauthorized (err u600))
(define-constant err-not-found (err u601))
(define-constant err-invalid-period (err u602))
(define-constant err-invalid-parameters (err u603))
(define-constant err-audit-locked (err u604))

;; Configuration constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-AUDIT-ENTRIES u1000)
(define-constant AUDIT-RETENTION-BLOCKS u525600) ;; ~1 year in blocks

;; Data variables
(define-data-var audit-enabled bool true)
(define-data-var last-transaction-id uint u0)
(define-data-var last-report-id uint u0)
(define-data-var audit-retention-period uint AUDIT-RETENTION-BLOCKS)

;; Transaction history tracking
(define-map transaction-records
  { transaction-id: uint }
  {
    user: principal,
    transaction-type: (string-ascii 20),
    total-amount: uint,
    timestamp: uint,
    block-height: uint,
    template-id: (optional uint),
    escrow-id: (optional uint),
    distribution: {
      savings-amount: uint,
      loan-amount: uint,
      investment-amount: uint,
      main-amount: uint
    }
  }
)

;; User audit summaries
(define-map user-audit-summaries
  { user: principal, period: uint }
  {
    total-processed: uint,
    total-to-savings: uint,
    total-to-loans: uint,
    total-to-investments: uint,
    total-to-main: uint,
    transaction-count: uint,
    period-start: uint,
    period-end: uint
  }
)

;; Financial reports
(define-map audit-reports
  { report-id: uint }
  {
    user: principal,
    report-type: (string-ascii 30),
    period-start: uint,
    period-end: uint,
    generated-at: uint,
    total-transactions: uint,
    total-volume: uint,
    summary-data: {
      savings-total: uint,
      loans-total: uint,
      investments-total: uint,
      main-total: uint
    }
  }
)

;; Verification and compliance tracking
(define-map compliance-records
  { user: principal }
  {
    verification-status: (string-ascii 20),
    last-audit-date: uint,
    compliance-score: uint,
    flags-count: uint,
    requires-review: bool
  }
)

;; Payment verification entries
(define-map payment-verifications
  { transaction-id: uint }
  {
    verified-by: (optional principal),
    verification-timestamp: uint,
    verification-notes: (string-ascii 100),
    compliance-checked: bool
  }
)

;; Record a new transaction for audit purposes
(define-public (record-transaction 
    (user principal) 
    (transaction-type (string-ascii 20))
    (total-amount uint)
    (savings-amount uint)
    (loan-amount uint) 
    (investment-amount uint)
    (main-amount uint)
    (template-id (optional uint))
    (escrow-id (optional uint)))
  (let
    (
      (new-tx-id (+ (var-get last-transaction-id) u1))
    )
    (asserts! (var-get audit-enabled) err-unauthorized)
    (asserts! (> total-amount u0) err-invalid-parameters)
    
    (var-set last-transaction-id new-tx-id)
    (map-set transaction-records
      { transaction-id: new-tx-id }
      {
        user: user,
        transaction-type: transaction-type,
        total-amount: total-amount,
        timestamp: burn-block-height,
        block-height: burn-block-height,
        template-id: template-id,
        escrow-id: escrow-id,
        distribution: {
          savings-amount: savings-amount,
          loan-amount: loan-amount,
          investment-amount: investment-amount,
          main-amount: main-amount
        }
      }
    )
    
    ;; Update user summary for current period
    (update-user-period-summary user total-amount savings-amount loan-amount investment-amount main-amount)
    (ok new-tx-id)
  )
)

;; Update period summary for user
(define-private (update-user-period-summary (user principal) (total uint) (savings uint) (loans uint) (investments uint) (main uint))
  (let
    (
      (period-key (- burn-block-height (mod burn-block-height u1440))) ;; Daily periods
      (current-summary (default-to
        { total-processed: u0, total-to-savings: u0, total-to-loans: u0, total-to-investments: u0, total-to-main: u0, transaction-count: u0, period-start: period-key, period-end: (+ period-key u1440) }
        (map-get? user-audit-summaries { user: user, period: period-key })
      ))
    )
    (map-set user-audit-summaries
      { user: user, period: period-key }
      {
        total-processed: (+ (get total-processed current-summary) total),
        total-to-savings: (+ (get total-to-savings current-summary) savings),
        total-to-loans: (+ (get total-to-loans current-summary) loans),
        total-to-investments: (+ (get total-to-investments current-summary) investments),
        total-to-main: (+ (get total-to-main current-summary) main),
        transaction-count: (+ (get transaction-count current-summary) u1),
        period-start: period-key,
        period-end: (+ period-key u1440)
      }
    )
    true
  )
)

;; Generate comprehensive audit report
(define-public (generate-audit-report (period-start uint) (period-end uint))
  (let
    (
      (new-report-id (+ (var-get last-report-id) u1))
      (report-period (- period-end period-start))
    )
    (asserts! (< period-start period-end) err-invalid-period)
    (asserts! (<= report-period u43200) err-invalid-period) ;; Max 30 days
    
    (var-set last-report-id new-report-id)
    (map-set audit-reports
      { report-id: new-report-id }
      {
        user: tx-sender,
        report-type: "user-period-summary",
        period-start: period-start,
        period-end: period-end,
        generated-at: burn-block-height,
        total-transactions: u0, ;; Will be calculated
        total-volume: u0, ;; Will be calculated
        summary-data: {
          savings-total: u0,
          loans-total: u0,
          investments-total: u0,
          main-total: u0
        }
      }
    )
    (ok new-report-id)
  )
)

;; Verify transaction compliance
(define-public (verify-transaction (transaction-id uint) (notes (string-ascii 100)))
  (let
    (
      (transaction (unwrap! (map-get? transaction-records { transaction-id: transaction-id }) err-not-found))
    )
    (map-set payment-verifications
      { transaction-id: transaction-id }
      {
        verified-by: (some tx-sender),
        verification-timestamp: burn-block-height,
        verification-notes: notes,
        compliance-checked: true
      }
    )
    (ok true)
  )
)

;; Update compliance status for user
(define-public (update-compliance-status (user principal) (status (string-ascii 20)) (score uint))
  (let
    (
      (current-record (default-to
        { verification-status: "unverified", last-audit-date: u0, compliance-score: u50, flags-count: u0, requires-review: false }
        (map-get? compliance-records { user: user })
      ))
    )
    (asserts! (<= score u100) err-invalid-parameters)
    
    (map-set compliance-records
      { user: user }
      {
        verification-status: status,
        last-audit-date: burn-block-height,
        compliance-score: score,
        flags-count: (get flags-count current-record),
        requires-review: (< score u70)
      }
    )
    (ok true)
  )
)

;; Get user transaction history within date range
(define-read-only (get-user-transactions (user principal) (start-block uint) (end-block uint))
  (ok {
    user: user,
    period-start: start-block,
    period-end: end-block,
    note: "transaction-history-query-processed"
  })
)

;; Get detailed audit summary for user
(define-read-only (get-user-audit-summary (user principal) (period uint))
  (map-get? user-audit-summaries { user: user, period: period })
)

;; Get transaction record details
(define-read-only (get-transaction-record (transaction-id uint))
  (map-get? transaction-records { transaction-id: transaction-id })
)

;; Get payment verification info
(define-read-only (get-payment-verification (transaction-id uint))
  (map-get? payment-verifications { transaction-id: transaction-id })
)

;; Get compliance status
(define-read-only (get-compliance-status (user principal))
  (map-get? compliance-records { user: user })
)

;; Get audit report
(define-read-only (get-audit-report (report-id uint))
  (map-get? audit-reports { report-id: report-id })
)

;; Calculate user financial overview
(define-read-only (get-financial-overview (user principal) (blocks-back uint))
  (let
    (
      (current-block burn-block-height)
      (start-block (- current-block blocks-back))
      (period-key (- start-block (mod start-block u1440)))
      (summary (map-get? user-audit-summaries { user: user, period: period-key }))
    )
    (match summary
      data (ok {
        period-analyzed: blocks-back,
        total-volume: (get total-processed data),
        average-transaction-size: (if (> (get transaction-count data) u0)
                                     (/ (get total-processed data) (get transaction-count data))
                                     u0),
        distribution-breakdown: {
          savings-percentage: (if (> (get total-processed data) u0) 
                                 (/ (* (get total-to-savings data) u100) (get total-processed data)) u0),
          loans-percentage: (if (> (get total-processed data) u0)
                               (/ (* (get total-to-loans data) u100) (get total-processed data)) u0),
          investments-percentage: (if (> (get total-processed data) u0)
                                     (/ (* (get total-to-investments data) u100) (get total-processed data)) u0),
          main-percentage: (if (> (get total-processed data) u0)
                              (/ (* (get total-to-main data) u100) (get total-processed data)) u0)
        }
      })
      (ok {
        period-analyzed: blocks-back,
        total-volume: u0,
        average-transaction-size: u0,
        distribution-breakdown: {
          savings-percentage: u0,
          loans-percentage: u0,
          investments-percentage: u0,
          main-percentage: u0
        }
      })
    )
  )
)

;; Admin functions
(define-public (toggle-audit-system (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) err-unauthorized)
    (var-set audit-enabled enabled)
    (ok true)
  )
)

(define-public (set-retention-period (blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) err-unauthorized)
    (asserts! (> blocks u0) err-invalid-parameters)
    (var-set audit-retention-period blocks)
    (ok true)
  )
)

;; Get system audit statistics
(define-read-only (get-audit-system-stats)
  (ok {
    enabled: (var-get audit-enabled),
    total-transactions: (var-get last-transaction-id),
    total-reports: (var-get last-report-id),
    retention-period: (var-get audit-retention-period),
    max-entries: MAX-AUDIT-ENTRIES
  })
)
