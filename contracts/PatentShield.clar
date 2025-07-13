;; PatentShield: Decentralized intellectual property registry platform
;; Enables inventors to register patents, acquire licenses, and get examiner approval

(define-data-var patent-examiner principal tx-sender)

(define-map patent-database
  { patent-id: uint }
  {
    inventor: principal,
    licensing-fee: uint,
    invention-title: (string-ascii 50),
    technical-specs: (string-ascii 500),
    protection-years: uint,
    approved: bool
  }
)

(define-map licensing-history
  { patent-id: uint, history-id: uint }
  {
    licensee: principal,
    license-date: uint,
    status: (string-ascii 20)
  }
)

(define-data-var next-patent-id uint u1)

(define-map history-tracker
  { patent-id: uint }
  { records: uint }
)

;; Register new patent application
(define-public (register-patent (title-input (string-ascii 50)) (specs-input (string-ascii 500)) (years-input uint) (fee-input uint))
  (let
    (
      (patent-id (var-get next-patent-id))
      (history-id u0)
      (title title-input)
      (specs specs-input)
      (years years-input)
      (fee fee-input)
    )
    ;; Input validation
    (asserts! (> fee u0) (err u1))
    (asserts! (> (len title) u0) (err u5))
    (asserts! (> (len specs) u0) (err u6))
    (asserts! (> years u0) (err u7))
    
    (map-set patent-database
      { patent-id: patent-id }
      {
        inventor: tx-sender,
        licensing-fee: fee,
        invention-title: title,
        technical-specs: specs,
        protection-years: years,
        approved: false
      }
    )
    (map-set licensing-history
      { patent-id: patent-id, history-id: history-id }
      {
        licensee: tx-sender,
        license-date: patent-id,
        status: "filed"
      }
    )
    (map-set history-tracker
      { patent-id: patent-id }
      { records: u1 }
    )
    (var-set next-patent-id (+ patent-id u1))
    (ok patent-id)
  )
)

;; Acquire patent license
(define-public (acquire-license (patent-id-input uint))
  (let
    (
      (patent-id patent-id-input)
      (patent-info (unwrap! (map-get? patent-database { patent-id: patent-id }) (err u2)))
      (fee (get licensing-fee patent-info))
      (inventor (get inventor patent-info))
      (history-data (default-to { records: u0 } (map-get? history-tracker { patent-id: patent-id })))
      (history-id (get records history-data))
      (new-history-id (+ history-id u1))
    )
    ;; Input validation
    (asserts! (> patent-id u0) (err u8))
    (asserts! (not (is-eq tx-sender inventor)) (err u3))
    
    (try! (stx-transfer? fee tx-sender inventor))
    (map-set licensing-history
      { patent-id: patent-id, history-id: history-id }
      {
        licensee: tx-sender,
        license-date: (var-get next-patent-id),
        status: "licensed"
      }
    )
    (map-set history-tracker
      { patent-id: patent-id }
      { records: new-history-id }
    )
    (ok true)
  )
)

;; Approve patent application (examiner only)
(define-public (approve-patent (patent-id-input uint))
  (let
    (
      (patent-id patent-id-input)
      (patent-info (unwrap! (map-get? patent-database { patent-id: patent-id }) (err u2)))
      (history-data (default-to { records: u0 } (map-get? history-tracker { patent-id: patent-id })))
      (history-id (get records history-data))
      (new-history-id (+ history-id u1))
    )
    ;; Input validation
    (asserts! (> patent-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get patent-examiner)) (err u4))
    
    (map-set patent-database
      { patent-id: patent-id }
      (merge patent-info { approved: true })
    )
    (map-set licensing-history
      { patent-id: patent-id, history-id: history-id }
      {
        licensee: (get inventor patent-info),
        license-date: (var-get next-patent-id),
        status: "approved"
      }
    )
    (map-set history-tracker
      { patent-id: patent-id }
      { records: new-history-id }
    )
    (ok true)
  )
)

;; Get patent details
(define-read-only (get-patent (patent-id uint))
  (map-get? patent-database { patent-id: patent-id })
)

;; Get licensing history entry
(define-read-only (get-licensing-history (patent-id uint) (history-id uint))
  (map-get? licensing-history { patent-id: patent-id, history-id: history-id })
)

;; Get total licensing records
(define-read-only (get-licensing-count (patent-id uint))
  (let
    (
      (history-data (default-to { records: u0 } (map-get? history-tracker { patent-id: patent-id })))
    )
    (get records history-data)
  )
)