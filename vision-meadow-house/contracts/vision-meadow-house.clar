;; VisionMeadowHouse - Decentralized Performance Rights Management
;; A platform for micro-licensing and real-time royalty distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-percentage (err u104))
(define-constant err-insufficient-payment (err u105))

;; Platform fee (2%)
(define-constant platform-fee u200) ;; 200 basis points = 2%

;; Data Variables
(define-data-var next-content-id uint u1)
(define-data-var total-royalties-paid uint u0)

;; Performance DNA Structure
(define-map performance-dna
  { content-id: uint }
  {
    creator: principal,
    content-hash: (buff 32),
    metadata-uri: (string-ascii 256),
    creation-timestamp: uint,
    license-price-per-use: uint,
    active: bool
  }
)

;; Rights Holders and Revenue Split
(define-map rights-holders
  { content-id: uint, holder: principal }
  { share-percentage: uint } ;; Basis points (10000 = 100%)
)

;; License Records
(define-map licenses
  { license-id: uint }
  {
    content-id: uint,
    licensee: principal,
    usage-type: (string-ascii 50),
    region: (string-ascii 50),
    duration-blocks: uint,
    start-block: uint,
    payment-amount: uint
  }
)

(define-data-var next-license-id uint u1)

;; Usage Tracking
(define-map usage-stats
  { content-id: uint }
  {
    total-licenses: uint,
    total-revenue: uint,
    last-usage-block: uint
  }
)

;; Read-only functions

(define-read-only (get-content-info (content-id uint))
  (map-get? performance-dna { content-id: content-id })
)

(define-read-only (get-rights-holder-share (content-id uint) (holder principal))
  (map-get? rights-holders { content-id: content-id, holder: holder })
)

(define-read-only (get-license-info (license-id uint))
  (map-get? licenses { license-id: license-id })
)

(define-read-only (get-usage-stats (content-id uint))
  (default-to 
    { total-licenses: u0, total-revenue: u0, last-usage-block: u0 }
    (map-get? usage-stats { content-id: content-id })
  )
)

(define-read-only (get-platform-stats)
  {
    total-content: (- (var-get next-content-id) u1),
    total-licenses: (- (var-get next-license-id) u1),
    total-royalties-paid: (var-get total-royalties-paid)
  }
)

;; Public functions

;; Register new Performance DNA
(define-public (register-content 
  (content-hash (buff 32))
  (metadata-uri (string-ascii 256))
  (license-price uint))
  (let
    (
      (content-id (var-get next-content-id))
    )
    ;; Create Performance DNA entry
    (map-set performance-dna
      { content-id: content-id }
      {
        creator: tx-sender,
        content-hash: content-hash,
        metadata-uri: metadata-uri,
        creation-timestamp: block-height,
        license-price-per-use: license-price,
        active: true
      }
    )
    
    ;; Set creator as 100% rights holder initially
    (map-set rights-holders
      { content-id: content-id, holder: tx-sender }
      { share-percentage: u10000 }
    )
    
    ;; Initialize usage stats
    (map-set usage-stats
      { content-id: content-id }
      { total-licenses: u0, total-revenue: u0, last-usage-block: u0 }
    )
    
    ;; Increment content ID
    (var-set next-content-id (+ content-id u1))
    
    (ok content-id)
  )
)

;; Add additional rights holder (for collaborations)
(define-public (add-rights-holder 
  (content-id uint)
  (holder principal)
  (share-percentage uint))
  (let
    (
      (content (unwrap! (map-get? performance-dna { content-id: content-id }) err-not-found))
    )
    ;; Only creator can add rights holders
    (asserts! (is-eq tx-sender (get creator content)) err-unauthorized)
    
    ;; Validate percentage (must be between 1 and 10000)
    (asserts! (and (> share-percentage u0) (<= share-percentage u10000)) err-invalid-percentage)
    
    ;; Add rights holder
    (map-set rights-holders
      { content-id: content-id, holder: holder }
      { share-percentage: share-percentage }
    )
    
    (ok true)
  )
)

;; Purchase micro-license
(define-public (purchase-license
  (content-id uint)
  (usage-type (string-ascii 50))
  (region (string-ascii 50))
  (duration-blocks uint))
  (let
    (
      (content (unwrap! (map-get? performance-dna { content-id: content-id }) err-not-found))
      (license-price (get license-price-per-use content))
      (license-id (var-get next-license-id))
      (stats (get-usage-stats content-id))
    )
    ;; Verify content is active
    (asserts! (get active content) err-unauthorized)
    
    ;; Transfer payment and distribute royalties
    (try! (distribute-royalties content-id license-price))
    
    ;; Create license record
    (map-set licenses
      { license-id: license-id }
      {
        content-id: content-id,
        licensee: tx-sender,
        usage-type: usage-type,
        region: region,
        duration-blocks: duration-blocks,
        start-block: block-height,
        payment-amount: license-price
      }
    )
    
    ;; Update usage statistics
    (map-set usage-stats
      { content-id: content-id }
      {
        total-licenses: (+ (get total-licenses stats) u1),
        total-revenue: (+ (get total-revenue stats) license-price),
        last-usage-block: block-height
      }
    )
    
    ;; Increment license ID
    (var-set next-license-id (+ license-id u1))
    
    (ok license-id)
  )
)

;; Private functions

;; Distribute royalties to rights holders
(define-private (distribute-royalties (content-id uint) (amount uint))
  (let
    (
      (content (unwrap! (map-get? performance-dna { content-id: content-id }) err-not-found))
      (platform-cut (/ (* amount platform-fee) u10000))
      (creator-amount (- amount platform-cut))
    )
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? platform-cut tx-sender contract-owner))
    
    ;; Transfer royalty to creator (simplified - in production would split among all rights holders)
    (try! (stx-transfer? creator-amount tx-sender (get creator content)))
    
    ;; Update total royalties paid
    (var-set total-royalties-paid (+ (var-get total-royalties-paid) amount))
    
    (ok true)
  )
)

;; Update content status
(define-public (toggle-content-status (content-id uint))
  (let
    (
      (content (unwrap! (map-get? performance-dna { content-id: content-id }) err-not-found))
    )
    ;; Only creator can toggle status
    (asserts! (is-eq tx-sender (get creator content)) err-unauthorized)
    
    (map-set performance-dna
      { content-id: content-id }
      (merge content { active: (not (get active content)) })
    )
    
    (ok true)
  )
)

;; Update license price
(define-public (update-license-price (content-id uint) (new-price uint))
  (let
    (
      (content (unwrap! (map-get? performance-dna { content-id: content-id }) err-not-found))
    )
    ;; Only creator can update price
    (asserts! (is-eq tx-sender (get creator content)) err-unauthorized)
    
    (map-set performance-dna
      { content-id: content-id }
      (merge content { license-price-per-use: new-price })
    )
    
    (ok true)
  )
)