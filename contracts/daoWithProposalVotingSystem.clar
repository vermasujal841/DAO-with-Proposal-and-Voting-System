(define-map proposals
  uint
  { proposer: principal, description: (string-ascii 100), votes-for: uint, votes-against: uint })

(define-map has-voted
  {proposal-id: uint, voter: principal}
  bool)

(define-data-var proposal-count uint u0)

;; Create a proposal
(define-public (create-proposal (description (string-ascii 100)))
  (let ((id (var-get proposal-count)))
    (begin
      (map-set proposals id
               { proposer: tx-sender, description: description, votes-for: u0, votes-against: u0 })
      (var-set proposal-count (+ id u1))
      (ok id))))

;; Vote on a proposal (true for yes, false for no)
(define-public (vote (proposal-id uint) (support bool))
  (begin
    (asserts! (is-none (map-get? has-voted {proposal-id: proposal-id, voter: tx-sender})) (err u101))
    (let ((proposal (map-get? proposals proposal-id)))
      (match proposal data
        (begin
          (if support
              (map-set proposals proposal-id
                       { proposer: (get proposer data),
                         description: (get description data),
                         votes-for: (+ (get votes-for data) u1),
                         votes-against: (get votes-against data) })
              (map-set proposals proposal-id
                       { proposer: (get proposer data),
                         description: (get description data),
                         votes-for: (get votes-for data),
                         votes-against: (+ (get votes-against data) u1) }))
          (map-set has-voted {proposal-id: proposal-id, voter: tx-sender} true)
          (ok true))
        (err u102)))))

;; View a proposal by ID
(define-read-only (get-proposal (id uint))
  (ok (map-get? proposals id)))