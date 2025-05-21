;; fleet-registry
;; This contract provides the core foundation for a fleet management system, 
;; allowing fleet owners to register vehicles, manage ownership, and organize 
;; vehicles into logical fleet groups with transparent and immutable record-keeping.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-VEHICLE-EXISTS (err u101))
(define-constant ERR-VEHICLE-NOT-FOUND (err u102))
(define-constant ERR-FLEET-EXISTS (err u103))
(define-constant ERR-FLEET-NOT-FOUND (err u104))
(define-constant ERR-NOT-OWNER (err u105))
(define-constant ERR-ALREADY-IN-FLEET (err u106))
(define-constant ERR-NOT-IN-FLEET (err u107))
(define-constant ERR-INVALID-PARAMS (err u108))

;; Data maps and variables

;; Vehicle structure with detailed specifications
(define-map vehicles
  { vehicle-id: (string-ascii 20) }
  {
    owner: principal,
    make: (string-ascii 50),
    model: (string-ascii 50),
    year: uint,
    capacity: uint,
    registration-date: uint,
    active: bool
  }
)

;; Fleet groups to organize vehicles
(define-map fleets
  { fleet-id: (string-ascii 20) }
  {
    owner: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    created-at: uint
  }
)

;; Map to track which vehicles belong to which fleets
(define-map fleet-memberships
  { fleet-id: (string-ascii 20), vehicle-id: (string-ascii 20) }
  { joined-at: uint }
)

;; Map to track ownership history of vehicles
(define-map vehicle-ownership-history
  { vehicle-id: (string-ascii 20), tx-id: uint }
  {
    previous-owner: (optional principal),
    new-owner: principal,
    transfer-date: uint
  }
)

;; Counter for ownership history transactions
(define-data-var ownership-tx-counter uint u0)

;; Private functions

;; Get the current block time for timestamp usage
(define-private (get-current-time)
  block-height
)

;; Check if the caller is the owner of a vehicle
(define-private (is-vehicle-owner (vehicle-id (string-ascii 20)) (caller principal))
  (match (map-get? vehicles { vehicle-id: vehicle-id })
    vehicle (is-eq (get owner vehicle) caller)
    false
  )
)

;; Check if the caller is the owner of a fleet
(define-private (is-fleet-owner (fleet-id (string-ascii 20)) (caller principal))
  (match (map-get? fleets { fleet-id: fleet-id })
    fleet (is-eq (get owner fleet) caller)
    false
  )
)

;; Generate a new transaction ID for ownership history
(define-private (generate-tx-id)
  (let ((current-id (var-get ownership-tx-counter)))
    (var-set ownership-tx-counter (+ current-id u1))
    current-id
  )
)

;; Add a new ownership record to the history
(define-private (record-ownership-change 
                (vehicle-id (string-ascii 20)) 
                (previous-owner (optional principal)) 
                (new-owner principal))
  (let ((tx-id (generate-tx-id)))
    (map-set vehicle-ownership-history
      { vehicle-id: vehicle-id, tx-id: tx-id }
      {
        previous-owner: previous-owner,
        new-owner: new-owner,
        transfer-date: (get-current-time)
      }
    )
  )
)

;; Read-only functions

;; Get vehicle details by ID
(define-read-only (get-vehicle-details (vehicle-id (string-ascii 20)))
  (match (map-get? vehicles { vehicle-id: vehicle-id })
    vehicle (ok vehicle)
    ERR-VEHICLE-NOT-FOUND
  )
)

;; Get fleet details by ID
(define-read-only (get-fleet-details (fleet-id (string-ascii 20)))
  (match (map-get? fleets { fleet-id: fleet-id })
    fleet (ok fleet)
    ERR-FLEET-NOT-FOUND
  )
)

;; Check if a vehicle is part of a specific fleet
(define-read-only (is-vehicle-in-fleet (fleet-id (string-ascii 20)) (vehicle-id (string-ascii 20)))
  (is-some (map-get? fleet-memberships { fleet-id: fleet-id, vehicle-id: vehicle-id }))
)

;; Check if a vehicle exists
(define-read-only (vehicle-exists (vehicle-id (string-ascii 20)))
  (is-some (map-get? vehicles { vehicle-id: vehicle-id }))
)

;; Check if a fleet exists
(define-read-only (fleet-exists (fleet-id (string-ascii 20)))
  (is-some (map-get? fleets { fleet-id: fleet-id }))
)

;; Public functions

;; Register a new vehicle with detailed specifications
(define-public (register-vehicle 
               (vehicle-id (string-ascii 20)) 
               (make (string-ascii 50)) 
               (model (string-ascii 50)) 
               (year uint) 
               (capacity uint))
  (let ((caller tx-sender))
    ;; Check if vehicle already exists
    (asserts! (not (vehicle-exists vehicle-id)) ERR-VEHICLE-EXISTS)
    
    ;; Validate input parameters
    (asserts! (and (> (len vehicle-id) u0) 
                  (> (len make) u0) 
                  (> (len model) u0) 
                  (> year u0) 
                  (> capacity u0)) 
              ERR-INVALID-PARAMS)
    
    ;; Register the vehicle
    (map-set vehicles
      { vehicle-id: vehicle-id }
      {
        owner: caller,
        make: make,
        model: model,
        year: year,
        capacity: capacity,
        registration-date: (get-current-time),
        active: true
      }
    )
    
    ;; Record initial ownership
    (record-ownership-change vehicle-id none caller)
    
    (ok true)
  )
)

;; Create a new fleet group
(define-public (create-fleet (fleet-id (string-ascii 20)) (name (string-ascii 100)) (description (string-ascii 500)))
  (let ((caller tx-sender))
    ;; Check if fleet already exists
    (asserts! (not (fleet-exists fleet-id)) ERR-FLEET-EXISTS)
    
    ;; Validate input parameters
    (asserts! (and (> (len fleet-id) u0) 
                  (> (len name) u0)) 
              ERR-INVALID-PARAMS)
    
    ;; Create the fleet
    (map-set fleets
      { fleet-id: fleet-id }
      {
        owner: caller,
        name: name,
        description: description,
        created-at: (get-current-time)
      }
    )
    
    (ok true)
  )
)

;; Add a vehicle to a fleet
(define-public (add-vehicle-to-fleet (fleet-id (string-ascii 20)) (vehicle-id (string-ascii 20)))
  (let ((caller tx-sender))
    ;; Check if fleet and vehicle exist
    (asserts! (fleet-exists fleet-id) ERR-FLEET-NOT-FOUND)
    (asserts! (vehicle-exists vehicle-id) ERR-VEHICLE-NOT-FOUND)
    
    ;; Check if caller is the fleet owner
    (asserts! (is-fleet-owner fleet-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Check if vehicle is already in the fleet
    (asserts! (not (is-vehicle-in-fleet fleet-id vehicle-id)) ERR-ALREADY-IN-FLEET)
    
    ;; Add vehicle to fleet
    (map-set fleet-memberships
      { fleet-id: fleet-id, vehicle-id: vehicle-id }
      { joined-at: (get-current-time) }
    )
    
    (ok true)
  )
)

;; Remove a vehicle from a fleet
(define-public (remove-vehicle-from-fleet (fleet-id (string-ascii 20)) (vehicle-id (string-ascii 20)))
  (let ((caller tx-sender))
    ;; Check if fleet and vehicle exist
    (asserts! (fleet-exists fleet-id) ERR-FLEET-NOT-FOUND)
    (asserts! (vehicle-exists vehicle-id) ERR-VEHICLE-NOT-FOUND)
    
    ;; Check if caller is the fleet owner
    (asserts! (is-fleet-owner fleet-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Check if vehicle is in the fleet
    (asserts! (is-vehicle-in-fleet fleet-id vehicle-id) ERR-NOT-IN-FLEET)
    
    ;; Remove vehicle from fleet
    (map-delete fleet-memberships { fleet-id: fleet-id, vehicle-id: vehicle-id })
    
    (ok true)
  )
)

;; Transfer vehicle ownership
(define-public (transfer-vehicle (vehicle-id (string-ascii 20)) (new-owner principal))
  (let ((caller tx-sender))
    ;; Check if vehicle exists
    (asserts! (vehicle-exists vehicle-id) ERR-VEHICLE-NOT-FOUND)
    
    ;; Check if caller is the current owner
    (asserts! (is-vehicle-owner vehicle-id caller) ERR-NOT-OWNER)
    
    ;; Transfer ownership
    (match (map-get? vehicles { vehicle-id: vehicle-id })
      vehicle 
      (begin
        (map-set vehicles
          { vehicle-id: vehicle-id }
          (merge vehicle { owner: new-owner })
        )
        
        ;; Record ownership change
        (record-ownership-change vehicle-id (some caller) new-owner)
        
        (ok true)
      )
      ERR-VEHICLE-NOT-FOUND
    )
  )
)

;; Update vehicle status (active/inactive)
(define-public (update-vehicle-status (vehicle-id (string-ascii 20)) (active bool))
  (let ((caller tx-sender))
    ;; Check if vehicle exists
    (asserts! (vehicle-exists vehicle-id) ERR-VEHICLE-NOT-FOUND)
    
    ;; Check if caller is the current owner
    (asserts! (is-vehicle-owner vehicle-id caller) ERR-NOT-OWNER)
    
    ;; Update status
    (match (map-get? vehicles { vehicle-id: vehicle-id })
      vehicle 
      (begin
        (map-set vehicles
          { vehicle-id: vehicle-id }
          (merge vehicle { active: active })
        )
        
        (ok true)
      )
      ERR-VEHICLE-NOT-FOUND
    )
  )
)

;; Transfer fleet ownership
(define-public (transfer-fleet (fleet-id (string-ascii 20)) (new-owner principal))
  (let ((caller tx-sender))
    ;; Check if fleet exists
    (asserts! (fleet-exists fleet-id) ERR-FLEET-NOT-FOUND)
    
    ;; Check if caller is the current owner
    (asserts! (is-fleet-owner fleet-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Transfer ownership
    (match (map-get? fleets { fleet-id: fleet-id })
      fleet 
      (begin
        (map-set fleets
          { fleet-id: fleet-id }
          (merge fleet { owner: new-owner })
        )
        
        (ok true)
      )
      ERR-FLEET-NOT-FOUND
    )
  )
)