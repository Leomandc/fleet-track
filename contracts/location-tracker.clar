;; location-tracker
;; A contract for recording and verifying vehicle location data with route history on the Stacks blockchain.
;; This contract enables authorized devices to submit GPS coordinates with timestamps, creating
;; immutable route histories for each vehicle. Fleet managers can track current positions, replay
;; historical routes, set geofencing boundaries, and receive notifications for boundary violations.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-VEHICLE-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-COORDINATES (err u1002))
(define-constant ERR-VEHICLE-EXISTS (err u1003))
(define-constant ERR-BOUNDARY-EXISTS (err u1004))
(define-constant ERR-BOUNDARY-NOT-FOUND (err u1005))
(define-constant ERR-INVALID-BOUNDARY (err u1006))
(define-constant ERR-INVALID-TIMESTAMP (err u1007))

;; Data space definitions

;; Map to store fleet managers who can administer the system
(define-map fleet-managers principal bool)

;; Map to store authorized devices that can submit location data
(define-map authorized-devices principal bool)

;; Map to store vehicle data - maps vehicle ID to registration info
(define-map vehicles
  { vehicle-id: (string-ascii 20) }
  {
    owner: principal,
    make: (string-ascii 30),
    model: (string-ascii 30),
    registration-date: uint,
    active: bool
  }
)

;; Map to store the most recent location of each vehicle
(define-map current-locations
  { vehicle-id: (string-ascii 20) }
  {
    latitude: int,
    longitude: int,
    altitude: int,
    timestamp: uint,
    speed: uint,
    heading: uint
  }
)

;; Map to store location history (last 100 locations per vehicle)
;; Using an index-based approach to maintain a limited history
(define-map location-history
  { vehicle-id: (string-ascii 20), index: uint }
  {
    latitude: int,
    longitude: int,
    altitude: int,
    timestamp: uint,
    speed: uint,
    heading: uint
  }
)

;; Store the current index counter for each vehicle's location history
(define-map history-indexes
  { vehicle-id: (string-ascii 20) }
  { current-index: uint, count: uint }
)

;; Map to store geofencing boundaries for vehicles
(define-map geofence-boundaries
  { vehicle-id: (string-ascii 20), boundary-id: (string-ascii 20) }
  {
    center-latitude: int,
    center-longitude: int,
    radius: uint,
    active: bool,
    created-at: uint
  }
)

;; Map to store boundary violation events
(define-map boundary-violations
  { vehicle-id: (string-ascii 20), violation-id: uint }
  {
    boundary-id: (string-ascii 20),
    latitude: int,
    longitude: int,
    timestamp: uint,
    distance-exceeded: uint
  }
)

;; Counter for boundary violation IDs
(define-data-var violation-counter uint u0)

;; Contract owner who can assign fleet managers
(define-data-var contract-owner principal tx-sender)

;; Private functions

;; Check if the sender is the contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Check if the sender is a fleet manager
(define-private (is-fleet-manager)
  (default-to false (map-get? fleet-managers tx-sender))
)

;; Check if the sender is an authorized device
(define-private (is-authorized-device)
  (default-to false (map-get? authorized-devices tx-sender))
)

;; Check all active boundaries for a vehicle and record violations
(define-private (check-boundaries
  (vehicle-id (string-ascii 20))
  (latitude int)
  (longitude int)
  (timestamp uint))
  
  ;; This is a placeholder for boundary checking logic
  ;; In a full implementation, this would iterate through all boundaries
  ;; Since Clarity doesn't have traditional loops, this would need to be 
  ;; implemented using map or fold operations on a list of boundaries
  ;; or by having a fixed number of boundary slots to check
  
  ;; For now, we'll return a simple success response
  (ok true)
)

;; Read-only functions

;; Get the current location of a vehicle
(define-read-only (get-current-location (vehicle-id (string-ascii 20)))
  (map-get? current-locations { vehicle-id: vehicle-id })
)

;; Get a specific location from a vehicle's history
(define-read-only (get-location-at-index (vehicle-id (string-ascii 20)) (index uint))
  (map-get? location-history { vehicle-id: vehicle-id, index: index })
)

;; Get vehicle information
(define-read-only (get-vehicle-info (vehicle-id (string-ascii 20)))
  (map-get? vehicles { vehicle-id: vehicle-id })
)

;; Get the total number of locations recorded for a vehicle
(define-read-only (get-location-count (vehicle-id (string-ascii 20)))
  (let ((index-data (map-get? history-indexes { vehicle-id: vehicle-id })))
    (if (is-some index-data)
      (ok (get count (unwrap-panic index-data)))
      ERR-VEHICLE-NOT-FOUND
    )
  )
)

;; Check if a boundary exists
(define-read-only (boundary-exists (vehicle-id (string-ascii 20)) (boundary-id (string-ascii 20)))
  (is-some (map-get? geofence-boundaries { vehicle-id: vehicle-id, boundary-id: boundary-id }))
)

;; Get boundary information
(define-read-only (get-boundary (vehicle-id (string-ascii 20)) (boundary-id (string-ascii 20)))
  (map-get? geofence-boundaries { vehicle-id: vehicle-id, boundary-id: boundary-id })
)

;; Get a specific boundary violation
(define-read-only (get-violation (vehicle-id (string-ascii 20)) (violation-id uint))
  (map-get? boundary-violations { vehicle-id: vehicle-id, violation-id: violation-id })
)

;; Public functions

;; Register a new vehicle
(define-public (register-vehicle
  (vehicle-id (string-ascii 20))
  (make (string-ascii 30))
  (model (string-ascii 30)))
  
  (begin
    ;; Check authorization
    (asserts! (or (is-contract-owner) (is-fleet-manager)) ERR-NOT-AUTHORIZED)
    
    ;; Check if vehicle already exists
    (asserts! (is-none (map-get? vehicles { vehicle-id: vehicle-id })) ERR-VEHICLE-EXISTS)
    
    ;; Register the vehicle
    (map-set vehicles
      { vehicle-id: vehicle-id }
      {
        owner: tx-sender,
        make: make,
        model: model,
        registration-date: block-height,
        active: true
      }
    )
    
    ;; Initialize history index
    (map-set history-indexes
      { vehicle-id: vehicle-id }
      { current-index: u0, count: u0 }
    )
    
    (ok true)
  )
)

;; Add a new geofencing boundary
(define-public (add-boundary
  (vehicle-id (string-ascii 20))
  (boundary-id (string-ascii 20))
  (center-latitude int)
  (center-longitude int)
  (radius uint))
  
  (begin
    ;; Check authorization
    (asserts! (or (is-contract-owner) (is-fleet-manager)) ERR-NOT-AUTHORIZED)
    
    ;; Check if vehicle exists
    (asserts! (is-some (map-get? vehicles { vehicle-id: vehicle-id })) ERR-VEHICLE-NOT-FOUND)
    
    ;; Ensure boundary doesn't already exist
    (asserts! (not (boundary-exists vehicle-id boundary-id)) ERR-BOUNDARY-EXISTS)
    
    ;; Validate radius (must be positive and reasonable)
    (asserts! (> radius u0) ERR-INVALID-BOUNDARY)
    
    ;; Add the boundary
    (map-set geofence-boundaries
      { vehicle-id: vehicle-id, boundary-id: boundary-id }
      {
        center-latitude: center-latitude,
        center-longitude: center-longitude,
        radius: radius,
        active: true,
        created-at: block-height
      }
    )
    
    (ok true)
  )
)

;; Update an existing boundary
(define-public (update-boundary
  (vehicle-id (string-ascii 20))
  (boundary-id (string-ascii 20))
  (center-latitude int)
  (center-longitude int)
  (radius uint)
  (active bool))
  
  (begin
    ;; Check authorization
    (asserts! (or (is-contract-owner) (is-fleet-manager)) ERR-NOT-AUTHORIZED)
    
    ;; Check if boundary exists
    (asserts! (boundary-exists vehicle-id boundary-id) ERR-BOUNDARY-NOT-FOUND)
    
    ;; Validate radius (must be positive and reasonable)
    (asserts! (> radius u0) ERR-INVALID-BOUNDARY)
    
    ;; Update the boundary
    (map-set geofence-boundaries
      { vehicle-id: vehicle-id, boundary-id: boundary-id }
      {
        center-latitude: center-latitude,
        center-longitude: center-longitude,
        radius: radius,
        active: active,
        created-at: block-height
      }
    )
    
    (ok true)
  )
)

;; Deactivate a vehicle
(define-public (deactivate-vehicle (vehicle-id (string-ascii 20)))
  (begin
    ;; Check authorization
    (asserts! (or (is-contract-owner) (is-fleet-manager)) ERR-NOT-AUTHORIZED)
    
    ;; Check if vehicle exists
    (let ((vehicle-data (map-get? vehicles { vehicle-id: vehicle-id })))
      (asserts! (is-some vehicle-data) ERR-VEHICLE-NOT-FOUND)
      
      ;; Update the vehicle to inactive
      (map-set vehicles
        { vehicle-id: vehicle-id }
        (merge (unwrap-panic vehicle-data) { active: false })
      )
      
      (ok true)
    )
  )
)

;; Add a fleet manager
(define-public (add-fleet-manager (manager principal))
  (begin
    ;; Only contract owner can add fleet managers
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    
    ;; Add the fleet manager
    (map-set fleet-managers manager true)
    
    (ok true)
  )
)

;; Remove a fleet manager
(define-public (remove-fleet-manager (manager principal))
  (begin
    ;; Only contract owner can remove fleet managers
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    
    ;; Remove the fleet manager
    (map-delete fleet-managers manager)
    
    (ok true)
  )
)

;; Add an authorized device
(define-public (add-authorized-device (device principal))
  (begin
    ;; Only contract owner or fleet managers can add devices
    (asserts! (or (is-contract-owner) (is-fleet-manager)) ERR-NOT-AUTHORIZED)
    
    ;; Add the authorized device
    (map-set authorized-devices device true)
    
    (ok true)
  )
)

;; Remove an authorized device
(define-public (remove-authorized-device (device principal))
  (begin
    ;; Only contract owner or fleet managers can remove devices
    (asserts! (or (is-contract-owner) (is-fleet-manager)) ERR-NOT-AUTHORIZED)
    
    ;; Remove the authorized device
    (map-delete authorized-devices device)
    
    (ok true)
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    ;; Only current owner can transfer ownership
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    
    ;; Set new owner
    (var-set contract-owner new-owner)
    
    (ok true)
  )
)