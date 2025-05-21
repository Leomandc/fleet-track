;; maintenance-logger
;; 
;; A contract to manage the complete maintenance lifecycle for fleet vehicles.
;; This includes scheduling maintenance based on time or mileage, logging service 
;; completions with verification from service providers, tracking parts replacement,
;; and generating maintenance alerts. The immutable service history creates a trusted 
;; record for compliance, accountability, and enhanced vehicle resale value.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-UNKNOWN-VEHICLE (err u101))
(define-constant ERR-UNKNOWN-MAINTENANCE-TASK (err u102))
(define-constant ERR-UNKNOWN-SERVICE-PROVIDER (err u103))
(define-constant ERR-INVALID-MILEAGE (err u104))
(define-constant ERR-MAINTENANCE-ALREADY-COMPLETED (err u105))
(define-constant ERR-MAINTENANCE-NOT-DUE (err u106))
(define-constant ERR-INVALID-TIMESTAMP (err u107))
(define-constant ERR-INVALID-TASK-INTERVAL (err u108))
(define-constant ERR-INVALID-PARTS-DATA (err u109))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data Maps

;; Stores information about fleet vehicles
(define-map vehicles
  { vehicle-id: (string-ascii 20) }
  {
    owner: principal,
    make: (string-ascii 50),
    model: (string-ascii 50),
    year: uint,
    current-mileage: uint,
    last-update-timestamp: uint,
    active: bool
  }
)

;; Stores information about maintenance service providers
(define-map service-providers
  { provider-id: (string-ascii 20) }
  {
    name: (string-ascii 100),
    address: (string-ascii 200),
    provider-principal: principal,
    authorized: bool
  }
)

;; Defines maintenance task types
(define-map maintenance-task-types
  { task-type-id: (string-ascii 20) }
  {
    name: (string-ascii 100),
    description: (string-utf8 500),
    mileage-interval: uint,  ;; How often task should be performed by mileage
    time-interval: uint,     ;; How often task should be performed by time (in days)
    priority: uint           ;; 1 = highest priority, 10 = lowest
  }
)

;; Stores scheduled maintenance tasks
(define-map scheduled-maintenance
  { vehicle-id: (string-ascii 20), task-id: uint }
  {
    task-type-id: (string-ascii 20),
    scheduled-date: uint,    ;; Unix timestamp
    scheduled-mileage: uint,
    status: (string-ascii 20), ;; "scheduled", "completed", "overdue", "cancelled"
    notes: (optional (string-utf8 500))
  }
)

;; Stores completed maintenance records
(define-map maintenance-records
  { vehicle-id: (string-ascii 20), record-id: uint }
  {
    task-id: uint,
    task-type-id: (string-ascii 20),
    completion-date: uint,    ;; Unix timestamp
    completion-mileage: uint,
    service-provider-id: (string-ascii 20),
    technician-name: (string-ascii 100),
    notes: (optional (string-utf8 500)),
    cost: uint,
    verification-hash: (optional (buff 32))  ;; Hash of any supporting documents
  }
)

;; Tracks parts replaced during maintenance
(define-map parts-replaced
  { record-id: uint, part-index: uint }
  {
    part-name: (string-ascii 100),
    part-number: (string-ascii 50),
    quantity: uint,
    cost-per-unit: uint
  }
)

;; Counters for IDs
(define-data-var next-task-id uint u1)
(define-data-var next-record-id uint u1)

;; Private Functions

;; Helper function to check if a vehicle exists
(define-private (vehicle-exists (vehicle-id (string-ascii 20)))
  (is-some (map-get? vehicles { vehicle-id: vehicle-id }))
)

;; Helper function to check if caller is vehicle owner
(define-private (is-vehicle-owner (vehicle-id (string-ascii 20)))
  (let ((vehicle-info (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) false)))
    (is-eq tx-sender (get owner vehicle-info))
  )
)

;; Helper function to check if a service provider exists and is authorized
(define-private (is-authorized-service-provider (provider-id (string-ascii 20)))
  (let ((provider (unwrap! (map-get? service-providers { provider-id: provider-id }) false)))
    (and 
      (is-eq tx-sender (get provider-principal provider))
      (get authorized provider)
    )
  )
)

;; Helper function to check if a maintenance task type exists
(define-private (task-type-exists (task-type-id (string-ascii 20)))
  (is-some (map-get? maintenance-task-types { task-type-id: task-type-id }))
)

;; Helper function to check if a scheduled maintenance exists
(define-private (scheduled-maintenance-exists (vehicle-id (string-ascii 20)) (task-id uint))
  (is-some (map-get? scheduled-maintenance { vehicle-id: vehicle-id, task-id: task-id }))
)

;; Helper function to check if maintenance is already completed
(define-private (is-maintenance-completed (vehicle-id (string-ascii 20)) (task-id uint))
  (let ((task (unwrap! (map-get? scheduled-maintenance { vehicle-id: vehicle-id, task-id: task-id }) false)))
    (is-eq (get status task) "completed")
  )
)

;; Helper function to get the next task ID and increment the counter
(define-private (get-and-increment-task-id)
  (let ((current-id (var-get next-task-id)))
    (var-set next-task-id (+ current-id u1))
    current-id
  )
)

;; Helper function to get the next record ID and increment the counter
(define-private (get-and-increment-record-id)
  (let ((current-id (var-get next-record-id)))
    (var-set next-record-id (+ current-id u1))
    current-id
  )
)

;; Read-only Functions

;; Get vehicle information
(define-read-only (get-vehicle-info (vehicle-id (string-ascii 20)))
  (map-get? vehicles { vehicle-id: vehicle-id })
)

;; Get service provider information
(define-read-only (get-service-provider (provider-id (string-ascii 20)))
  (map-get? service-providers { provider-id: provider-id })
)

;; Get maintenance task type details
(define-read-only (get-maintenance-task-type (task-type-id (string-ascii 20)))
  (map-get? maintenance-task-types { task-type-id: task-type-id })
)

;; Get scheduled maintenance details
(define-read-only (get-scheduled-maintenance (vehicle-id (string-ascii 20)) (task-id uint))
  (map-get? scheduled-maintenance { vehicle-id: vehicle-id, task-id: task-id })
)

;; Get maintenance record details
(define-read-only (get-maintenance-record (vehicle-id (string-ascii 20)) (record-id uint))
  (map-get? maintenance-records { vehicle-id: vehicle-id, record-id: record-id })
)

;; Get part replacement details for a maintenance record
(define-read-only (get-part-replacement (record-id uint) (part-index uint))
  (map-get? parts-replaced { record-id: record-id, part-index: part-index })
)

;; Public Functions

;; Register a service provider
(define-public (register-service-provider
  (provider-id (string-ascii 20))
  (name (string-ascii 100))
  (address (string-ascii 200))
  (provider-principal principal)
)
  (begin
    ;; Check authorization 
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Set the provider
    (map-set service-providers
      { provider-id: provider-id }
      {
        name: name,
        address: address,
        provider-principal: provider-principal,
        authorized: true
      }
    )
    
    (ok true)
  )
)

;; Define a maintenance task type
(define-public (define-maintenance-task-type
  (task-type-id (string-ascii 20))
  (name (string-ascii 100))
  (description (string-utf8 500))
  (mileage-interval uint)
  (time-interval uint)
  (priority uint)
)
  (begin
    ;; Check authorization
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Validate inputs
    (asserts! (and (> mileage-interval u0) (> time-interval u0)) ERR-INVALID-TASK-INTERVAL)
    (asserts! (and (>= priority u1) (<= priority u10)) ERR-INVALID-TASK-INTERVAL)
    
    ;; Set the task type
    (map-set maintenance-task-types
      { task-type-id: task-type-id }
      {
        name: name,
        description: description,
        mileage-interval: mileage-interval,
        time-interval: time-interval,
        priority: priority
      }
    )
    
    (ok true)
  )
)

;; Cancel scheduled maintenance
(define-public (cancel-scheduled-maintenance (vehicle-id (string-ascii 20)) (task-id uint))
  (let (
    (task (unwrap! (map-get? scheduled-maintenance { vehicle-id: vehicle-id, task-id: task-id }) ERR-UNKNOWN-MAINTENANCE-TASK))
  )
    ;; Check authorization and prerequisites
    (asserts! (is-vehicle-owner vehicle-id) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-maintenance-completed vehicle-id task-id)) ERR-MAINTENANCE-ALREADY-COMPLETED)
    
    ;; Update the status to cancelled
    (map-set scheduled-maintenance
      { vehicle-id: vehicle-id, task-id: task-id }
      (merge task { status: "cancelled" })
    )
    
    (ok true)
  )
)

;; Authorize or deauthorize a service provider
(define-public (set-service-provider-authorization (provider-id (string-ascii 20)) (authorized bool))
  (let (
    (provider (unwrap! (map-get? service-providers { provider-id: provider-id }) ERR-UNKNOWN-SERVICE-PROVIDER))
  )
    ;; Check authorization
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Update the provider's authorization
    (map-set service-providers
      { provider-id: provider-id }
      (merge provider { authorized: authorized })
    )
    
    (ok true)
  )
)

;; Mark a vehicle as inactive (e.g., sold or retired)
(define-public (set-vehicle-active-status (vehicle-id (string-ascii 20)) (active bool))
  (let (
    (vehicle (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) ERR-UNKNOWN-VEHICLE))
  )
    ;; Check authorization
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Update the vehicle's active status
    (map-set vehicles
      { vehicle-id: vehicle-id }
      (merge vehicle { active: active })
    )
    
    (ok true)
  )
)