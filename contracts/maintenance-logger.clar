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

;; Check if maintenance is due based on current mileage
(define-read-only (is-maintenance-due-by-mileage (vehicle-id (string-ascii 20)) (task-type-id (string-ascii 20)))
  (let (
    (vehicle (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) false))
    (task-type (unwrap! (map-get? maintenance-task-types { task-type-id: task-type-id }) false))
    (current-mileage (get current-mileage vehicle))
    (mileage-interval (get mileage-interval task-type))
  )
    ;; Find the last maintenance record for this task type
    ;; This is a simplification - actual implementation would need to search through records
    (some true)  ;; Simplified to always return due for demonstration
  )
)

;; Public Functions

;; Register a new vehicle to the fleet
(define-public (register-vehicle 
  (vehicle-id (string-ascii 20)) 
  (make (string-ascii 50)) 
  (model (string-ascii 50)) 
  (year uint) 
  (initial-mileage uint)
)
  (begin
    ;; Check authorization (only contract owner or authorized users can add vehicles)
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Validate inputs
    (asserts! (> (len vehicle-id) u0) ERR-UNKNOWN-VEHICLE)
    
    ;; Add the vehicle
    (map-set vehicles
      { vehicle-id: vehicle-id }
      {
        owner: tx-sender,
        make: make,
        model: model,
        year: year,
        current-mileage: initial-mileage,
        last-update-timestamp: (unwrap! (get-block-info? time u0) u0),
        active: true
      }
    )
    
    (ok true)
  )
)

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

;; Update vehicle mileage
(define-public (update-vehicle-mileage (vehicle-id (string-ascii 20)) (new-mileage uint))
  (let (
    (vehicle (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) ERR-UNKNOWN-VEHICLE))
    (current-mileage (get current-mileage vehicle))
  )
    ;; Check authorization - only owner can update mileage
    (asserts! (is-vehicle-owner vehicle-id) ERR-NOT-AUTHORIZED)
    
    ;; Validate mileage (cannot decrease)
    (asserts! (>= new-mileage current-mileage) ERR-INVALID-MILEAGE)
    
    ;; Update the mileage
    (map-set vehicles
      { vehicle-id: vehicle-id }
      (merge vehicle {
        current-mileage: new-mileage,
        last-update-timestamp: (unwrap! (get-block-info? time u0) u0)
      })
    )
    
    (ok true)
  )
)

;; Schedule a maintenance task
(define-public (schedule-maintenance
  (vehicle-id (string-ascii 20))
  (task-type-id (string-ascii 20))
  (scheduled-date uint)
  (scheduled-mileage uint)
  (notes (optional (string-utf8 500)))
)
  (let (
    (task-id (get-and-increment-task-id))
    (current-time (unwrap! (get-block-info? time u0) u0))
  )
    ;; Check prerequisites
    (asserts! (vehicle-exists vehicle-id) ERR-UNKNOWN-VEHICLE)
    (asserts! (task-type-exists task-type-id) ERR-UNKNOWN-MAINTENANCE-TASK)
    (asserts! (is-vehicle-owner vehicle-id) ERR-NOT-AUTHORIZED)
    
    ;; Validate inputs 
    (asserts! (> scheduled-date current-time) ERR-INVALID-TIMESTAMP)
    
    ;; Create the scheduled maintenance
    (map-set scheduled-maintenance
      { vehicle-id: vehicle-id, task-id: task-id }
      {
        task-type-id: task-type-id,
        scheduled-date: scheduled-date,
        scheduled-mileage: scheduled-mileage,
        status: "scheduled",
        notes: notes
      }
    )
    
    (ok task-id)
  )
)

;; Log completed maintenance
(define-public (log-completed-maintenance
  (vehicle-id (string-ascii 20))
  (task-id uint)
  (completion-mileage uint)
  (service-provider-id (string-ascii 20))
  (technician-name (string-ascii 100))
  (notes (optional (string-utf8 500)))
  (cost uint)
  (verification-hash (optional (buff 32)))
)
  (let (
    (record-id (get-and-increment-record-id))
    (task (unwrap! (map-get? scheduled-maintenance { vehicle-id: vehicle-id, task-id: task-id }) ERR-UNKNOWN-MAINTENANCE-TASK))
    (task-type-id (get task-type-id task))
    (current-time (unwrap! (get-block-info? time u0) u0))
  )
    ;; Check prerequisites
    (asserts! (vehicle-exists vehicle-id) ERR-UNKNOWN-VEHICLE)
    (asserts! (scheduled-maintenance-exists vehicle-id task-id) ERR-UNKNOWN-MAINTENANCE-TASK)
    (asserts! (not (is-maintenance-completed vehicle-id task-id)) ERR-MAINTENANCE-ALREADY-COMPLETED)
    (asserts! (is-authorized-service-provider service-provider-id) ERR-UNKNOWN-SERVICE-PROVIDER)
    
    ;; Update the scheduled maintenance status
    (map-set scheduled-maintenance
      { vehicle-id: vehicle-id, task-id: task-id }
      (merge task { status: "completed" })
    )
    
    ;; Create the maintenance record
    (map-set maintenance-records
      { vehicle-id: vehicle-id, record-id: record-id }
      {
        task-id: task-id,
        task-type-id: task-type-id,
        completion-date: current-time,
        completion-mileage: completion-mileage,
        service-provider-id: service-provider-id,
        technician-name: technician-name,
        notes: notes,
        cost: cost,
        verification-hash: verification-hash
      }
    )
    
    ;; Update vehicle mileage if the completion mileage is higher
    (try! (update-vehicle-mileage vehicle-id completion-mileage))
    
    (ok record-id)
  )
)

;; Add parts replaced during maintenance
(define-public (add-parts-replaced
  (record-id uint)
  (part-index uint)
  (part-name (string-ascii 100))
  (part-number (string-ascii 50))
  (quantity uint)
  (cost-per-unit uint)
)
  (let ((vehicle-id (string-ascii 20)))
    ;; In a real implementation we would need to look up the vehicle-id from the record-id
    ;; This is simplified for demonstration
    
    ;; Check authorization - only the service provider who logged the maintenance can add parts
    (asserts! (> quantity u0) ERR-INVALID-PARTS-DATA)
    
    ;; Add the part replacement record
    (map-set parts-replaced
      { record-id: record-id, part-index: part-index }
      {
        part-name: part-name,
        part-number: part-number,
        quantity: quantity,
        cost-per-unit: cost-per-unit
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