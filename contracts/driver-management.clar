;; driver-management
;; A contract for tracking driver assignments, qualifications, performance metrics
;; and managing logistics fleet drivers securely on the Stacks blockchain.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-DRIVER-EXISTS (err u101))
(define-constant ERR-DRIVER-NOT-FOUND (err u102))
(define-constant ERR-VEHICLE-EXISTS (err u103))
(define-constant ERR-VEHICLE-NOT-FOUND (err u104))
(define-constant ERR-VEHICLE-ALREADY-ASSIGNED (err u105))
(define-constant ERR-DRIVER-UNAVAILABLE (err u106))
(define-constant ERR-INSUFFICIENT-QUALIFICATIONS (err u107))
(define-constant ERR-CERTIFICATION-EXPIRED (err u108))
(define-constant ERR-INVALID-METRIC-VALUE (err u109))
(define-constant ERR-ASSIGNMENT-NOT-FOUND (err u110))

;; Data structures

;; Driver status states
(define-constant DRIVER-STATUS-AVAILABLE u1)
(define-constant DRIVER-STATUS-ASSIGNED u2)
(define-constant DRIVER-STATUS-OFF-DUTY u3)
(define-constant DRIVER-STATUS-ON-LEAVE u4)

;; Store fleet managers who can administer the contract
(define-map fleet-managers principal bool)

;; Driver profile information
(define-map drivers
    principal
    {
        name: (string-ascii 50),
        license-number: (string-ascii 20),
        status: uint,
        join-date: uint
    }
)

;; Driver certifications with expiration dates
(define-map driver-certifications
    { driver: principal, certification-id: (string-ascii 20) }
    {
        name: (string-ascii 50),
        issued-date: uint,
        expiry-date: uint,
        issuing-authority: (string-ascii 50)
    }
)

;; Vehicle registry
(define-map vehicles
    (string-ascii 20) ;; vehicle-id
    {
        model: (string-ascii 50),
        type: (string-ascii 20),
        year: uint,
        required-qualifications: (list 10 (string-ascii 20))
    }
)

;; Driver-to-vehicle assignments
(define-map vehicle-assignments
    (string-ascii 20) ;; vehicle-id
    {
        driver: principal,
        assigned-at: uint,
        expected-return: uint
    }
)

;; Performance metrics for drivers
(define-map driver-performance
    { driver: principal, period: (string-ascii 10) }
    {
        fuel-efficiency: uint,
        on-time-deliveries: uint,
        late-deliveries: uint,
        safety-incidents: uint,
        customer-rating: uint
    }
)

;; Contract owner who can add/remove fleet managers
(define-data-var contract-owner principal tx-sender)

;; Functions

;; Administrative functions

;; Set the contract owner
(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)

;; Add a new fleet manager
(define-public (add-fleet-manager (manager principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set fleet-managers manager true))
    )
)

;; Remove a fleet manager
(define-public (remove-fleet-manager (manager principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-delete fleet-managers manager))
    )
)

;; Check if caller is authorized (contract owner or fleet manager)
(define-private (is-authorized)
    (or 
        (is-eq tx-sender (var-get contract-owner))
        (default-to false (map-get? fleet-managers tx-sender))
    )
)

;; Driver management functions

;; Register a new driver
(define-public (register-driver (driver-principal principal) (name (string-ascii 50)) (license-number (string-ascii 20)))
    (begin
        (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? drivers driver-principal)) ERR-DRIVER-EXISTS)
        
        (ok (map-set drivers
            driver-principal
            {
                name: name,
                license-number: license-number,
                status: DRIVER-STATUS-AVAILABLE,
                join-date: block-height
            }
        ))
    )
)

;; Update driver status
(define-public (update-driver-status (driver-principal principal) (new-status uint))
    (let (
        (driver-info (unwrap! (map-get? drivers driver-principal) ERR-DRIVER-NOT-FOUND))
    )
        (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
        (asserts! (or
            (is-eq new-status DRIVER-STATUS-AVAILABLE)
            (is-eq new-status DRIVER-STATUS-ASSIGNED)
            (is-eq new-status DRIVER-STATUS-OFF-DUTY)
            (is-eq new-status DRIVER-STATUS-ON-LEAVE)) ERR-INVALID-METRIC-VALUE)
        
        (ok (map-set drivers
            driver-principal
            (merge driver-info { status: new-status })
        ))
    )
)

;; Add or update driver certification
(define-public (add-certification 
    (driver-principal principal) 
    (certification-id (string-ascii 20)) 
    (name (string-ascii 50)) 
    (issued-date uint) 
    (expiry-date uint) 
    (issuing-authority (string-ascii 50)))
    
    (begin
        (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? drivers driver-principal)) ERR-DRIVER-NOT-FOUND)
        
        (ok (map-set driver-certifications
            { driver: driver-principal, certification-id: certification-id }
            {
                name: name,
                issued-date: issued-date,
                expiry-date: expiry-date,
                issuing-authority: issuing-authority
            }
        ))
    )
)

;; Vehicle management functions

;; Register a new vehicle
(define-public (register-vehicle 
    (vehicle-id (string-ascii 20)) 
    (model (string-ascii 50)) 
    (type (string-ascii 20)) 
    (year uint) 
    (required-qualifications (list 10 (string-ascii 20))))
    
    (begin
        (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? vehicles vehicle-id)) ERR-VEHICLE-EXISTS)
        
        (ok (map-set vehicles
            vehicle-id
            {
                model: model,
                type: type,
                year: year,
                required-qualifications: required-qualifications
            }
        ))
    )
)

;; Helper function to check if a driver has a specific qualification and it's not expired
(define-private (check-qualification (acc uint) (qualification-id (string-ascii 20)))
    (let (
        (certification (map-get? driver-certifications { driver: (unwrap-panic (element-at (list tx-sender) u0)), certification-id: qualification-id }))
    )
        (if (and
                (is-some certification)
                (> (get expiry-date (unwrap-panic certification)) block-height)
            )
            (+ acc u1)
            acc
        )
    )
)

;; Assignment functions

;; End a vehicle assignment
(define-public (end-assignment (vehicle-id (string-ascii 20)))
    (let (
        (assignment (unwrap! (map-get? vehicle-assignments vehicle-id) ERR-ASSIGNMENT-NOT-FOUND))
        (driver-principal (get driver assignment))
        (driver (unwrap! (map-get? drivers driver-principal) ERR-DRIVER-NOT-FOUND))
    )
        (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
        
        ;; Remove vehicle assignment
        (map-delete vehicle-assignments vehicle-id)
        
        ;; Update driver status to available
        (map-set drivers
            driver-principal
            (merge driver { status: DRIVER-STATUS-AVAILABLE })
        )
        
        (ok true)
    )
)

;; Performance tracking functions

;; Record performance metrics for a driver
(define-public (record-performance 
    (driver-principal principal) 
    (period (string-ascii 10)) 
    (fuel-efficiency uint) 
    (on-time-deliveries uint) 
    (late-deliveries uint) 
    (safety-incidents uint) 
    (customer-rating uint))
    
    (begin
        (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? drivers driver-principal)) ERR-DRIVER-NOT-FOUND)
        (asserts! (<= customer-rating u1000) ERR-INVALID-METRIC-VALUE) ;; Rating should be 0-1000
        
        (ok (map-set driver-performance
            { driver: driver-principal, period: period }
            {
                fuel-efficiency: fuel-efficiency,
                on-time-deliveries: on-time-deliveries,
                late-deliveries: late-deliveries,
                safety-incidents: safety-incidents,
                customer-rating: customer-rating
            }
        ))
    )
)

;; Read-only functions

;; Get driver information
(define-read-only (get-driver (driver-principal principal))
    (map-get? drivers driver-principal)
)

;; Get driver certification
(define-read-only (get-certification (driver-principal principal) (certification-id (string-ascii 20)))
    (map-get? driver-certifications { driver: driver-principal, certification-id: certification-id })
)

;; Get vehicle information
(define-read-only (get-vehicle (vehicle-id (string-ascii 20)))
    (map-get? vehicles vehicle-id)
)

;; Get current vehicle assignment
(define-read-only (get-vehicle-assignment (vehicle-id (string-ascii 20)))
    (map-get? vehicle-assignments vehicle-id)
)

;; Get driver performance for a specific period
(define-read-only (get-driver-performance (driver-principal principal) (period (string-ascii 10)))
    (map-get? driver-performance { driver: driver-principal, period: period })
)

;; Check if a driver is currently available
(define-read-only (is-driver-available (driver-principal principal))
    (let (
        (driver (map-get? drivers driver-principal))
    )
        (if (is-some driver)
            (is-eq (get status (unwrap-panic driver)) DRIVER-STATUS-AVAILABLE)
            false
        )
    )
)

;; Check if driver has valid certification
(define-read-only (has-valid-certification (driver-principal principal) (certification-id (string-ascii 20)))
    (let (
        (certification (map-get? driver-certifications { driver: driver-principal, certification-id: certification-id }))
    )
        (if (is-some certification)
            (> (get expiry-date (unwrap-panic certification)) block-height)
            false
        )
    )
)