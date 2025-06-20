(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-project-exists (err u102))
(define-constant err-project-not-found (err u103))
(define-constant err-goal-not-met (err u104))
(define-constant err-already-funded (err u105))

(define-data-var minimum-donation uint u1000)
(define-data-var platform-fee uint u25)

(define-map Projects
    { project-id: uint }
    {
        owner: principal,
        target-amount: uint,
        current-amount: uint,
        deadline: uint,
        status: (string-ascii 20),
        beneficiary: principal,
    }
)

(define-map Donations
    {
        donor: principal,
        project-id: uint,
    }
    {
        amount: uint,
        timestamp: uint,
    }
)

(define-map ProjectMilestones
    {
        project-id: uint,
        milestone-id: uint,
    }
    {
        description: (string-ascii 100),
        target-amount: uint,
        completed: bool,
    }
)

(define-data-var project-count uint u0)

(define-private (get-project-count)
    (var-get project-count)
)

(define-public (create-project
        (target-amount uint)
        (deadline uint)
        (beneficiary principal)
    )
    (let ((project-id (get-project-count)))
        (asserts! (> target-amount u0) err-invalid-amount)
        (asserts! (> deadline burn-block-height) err-invalid-amount)
        ;; (try! (create-new-project project-id target-amount deadline beneficiary))
        (var-set project-count (+ (var-get project-count) u1))
        (ok project-id)
    )
)

(define-private (create-new-project
        (project-id uint)
        (target-amount uint)
        (deadline uint)
        (beneficiary principal)
    )
    (ok (map-set Projects { project-id: project-id } {
        owner: tx-sender,
        target-amount: target-amount,
        current-amount: u0,
        deadline: deadline,
        status: "active",
        beneficiary: beneficiary,
    }))
)

(define-public (donate
        (project-id uint)
        (amount uint)
    )
    (let (
            (project (unwrap! (map-get? Projects { project-id: project-id })
                err-project-not-found
            ))
            (current-height burn-block-height)
        )
        (asserts! (>= amount (var-get minimum-donation)) err-invalid-amount)
        (asserts! (< current-height (get deadline project)) err-goal-not-met)
        (asserts! (is-eq (get status project) "active") err-already-funded)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (try! (update-project-amount project-id amount))
        (map-insert Donations {
            donor: tx-sender,
            project-id: project-id,
        } {
            amount: amount,
            timestamp: current-height,
        })
        (ok true)
    )
)

(define-public (release-funds (project-id uint))
    (let (
            (project (unwrap! (map-get? Projects { project-id: project-id })
                err-project-not-found
            ))
            (current-amount (get current-amount project))
            (target-amount (get target-amount project))
            (beneficiary (get beneficiary project))
        )
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (>= current-amount target-amount) err-goal-not-met)
        (try! (as-contract (stx-transfer? current-amount tx-sender beneficiary)))
        (try! (update-project-status project-id "completed"))
        (ok true)
    )
)

(define-private (update-project-amount
        (project-id uint)
        (amount uint)
    )
    (let ((project (unwrap! (map-get? Projects { project-id: project-id })
            err-project-not-found
        )))
        (map-set Projects { project-id: project-id }
            (merge project { current-amount: (+ (get current-amount project) amount) })
        )
        (ok true)
    )
)

(define-private (update-project-status
        (project-id uint)
        (new-status (string-ascii 20))
    )
    (let ((project (unwrap! (map-get? Projects { project-id: project-id })
            err-project-not-found
        )))
        (map-set Projects { project-id: project-id }
            (merge project { status: new-status })
        )
        (ok true)
    )
)

(define-read-only (get-project-details (project-id uint))
    (map-get? Projects { project-id: project-id })
)

(define-read-only (get-donation-details
        (donor principal)
        (project-id uint)
    )
    (map-get? Donations {
        donor: donor,
        project-id: project-id,
    })
)

(define-public (add-milestone
        (project-id uint)
        (description (string-ascii 100))
        (target-amount uint)
    )
    (let ((milestone-id (get-milestone-count project-id)))
        (asserts! (is-project-owner project-id) err-unauthorized)
        (map-insert ProjectMilestones {
            project-id: project-id,
            milestone-id: milestone-id,
        } {
            description: description,
            target-amount: target-amount,
            completed: false,
        })
        (ok milestone-id)
    )
)

(define-private (is-project-owner (project-id uint))
    (let ((project (unwrap! (map-get? Projects { project-id: project-id }) false)))
        (is-eq tx-sender (get owner project))
    )
)

(define-read-only (get-milestone-count (project-id uint))
    (let ((milestones (map-get? ProjectMilestones {
            project-id: project-id,
            milestone-id: u0,
        })))
        (if (is-some milestones)
            u1
            u0
        )
    )
)

(define-public (update-minimum-donation (new-minimum uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (var-set minimum-donation new-minimum)
        (ok true)
    )
)
