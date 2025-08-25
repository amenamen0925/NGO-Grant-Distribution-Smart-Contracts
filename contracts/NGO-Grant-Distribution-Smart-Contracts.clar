(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-project-exists (err u102))
(define-constant err-project-not-found (err u103))
(define-constant err-goal-not-met (err u104))
(define-constant err-already-funded (err u105))
(define-constant err-invalid-category (err u106))

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
        category: (string-ascii 20),
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
        (category (string-ascii 20))
    )
    (let ((project-id (get-project-count)))
        (asserts! (> target-amount u0) err-invalid-amount)
        (asserts! (> deadline burn-block-height) err-invalid-amount)
        (asserts! (is-valid-category category) err-invalid-category)
        (try! (create-new-project-with-category project-id target-amount deadline
            beneficiary category
        ))
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
    (let ((existing-project (map-get? Projects { project-id: project-id })))
        (asserts! (is-none existing-project) err-project-exists)
        (map-set Projects { project-id: project-id } {
            owner: tx-sender,
            target-amount: target-amount,
            current-amount: u0,
            deadline: deadline,
            status: "active",
            beneficiary: beneficiary,
            category: "general",
        })
        (ok true)
    )
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
        (begin
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
        (begin
            (map-set Projects { project-id: project-id }
                (merge project { current-amount: (+ (get current-amount project) amount) })
            )
            (ok true)
        )
    )
)

(define-private (update-project-status
        (project-id uint)
        (new-status (string-ascii 20))
    )
    (let ((project (unwrap! (map-get? Projects { project-id: project-id })
            err-project-not-found
        )))
        (begin
            (map-set Projects { project-id: project-id }
                (merge project { status: new-status })
            )
            (ok true)
        )
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
        (begin
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
(define-map CreatorReputation
    { creator: principal }
    {
        total-projects: uint,
        successful-projects: uint,
        total-raised: uint,
        reputation-score: uint,
        last-updated: uint,
    }
)

(define-map ProjectCreators
    { project-id: uint }
    { creator: principal }
)

(define-private (initialize-creator-reputation (creator principal))
    (begin
        (if (is-none (map-get? CreatorReputation { creator: creator }))
            (map-set CreatorReputation { creator: creator } {
                total-projects: u0,
                successful-projects: u0,
                total-raised: u0,
                reputation-score: u50,
                last-updated: burn-block-height,
            })
            true
        )
        (ok true)
    )
)

(define-private (update-creator-on-project-creation
        (project-id uint)
        (creator principal)
    )
    (let ((current-rep (default-to {
            total-projects: u0,
            successful-projects: u0,
            total-raised: u0,
            reputation-score: u50,
            last-updated: burn-block-height,
        }
            (map-get? CreatorReputation { creator: creator })
        )))
        (begin
            (unwrap! (initialize-creator-reputation creator) err-unauthorized)
            (map-set ProjectCreators { project-id: project-id } { creator: creator })
            (map-set CreatorReputation { creator: creator }
                (merge current-rep {
                    total-projects: (+ (get total-projects current-rep) u1),
                    last-updated: burn-block-height,
                })
            )
            (ok true)
        )
    )
)

(define-private (update-creator-on-project-success
        (project-id uint)
        (amount-raised uint)
    )
    (let (
            (creator-data (unwrap! (map-get? ProjectCreators { project-id: project-id })
                err-project-not-found
            ))
            (creator (get creator creator-data))
            (current-rep (unwrap! (map-get? CreatorReputation { creator: creator })
                err-project-not-found
            ))
            (new-successful (+ (get successful-projects current-rep) u1))
            (new-total-raised (+ (get total-raised current-rep) amount-raised))
            (success-rate (/ (* new-successful u100) (get total-projects current-rep)))
            (new-score (+ u50 (/ success-rate u2)))
        )
        (map-set CreatorReputation { creator: creator }
            (merge current-rep {
                successful-projects: new-successful,
                total-raised: new-total-raised,
                reputation-score: (if (> new-score u100)
                    u100
                    new-score
                ),
                last-updated: burn-block-height,
            })
        )
        (ok true)
    )
)

(define-read-only (get-creator-reputation (creator principal))
    (map-get? CreatorReputation { creator: creator })
)

(define-read-only (get-creator-success-rate (creator principal))
    (let ((rep (map-get? CreatorReputation { creator: creator })))
        (match rep
            reputation-data (if (> (get total-projects reputation-data) u0)
                (/ (* (get successful-projects reputation-data) u100)
                    (get total-projects reputation-data)
                )
                u0
            )
            u0
        )
    )
)

(define-read-only (is-creator-trusted (creator principal))
    (let ((rep (map-get? CreatorReputation { creator: creator })))
        (match rep
            reputation-data (and
                (>= (get reputation-score reputation-data) u70)
                (>= (get total-projects reputation-data) u3)
            )
            false
        )
    )
)

(define-public (get-top-creators (limit uint))
    (begin
        (asserts! (<= limit u10) err-invalid-amount)
        (ok "Feature requires off-chain indexing for full implementation")
    )
)
(define-public (create-project-with-reputation
        (target-amount uint)
        (deadline uint)
        (beneficiary principal)
        (category (string-ascii 20))
    )
    (let ((project-id (get-project-count)))
        (asserts! (> target-amount u0) err-invalid-amount)
        (asserts! (> deadline burn-block-height) err-invalid-amount)
        (asserts! (is-valid-category category) err-invalid-category)
        (try! (create-new-project-with-category project-id target-amount deadline
            beneficiary category
        ))
        (begin
            (map-set ProjectCreators { project-id: project-id } { creator: tx-sender })
            true
        )
        (var-set project-count (+ (var-get project-count) u1))
        (ok project-id)
    )
)

(define-public (release-funds-with-reputation (project-id uint))
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
        (try! (update-creator-on-project-success project-id current-amount))
        (ok true)
    )
)

(define-map ProjectCategories
    { category: (string-ascii 20) }
    {
        project-count: uint,
        total-funding: uint,
        last-updated: uint,
    }
)

(define-private (is-valid-category (category (string-ascii 20)))
    (or
        (is-eq category "education")
        (is-eq category "healthcare")
        (is-eq category "environment")
        (is-eq category "poverty")
        (is-eq category "disaster-relief")
        (is-eq category "technology")
        (is-eq category "general")
    )
)

(define-private (create-new-project-with-category
        (project-id uint)
        (target-amount uint)
        (deadline uint)
        (beneficiary principal)
        (category (string-ascii 20))
    )
    (let ((existing-project (map-get? Projects { project-id: project-id })))
        (asserts! (is-none existing-project) err-project-exists)
        (begin
            (map-set Projects { project-id: project-id } {
                owner: tx-sender,
                target-amount: target-amount,
                current-amount: u0,
                deadline: deadline,
                status: "active",
                beneficiary: beneficiary,
                category: category,
            })
            (update-category-stats category)
            (ok true)
        )
    )
)

(define-private (update-category-stats (category (string-ascii 20)))
    (let ((current-stats (default-to {
            project-count: u0,
            total-funding: u0,
            last-updated: burn-block-height,
        }
            (map-get? ProjectCategories { category: category })
        )))
        (map-set ProjectCategories { category: category }
            (merge current-stats {
                project-count: (+ (get project-count current-stats) u1),
                last-updated: burn-block-height,
            })
        )
    )
)

(define-read-only (get-category-stats (category (string-ascii 20)))
    (map-get? ProjectCategories { category: category })
)

(define-read-only (get-projects-by-category (category (string-ascii 20)))
    (let ((stats (map-get? ProjectCategories { category: category })))
        (match stats
            category-data (get project-count category-data)
            u0
        )
    )
)

(define-read-only (get-valid-categories)
    (list
        "education"         "healthcare"         "environment"         "poverty"
        "disaster-relief"
        "technology"         "general"
    )
)
