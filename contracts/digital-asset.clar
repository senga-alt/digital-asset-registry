;; DigitalAssetRegistry
;; A decentralized registry for managing digital assets with granular access control
;; Built with Clarity 4 for the Stacks blockchain, leveraging Bitcoin's security
;; This contract implements a multi-token system where each asset has unique properties,
;; pricing history, and sophisticated transfer mechanics with allowance-based delegation

;; ============================================================================
;; ERROR CONSTANTS
;; ============================================================================
;; Clarity best practice: Define all error codes as constants for clarity and maintainability
;; Using descriptive error codes helps with debugging and provides clear feedback

(define-constant ERR_UNAUTHORIZED (err u100))              ;; Caller lacks permission to execute action
(define-constant ERR_ASSET_ALREADY_EXISTS (err u101))      ;; Asset with this ID already registered
(define-constant ERR_ASSET_NOT_FOUND (err u102))           ;; Referenced asset does not exist
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))      ;; Account balance too low for operation
(define-constant ERR_INVALID_NAME (err u104))              ;; Asset name validation failed
(define-constant ERR_INVALID_CATEGORY (err u105))          ;; Category string validation failed
(define-constant ERR_INVALID_SUPPLY (err u106))            ;; Max supply must be greater than zero
(define-constant ERR_INVALID_PRICE (err u107))             ;; Price must be greater than zero
(define-constant ERR_INVALID_RECIPIENT (err u108))         ;; Recipient address is malformed
(define-constant ERR_INVALID_AMOUNT (err u109))            ;; Transfer amount must be positive
(define-constant ERR_ALLOWANCE_EXCEEDED (err u110))        ;; Attempted transfer exceeds approved allowance
(define-constant ERR_INVALID_SPENDER (err u111))           ;; Spender address validation failed
(define-constant ERR_INVALID_PRICE_UPDATE (err u112))      ;; Price update conditions not met

;; ============================================================================
;; DATA VARIABLES
;; ============================================================================
;; Data variables are mutable state stored on-chain
;; In Clarity, these are the only mutable storage primitives besides maps

;; Sequential counter for generating unique asset IDs
;; Starts at u0 and increments with each new asset registration
(define-data-var asset-id-counter uint u0)

;; The principal address that has administrative privileges
;; Initialized to tx-sender (the deployer) at contract deployment
;; This follows the common pattern of deployer-as-admin
(define-data-var registry-administrator principal tx-sender)

;; ============================================================================
;; DATA MAPS
;; ============================================================================
;; Maps are key-value stores that form the core of on-chain data storage in Clarity
;; They are typed and immutable in structure (but values can be updated)

;; Primary asset registry: Maps asset IDs to their metadata
;; Each asset is a semi-fungible token with defined supply and properties
(define-map digital-assets
  { asset-id: uint }                                       ;; Key: unique identifier for the asset
  {
    asset-name: (string-ascii 64),                         ;; Human-readable name (ASCII only, max 64 chars)
    asset-category: (string-ascii 32),                     ;; Classification/category tag
    maximum-supply: uint,                                  ;; Total mintable quantity (immutable after creation)
    current-price: uint,                                   ;; Current price in micro-STX (1 STX = 1,000,000 uSTX)
    last-price-update-time: uint                           ;; Clarity 4: Using stacks-block-time for precise timestamps
  }
)

;; Account balances: Tracks how many units each address holds of each asset
;; This is the core accounting ledger for the multi-token system
(define-map account-balances
  { account: principal, asset-id: uint }                   ;; Composite key: (address, asset)
  { balance: uint }                                        ;; Value: quantity owned
)

;; Spending allowances: Implements ERC20-style approve/transferFrom pattern
;; Allows account owners to delegate spending authority to other addresses
;; This enables DEX integrations, escrow services, and automated payments
(define-map spending-allowances
  { 
    owner: principal,                                      ;; The account that owns the assets
    spender: principal,                                    ;; The account authorized to spend
    asset-id: uint                                         ;; The specific asset type
  }
  { approved-amount: uint }                                ;; Maximum amount spender can transfer
)

;; Historical price tracking: Records price changes over time
;; Enables on-chain price history queries and analytics
;; Key uses both asset-id and timestamp for time-series data
(define-map historical-prices
  { asset-id: uint, recorded-at: uint }                    ;; Composite key for temporal lookups
  { price-value: uint }                                    ;; Price at that specific timestamp
)

;; ============================================================================
;; READ-ONLY FUNCTIONS
;; ============================================================================
;; Read-only functions do not modify state and can be called without transactions
;; They are free to execute and return data immediately

;; Validates whether an asset ID exists in the registry
;; Returns true if asset is registered, false otherwise
;; Used internally by other functions to prevent operations on non-existent assets
(define-read-only (asset-exists (asset-id uint))
  (is-some (map-get? digital-assets { asset-id: asset-id }))
)

;; Retrieves complete metadata for a specific asset
;; Returns (some {...}) if asset exists, none if not found
;; Clarity pattern: Using optional types for safe data access
(define-read-only (fetch-asset-metadata (asset-id uint))
  (map-get? digital-assets { asset-id: asset-id })
)

;; Gets the balance of a specific account for a specific asset
;; Returns u0 (zero) if no balance record exists (implicitly zero balance)
;; This default-to pattern is common in Clarity for safe map access
(define-read-only (get-account-balance (account principal) (asset-id uint))
  (default-to u0 
    (get balance 
      (map-get? account-balances { account: account, asset-id: asset-id })
    )
  )
)

;; Queries the approved spending allowance for a spender on an owner's assets
;; Returns u0 if no allowance has been set
;; Used by transferFrom to validate delegated transfers
(define-read-only (get-spending-allowance (owner principal) (spender principal) (asset-id uint))
  (default-to u0 
    (get approved-amount 
      (map-get? spending-allowances { owner: owner, spender: spender, asset-id: asset-id })
    )
  )
)

;; Retrieves historical price data for an asset at a specific timestamp
;; Returns (some {price-value: uint}) if price was recorded at that time
;; Returns none if no price record exists for that timestamp
(define-read-only (fetch-historical-price (asset-id uint) (timestamp uint))
  (map-get? historical-prices { asset-id: asset-id, recorded-at: timestamp })
)

;; Returns the current value of the asset ID counter
;; Useful for frontends to know how many assets have been registered
(define-read-only (get-total-assets-count)
  (var-get asset-id-counter)
)

;; Returns the current administrator principal
;; Public visibility allows verification of who controls the contract
(define-read-only (get-administrator)
  (var-get registry-administrator)
)

;; ============================================================================
;; PUBLIC FUNCTIONS - ADMINISTRATIVE
;; ============================================================================
;; Public functions can modify state and must be called via transactions
;; These require gas fees and are recorded on the blockchain

;; Creates and registers a new digital asset in the registry
;; Only the administrator can mint new asset types
;; 
;; Parameters:
;;   asset-name: Descriptive name for the asset (1-64 ASCII characters)
;;   asset-category: Classification tag (1-32 ASCII characters)
;;   maximum-supply: Total number of units to mint (must be > 0)
;;   initial-price: Starting price in micro-STX (must be > 0)
;;
;; Returns: (ok asset-id) with the newly created asset's ID on success
;; 
;; Clarity 4 features used:
;;   - stacks-block-time: Provides Unix timestamp of current block
;;   - This is more reliable than block height for time-based logic
(define-public (register-asset 
    (asset-name (string-ascii 64)) 
    (asset-category (string-ascii 32)) 
    (maximum-supply uint) 
    (initial-price uint))
  (let
    (
      ;; Generate new asset ID by incrementing counter
      (new-asset-id (+ (var-get asset-id-counter) u1))
      
      ;; Clarity 4 feature: stacks-block-time returns current block timestamp
      ;; This enables accurate time-based logic (essential for DeFi)
      ;; More reliable than using block heights for temporal calculations
      (current-timestamp stacks-block-time)
    )
    ;; Authorization: Only administrator can register new assets
    ;; tx-sender is the transaction originator (cannot be spoofed)
    (asserts! (is-eq tx-sender (var-get registry-administrator)) ERR_UNAUTHORIZED)
    
    ;; Prevent duplicate registration (defensive programming)
    ;; Although counter ensures uniqueness, this check prevents logical errors
    (asserts! (is-none (map-get? digital-assets { asset-id: new-asset-id })) ERR_ASSET_ALREADY_EXISTS)
    
    ;; Input validation: Ensure all parameters meet business rules
    ;; Empty names/categories are rejected for data quality
    (asserts! (> (len asset-name) u0) ERR_INVALID_NAME)
    (asserts! (> (len asset-category) u0) ERR_INVALID_CATEGORY)
    (asserts! (> maximum-supply u0) ERR_INVALID_SUPPLY)
    (asserts! (> initial-price u0) ERR_INVALID_PRICE)
    
    ;; Create the asset record in the registry
    ;; map-set creates or updates; here we know it's a new entry
    (map-set digital-assets
      { asset-id: new-asset-id }
      { 
        asset-name: asset-name, 
        asset-category: asset-category, 
        maximum-supply: maximum-supply, 
        current-price: initial-price,
        last-price-update-time: current-timestamp
      }
    )
    
    ;; Record initial price in historical tracking
    ;; This creates the first data point for price history
    (map-set historical-prices
      { asset-id: new-asset-id, recorded-at: current-timestamp }
      { price-value: initial-price }
    )
    
    ;; Mint entire supply to administrator's account
    ;; Administrator can then distribute assets as needed
    ;; This is safer than allowing arbitrary minting after creation
    (map-set account-balances
      { account: (var-get registry-administrator), asset-id: new-asset-id }
      { balance: maximum-supply }
    )
    
    ;; Update the global counter for next asset
    ;; var-set is the only way to modify data-vars in Clarity
    (var-set asset-id-counter new-asset-id)
    
    ;; Return success with the new asset ID
    ;; Clarity uses response types: (ok value) or (err value)
    (ok new-asset-id)
  )
)

;; ============================================================================
;; PUBLIC FUNCTIONS - ASSET TRANSFERS
;; ============================================================================

;; Transfers assets directly from sender to recipient
;; This is the basic transfer function for moving assets between accounts
;;
;; Parameters:
;;   asset-id: The asset type to transfer
;;   amount: Quantity to transfer (must be > 0)
;;   sender: The account sending the assets (must be tx-sender)
;;   recipient: The receiving account (must be valid principal)
;;
;; Returns: (ok true) on successful transfer
;;
;; Security notes:
;;   - Sender must be tx-sender (prevents unauthorized transfers)
;;   - Uses is-standard to validate recipient address
;;   - Clarity prevents reentrancy by design (no callbacks possible)
(define-public (transfer-assets 
    (asset-id uint) 
    (amount uint) 
    (sender principal) 
    (recipient principal))
  (let
    (
      ;; Fetch current balances for both parties
      ;; default-to ensures we get u0 for new accounts
      (sender-current-balance 
        (default-to u0 
          (get balance 
            (map-get? account-balances { account: sender, asset-id: asset-id })
          )
        )
      )
      (recipient-current-balance 
        (default-to u0 
          (get balance 
            (map-get? account-balances { account: recipient, asset-id: asset-id })
          )
        )
      )
    )
    ;; Validation sequence - fail fast on any violation
    ;; This ordering is optimized for gas efficiency (cheap checks first)
    
    ;; Verify asset exists in registry
    (asserts! (asset-exists asset-id) ERR_ASSET_NOT_FOUND)
    
    ;; Amount must be positive (zero transfers are rejected)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Critical security check: tx-sender must match sender parameter
    ;; This prevents one account from transferring another's assets
    ;; tx-sender is cryptographically verified by the blockchain
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    
    ;; Recipient must be a valid principal address
    ;; is-standard returns true for valid standard principals
    ;; (excludes contract principals if needed)
    (asserts! (is-standard recipient) ERR_INVALID_RECIPIENT)