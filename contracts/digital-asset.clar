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