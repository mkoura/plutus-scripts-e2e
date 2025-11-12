# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains a collection of Plutus scripts for Cardano end-to-end (E2E) testing. Originally copied from the [Antaeus repository](https://github.com/IntersectMBO/antaeus), it provides serialized Plutus scripts used by `cardano-node-tests`.

## Build System and Development Environment

This project uses the IOG ecosystem tooling with Nix flakes and haskell.nix:

- **Nix Flakes**: Project uses direct `haskell.nix` integration with `flake-utils` for build configuration
- **direnv**: Development environment is auto-loaded via `.envrc`
- **CHaP**: Cardano Haskell Packages repository for Cardano-specific dependencies
- **GHC Version**: GHC 9.6 (configured in `nix/project.nix`)
- **Build System**: Migrated from iogx to direct haskell.nix usage for simplified build configuration

### Development Commands

```bash
# Enter development shell
nix develop

# Build the project
cabal build plutus-scripts

# Run tests (generates serialized script files)
cabal test plutus-scripts

# Clean build artifacts
cabal clean
```

The test suite (`Spec.hs`) runs `writeScriptFiles` which serializes all Plutus scripts to `plutus-scripts/serialised-plutus-scripts/` directory (git-ignored).

## Architecture

### Project Structure

```
plutus-scripts/
├── PlutusScripts/           # Plutus script implementations
│   ├── Basic/              # Basic validators and policies
│   ├── Bitwise/            # Bitwise operation scripts
│   ├── BLS/                # BLS cryptography scripts
│   ├── Governance/         # Governance-related scripts
│   ├── Hashing/            # Hash function scripts
│   └── SECP256k1/          # SECP256k1 signature scripts
├── Helpers/                # Helper utilities
│   └── ScriptUtils.hs     # Script context utilities (subset of plutus-script-utils)
├── Spec/                   # Test specifications
│   └── WriteScriptFiles.hs # Serializes all scripts to .plutus files
└── plutus-scripts.cabal    # Cabal package definition
```

### Plutus Version Targeting

All scripts target **Plutus Core 1.1.0** and use **PlutusV3** ledger API:

- Scripts use `{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}`
- V3 validators receive `ScriptContext` with enhanced governance support
- Scripts are organized by Plutus version: `V_1_0.hs` and `V_1_1.hs` modules

### Script Organization Pattern

Each script category follows a consistent pattern:

1. **Common module** (`Common.hs`): Contains `INLINEABLE` validator logic
2. **Versioned modules** (`V_1_0.hs`, `V_1_1.hs`): Compile and serialize scripts
3. **Write functions**: Export `write*` functions that serialize to `.plutus` files

Example:
```haskell
-- PlutusScripts/Basic/Common.hs
{-# INLINEABLE mkAlwaysSucceedPolicyV3 #-}
mkAlwaysSucceedPolicyV3 :: P.BuiltinData -> P.BuiltinUnit

-- PlutusScripts/Basic/V_1_1.hs
alwaysSucceedPolicyCompiled :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysSucceedPolicyCompiled = $$(PlutusTx.compile [||mkAlwaysSucceedPolicyV3||])

alwaysSucceedPolicy :: SerialisedScript
alwaysSucceedPolicy = serialiseCompiledCode alwaysSucceedPolicyCompiled

writeAlwaysSucceedPolicyScriptV3 :: IO ()
writeAlwaysSucceedPolicyScriptV3 =
  writeCompiledScript PlutusV3 "alwaysSucceedPolicyScriptV3" alwaysSucceedPolicyCompiled
```

### Helper Utilities

**`PlutusScripts.Helpers`** provides:
- `writeCompiledScript` - Direct serialization from `CompiledCode` to JSON envelope using `PlutusLedgerApi.Envelope`
- `bytesFromHex`, `hxs` - Hex string conversion utilities
- `asRedeemer`, `asDatum` - Plutus data conversions

**`Helpers.ScriptUtils`** provides:
- Untyped validator/policy/stake validator types
- `IsScriptContext` typeclass for version-agnostic script construction
- `check` utility for validation
- `constrArgs` for deconstructing BuiltinData constructors

## Dependencies

Key Plutus ecosystem dependencies (from CHaP):
- `plutus-core ^>=1.53.1.0`
- `plutus-ledger-api ^>=1.53.1.0`
- `plutus-tx ^>=1.53.1.0`
- `plutus-tx-plugin ^>=1.53.1.0`
- `data-default >=0.8`

**Note**: cardano-api dependency was completely removed in November 2025. Scripts now serialize using native `PlutusLedgerApi.Envelope` tooling.

### Recent API Changes

**Plutus 1.53 Updates:**
- `txInfoMint` field now returns a structure with `mintValueMinted` accessor
- Use `PV3.mintValueMinted (PV3.txInfoMint txInfo)` to access minted values
- Some unused PlutusLedgerApi.V1 qualified imports removed for cleaner code

**November 2025 - cardano-api Removal:**
- Completely removed cardano-api dependency
- All witness construction code removed (was unused legacy from Antaeus)
- Scripts now serialize using `PlutusLedgerApi.Envelope.writeCodeEnvelopeForVersion`
- Output format remains identical for cardano-node-tests compatibility

## Development Practices

### Cabal Configuration

- **Tests enabled by default**: `tests: true` in `cabal.project`
- **Optimization disabled** for large dependencies (cardano-ledger-alonzo, ouroboros-consensus, cardano-api) to speed up development builds
- **Test output**: `test-show-details: direct` for colorized tasty output

### GHC Pragmas

Scripts consistently use:
```haskell
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
```

Common language extensions (defined in `cabal.project`):
- `ImportQualifiedPost`
- `OverloadedStrings`
- `TemplateHaskell` (for PlutusTx compilation)
- `DataKinds`, `ScopedTypeVariables`

### Pre-commit Hooks

Pre-commit hooks are configured and enabled via `.pre-commit-config.yaml`. The hooks automatically stash unstaged files during commits to ensure clean staging. Available hooks:
- `fourmolu` (Haskell formatting)
- `hlint` (Haskell linting)
- `cabal-fmt` (Cabal file formatting)
- `nixpkgs-fmt` (Nix formatting)

## Script Categories

### Basic Scripts
- Always succeed/fail validators and policies
- Token name validation
- Time range validation
- Witness redeemer policies

### Bitwise Operations
- Bitwise complement, logical operations (AND, OR, XOR)
- Shift and rotate operations
- Byte conversions and replication
- Bit reading/writing

### Cryptographic Scripts
- **BLS**: BLS12-381 signatures, Groth16 proofs, Schnorr signatures, VRF
- **SECP256k1**: ECDSA and Schnorr signature verification
- **Hashing**: RIPEMD-160, SHA2-256, SHA3-256, BLAKE2b-224/256

### Governance
Scripts demonstrating PlutusV3 governance features (voting, proposals)

## Testing

The test suite is a single executable that serializes all scripts:

```bash
cabal test
```

Output: `.plutus` files in `plutus-scripts/serialised-plutus-scripts/`

These serialized scripts are consumed by external E2E tests (e.g., `cardano-node-tests`).

## Nix Binary Cache

The project is configured to use IOG's Hydra cache:
```
extra-substituters = https://cache.iog.io
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

This significantly speeds up builds by avoiding recompilation of Cardano dependencies.