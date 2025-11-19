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

# Build the project (library and executable)
cabal build plutus-scripts

# Build only the library
cabal build scripts

# Run the envelopes executable (generates serialized script files)
cabal run envelopes

# Clean build artifacts
cabal clean
```

The `envelopes` executable (`app/Main.hs`) serializes all Plutus scripts to `plutus-scripts/serialised-plutus-scripts/` directory (git-ignored). This executable uses `PlutusLedgerApi.Envelope.writeCodeEnvelopeForVersion` to generate `.plutus` files compatible with cardano-node-tests.

## Architecture

### Cabal Package Structure

The `plutus-scripts` package contains:

- **Library `scripts`**: Core Plutus script implementations and helpers
- **Executable `envelopes`**: Serializes compiled scripts to `.plutus` files

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
├── app/                    # Executable applications
│   └── Main.hs            # Envelopes executable - serializes all scripts to .plutus files
└── plutus-scripts.cabal    # Cabal package definition
```

### Plutus Version Targeting

All scripts target **Plutus Core 1.1.0** and use **PlutusV3** ledger API:

- Scripts use `{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}`
- V3 validators receive `ScriptContext` with enhanced governance support
- Scripts are organized by Plutus version: `V_1_0.hs` and `V_1_1.hs` modules

### Script Organization Pattern

Each script category follows a consistent pattern:

1. **Common module** (`Common.hs`): Contains `INLINEABLE` validator logic and test parameters
2. **Versioned modules** (`V_1_0.hs`, `V_1_1.hs`): Compile scripts with Plutus Core target versions
3. **Library exports**: Pure `CompiledCode` values (no IO operations)
4. **Executable serialization**: `app/Main.hs` handles all file writing

#### Single Script Pattern

For simple scripts without test variants:

```haskell
-- PlutusScripts/Basic/Common.hs
{-# INLINEABLE mkAlwaysSucceedPolicyV3 #-}
mkAlwaysSucceedPolicyV3 :: P.BuiltinData -> P.BuiltinUnit

-- PlutusScripts/Basic/V_1_1.hs
alwaysSucceedPolicyCompiled :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysSucceedPolicyCompiled = $$(compile [||mkAlwaysSucceedPolicyV3||])

-- app/Main.hs
writeEnvelopeV3 "alwaysSucceedPolicyScriptV3" Basic.alwaysSucceedPolicyCompiled
```

#### Script Group Pattern (For Test Variants)

For scripts with multiple parameter combinations (e.g., failing tests):

```haskell
-- PlutusScripts/Bitwise/ReadBit.hs (Common module)
data Params = Params { s :: BuiltinByteString, i :: Integer, output :: Bool }
failingReadBitParams :: [Params]  -- Test cases for edge conditions

-- PlutusScripts/Bitwise/V_1_1.hs
failingBitwiseScriptGroupsV3 :: [ScriptGroup DefaultUni DefaultFun (BuiltinData -> BuiltinUnit)]
failingBitwiseScriptGroupsV3 =
  [ ScriptGroup
      { sgBaseName = "failingReadBitPolicyScriptV3"
      , sgScripts = map compileReadBit failingReadBitParams  -- Iteration in library
      }
  ]
  where
    compileReadBit param = $$(compile [||mkReadBitPolicy||]) `unsafeApplyCode` liftCode plcVersion110 [param]

-- app/Main.hs
mapM_ writeScriptGroup BitwiseV1.failingBitwiseScriptGroupsV3
-- Generates: failingReadBitPolicyScriptV3_1.plutus, _2.plutus, ..., _14.plutus
```

This pattern:
- ✅ Encapsulates parameter iteration in the library
- ✅ Keeps executable unaware of Params types
- ✅ Generates numbered script files automatically
- ✅ Reusable for any parameterized test variants

### Helper Utilities

**`PlutusScripts.Helpers`** provides:

- `writeCompiledScript` - Direct serialization from `CompiledCode` to JSON envelope using `PlutusLedgerApi.Envelope`
- `bytesFromHex`, `hxs` - Hex string conversion utilities
- `asRedeemer`, `asDatum` - Plutus data conversions

**`Helpers.ScriptUtils`** provides:

- Untyped validator/policy/stake validator types
- `IsScriptContext` typeclass for version-agnostic script construction
- `ScriptGroup` type for organizing parameterized test variants
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

## Script Generation

The `envelopes` executable serializes all scripts to `.plutus` files:

```bash
cabal run envelopes
```

Output: Multiple `.plutus` files in `serialised-plutus-scripts/` (git-ignored)

**Generated Scripts**:
- PlutusV2 scripts (bytestring/integer conversions)
- Basic PlutusV3 scripts (always succeed/fail, token names, time ranges, redeemers)
- SECP256k1 scripts (Schnorr and ECDSA signature verification)
- Hashing scripts (RIPEMD-160)
- Bitwise succeeding tests (AND, OR, XOR, complement, shift, rotate, bit operations)
- Bitwise failing tests (ReadBit, WriteBits, ReplicateByte edge case validation)

These serialized scripts are consumed by external E2E tests (e.g., `cardano-node-tests`).

### Adding New Scripts

#### Single Script

To add a simple script:

1. Create the validator logic in a `Common.hs` module with `INLINEABLE` pragma
2. Add versioned modules (`V_1_0.hs`, `V_1_1.hs`) that compile and export the script as `CompiledCode`
3. Update `app/Main.hs` to add a `writeEnvelopeV3` call
4. Run `cabal run envelopes` to generate the `.plutus` file

#### Multiple Test Variants (Script Group Pattern)

For parameterized tests (e.g., failing tests with multiple edge cases):

1. Define test parameters in the `Common` module:
   ```haskell
   data Params = Params { input :: ByteString, expected :: Bool }
   testParams :: [Params]
   ```

2. Create a `ScriptGroup` in the versioned module:
   ```haskell
   myScriptGroup :: [ScriptGroup DefaultUni DefaultFun (BuiltinData -> BuiltinUnit)]
   myScriptGroup = [ScriptGroup
     { sgBaseName = "myTestScript"
     , sgScripts = map compile testParams
     }]
   ```

3. Use `mapM_ writeScriptGroup` in `app/Main.hs`
4. Run `cabal run envelopes` - generates numbered files (myTestScript_1.plutus, _2.plutus, ...)

## Nix Binary Cache

The project is configured to use IOG's Hydra cache:

```
extra-substituters = https://cache.iog.io
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

This significantly speeds up builds by avoiding recompilation of Cardano dependencies.
