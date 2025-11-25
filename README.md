# Plutus Scripts for E2E Testing

Collection of Plutus scripts used for end-to-end testing in [cardano-node-tests](https://github.com/IntersectMBO/cardano-node-tests).

Originally based on the [Antaeus repository](https://github.com/IntersectMBO/antaeus), now maintained independently as a dedicated script library.

## Overview

This repository provides serialized Plutus scripts compiled for **Plutus Core 1.0.0 and 1.1.0** targeting **PlutusV2** and **PlutusV3** ledger APIs. The scripts are generated as `.plutus` envelope files consumed by the [cardano-node-tests](https://github.com/IntersectMBO/cardano-node-tests) E2E test suite.

**Key Features:**

- Comprehensive script library covering basic validators, bitwise operations, cryptography, and governance
- Plutus Core 1.0.0 and 1.1.0 support with PlutusV2 and PlutusV3 targeting
- Modular architecture with Common/Versioned module pattern
- Native PlutusLedgerApi.Envelope serialization (no cardano-api dependency)
- Integrated formatting and linting (treefmt, fourmolu, hlint)

## Quick Start

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- direnv (optional, but recommended for automatic environment loading)

### Build and Generate Scripts

```bash
# Enter development shell
nix develop

# Build the project
cabal build plutus-scripts

# Generate serialized script envelopes
cabal run envelopes

# Output: serialised-plutus-scripts/*.plutus (59 scripts total)
```

The generated `.plutus` files are JSON envelopes containing the serialized Plutus scripts, ready for use in cardano-node-tests.

**Generated Scripts**:

- PlutusV2 scripts (bytestring/integer conversions)
- Basic PlutusV3 scripts (always succeed/fail, token names, time ranges, redeemers)
- SECP256k1 scripts (Schnorr and ECDSA signature verification)
- Hashing scripts (RIPEMD-160)
- Bitwise succeeding tests (AND, OR, XOR, complement, shift, rotate, bit operations)
- Bitwise failing tests (ReadBit, WriteBits, ReplicateByte edge case variants)

## Project Structure

```
plutus-scripts/
├── PlutusScripts/              # Script implementations by category
│   ├── Basic/                  # Always succeed/fail, token name, time range
│   │   ├── Common.hs          # INLINEABLE validator logic
│   │   ├── V_1_0.hs           # Plutus Core 1.0.0 compilation
│   │   └── V_1_1.hs           # Plutus Core 1.1.0 compilation
│   ├── Bitwise/                # Bitwise operations (AND, OR, XOR, shift, rotate)
│   ├── BLS/                    # BLS12-381 signatures, Groth16, Schnorr, VRF
│   ├── Governance/             # PlutusV3 governance features
│   ├── Hashing/                # RIPEMD-160, SHA2-256, SHA3-256, BLAKE2b
│   └── SECP256k1/              # ECDSA and Schnorr signature verification
├── Helpers/                    # Utility modules
│   ├── ScriptUtils.hs         # Untyped validators, context handling
│   └── ...
├── app/                        # Executable applications
│   └── Main.hs                # Envelopes executable (script serialization)
├── plutus-scripts.cabal        # Package definition
├── cabal.project               # Build configuration
└── flake.nix                   # Nix flake for reproducible builds
```

## Script Categories

### Basic Validators and Policies

Core testing scripts for fundamental blockchain operations:

- **Always Succeed/Fail**: Testing transaction validation boundaries
- **Token Name Policy**: Minting policies with token name constraints
- **Time Range Policy**: Time-locked validators
- **Witness Redeemer Policy**: Redeemer verification patterns

### Bitwise Operations

Scripts demonstrating Plutus bitwise primitives (Plutus Core 1.1.0+):

**Succeeding Tests**:

- Logical operations: `andByteString`, `orByteString`, `xorByteString`, `complementByteString`
- Shifts and rotates: `shiftByteString`, `rotateByteString`
- Bit manipulation: `readBit`, `writeBits`, `countSetBits`, `findFirstSetBit`
- Byte operations: `replicateByte`, conversions between integers and bytestrings

**Failing Tests**:

- ReadBit edge cases: empty bytestring, negative indices, out of bounds, Int64 limits
- WriteBits edge cases: empty bytestring, negative indices, out of bounds
- ReplicateByte edge cases: negative count, invalid byte values, size limits

These failing tests validate proper error handling for invalid inputs adapted from `plutus-conformance` tests.

### Cryptographic Scripts

Advanced cryptography using Plutus built-in functions:

- **BLS12-381**: Signature verification, aggregate signatures, Groth16 zero-knowledge proofs, VRF
- **SECP256k1**: ECDSA and Schnorr signature verification
- **Hashing**: RIPEMD-160, SHA2-256, SHA3-256, BLAKE2b-224/256

### Governance Scripts

PlutusV3-specific scripts demonstrating governance features:

- Proposal validation
- Voting mechanisms
- Constitutional committee operations

## Architecture

### Module Organization Pattern

Each script category follows a consistent three-layer architecture:

1. **Common.hs**: Contains `INLINEABLE` validator logic shared across versions
2. **V_1_0.hs**: Compiles scripts targeting Plutus Core 1.0.0
3. **V_1_1.hs**: Compiles scripts targeting Plutus Core 1.1.0

Example:

```haskell
-- PlutusScripts/Basic/Common.hs
{-# INLINEABLE mkAlwaysSucceedPolicyV3 #-}
mkAlwaysSucceedPolicyV3 :: BuiltinData -> BuiltinUnit
mkAlwaysSucceedPolicyV3 _ = ()

-- PlutusScripts/Basic/V_1_1.hs
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

alwaysSucceedPolicyCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysSucceedPolicyCompiled = $$(PlutusTx.compile [||mkAlwaysSucceedPolicyV3||])
```

### Plutus Version Targeting

Scripts are compiled for both **Plutus Core 1.0.0** (PlutusV2) and **Plutus Core 1.1.0** (PlutusV3):

**Plutus Core 1.0.0 scripts:**

```haskell
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
```

**Plutus Core 1.1.0 scripts:**

```haskell
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
```

PlutusV3 (1.1.0) validators receive enhanced `ScriptContext` with governance support:

- Committee actions and voting
- Proposal transactions
- Constitutional committee hot key operations

### Script Serialization

The `envelopes` executable serializes compiled scripts to JSON envelope format:

```haskell
writeEnvelope :: PlutusLedgerLanguage -> FilePath -> CompiledCode a -> IO ()
writeEnvelope lang filename compiledCode = do
  let description = T.pack filename
  Envelope.writeCodeEnvelopeForVersion lang description compiledCode filePath
```

Output format: JSON envelope compatible with `cardano-cli` and `cardano-node-tests`.

## Development

### Build System

The project uses **haskell.nix** with Nix flakes for reproducible builds:

- **Nix Flakes**: Direct haskell.nix integration via `flake-utils`
- **CHaP**: Cardano Haskell Packages repository for Cardano-specific dependencies
- **GHC 9.6**: Configured in `nix/project.nix`
- **Binary Cache**: IOG Hydra cache for faster builds

### Common Commands

```bash
# Build library and executable
cabal build plutus-scripts

# Build only the library
cabal build scripts

# Generate script envelopes
cabal run envelopes

# Clean build artifacts
cabal clean

# Format code (treefmt)
treefmt

# Check formatting
treefmt --check

# Format Haskell files
fourmolu -i plutus-scripts/**/*.hs

# Run hlint
hlint plutus-scripts
```

### Pre-commit Hooks

Configured via `.pre-commit-config.yaml`:

- **fourmolu**: Haskell formatting (2-space indent, 100-char lines)
- **hlint**: Static analysis and linting
- **cabal-fmt**: Cabal file formatting
- **nixpkgs-fmt**: Nix expression formatting

Install hooks:

```bash
pre-commit install
```

### Code Style

- **Indentation**: 2 spaces (never tabs)
- **Line length**: <100 characters
- **Imports**: `ImportQualifiedPost` style
- **Comments**: Section-style comments for organization
- **Helper functions**: Place at module bottom

## Dependencies

### Plutus Ecosystem (from CHaP)

```
plutus-core        ^>=1.53.1.0    # Plutus Core language and evaluator
plutus-ledger-api  ^>=1.53.1.0    # Ledger API types and functions
plutus-tx          ^>=1.53.1.0    # Haskell to Plutus Core compiler
plutus-tx-plugin   ^>=1.53.1.0    # GHC plugin for PlutusTx
```

### Build Configuration

Key cabal.project settings:

- **Tests enabled**: `tests: true` by default
- **Optimization disabled**: For large dependencies (faster dev builds)
- **Test output**: `test-show-details: direct` (colorized tasty output)

### Nix Binary Cache

IOG Hydra cache significantly speeds up builds:

```nix
extra-substituters = https://cache.iog.io
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

## Integration with cardano-node-tests

### Script Envelope Format

Generated `.plutus` files are JSON envelopes containing:

- **type**: Script type (PlutusScriptV2 or PlutusScriptV3)
- **description**: Human-readable script name
- **cborHex**: CBOR-encoded Plutus script

Example envelope structure:

```json
{
  "type": "PlutusScriptV3",
  "description": "alwaysSucceedPolicyScriptV3",
  "cborHex": "..."
}
```

### E2E Test Workflow

1. **Script Generation**: `cabal run envelopes` produces `.plutus` files
2. **Distribution**: Scripts are published or copied to test environment
3. **Test Consumption**: [cardano-node-tests](https://github.com/IntersectMBO/cardano-node-tests) loads envelopes
4. **Transaction Building**: cardano-cli uses scripts in test transactions
5. **Validation**: Cardano node validates scripts during E2E tests

The scripts enable comprehensive testing of:

- Plutus V2 and V3 features (including governance)
- Bitwise operations introduced in Plutus Core 1.1.0
- Cryptographic primitives (BLS, SECP256k1)
- Validator and minting policy patterns across both versions

## Contributing

### Adding New Scripts

1. **Create validator logic** in `PlutusScripts/<Category>/Common.hs`:

   ```haskell
   {-# INLINEABLE myValidator #-}
   myValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
   ```

2. **Add versioned module** `PlutusScripts/<Category>/V_1_1.hs`:

   ```haskell
   myValidatorCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
   myValidatorCompiled = $$(PlutusTx.compile [||myValidator||])
   ```

3. **Update envelopes executable** in `app/Main.hs`:

   ```haskell
   writeEnvelopeV3 "myValidatorScript" MyCategory.myValidatorCompiled
   ```

4. **Generate and test**:
   ```bash
   cabal run envelopes
   # Verify: plutus-scripts/serialised-plutus-scripts/myValidatorScript.plutus
   ```

### Code Quality

- Run pre-commit hooks before committing
- Ensure zero hlint warnings
- Follow existing code style and organization
- Add section comments for new modules
- Test script serialization succeeds

### Commit Guidelines

- Use conventional commits: `feat:`, `fix:`, `refactor:`, `docs:`
- Reference issue numbers when applicable
- Keep commits focused and atomic

## License

Apache-2.0

Copyright 2024 Intersect.

## Resources

- [cardano-node-tests](https://github.com/IntersectMBO/cardano-node-tests) - E2E test suite consuming these scripts
- [Plutus Documentation](https://plutus.readthedocs.io/) - Plutus platform documentation
- [CHaP](https://github.com/IntersectMBO/cardano-haskell-packages) - Cardano Haskell Packages repository
- [Original Antaeus Repository](https://github.com/IntersectMBO/antaeus) - Historical origin
