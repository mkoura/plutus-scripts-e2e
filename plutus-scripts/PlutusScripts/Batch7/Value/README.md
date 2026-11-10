# CIP-0168 Value scripts

`cabal run envelopes` generates 37 scripts for each of PlutusV3 and PlutusV4,
targeting PLC 1.1.0 and 1.2.0: 30 semantic policies, six Value-size profiles,
and one spending validator per combination. `Scenarios.hs` records the inputs
and complete expected Values. The two compilation modules have separate target
pragmas; V3 and V4 share code only for context-independent policies.

The compiler must expose `policies`, `assetCount`, `keepPolicies` and
`dropPolicies`. CHaP Plutus 1.72 provides these APIs. The existing
`no-simplifier-evaluate-builtins` option keeps their evaluation at runtime.
Check emitted UPLC for the intended builtin calls when changing these scripts.

The evaluator must also implement the 256-entry filter-list limit introduced
by IntersectMBO/plutus#8013 (commit `5644c149a6`). Released 1.72 does not provide
that limit. Batch-7 activation, PLC-version support and complete cost models
must be configured in the node/CLI used by the companion tests. Compilation
alone does not establish node support. The integration used during development
pins Plutus `1ef5a0eef37772e35b1e587f7a3b0afa61be4358` and uses 369-entry
V3/V4 cost models.

The 256/257 cases count every input-list entry, including repeated, absent and
overlong IDs. The failures accept any returned Value, so an unexpectedly
successful builtin cannot be hidden by a result assertion. The paired 256-entry
cases must succeed. This limit applies to filter inputs, not to Value size.

`succeedingValueSize{SamePolicy,MultiPolicy}{1,16,64}PolicyScript` constructs
distinct assets and observes all four builtins. Its budgets include Value
construction and checks. These profiles and the context validator measure
whole scripts; parameter-perturbation tests on isolated semantic policies
check cost-model wiring separately.

`valueStateValidator` selects its own spending input and requires exactly one
input and one continuing output at that address. Its redeemer is
`Constr 0 [mode, protocolPolicy, expectedProtocolAssetCount]`, where mode 0
preserves `keepPolicies [protocolPolicy]`, and mode 1 preserves
`dropPolicies [""]`. Both modes require a nonempty, present protocol policy
and the specified positive count in that protocol partition on both sides.
The fixture chooses the policy through its redeemer; this is not a production
state-machine validator with a fixed protected policy.

The validator checks keep/drop reconstruction for ADA and the protocol policy,
ordered policy IDs, exactly one ADA entry, and exact ADA/non-ADA asset-count
partitioning on actual input/output Values. ADA quantities may change. In keep
mode unrelated assets may change; in drop mode all native assets must remain
equal. Datum contents are irrelevant. Typed V3/V4 wrappers use their respective
ledger context encodings, including the list-encoded V4 context; regenerate
them if that schema changes.
