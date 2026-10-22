# Opt-in consolidated snapshot checks

```r
source("checks/snapshot-quality.R")
issues <- remp_snapshot_issues(result_national)
assert_remp_snapshots(result_national, expected_keys = independently_established_keys)
```

The checker reads and writes no files, does not change its inputs, and is not wired into targets or either app. It returns `row`, `field` and `message` diagnostics. The assertion raises `remp_snapshot_error` with `$issues`; omit `expected_keys` if no independent manifest exists.

The supported analytical contract is a non-empty table with the current consolidated columns: Date-typed quarter starts, matching fiscal year/quarter metadata, character retailer identifiers, National/Residential/Electricity/Number labels, unique retailer/date keys, and finite non-negative whole customer counts. Zero is a valid count. Each quarter must contain exactly one `National Total` row and constituent retailer rows, whose sum must equal that quarter's total. Counts are snapshots; the checker never sums across quarters. Fractional or suppressed/non-exhaustive counts require review under a different explicit policy.

If available, `expected_keys` must contain independently established character `retailer` and Date-typed `date` pairs. The checker compares both directions; extra as well as missing observations produce a coverage diagnostic. Building this expectation from the same filtered output provides no independent completeness evidence. Without it, a wholly missing quarter can pass if the remaining quarters reconcile.

The production reader still infers panel identity from the largest National Total, scans all sheets, suppresses read failures and discards file/sheet provenance. It hard-codes the final population labels. These checks can reject inconsistent output, but cannot recover that lost provenance or distinguish a coherent business panel mislabelled as residential. Malformed fiscal labels, negative/fractional counts and inconsistent totals remain accepted by production ingestion until a caller explicitly invokes a check. The app's bundled/processed RDS mismatch and browser rendering remain outside coverage. No source workbooks or RDS artefacts are opened by these tests; workbook fixtures and customer counts are fabricated.
