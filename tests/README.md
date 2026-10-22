# Synthetic checks

From the repository root, run:

```sh
Rscript --vanilla tests/run-tests.R
```

The suite uses `testthat` and the analysis libraries in a compatible existing R environment. It never restores packages, downloads inputs, runs a collector, invokes `tar_make()` or starts Shiny. Use `--vanilla` to avoid the project profile bootstrapping `renv` during verification. On a busy host, prefix the command with `nice -n 10`.

All observations are fabricated. Passing these checks protects the listed examples; it does not establish the correctness of real source data or resolve every known methodological risk.

Dependencies: `testthat`, `dplyr`, `tidyr`, `stringr`, `purrr`, `tibble`, `readxl`, `shiny`, and test-only `writexl`.

The helper loads function definitions from the existing reader without starting either app. Temporary Excel workbooks contain invented retailer names and counts. Tests exercise real workbook writing/reading, quarter dates, numeric parsing, national totals and overlapping release precedence. Counts are quarter snapshots and must not be summed across quarters as customer totals.

Remaining production risks: panel identity is inferred from the largest National Total; errors in workbook/sheet ingestion are suppressed; negative counts and inconsistent totals have no validation; malformed quarter labels can survive as NA dates; tied release precedence depends on file/sheet order; and app data may differ from the pipeline export. This suite does not establish source workbook provenance or test browser rendering.

The app checks extract the `base_data`, `retailer_status` and `filtered_data` reactive bodies from both existing app sources. Invented base tables and inputs are injected; the actual status calculation, filter modes and status join execute. An existing tier-one brand identifier exercises that code branch, with fabricated counts only. The checks do not read RDS data, start the apps, run the remaining reactive bodies or render Plotly. App startup and the UI remain unverified; base-data preparation is exercised separately below.

The workbook fixture varies retailer counts across quarters so swapped date/value pairs are detectable. Source extraction is specific to the current function and reactive definitions and should be reviewed when those definitions move or gain external configuration.

Filter fixtures include an inactive startup and a second active startup. Separate Include/Exclude cases omit retailer selection, and the intersection case makes both mode restrictions and retailer selection consequential.

The follow-up `test-snapshot-quality.R` tests the opt-in consolidated snapshot checker, including count validity, consistent fiscal metadata, population labels, unique keys, per-quarter reconciliation and independent expected-key coverage. It checks actual ingestion of a fabricated workbook against that contract. `checks/README.md` describes limits, including the inability to recover discarded sheet identity. On CISBox3 (whose `Rscript` wrapper does not support `--vanilla`) the complete suite runs as:

```sh
nice -n 10 R --vanilla --slave --file=tests/run-tests.R
```

`test-app-preparation.R` extends extraction to both real `base_data` bodies with fabricated datasets, then checks the resulting status classification and the Shiny validation condition for an empty selection. This covers coercion, dropped invalid rows, removal of National Total and sorting; it still does not start an app or read an RDS file.
