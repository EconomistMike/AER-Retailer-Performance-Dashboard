# Synthetic checks

From the repository root, run:

```sh
Rscript --vanilla tests/run-tests.R
```

The suite uses `testthat` and the analysis libraries in a compatible existing R environment. It never restores packages, downloads inputs, runs a collector, invokes `tar_make()` or starts Shiny. Use `--vanilla` to avoid the project profile bootstrapping `renv` during verification. On a busy host, prefix the command with `nice -n 10`.

All observations are fabricated. Passing these checks protects the listed examples; it does not establish the correctness of real source data or resolve every known methodological risk.

Dependencies: `testthat`, `dplyr`, `tidyr`, `stringr`, `purrr`, `tibble`, `readxl`, `shiny`, and test-only `writexl`.

The helper loads function definitions from the existing reader without starting either app. Temporary Excel workbooks contain invented retailer names and counts. Tests exercise real workbook writing/reading, quarter dates, numeric parsing, national totals and overlapping release precedence. Counts are quarter snapshots and must not be summed across quarters as customer totals.

Open risks outside this suite: panel identity is inferred from the largest National Total; errors in workbook/sheet ingestion are suppressed; negative counts and inconsistent totals have no validation; malformed quarter labels can survive as NA dates; tied release precedence depends on file/sheet order; and app data may differ from the pipeline export. This suite does not establish source workbook provenance or test browser rendering.

The app checks extract only the `retailer_status` and `filtered_data` reactive bodies from both existing app sources. Invented base tables and inputs are injected; the actual status calculation, filter modes and status join execute. An existing tier-one brand identifier exercises that code branch, with fabricated counts only. The checks do not read RDS data, start the apps, run other reactive bodies or render Plotly. App startup, base-data preparation and the UI remain unverified.

The workbook fixture varies retailer counts across quarters so swapped date/value pairs are detectable. Source extraction is specific to the current function and reactive definitions and should be reviewed when those definitions move or gain external configuration.

Filter fixtures include an inactive startup and a second active startup. Separate Include/Exclude cases omit retailer selection, and the intersection case makes both mode restrictions and retailer selection consequential.
