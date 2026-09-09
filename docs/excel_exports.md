# Excel export maintenance

`code/export_excel.py` produces an `.xlsx` sibling for every Git-tracked table
directly in a public `data/**/final/` directory. At introduction this covers 47
tables, including three RDS-only crosswalks. Archived subdirectories, intermediate
and raw data, restricted candidate files, and spatial geometry are excluded.
The website offers an Excel link beside each of its existing CSV/RDS downloads.

## Rebuild and validate

Requirements: Python 3.10+ (standard library), R, `jsonlite`, and a checkout with
the final RDS files and `code/shared/excel_template.xlsx` hydrated through Git LFS.
CSV LFS pointers are retrieved from the public GitHub media endpoint and checked
against the pointer's SHA-256 and byte count. The script does not change sources.

Run from the data repository:

```sh
python3 code/export_excel.py
python3 code/checks/test_excel_export.py
python3 code/checks/check_excel_exports.py
```

For a targeted rebuild:

```sh
python3 code/export_excel.py --only municipal_harm municipal_unharm
```

The manifest `docs/excel_exports.json` records source/RDS/exporter/output hashes,
row and column counts, workbook sizes and percentage columns. Matching exports
are skipped; `--force` rebuilds them. Each workbook is written to a temporary file
and atomically replaces its predecessor only after its CSV/RDS shape checks pass.
The validator reads the saved worksheet XML in full, verifies row and populated
cell counts, rejects formulas, and compares the header, first rows, middle row
and last row to the source. It also checks hashes and percentage formats.

To make small excerpts for visual inspection, pass
`--previews /tmp/gerda-workbook-previews` to the validator. These are QA excerpts,
not public datasets. The full-file validator currently expects one sheet per
dataset, as all current tables fit; extend its source-row offsets before releasing
a future table that exceeds Excel's row limit. Sheet splitting itself is covered
by `test_excel_export.py` using a small artificial row limit.

## Values and formatting

- CSV values are copied with column classes taken from the matching RDS. The
  export stops if headers, their order, or row counts differ. RDS-only crosswalks
  pass through a temporary CSV produced by `code/shared/excel_schema.R`.
- Identifiers, including AGS, state/county/constituency codes and person IDs,
  are explicit text cells. Existing leading zeros survive. The exporter does
  not invent zeros that were already absent in the source.
- Numeric values stay numeric. Party vote shares, turnout and other documented
  proportions use `0.00%`. The raw federal municipality file's party values are
  counts. Seat counts, flags, years, population and crosswalk weights remain
  numbers. Whole numbers display without a decimal suffix; fractional numbers
  display two decimal places. Stored values are not rounded to their displayed precision.
- Date and timestamp columns are Excel serial values with ISO display formats;
  timestamp instants are represented in UTC. Text dates in a character column
  retain the source representation.
- Empty fields, `NA` and `NaN` become empty cells. Real zero stays zero. Non-finite
  numeric values are preserved as text because Excel cannot store them as numeric
  cells. Literal text beginning with `=` is stored as text, never as a formula.
- Each sheet has a filter and a frozen header and first identifier column. No
  source columns are renamed, dropped, or reordered. Tables exceeding 1,048,575
  data rows continue on another sheet with the same header; oversized column
  counts or cell text fail rather than truncate.
- Workbooks use classic ZIP packaging to avoid ZIP64 repair prompts in Excel.
  A future worksheet exceeding Python's classic ZIP size limit fails atomically
  and will need splitting into smaller sheets before publication.
- Excel itself has roughly 15 significant digits of numerical precision; CSV
  and RDS remain the preferred formats for full-precision analysis.

The formatter identifies party columns in the established wide election
schemas after excluding metadata. When adding a new numeric metadata column to
one of those outputs, update `METADATA`/`column_kind` in `code/export_excel.py`
and its regression checks so it is not displayed as a percentage. Inspect
`percentage_columns` in the manifest during release review.

## Workbook template

`code/shared/excel_template.xlsx` supplies the workbook styles, created with
Artifact Tool. The exporter reads its header style from A1 and its text, number,
percentage, date, boolean, integer, timestamp and count styles from A2:H2. It streams
worksheet XML into the XLSX ZIP container to keep memory bounded for wide files;
it does not copy the template's example values or Excel Table. Modern checkbox
extensions are removed for compatibility with Excel 2019.

## Website and release order

The Jekyll website lives in the sibling `awiedem.github.io` repository.
`election-data.md` contains the download links and loads
`assets/js/gerda-downloads.js`. That script fetches the CORS-enabled GitHub media
URL and saves a Blob with an explicit `.csv` or `.xlsx` filename. Normal links
remain available without JavaScript and as a fallback after an error. CSV data
is not reformatted or converted in the browser.

1. Regenerate and validate the data exports.
2. Commit and publish the `.xlsx` files, exporter, template and manifest in the
   data repository. `.xlsx` is already covered by the repository's Git LFS rules.
3. Verify the new Excel files are downloadable through GitHub's media endpoint.
4. Publish the website links and guidance. Publishing the website first would
   expose Excel links before their files exist remotely.

After later data updates, rerun the exporter and validator before publishing the
updated data. The Excel links are stable and do not need editing for a refresh.
