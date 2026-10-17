# minaj 0.1.0

First public release. minaj builds GCAM input XML files from gcamdata one file at a time, for quick testing
between full builds. It runs gcamdata's own chunks, CSV reader and XML writer, so its files are the same as
`driver_drake()`'s; it only skips the work you did not ask for.

## Highlights

- **`driver_minaj()`** takes `driver_drake()`'s arguments (`stop_before`, `stop_after`, `return_inputs_of`,
  `return_outputs_of`, `return_data_names`, `write_xml`, `xmldir`, `quiet`), plus `config =` to build only the XML
  files a GCAM configuration loads. `minaj::readd()`, `loadd()` and `outdated()` work like drake's.
- **One XML at a time.** `make_minaj("electricity.xml")` re-runs only the chunks between your edit and that file.
  Saved CSV and R edits are picked up in the open session; an edit that adds or removes a top-level name, or
  changes NAMESPACE, DESCRIPTION or package data, triggers one automatic reload.
- **Nothing stale.** Each stored result is keyed by the chunk's code, every gcamdata function and constant value it
  reaches, package data, its inputs and the R environment. Editing one constant re-runs only the chunks that use it.
  A re-run chunk whose output is unchanged stops the rebuild there.
- **Only changed files are written**, to `input/gcamdata/xml_minaj/`; `promote_minaj()` copies finished files into `xml/`.
- **Sensitivity versions in one call.** `variants_minaj()` writes low, mid and high (or any) versions of one XML file
  from overrides or a small table.
- **Look inside.** `get_minaj()`, `run_minaj()`, `inputs_minaj()`, `debug_minaj()`, `plan_minaj()`, `why_minaj()`,
  `downstream_minaj()`, `status_minaj()`, `info_minaj()`.
- **Start from drake.** `drake_to_minaj()` adopts an existing drake cache where it can show the results are current.

## Checked against driver_drake()

GCAM 9.1 (tag `gcam-v9.1`), all runs on the same Windows 11 computer:

| | `driver_drake()` | minaj |
|---|---|---|
| Same XML files | | 221 of 221 byte-identical |
| Income-elasticity CSV edit, rebuild its 5 XML files (driver call; package loading, about 7 s for both, not counted) | 14.2 s (`stop_after`) | 6.0 s |
| `constants.R` edit in an open session | 232 s; cache wiped and rebuilt with the old value, still wrong after load_all() | 6.8 s, 2 chunks, no reload |
| PV capacity factor low/mid/high XML files | 46.8 s, three runs | 13.7 s, one call |

## Install

```r
remotes::install_github("GCAM-KAIST/driver_minaj")
```

Needs R 4.1 or later, a GCAM source tree with `input/gcamdata` and the R packages gcamdata itself needs (these
include drake), and Java 8 or later on `PATH` to write XML files.

## Not in this release

- The CSV outputs of `driver(write_outputs = TRUE)` that gcamdata's own tests read, and the data-map helpers.
- A full build from an empty store is still a full build.
- Tested on Windows only.
