# minaj 0.2.0

minaj 0.2.0 adds a session that AI assistants and scripts can call, add-on XML files that hold only what an override
changes, background XML writers, and the CSV outputs and data map of gcamdata's `driver()`.

## Highlights

- **`persistent_minaj()`** keeps one gcamdata tree loaded and answers commands from a running R process:
  `persistent_minaj(mcp = TRUE)` serves the Model Context Protocol on standard input and output (tools `status`, `make`,
  `plan`, `get`, `diff`, `trace`, `downstream`, `variants`, `addon` and `try`), and `pm` sends the same commands from a
  shell. `make`, `get` and `variants` do not run chunks while free memory is below option `minaj.ram_floor_gb`
  (default 4 GB).
- **Add-on files.** `addon_minaj()` writes `<stem>_addon.xml`: only what an override changes, to load directly after its
  base file. A universal add-on works in any configuration that loads that file; with `config =`, a
  configuration-specific one in that configuration. What an add-on cannot carry on its own stops with a list of each
  place and reason; use `<stem>_override.xml` for those. Before an add-on is kept, GCAM 9.1's parse rules
  (`inst/extdata/gcam_parse_rules.csv`) are applied to the base file plus the add-on and to the override file, and
  the two must agree. `status_minaj(config)` reports each add-on the configuration loads.
- **Background XML writers.** Option `minaj.xml_workers` (default 3): `driver_minaj()` converts up to this many XML files at
  once in background R processes while it keeps running chunks; the files are byte-identical to those written one at a
  time.
- **CSV outputs and the data map.** `driver_minaj(write_outputs = TRUE, outdir =)` writes each table with the tree's own
  `save_chunkdata()`, so the files are those `driver(write_outputs = TRUE)` writes; a file whose content is current is
  not written again. `driver_minaj(return_data_map_only = TRUE)` returns the data map as `driver()` does.
- **Tests.** `R CMD check` and `devtools::test()` run testthat tests on a small made-up gcamdata tree, with no GCAM tree
  needed. `tests/run_tree_tests.R` runs the tests that need a GCAM tree and writes one summary.

## Other changes

- `driver_minaj(config =)`, `status_minaj()`, `outdated(config =)` and `addon_minaj(config =)` skip comments in the
  configuration that span several lines.
- No function runs the Java XML writer while free memory is below option `minaj.ram_floor_gb`; files already in the
  store are still copied.
- An XML file larger than `minaj.keep_xml_mb` is moved into its folder instead of copied from the store.
- Relative CSV paths in `override =` are taken from the working directory in every function.
- Java is also found through `JAVA_HOME` and in the usual install folders on Windows, macOS and Linux.
- `open_minaj()` with a different store opens the tree again with that store.
- `pm -h`, `pm --help` and `pm help` print the usage.
- minaj now imports callr and xml2; testthat is suggested.

## Fixes

- persistent_minaj: `get` without `n`; `plan` and `downstream` without targets, and `trace` and `downstream` with an
  unknown name, give an error; `get` with a filter column the table lacks gives an error; row operations with several
  values, or one operation sent as an object, are read correctly; a number sent as text is a factor; `try` leaves the
  sandbox unchanged when an override is invalid; `pm` prints the whole summary line.
- Stopping one persistent_minaj worker no longer removes the worker files another worker on the same store has written.

## Checked

GCAM 9.1, one run each, on the computer used for 0.1.0 (Intel Core Ultra 7 258V, 32 GB RAM, Windows 11 in the "best
power efficiency" power mode, R 4.4.2):

| | Result |
|---|---|
| Full build of all 221 XML files from an empty store or cache (driver call), one after the other on the same night, desktop programs open, 11 to 12 GB of memory free at the start | 2215 s; `driver_drake()`: 3412 s; all 221 files byte-identical |
| All 221 XML files from stored values, 1 and 3 XML writers | 783 s and 416 s (peak memory 2.8 and 6.2 GB), byte-identical |
| `driver_minaj(write_outputs = TRUE)` against `driver(write_outputs = TRUE)` | 1,451 of 1,451 CSV files byte-identical |
| `driver_minaj(return_data_map_only = TRUE)` against `driver()` | identical, 3,527 rows |
| One CSV edit, 5 XML files: `persistent_minaj()` call against `driver_minaj()` in a new R session | 6.2 s against 17.8 s |
| Add-on files | checked against GCAM 9.1 itself, universal and configuration-specific |

## Install

```r
remotes::install_github("GCAM-KAIST/driver_minaj")
```

Needs R 4.1 or later, a GCAM source tree with `input/gcamdata` and the R packages gcamdata itself needs (these
include drake), and Java 8 or later to write XML files.

## Not in this release

- A full build from an empty store still runs every chunk.
- Tested on Windows only.

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

GCAM 9.1 (tag `gcam-v9.1`), one run each, all on the same computer (Intel Core Ultra 7 258V, 32 GB RAM, Windows 11 in the "best power efficiency" power mode, R 4.4.2), one after another, with no other computing jobs running:

| | `driver_drake()` | minaj |
|---|---|---|
| Full build of all 221 XML files from an empty cache or store (driver call; drake's load_all(), 11.5 s, and minaj's package loading and open_minaj(), 16.2 s, not counted) | 1930 s | 1790 s |
| Same XML files | | 221 of 221 byte-identical |
| Income-elasticity CSV edit, rebuild its 5 XML files (driver call; package loading, 8-9 s for both, not counted) | 25.2 s (`stop_after`) | 8.9 s |
| `constants.R` edit in an open session | 326 s; cache cleared and rebuilt with the previous value; after load_all() the files still had the previous value | 8.2 s, 2 chunks, no reload |
| PV capacity factor low/mid/high XML files | 36.9 s, three runs | 11.5 s, one call |

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
