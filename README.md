# driver_minaj

minaj is a lightweight GCAM input-file maker for quick testing. It builds gcamdata outputs one chunk or one
XML file at a time from an open R session, reusing every result whose code and inputs are unchanged. Keep
`driver_drake()` for full builds.

## Install

```r
remotes::install_github("GCAM-KAIST/driver_minaj")
```

Requirements:

- R 4.1 or later.
- A GCAM source tree with `input/gcamdata` (https://github.com/JGCRI/gcam-core) and the R packages gcamdata
  itself needs, which include drake: `remotes::install_deps("path/to/gcam-core/input/gcamdata")`.
- Java 8 or later on `PATH` to write XML files (on Windows, option `minaj.java_bin` can point to it instead).

```r
library(minaj)
open_minaj("path/to/gcam-core")      # loads gcamdata from that tree, once per session
```

## Coming from driver_drake()

| driver_drake() / drake | minaj |
|---|---|
| `driver_drake()` | `driver_minaj()`, with the same `stop_before`, `stop_after`, `return_inputs_of`, `return_outputs_of`, `return_data_names`, `write_xml`, `xmldir` and `quiet`, plus `config =` to build only the XML files a configuration loads |
| `drake::readd()`, `drake::loadd()`, `drake::outdated()` | `minaj::readd()`, `minaj::loadd()`, `minaj::outdated()` |
| `user_modifications` + `xml_suffix` | `variants_minaj()` |
| a drake cache (`.drake/`) | `drake_to_minaj()` adopts it as starting values |

`return_data_map_only` and `return_plan_only` are not supported. XML files go to `input/gcamdata/xml_minaj/`;
`promote_minaj()` copies finished files into `xml/`, where configurations usually point.

## Daily use

| Task | Command |
|---|---|
| Build every XML file, or only those a configuration loads | `driver_minaj()`, `driver_minaj(config = "path/to/gcam-core/exe/configuration_ref.xml")` |
| Rebuild one XML after editing a CSV or a chunk file (save, then run) | `make_minaj("electricity_water.xml")` |
| See what would run and why | `plan_minaj("electricity_water.xml")`, `why_minaj("module_energy_L223.electricity")` |
| Check the files a configuration loads | `status_minaj("path/to/gcam-core/exe/configuration_ref.xml")` |
| Copy finished files into `xml/` | `promote_minaj("electricity_water.xml")` |
| Get any table | `get_minaj("L223.StubTechCapFactor_elec")` |
| Run one chunk | `run_minaj("module_energy_L223.electricity")` |
| Step through a chunk line by line | `inputs_minaj("module_energy_L223.electricity")`, then run the lines of its MAKE branch, skipping the first (`all_data <- list(...)[[1]]`) |
| Debugger on a chunk | `debug_minaj("module_energy_L223.electricity")` |
| Low / mid / high versions of one XML | `variants_minaj("electricity.xml", list(low = list("energy/A23.globaltech_capacity_factor" = 0.9), high = list("energy/A23.globaltech_capacity_factor" = 1.1)))` |
| Same, from a table | `variants_minaj("electricity.xml", "variants.csv")` |
| What an edit reaches | `downstream_minaj("energy/A23.globaltech_capacity_factor")` |
| Tree, store and XML folder in use, with counts of chunks, XML files and CSV inputs | `info_minaj()` |

- XML files go to `input/gcamdata/xml_minaj/`, variants to `xml_minaj/variants/<stem>_<variant>.xml`; each
  folder has a `minaj_manifest.csv` with one row per file written. `make_minaj(..., override = ...)` writes
  `<stem>_override.xml`.
- After a run that re-ran chunks, `make_minaj` lists the other XML files the same edit changed.
- `status_minaj(config)` checks the exact files the configuration loads. `promote_minaj` copies only files that
  are the current base result.
- Saved edits to gcamdata's `R/*.R` files are re-sourced into the loaded package at the next call. A file that
  adds or removes a top-level name, or a change to gcamdata's NAMESPACE, DESCRIPTION or package data, triggers
  one package reload.
- In an interactive session, calls estimated to take longer than `minaj.ask_secs` (120 s) ask first.

## Overrides and variant tables

An override replaces one CSV input or data object for one call: a number (multiplies every year column,
including gcamdata's year keywords such as `final-calibration-year`, or else the `value` column), a function
of the table, a table, or, for a CSV input only, the path of a CSV file with the same columns.

A variant table (a data frame or CSV file) has columns `variant, input, op, value` (`op` is `scale`, `add` or
`set`), optionally `column` (default: every year column, else `value`; several separated by `;`), plus filter
columns named like the table's columns (for example `region`, `technology`; empty or `*` means all rows;
several values separated by `;`). Columns `note`, `family`, `mode` and `file` are ignored. A filter that
matches no rows stops with an error.

```csv
variant,input,op,value,technology
low,energy/A23.globaltech_capacity_factor,scale,0.9,PV
high,energy/A23.globaltech_capacity_factor,scale,1.1,PV
```

## What counts as a change

A stored result is reused only if all of these are unchanged: the chunk's code and every gcamdata function
it reaches; the values of the constants and package data (`data/*.rda`, `R/sysdata.rda`) those functions use;
files some functions read directly (`ModelInterface_headers.txt`, files listed in option `minaj.file_deps`);
the content of its inputs; the CSV reader or XML writer code; R and package versions; NAMESPACE. Comments and
formatting do not count. A re-run chunk whose outputs come out the same does not re-run anything downstream.

`drake_to_minaj()` adopts a drake result only when it matches the current code, inputs and package data.
It adopts nothing when `R/constants.R` differs from the one the cache was built with, the cache holds
user-modification targets, the cache was written in the last 3 minutes, or a spot check (chunks without
inputs re-run, 25 CSV files re-read) disagrees with the cache. Everything not adopted is recomputed when needed.

## Options

Set with `options()` before the first `open_minaj()` of the session; `minaj.store`, `minaj.dest` and `minaj.java_bin` are read then.

| Option | Default | Meaning |
|---|---|---|
| `minaj.store` | `file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")` | Folder for stored results, shared by all trees |
| `minaj.dest` | `"xml_minaj"` | XML folder, relative to the gcamdata folder, or absolute |
| `minaj.ask_secs` | `120` | Interactive calls estimated to take longer ask first |
| `minaj.mem_mb` | `1500` | Memory for values kept within one call (MB) |
| `minaj.compress` | `1` | gzip level for stored values (0: none) |
| `minaj.keep_xml_mb` | `200` | Largest XML file kept in the store (MB) |
| `minaj.file_deps` | none | Named list: function or chunk name to the files it reads, relative to the gcamdata folder |
| `minaj.check_outputs` | `TRUE` | Run gcamdata's output check after each chunk |
| `minaj.report_siblings` | `TRUE` | `make_minaj` lists the other XML files an edit changed |
| `minaj.java_bin` | none | Windows: folder holding `java.exe`, used when `java` is not on `PATH` |

## Results

On GCAM 9.1, all 221 XML files are byte-identical to those of `driver_drake()`. Rebuilds after small edits took
6.0 s (`driver_drake()`: 14.2 s) for an income-elasticity CSV, 6.8 s (232 s, with the old value kept) for a
`constants.R` value, and 13.7 s (46.8 s) for low / mid / high PV capacity factors. Figures, captions and scripts
are in `tests/`.

minaj runs JGCRI's gcamdata (https://github.com/JGCRI/gcam-core); its interface follows rOpenSci's drake
(https://github.com/ropensci/drake).

## License

ECL-2.0, KAIST IAM Group.
