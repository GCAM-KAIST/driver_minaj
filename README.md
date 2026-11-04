# driver_minaj

minaj is a lightweight GCAM input-file maker for quick testing. It builds gcamdata outputs one chunk or one
XML file at a time from an open R session, reusing every result whose code and inputs are unchanged.

## Install

```r
remotes::install_github("GCAM-KAIST/driver_minaj")
```

Requirements:

- R 4.1 or later.
- A GCAM source tree with `input/gcamdata` (https://github.com/JGCRI/gcam-core) and the R packages gcamdata
  itself needs, which include drake: `remotes::install_deps("path/to/gcam-core/input/gcamdata")`.
- Java 8 or later to write XML files: `java` on `PATH`, else the folder in option `minaj.java_bin`, `JAVA_HOME`, or a JDK in the usual install folders.

```r
library(minaj)
open_minaj("path/to/gcam-core")      # loads gcamdata from that tree
```

Results go to one store per computer, shared by all trees: the folder in option `minaj.store`, by default `minaj_store`
in `tools::R_user_dir("minaj", "cache")`. To move it, set the option in `.Rprofile` and give the same folder to
persistent_minaj. A full
GCAM 9.1 build takes about 1 GB there, plus up to about 3.5 GB of XML files kept for copying (option `minaj.keep_xml_mb`).
Keep the store on a local drive, not in a synced folder such as OneDrive or Google Drive.

## Coming from driver_drake()

| driver_drake() / drake | minaj |
|---|---|
| `driver_drake()` | `driver_minaj()`, with the same `stop_before`, `stop_after`, `return_inputs_of`, `return_outputs_of`, `return_data_names`, `return_data_map_only`, `write_xml`, `xmldir` and `quiet`, plus `config =` to build only the XML files a configuration loads |
| `driver(write_outputs = TRUE, outdir =)` | `driver_minaj(write_outputs = TRUE, outdir =)`: the same CSV files, written with the tree's own `save_chunkdata()`; a current file is not written again, and the folder is not cleared |
| `drake::readd()`, `drake::loadd()`, `drake::outdated()` | `minaj::readd()`, `minaj::loadd()`, `minaj::outdated()` |
| `user_modifications` + `xml_suffix` | `variants_minaj()` |
| a drake cache (`.drake/`) | `drake_to_minaj()` adopts it as starting values |

`return_plan_only` is not supported. XML files go to `input/gcamdata/xml_minaj/`; `promote_minaj()` copies finished
files into `xml/`, where configurations usually point. `driver_minaj(return_data_map_only = TRUE)` returns the data map
as `driver()` does; to refresh gcamdata's packaged copy, run from the gcamdata folder:

```r
GCAM_DATA_MAP <- driver_minaj(return_data_map_only = TRUE)
usethis::use_data(GCAM_DATA_MAP, overwrite = TRUE)
```

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
| Only what an override changes, to load directly after the base file | `addon_minaj("electricity_water.xml", override = list("energy/A23.globaltech_capacity_factor" = 1.1))` |
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

## persistent_minaj (for AI assistants and scripts)

One R process keeps gcamdata loaded and answers commands, so a caller pays no R start-up per command.

As an MCP server for an AI assistant such as Claude Code (run in the GCAM project folder; `--scope local` keeps it to
that project):

```bash
claude mcp add minaj --scope local -- Rscript -e "minaj::persistent_minaj('path/to/gcam-core', store = 'path/to/minaj_store', mcp = TRUE)"
```

- Tools: `status`, `make`, `plan`, `get`, `diff`, `trace`, `downstream`, `variants`, `addon`, `try`. The tree opens at the
  first tool call. `addon` runs `addon_minaj()` on its `override`, or on the sandbox set with `try`; give `config` and
  `dest` as absolute paths.
- Use forward slashes in the paths. Give the store in the call: options set in `.Rprofile` may not apply when the client
  starts R. On Windows, give the full path to `Rscript.exe` if it is not on `PATH`.
- Each assistant session starts its own server (about 130 MB before the first tool call, about 430 MB after it).

Over HTTP, for scripts:

```bash
Rscript -e "minaj::persistent_minaj('path/to/gcam-core')"    # in its own terminal; runs until pm stop
```

`pm` (`system.file("bin", "pm", package = "minaj")`, bash and curl) sends the same commands, plus `stop`: for example
`pm make electricity.xml`, `pm diff L223.StubTechCapFactor_elec`, `pm try energy/A23.globaltech_capacity_factor=1.1`,
`pm addon electricity.xml energy/A23.globaltech_capacity_factor=1.1`.
It reads the port and token from `<store>/server/current.json`, written by the worker started last on that store; for
another worker, set `PM_INFO` to its `<store>/server/<tree>_<id>.json`.

In both modes, `make`, `get`, `variants` and `addon` do not run chunks, and `make`, `variants` and `addon` do not run the Java
XML writer, while free memory is below option `minaj.ram_floor_gb`,
and `make` and `addon` need `confirm` when their estimate exceeds option `minaj.server_ask_secs`.

## Overrides and variant tables

An override replaces one CSV input or data object for one call: a number (multiplies every year column,
including gcamdata's year keywords such as `final-calibration-year`, or else the `value` column), a function
of the table, a table, or, for a CSV input only, the path of a CSV file with the same columns (a relative path is
taken from the working directory).

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

## Add-on files

`addon_minaj(xml, override)` writes the base file and `<stem>_override.xml` as `make_minaj()` does, compares the two
the way GCAM parses them, and writes `xml_minaj/addon/<stem>_addon.xml` (`dest =` sets the folder) with only what
differs. Loaded directly after that exact base file, a universal add-on gives the same model as `<stem>_override.xml`
in place of the base file in any configuration that loads it; a configuration-specific add-on (below) does so only in
its configuration, while the files loaded before the base file keep their content. Both apart from the entry order in
some lists, which is reported. It prints the base file's path and md5 and the configuration line to add after it, for
example `<Value name="electricity_water_addon">.../electricity_water_addon.xml</Value>`.

- Each call that writes an add-on replaces `<stem>_addon.xml` in its folder, as `make_minaj()` does with
  `<stem>_override.xml`, and says so when it replaces a different file. Use `dest =` to keep add-ons for several
  overrides. Load one add-on per base file; to combine overrides, pass them together in one call.
- With `config =`, it reads the configuration as GCAM 9.1 does (its `xmlInputFileName`, then its `ScenarioComponents`
  values in order, and the batch file's in batch mode; paths with surrounding spaces, `&` or a drive-relative form such
  as `C:x.xml` are refused) and shows where it loads the base file from; that file must be the current base result,
  and the base file must be loaded exactly once, in `ScenarioComponents` (not only through the batch file). It names each listed file that does not exist, and stops when another
  add-on to the same base file is loaded after it. Relative paths in the configuration are read from its folder,
  which must be the folder GCAM runs in; when the base file's path is relative, the add-on line is printed relative to
  that folder and as an absolute path. Without `config =`, a different copy in the tree's `xml/` folder gives a warning.
- It stops, listing each place and reason (the first 2000 when there are more), when a change cannot be written this
  way, for example (without `config =`): removals, values or array years set only in the base file, deletes made only
  by the base file, type changes, `nocreate` patterns that depend on earlier files, rule lists that are not extended,
  dropped or changed keywords, removed key-map keys or input flags, a new order of grades or food-storage inputs (or
  of inputs inside a stub-technology), calc-base-price blocks whose (from-name, to-name) pairs the override does not
  keep, new or removed regions, deletes of containers addon_minaj never rebuilds (regions, the global technology
  database, climate models, the land allocator), tags GCAM does not read, year attributes not written as plain whole
  numbers, empty names, names holding `& < > " '` or non-ASCII characters, and modeltime, solver or other
  scenario-level changes. Use `<stem>_override.xml` for those. An add-on left from an earlier call at the same path
  is named in the message.
- Places marked "config= can decide it from the configuration's earlier files" depend on what the files loaded
  before the base file set. With `config =`, those places are decided from the earlier files: values and array years
  set only in the base file are restored (the earlier files' value, else the default), what only the base file
  creates is deleted, and an element, a rule list, a grade or food-storage input list, or a stub-technology is
  rebuilt where needed. A place stays refused, for example, when an earlier file deletes there, writes there with
  `nocreate` after an earlier file created it, created it as another type, is missing, or holds content the add-on
  would have to restore. The first call that needs the earlier files reads each of them once in full to index it
  (minutes for a base file late in a full configuration); after that only the blocks needed are read (a file not laid
  out as gcamdata writes it: whole). Indexes and answers are kept in the store per file version, so a changed earlier
  file is indexed again (without a store, every call indexes again). The check of a configuration-specific add-on
  reads the earlier files in full once per R session and refuses when one of them sets a name or year with a child
  element. An add-on that depends on the earlier files is configuration-specific: valid directly after the base file
  while the files loaded before it keep their content; its header comment and the final message say so, and
  `status_minaj(config)` reports it as current or stale. Other add-ons stay universal. A value with no default is
  un-set by writing `-0`; with option `minaj.addon_neg0 = FALSE` the element holding it is rebuilt instead where possible, else the change is refused. Defaults come from the parse-rule file, which
  the check below also reads; the message after the counts gives the number of restored defaults and the first 5
  (`restored defaults`).
- When nothing differs, the add-on is an empty `<scenario></scenario>`. An add-on larger than option
  `minaj.addon_max_ratio` times `<stem>_override.xml` is not written.
- Before it keeps the add-on, a separate check applies GCAM's parse rules to the base file followed by the add-on and
  to `<stem>_override.xml` alone, whether or not earlier files hold the same objects; a configuration-specific add-on
  is checked again, where it differs, after the configuration's earlier files for its model years, and only that
  comparison decides. Any difference stops with an internal error that lists the places, and no add-on is written.
  The check uses the same parse-rule file, XML reader and (with `config =`) configuration reader as the writer.
- GCAM's parse rules come from `inst/extdata/gcam_parse_rules.csv`, built from the GCAM 9.1 C++ code. When the tree has
  `cvs/objects`, its parse code (the files the rules cite and the files holding XML parse code, without
  `climate/source/hector/` and `build/`) must match the code the file was built from; otherwise the call stops and
  names the `minaj:::gcam_rules_build()` call that rebuilds it. One rules file serves every tree, so after a rebuild for other
  code, trees with the earlier code are refused until it is rebuilt again.
- Only the blocks (regions and other top-level parts) that differ are compared, one at a time. It does not run chunks,
  convert files to XML while free memory is below option `minaj.ram_floor_gb`, and compares files only when free memory is at
  least that floor plus about 16 times the size of the larger file (0.3 GB for a 10 MB file, 7.6 GB for a 480 MB file).

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

Set with `options()` before `open_minaj()`, which reads `minaj.store`, `minaj.dest` and, when `java` is not on `PATH`, `minaj.java_bin`.

| Option | Default | Meaning |
|---|---|---|
| `minaj.store` | `file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")` | Folder for stored results, shared by all trees |
| `minaj.dest` | `"xml_minaj"` | XML folder, relative to the gcamdata folder, or absolute |
| `minaj.ask_secs` | `120` | Interactive calls estimated to take longer ask first |
| `minaj.mem_mb` | `1500` | Memory for values kept within one call (MB) |
| `minaj.compress` | `1` | gzip level for stored values (0: none) |
| `minaj.keep_xml_mb` | `200` | Largest XML file kept in the store (MB) |
| `minaj.xml_workers` | `3` | `driver_minaj()` converts up to this many XML files at once, each in a background R process started only while free memory stays above `minaj.ram_floor_gb` plus what the file needs (1: one at a time in the main process) |
| `minaj.file_deps` | none | Named list: function or chunk name to the files it reads, relative to the gcamdata folder |
| `minaj.check_outputs` | `TRUE` | Run gcamdata's output check after each chunk |
| `minaj.report_siblings` | `TRUE` | `make_minaj` lists the other XML files an edit changed |
| `minaj.java_bin` | none | Folder holding `java` (`java.exe` on Windows), used when `java` is not on `PATH` |
| `minaj.server_ask_secs` | `300` | persistent_minaj: `make` or `addon` estimated to take longer needs `confirm` |
| `minaj.ram_floor_gb` | `4` | persistent_minaj (`make`, `get`, `variants`) and `addon_minaj` do not run chunks while free memory is below this (GB); no function runs the Java XML writer below it, and `addon_minaj` compares files only above it plus about 16 times the larger file size |
| `minaj.addon_guard` | `TRUE` | `addon_minaj` adds `nocreate="1"` to the wrappers below `<world>` (not inside stub-technologies or location-info), so that, loaded without its base file, they create nothing |
| `minaj.addon_max_ratio` | `0.5` | `addon_minaj` writes no add-on larger than this times `<stem>_override.xml` (`Inf`: always write) |
| `minaj.addon_parse_cap_mb` | `100` | `addon_minaj` reads a file not laid out as gcamdata writes it, or one block of a file, only up to this size (MB) |
| `minaj.addon_neg0` | `TRUE` | `addon_minaj` (with `config =`) writes `-0` to un-set a numeric value with no default (`FALSE`: rebuilds the element holding it where possible, else refuses) |

## Results

On GCAM 9.1, all 221 XML files are byte-identical to those of `driver_drake()`. A full build from an empty store
took 2215 s (`driver_drake()` from an empty cache: 3412 s), one after the other on the same night. Rebuilds after
small edits took 8.9 s (`driver_drake()`: 25.2 s) for an income-elasticity CSV, 8.2 s (326 s, with the previous value
kept) for a `constants.R` value, and 11.5 s (36.9 s) for low / mid / high PV capacity factors. One run each, on one
computer. Figures, captions and the edit-test scripts are in `tests/`.

minaj runs JGCRI's gcamdata (https://github.com/JGCRI/gcam-core); its interface follows rOpenSci's drake
(https://github.com/ropensci/drake).

## License

ECL-2.0, KAIST IAM Group.
