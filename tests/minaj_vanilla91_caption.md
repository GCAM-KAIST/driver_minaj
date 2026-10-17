Fig. 1 | Vanilla GCAM 9.1 input files built with drake and with minaj. a, Wall-clock time to build all 221
gcamdata XML files of a fresh clone of JGCRI's gcam-v9.1 tag (the same source as the official release package),
both on the same computer: drake with driver_drake(memory_strategy = "autoclean", garbage_collection = TRUE),
the memory-saving strategy described in gcamdata's driver_drake documentation, from an empty cache; minaj building every XML file from an empty
store. b, Agreement of each set of files with drake's build: minaj's 221 files are byte-identical to drake's.
JGCRI's shipped release files differ from drake's (and identically from minaj's) only in formatting from a
newer Java (152 files) and in numbers (69 files), mostly below 1e-9 relative; 9 files exceed 0.01%, the
largest 1.5% in the transportation_UCD files.
