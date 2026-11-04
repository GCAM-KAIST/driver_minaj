Fig. 1 | Vanilla GCAM 9.1 input files built with driver_drake() and with driver_minaj(). a, Wall-clock time of the driver call to
build all 221 gcamdata XML files of a fresh clone of JGCRI's gcam-v9.1 tag (the same source as the official release package), from an
empty cache or store: driver_drake() at its default settings, driver_minaj() of minaj 0.2.0 at its default settings (not included:
drake's load_all(), 9.9 s; minaj's package loading and open_minaj(), 18.9 s). One run each, on one computer (Windows power mode: best
power efficiency), one after the other on the same night, with desktop programs open and 11 to 12 GB of memory free at the start.
b, Agreement of each set of files with driver_drake()'s build: driver_minaj()'s 221 files are byte-identical to driver_drake()'s.
JGCRI's shipped release files differ from driver_drake()'s (and identically from driver_minaj()'s) only in formatting, with the same
content (152 files), and in numbers (69 files), mostly below 1e-9 relative; 9 files exceed 0.01%, the largest 1.5% in the
transportation_UCD files.
