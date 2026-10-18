Fig. 1 | Vanilla GCAM 9.1 input files built with drake and with minaj. a, Wall-clock time of the driver call to build all 221
gcamdata XML files of a fresh clone of JGCRI's gcam-v9.1 tag (the same source as the official release package), from an empty cache or
store: drake with driver_drake() at its default settings, minaj with driver_minaj() (not included: drake's load_all(), 11.5 s; minaj's
package loading and open_minaj(), 16.2 s). Times were logged to 0.1 min and are shown rounded to 10 s. One run each, on one computer
(Windows power mode: best power efficiency), one after the other, with no other computing jobs running. b, Agreement of each set of files
with drake's build: minaj's 221 files are byte-identical to drake's. JGCRI's shipped release files differ from drake's (and identically
from minaj's) only in formatting, with the same content (152 files), and in numbers (69 files), mostly below 1e-9 relative; 9 files
exceed 0.01%, the largest 1.5% in the transportation_UCD files.
