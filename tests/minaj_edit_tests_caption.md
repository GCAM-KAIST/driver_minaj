Fig. 2 | Rebuild times after small edits, drake and minaj, vanilla GCAM 9.1. a, Industrial income elasticity at 3 GJ per person
1.12 -> 1.20, rebuilding the five socioeconomics_incelas files with driver_drake(stop_after) and driver_minaj(stop_after), each in a fresh R
session; package loading (8-12 s) not shown. Switch back: the original value restored and the files rebuilt. b, socioeconomics.GDP_DIGITS
0 -> 1 in constants.R, rebuilding the six socioeconomics files in one open R session. driver_drake() cleared the whole drake cache after the
edit and again after switching back (its message: 'constants.R has been changed, all cached data will be removed'); *its files kept the
previous value, and still did after load_all() (16.6 s, cache not cleared); its switch-back run followed a load_all() (8.5 s, not shown).
minaj re-sourced constants.R in place without reloading and re-ran 2 chunks; all six files changed. c, PV and PV_storage future capacity
factors x0.9 / 1.0 / 1.1, three versions of electricity.xml in one open R session: drake needs one CSV swap and driver_drake run per
version; minaj makes all three in one variants_minaj() call; Again: the same call repeated. Where both reflect the same inputs, drake and
minaj files are byte-identical. One run each, on one computer (Windows power mode: best power efficiency), one after another, with no other
computing jobs running.
