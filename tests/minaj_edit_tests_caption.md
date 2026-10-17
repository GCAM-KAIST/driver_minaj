Fig. 2 | Rebuild times after small edits, drake and minaj, vanilla GCAM 9.1. a, Industrial income elasticity at 3 GJ per person
1.12 -> 1.20, rebuilding the five socioeconomics_incelas files with driver_drake(stop_after) and driver_minaj(stop_after), each in a fresh R
session; package loading (about 7 s for both) not shown. b, socioeconomics.GDP_DIGITS 0 -> 1 in constants.R, rebuilding the six socioeconomics
files in one open R session. drake wipes its whole cache on any constants.R change; *its files kept the old value (constants are read only
when the package loads), and stayed wrong after load_all(). minaj re-sourced constants.R in place, re-ran 2 chunks and wrote the correct
files. c, PV and PV_storage future capacity factors x0.9 / 1.0 / 1.1, three versions of electricity.xml in one open R session: drake needs
one CSV swap and driver_drake run per version; minaj makes all three in one variants_minaj() call. All drake and minaj files are byte-identical
where both are correct.
