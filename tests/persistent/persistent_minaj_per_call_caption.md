Fig. 3 | Cost of one edit for a caller that starts each command fresh, vanilla GCAM 9.1. drake (driver_drake(stop_after)) and minaj
(driver_minaj or variants_minaj) each run in a new R session, so package loading (6-13 s) is included; persistent_minaj answers through pm from a
worker that keeps gcamdata loaded. Income elasticity: A32 industrial output elasticity edited (1.20 for drake/minaj, 1.23 for pm, both new
values), five XML files rebuilt. constants.R: pm only, socioeconomics.GDP_DIGITS 0 -> 2, six XML files rebuilt without reloading gcamdata;
the drake and minaj constants.R case is in Fig. 2b. PV: future PV and PV_storage capacity factors x0.9/1.0/1.1; drake = load + three driver_drake runs,
minaj = load + one variants_minaj call, pm reused results stored by the earlier minaj run. Check what changed: pm diff of an XML file's tables;
neither driver offers an equivalent. Outputs identical across tools wherever both are correct.
