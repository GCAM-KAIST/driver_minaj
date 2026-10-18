# What one edit costs a caller that starts each command fresh, Fig. 3: drake and minaj each run in a fresh R session (load included);
# persistent_minaj answers through pm from a running worker. Vanilla GCAM 9.1. 180 mm wide. R and ggplot2.
# Usage (from the package root): Rscript tests/persistent/fig_persistent.R   (writes tests/persistent/persistent_minaj_per_call.{pdf,png} and the caption)
suppressMessages(library(ggplot2))
tasks <- c("Income-elasticity edit", "Switch it back", "constants.R edit", "PV low / mid / high", "Check what changed")
tools <- c("drake (fresh R session)", "minaj (fresh R session)", "persistent_minaj (pm)")
# from incelas_timing/incelas_test.log (load + driver totals), pv_hml/drake_insession.log and minaj_insession.log (load + three versions),
# persistent/pm_tests.log (pm calls)
d <- data.frame(task = rep(tasks, each = 3), tool = rep(tools, 5),
                secs = c(33.3, 17.8, 6.2,   33.0, 14.7, 2.2,   NA, NA, 8.1,   44.7, 20.6, 6.2,   NA, NA, 0.3))
d <- d[!is.na(d$secs), ]
d$y <- (length(tasks) + 1 - match(d$task, tasks)) + c(0.22, 0, -0.22)[match(d$tool, tools)]
p <- ggplot(d, aes(secs, y, colour = tool)) +
  geom_segment(aes(x = 0.2, xend = secs, yend = y), linewidth = 0.3) + geom_point(size = 1.4) +
  geom_text(aes(label = sprintf("%.1f s", secs)), hjust = -0.25, size = 5.5 / .pt,
            family = "Arial", show.legend = FALSE) +
  scale_x_log10(breaks = c(0.3, 1, 3, 10, 30, 100), labels = c("0.3", "1", "3", "10", "30", "100"), limits = c(0.2, 200)) +
  scale_y_continuous(breaks = rev(seq_along(tasks)), labels = tasks, expand = expansion(add = 0.5)) +
  scale_colour_manual(values = c("drake (fresh R session)" = "#E69F00", "minaj (fresh R session)" = "#56B4E9", "persistent_minaj (pm)" = "#0072B2"), breaks = tools) +
  labs(x = "Wall-clock time per call (s, log scale)", y = NULL) +
  theme_classic(base_size = 6, base_family = "Arial") +
  theme(axis.line = element_line(linewidth = 0.25), axis.ticks = element_line(linewidth = 0.25), axis.text = element_text(size = 5.5, colour = "black"),
        axis.title = element_text(size = 6), legend.text = element_text(size = 5.5), legend.title = element_blank(), legend.key.size = unit(2.5, "mm"),
        legend.position = "bottom", plot.margin = margin(2, 4, 2, 2, "mm"))
for (dev in c("pdf", "png")) {
  f <- file.path("tests/persistent", paste0("persistent_minaj_per_call.", dev))
  if (dev == "pdf") cairo_pdf(f, width = 180 / 25.4, height = 85 / 25.4, family = "Arial") else png(f, width = 180 / 25.4, height = 85 / 25.4, units = "in", res = 450, family = "Arial")
  print(p); invisible(dev.off())
}
writeLines(c("Fig. 3 | Cost of one edit for a caller that starts each command fresh, vanilla GCAM 9.1. drake (driver_drake(stop_after)) and minaj",
  "(driver_minaj or variants_minaj) each run in a new R session, so package loading (8-12 s) is included; persistent_minaj answers through pm from a",
  "worker that keeps gcamdata loaded. Income elasticity: A32 industrial output elasticity edited (1.20 for drake/minaj, 1.23 for pm, both new",
  "values), five XML files rebuilt. constants.R: pm only, socioeconomics.GDP_DIGITS 0 -> 2, six XML files rebuilt without reloading gcamdata;",
  "the drake and minaj constants.R case is in Fig. 2b. PV: future PV and PV_storage capacity factors x0.9/1.0/1.1; drake = load + three driver_drake runs,",
  "minaj = load + one variants_minaj call, pm reused results stored by the earlier minaj run. Check what changed: pm diff of an XML file's tables;",
  "neither driver offers an equivalent. Outputs identical across tools wherever both are correct."), "tests/persistent/persistent_minaj_per_call_caption.md")
cat("written\n")
