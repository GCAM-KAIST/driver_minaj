# Three edit tests on vanilla GCAM 9.1, drake vs minaj, Fig. 2 (180 mm wide). R and ggplot2.
# Usage (from the package root): Rscript tests/fig_edit_tests.R   (writes tests/minaj_edit_tests.{pdf,png} and the caption)
suppressMessages(library(ggplot2))
# from incelas_timing/incelas_test.log (a), constants_timing/insession_drake.log and insession_minaj.log (b),
# pv_hml/drake_insession.log and minaj_insession.log (c); the caption describes each panel
d <- rbind(
  data.frame(panel = "a", task = c("Edit", "Edit", "Switch back", "Switch back"),
             tool = c("drake", "minaj", "drake", "minaj"), secs = c(25.2, 8.9, 21.1, 5.0), flag = ""),
  data.frame(panel = "b", task = c("Edit", "Edit", "Switch back", "Switch back"),
             tool = c("drake", "minaj", "drake", "minaj"), secs = c(326.3, 8.2, 326.9, 4.8), flag = c("*", "", "", "")),
  data.frame(panel = "c", task = c("Three versions", "Three versions", "Again", "Again"),
             tool = c("drake", "minaj", "drake", "minaj"), secs = c(36.9, 11.5, NA, 4.0), flag = ""))
d <- d[!is.na(d$secs), ]
d$task <- factor(d$task, levels = rev(c("Edit", "Switch back", "Three versions", "Again"))); d$tool <- factor(d$tool, levels = c("minaj", "drake"))
th <- theme_classic(base_size = 6, base_family = "Arial") +
  theme(axis.line = element_line(linewidth = 0.25), axis.ticks = element_line(linewidth = 0.25), axis.text = element_text(size = 5.5, colour = "black"),
        axis.title = element_text(size = 6), legend.text = element_text(size = 5.5), legend.title = element_blank(), legend.key.size = unit(2.5, "mm"),
        strip.background = element_blank(), strip.text = element_text(size = 8, hjust = 0, face = "bold"), plot.margin = margin(2, 4, 2, 2, "mm"))
p <- ggplot(d, aes(secs, task, fill = tool)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = paste0(sprintf("%.1f s", secs), flag)),
            position = position_dodge(width = 0.75), hjust = -0.15, size = 5.5 / .pt, family = "Arial") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000), labels = c("1", "3", "10", "30", "100", "300", "1000"), expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(values = c(drake = "#E69F00", minaj = "#0072B2"), breaks = c("drake", "minaj"), labels = c(drake = "drake (driver_drake)", minaj = "minaj")) +
  labs(x = "Wall-clock time (s, log scale)", y = NULL) + th + theme(legend.position = "bottom")
for (dev in c("pdf", "png")) {
  f <- file.path("tests", paste0("minaj_edit_tests.", dev))
  if (dev == "pdf") cairo_pdf(f, width = 180 / 25.4, height = 110 / 25.4, family = "Arial") else png(f, width = 180 / 25.4, height = 110 / 25.4, units = "in", res = 450, family = "Arial")
  print(p); invisible(dev.off())
}
writeLines(c("Fig. 2 | Rebuild times after small edits, drake and minaj, vanilla GCAM 9.1. a, Industrial income elasticity at 3 GJ per person",
  "1.12 -> 1.20, rebuilding the five socioeconomics_incelas files with driver_drake(stop_after) and driver_minaj(stop_after), each in a fresh R",
  "session; package loading (8-12 s) not shown. Switch back: the original value restored and the files rebuilt. b, socioeconomics.GDP_DIGITS",
  "0 -> 1 in constants.R, rebuilding the six socioeconomics files in one open R session. driver_drake() cleared the whole drake cache after the",
  "edit and again after switching back (its message: 'constants.R has been changed, all cached data will be removed'); *its files kept the",
  "previous value, and still did after load_all() (16.6 s, cache not cleared); its switch-back run followed a load_all() (8.5 s, not shown).",
  "minaj re-sourced constants.R in place without reloading and re-ran 2 chunks; all six files changed. c, PV and PV_storage future capacity",
  "factors x0.9 / 1.0 / 1.1, three versions of electricity.xml in one open R session: drake needs one CSV swap and driver_drake run per",
  "version; minaj makes all three in one variants_minaj() call; Again: the same call repeated. Where both reflect the same inputs, drake and",
  "minaj files are byte-identical. One run each, on one computer (Windows power mode: best power efficiency), one after another, with no other",
  "computing jobs running."), "tests/minaj_edit_tests_caption.md")
cat("written minaj_edit_tests.{pdf,png} and caption\n")
