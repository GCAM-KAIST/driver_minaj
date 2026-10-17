# Vanilla GCAM 9.1 test figure, Fig. 1 (180 mm wide). R and ggplot2.
# Usage (from the package root): Rscript tests/fig_vanilla.R   (writes tests/minaj_vanilla91.{pdf,png})
suppressMessages(library(ggplot2))
D <- "tests/vanilla91"
# written by: Rscript tests/compare_xml_dirs.R tests/vanilla91/compare_drake_minaj_jgcri.csv drake=<tree>/input/gcamdata/xml
#   minaj=<tree>/input/gcamdata/xml_minaj jgcri=<XML folder of the GCAM 9.1 release package>
cmp <- read.csv(file.path(D, "compare_drake_minaj_jgcri.csv"), stringsAsFactors = FALSE)
bin <- function(v) {
  rel <- suppressWarnings(as.numeric(sub(".*max rel ([0-9.e+-]+) .*", "\\1", v)))
  ifelse(v == "identical", "Byte-identical",
  ifelse(grepl("^same content", v), "Same content, formatting only",
  ifelse(rel < 1e-9, "Numbers differ < 1e-9", ifelse(rel < 1e-4, "Numbers differ 1e-9 to 0.01%", "Numbers differ > 0.01%"))))
}
lev <- c("Byte-identical", "Same content, formatting only", "Numbers differ < 1e-9", "Numbers differ 1e-9 to 0.01%", "Numbers differ > 0.01%")
b <- rbind(data.frame(cmp = "minaj vs drake", cat = bin(cmp$minaj_vs_drake)), data.frame(cmp = "JGCRI release vs drake", cat = bin(cmp$jgcri_vs_drake)))
b <- as.data.frame(table(cmp = b$cmp, cat = factor(b$cat, levels = lev))); b <- b[b$Freq > 0, ]
b$cmp <- factor(b$cmp, levels = c("JGCRI release vs drake", "minaj vs drake")); b$cat <- factor(b$cat, levels = rev(lev))
# measured: wall-clock minutes for a full build of all 221 XML files on the same computer, package loading included:
#   drake with driver_drake(memory_strategy = "autoclean", garbage_collection = TRUE) from an empty cache;
#   minaj building every XML file from an empty store
a <- data.frame(tool = factor(c("drake (driver_drake)", "minaj"), levels = c("minaj", "drake (driver_drake)")), min = c(68.7, 44.5))
oi <- c("Byte-identical" = "#0072B2", "Same content, formatting only" = "#56B4E9", "Numbers differ < 1e-9" = "#009E73",
        "Numbers differ 1e-9 to 0.01%" = "#E69F00", "Numbers differ > 0.01%" = "#D55E00")
th <- theme_classic(base_size = 6, base_family = "Arial") +
  theme(axis.line = element_line(linewidth = 0.25), axis.ticks = element_line(linewidth = 0.25), axis.text = element_text(size = 5.5, colour = "black"),
        axis.title = element_text(size = 6), legend.text = element_text(size = 5.5), legend.title = element_blank(), legend.key.size = unit(2.5, "mm"),
        plot.tag = element_text(size = 8, face = "bold"), plot.margin = margin(2, 4, 2, 2, "mm"))
pa <- ggplot(a, aes(min, tool, fill = tool)) + geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%.1f min", min)), hjust = -0.15, size = 5.5 / .pt, family = "Arial") +
  scale_fill_manual(values = c("drake (driver_drake)" = "#E69F00", "minaj" = "#0072B2"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) + labs(x = "Full build of all 221 XML files from scratch (min)", y = NULL, tag = "a") + th
pb <- ggplot(b, aes(Freq, cmp, fill = cat)) + geom_col(width = 0.55, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 5.5 / .pt, family = "Arial", colour = "white") +
  scale_fill_manual(values = oi, breaks = lev) + scale_x_continuous(breaks = seq(0, 221, 50), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "XML files (number)", y = NULL, tag = "b") + th + theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
w <- 180 / 25.4; h <- 80 / 25.4
for (dev in c("pdf", "png")) {
  f <- file.path("tests", paste0("minaj_vanilla91.", dev))
  if (dev == "pdf") cairo_pdf(f, width = w, height = h, family = "Arial") else png(f, width = w, height = h, units = "in", res = 450, family = "Arial")
  grid::grid.newpage(); grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2, widths = grid::unit(c(0.42, 0.58), "npc"))))
  print(pa, vp = grid::viewport(layout.pos.col = 1)); print(pb, vp = grid::viewport(layout.pos.col = 2))
  invisible(dev.off())
}
cat("written minaj_vanilla91.{pdf,png}\n")
