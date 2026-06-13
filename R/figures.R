library(ggplot2)
library(dplyr)
library(ggrepel)
library(reshape2)
library(ggnewscale)
library(patchwork)

# Figure 1

kat <- read.csv("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/mca_kategori_koord.csv",
                stringsAsFactors = FALSE)
birey <- read.csv("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/mca_birey_koord.csv",
                  stringsAsFactors = FALSE)

set.seed(42)
birey$Dim1_j <- birey$Dim1 + rnorm(nrow(birey), 0, 0.035)
birey$Dim2_j <- birey$Dim2 + rnorm(nrow(birey), 0, 0.035)

eig1 <- 22.84
eig2 <- 21.40

kat <- kat %>%
  mutate(
    degisken = case_when(
      grepl("^zorba",  kategori) ~ "Peer victimization",
      grepl("^alien",  kategori) ~ "School alienation",
      grepl("^parent", kategori) ~ "Parental support"
    ),
    duzey = sub(".*_", "", kategori),
    etiket = paste0(degisken, ": ", duzey),
    degisken = factor(degisken,
                      levels = c("Parental support", "School alienation", "Peer victimization")),
    duzey_kenar = case_when(
      duzey %in% c("Low")               ~ "Low",
      duzey %in% c("Moderate")          ~ "Moderate",
      duzey %in% c("High", "None")      ~ duzey
    ),
    duzey_kenar = factor(duzey_kenar, levels = c("Low", "Moderate", "High", "None"))
  )

birey$somatik <- factor(birey$somatik, levels = c("Dusuk", "Orta", "Yuksek"),
                        labels = c("Low", "Moderate", "High"))

centroid <- birey %>%
  group_by(somatik) %>%
  summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop") %>%
  mutate(etiket = paste0(somatik, " somatic"))

renk_dom <- c("Parental support"  = "#2E7D8C",
              "School alienation"  = "#E0902A",
              "Peer victimization" = "#C1432E")

renk_som <- c("Low" = "#4C7A34", "Moderate" = "#7A4FA3", "High" = "#1F3B73")

kenar_renk <- c("Low" = "#BDBDBD", "Moderate" = "#6E6E6E", "High" = "#000000", "None" = "#9E9E9E")

nudge_x_kat <- dplyr::case_when(
  kat$kategori == "zorba_kat_None"      ~ -1.1,
  kat$kategori == "parent_kat_Moderate" ~  0.55,
  kat$kategori == "alien_kat_Low"       ~ -0.70,
  TRUE                                  ~  0
)
nudge_y_kat <- dplyr::case_when(
  kat$kategori == "zorba_kat_None"      ~  0.60,
  kat$kategori == "parent_kat_Moderate" ~ -0.55,
  kat$kategori == "alien_kat_Low"       ~ -0.123,
  TRUE                                  ~  0
)

lim <- max(abs(c(kat$Dim1, kat$Dim2, birey$Dim1, birey$Dim2))) * 1.15

fig <- ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "21", linewidth = 0.45) +
  geom_vline(xintercept = 0, color = "grey55", linetype = "21", linewidth = 0.45) +
  
  geom_point(data = birey,
             aes(x = Dim1_j, y = Dim2_j, shape = "Individual observations (jittered)"),
             color = "#C9B79C", alpha = 0.30, size = 0.7) +
  scale_shape_manual(
    values = c("Individual observations (jittered)" = 16),
    name = NULL,
    guide = guide_legend(order = 4, override.aes = list(alpha = 0.7, size = 2, color = "#C9B79C"))
  ) +
  
  geom_point(data = kat,
             aes(x = Dim1, y = Dim2, fill = degisken, color = duzey_kenar),
             shape = 21, size = 4.3, stroke = 1.1) +
  geom_text_repel(data = kat,
                  aes(x = Dim1, y = Dim2, label = etiket),
                  color = renk_dom[as.character(kat$degisken)],
                  family = "serif", fontface = "bold", size = 3.1,
                  box.padding = 0.95, point.padding = 0.45, force = 4,
                  nudge_x = nudge_x_kat, nudge_y = nudge_y_kat,
                  min.segment.length = 0, segment.size = 0.3,
                  segment.color = "grey55", max.overlaps = Inf,
                  seed = 7) +
  scale_fill_manual(
    values = renk_dom,
    name = "Psychosocial domain",
    guide = guide_legend(order = 1, override.aes = list(shape = 21, size = 4, color = "white"))
  ) +
  scale_color_manual(
    values = kenar_renk,
    name = "Category level (point border)",
    breaks = c("Low", "Moderate", "High"),
    guide = guide_legend(order = 2,
                         override.aes = list(shape = 21, size = 4, fill = "grey85", stroke = 1.1))
  ) +
  
  ggnewscale::new_scale_fill() +
  
  geom_point(data = centroid,
             aes(x = Dim1, y = Dim2, fill = somatik),
             shape = 23, size = 4.4, color = "white", stroke = 0.8) +
  geom_text_repel(data = centroid,
                  aes(x = Dim1, y = Dim2, label = etiket),
                  color = c("Low" = "#2F4D20", "Moderate" = "#4A2F66", "High" = "#122444")[as.character(centroid$somatik)],
                  family = "serif", fontface = "plain", size = 3.3,
                  box.padding = 1.1, point.padding = 0.6, force = 6,
                  min.segment.length = 0, segment.size = 0.35,
                  segment.color = "grey40",
                  nudge_x = ifelse(centroid$somatik == "Low", -0.45, 0),
                  nudge_y = ifelse(centroid$somatik == "Low", -0.45, 0.10),
                  seed = 3, max.overlaps = Inf) +
  scale_fill_manual(
    values = renk_som,
    name = "Somatic burden (group centroid)",
    guide = guide_legend(order = 3, override.aes = list(shape = 23, size = 4, color = "white"))
  ) +
  
  coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
  labs(
    x = paste0("MCA Dimension 1 (", formatC(eig1, format = "f", digits = 2), "%)"),
    y = paste0("MCA Dimension 2 (", formatC(eig2, format = "f", digits = 2), "%)")
  ) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey94", linewidth = 0.3),
    axis.title = element_text(size = 10, color = "grey25"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    legend.key.size = unit(11, "pt"),
    legend.spacing.y = unit(2, "pt"),
    plot.margin = margin(12, 12, 12, 12)
  )

ggsave("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/Figure_MCA_biplot.png",
       fig, width = 9.5, height = 7.5, dpi = 600, bg = "white")
ggsave("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/Figure_MCA_biplot.pdf",
       fig, width = 9.5, height = 7.5, bg = "white")
cat("Figure_MCA_biplot guncellendi (kategori kenar renkleri, sade centroid, None etiketi ters yon).\n")


# figure s1

surekli_gosterge <- c("BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
                      "fiziksel_aktivite_gun", "gelir_ordinal", "konut_sorunu")

gosterge_etiket <- c(
  BMI_z = "BMI z-score",
  saglikli_beslenme = "Healthy diet",
  sagliksiz_beslenme = "Unhealthy diet",
  fiziksel_aktivite_gun = "Physical activity",
  gelir_ordinal = "Income strain",
  konut_sorunu = "Housing problems"
)

kor_mat <- cor(analitik_orneklem[, surekli_gosterge], use = "complete.obs")
rownames(kor_mat) <- gosterge_etiket[rownames(kor_mat)]
colnames(kor_mat) <- gosterge_etiket[colnames(kor_mat)]

sira <- gosterge_etiket
kor_mat <- kor_mat[sira, sira]

kor_mat[upper.tri(kor_mat, diag = FALSE)] <- NA

kor_uzun <- melt(kor_mat, na.rm = TRUE)
kor_uzun$Var1 <- factor(kor_uzun$Var1, levels = rev(sira))
kor_uzun$Var2 <- factor(kor_uzun$Var2, levels = sira)

kor_uzun <- kor_uzun %>% mutate(kosegen = as.character(Var1) == as.character(Var2))

p_kor <- ggplot(kor_uzun, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = ifelse(kosegen, "1", sprintf("%.2f", value)),
                fontface = ifelse(!kosegen & abs(value) >= 0.25, "bold", "plain")),
            family = "serif", size = 3.6,
            color = ifelse(abs(kor_uzun$value) >= 0.20, "white", "grey15")) +
  scale_fill_gradient2(low = "#2E5E8C", mid = "#F4EFE6", high = "#C1432E",
                       midpoint = 0, limits = c(-0.30, 0.30),
                       name = "Pearson r",
                       breaks = c(-0.3, -0.15, 0, 0.15, 0.3)) +
  coord_fixed() +
  scale_x_discrete() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 10, color = "grey20"),
    axis.text.y = element_text(size = 10, color = "grey20"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5),
    legend.key.height = unit(1.1, "cm"),
    plot.margin = margin(10, 14, 10, 10)
  )

ggsave("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/Figure_lifestyle_corr.png",
       p_kor, width = 7.2, height = 6, dpi = 600, bg = "white")
ggsave("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/Figure_lifestyle_corr.pdf",
       p_kor, width = 7.2, height = 6, bg = "white")
cat("Figure_lifestyle_corr kaydedildi (alt ucgen, 600 dpi).\n")

citation("tidyLPA")


# figure 2


preds <- c("Peer victimization", "School alienation", "Parental support",
           "BMI z-score", "Healthy diet", "Unhealthy diet", "Physical activity",
           "Chronic illness", "Organized sport",
           "Income strain", "Housing problems",
           "Age", "Sex (female)")

blocks <- c(rep("Psychosocial microsystems", 3),
            rep("Lifestyle and health", 6),
            rep("Socioeconomic context", 2),
            rep("Sociodemographic", 2))

dat <- data.frame(
  block     = rep(blocks, times = 2),
  predictor = rep(preds, times = 2),
  model     = rep(c("Physical somatic burden", "Psychological somatic burden"), each = 13),
  or = c(1.429, 1.121, 1.001, 0.958, 0.842, 1.004, 1.061, 1.612, 1.421, 0.988, 1.054, 1.259, 2.351,
         1.840, 1.177, 0.883, 0.988, 0.860, 1.041, 1.114, 1.374, 1.573, 1.044, 0.968, 1.205, 1.820),
  lo = c(1.268, 1.041, 0.933, 0.895, 0.784, 0.938, 0.991, 1.278, 1.201, 0.923, 0.980, 1.180, 2.041,
         1.603, 1.088, 0.819, 0.922, 0.804, 0.973, 1.041, 1.085, 1.333, 0.975, 0.902, 1.127, 1.584),
  hi = c(1.611, 1.207, 1.075, 1.025, 0.904, 1.075, 1.136, 2.034, 1.681, 1.059, 1.133, 1.345, 2.707,
         2.112, 1.273, 0.953, 1.060, 0.920, 1.114, 1.192, 1.739, 1.857, 1.119, 1.040, 1.287, 2.091)
)

dat$block     <- factor(dat$block, levels = unique(blocks))
dat$predictor <- factor(dat$predictor, levels = rev(preds))
dat$model     <- factor(dat$model, levels = c("Psychological somatic burden", "Physical somatic burden"))

bright <- c("Physical somatic burden" = "#2E5E8C", "Psychological somatic burden" = "#C1432E")
matte  <- c("Physical somatic burden" = "#A1B7CB", "Psychological somatic burden" = "#E3AAA1")
txtmat <- c("Physical somatic burden" = "#7796B4", "Psychological somatic burden" = "#D78577")

dat$sig      <- dat$lo > 1 | dat$hi < 1
dat$line_col <- ifelse(dat$sig,
                       unname(bright[as.character(dat$model)]),
                       unname(matte[as.character(dat$model)]))
dat$lab      <- sprintf("%.2f (%.2f\u2013%.2f)", dat$or, dat$lo, dat$hi)
dat$xlab     <- ifelse(dat$model == "Physical somatic burden", 3.45, 5.35)
dat$lab_col  <- ifelse(dat$sig,
                       unname(bright[as.character(dat$model)]),
                       unname(txtmat[as.character(dat$model)]))

accent  <- c("#2E7D8C", "#55702F", "#B07D2A", "#6B4E71")
blk_lev <- unique(blocks)
blk_n   <- rle(blocks)$lengths

name_df <- data.frame(predictor = factor(preds, levels = rev(preds)),
                      block     = factor(blocks, levels = blk_lev),
                      col       = rep(accent, times = blk_n))

brace_df <- function(ybot, ytop, x0, w, ry, n = 25) {
  ymid <- (ybot + ytop) / 2
  th   <- function(a, b) seq(a, b, length.out = n)
  p1 <- data.frame(x = x0 + (w / 2) * cos(th(pi / 2, pi)),
                   y = (ytop - ry) + ry * sin(th(pi / 2, pi)))
  p2 <- data.frame(x = rep(x0 - w / 2, 2), y = c(ytop - ry, ymid + ry))
  p3 <- data.frame(x = (x0 - w) + (w / 2) * cos(th(0, -pi / 2)),
                   y = (ymid + ry) + ry * sin(th(0, -pi / 2)))
  p4 <- data.frame(x = (x0 - w) + (w / 2) * cos(th(pi / 2, 0)),
                   y = (ymid - ry) + ry * sin(th(pi / 2, 0)))
  p5 <- data.frame(x = rep(x0 - w / 2, 2), y = c(ymid - ry, ybot + ry))
  p6 <- data.frame(x = x0 + (w / 2) * cos(th(pi, 3 * pi / 2)),
                   y = (ybot + ry) + ry * sin(th(pi, 3 * pi / 2)))
  rbind(p1, p2, p3, p4, p5, p6)
}

brc <- do.call(rbind, lapply(seq_along(blk_n), function(i) {
  b <- brace_df(ybot = 0.62, ytop = blk_n[i] + 0.38,
                x0 = 0.285, w = 0.04, ry = 0.18)
  b$block <- blk_lev[i]
  b$col   <- accent[i]
  b
}))
brc$block <- factor(brc$block, levels = blk_lev)

blab <- data.frame(block = factor(blk_lev, levels = blk_lev),
                   x = 0.230,
                   y = (blk_n + 1) / 2,
                   lab = c("Psychosocial\nmicrosystems", "Lifestyle and health",
                           "Socioeconomic\ncontext", "Sociodemographic"),
                   col = accent)

hdr1 <- data.frame(block = factor(rep(blk_lev[1], 2), levels = blk_lev),
                   x = c(3.45, 5.35), lab = c("Physical", "Psychological"),
                   col = c("#2E5E8C", "#C1432E"))
hdr2 <- data.frame(block = factor(rep(blk_lev[1], 2), levels = blk_lev),
                   x = c(3.45, 5.35), lab = rep("OR (95% CI)", 2), col = rep("grey40", 2))

dirlab <- data.frame(block = factor(rep(blk_lev[1], 2), levels = blk_lev),
                     x = c(0.80, 1.85),
                     lab = c("\u2190 lower burden", "higher burden \u2192"))

dodge <- position_dodge(width = 0.65)

p <- ggplot(dat, aes(x = or, y = predictor, group = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.45) +
  geom_linerange(aes(xmin = lo, xmax = hi, color = line_col),
                 position = dodge, linewidth = 0.7) +
  geom_text(aes(x = xlab, label = lab, color = lab_col),
            size = 2.9, family = "serif") +
  geom_text(data = name_df, aes(x = 0.60, y = predictor, label = predictor, color = col),
            hjust = 1, size = 3.5, family = "serif", inherit.aes = FALSE) +
  geom_path(data = brc, aes(x = x, y = y, color = col, group = block),
            linewidth = 0.5, lineend = "round", inherit.aes = FALSE) +
  geom_text(data = blab, aes(x = x, y = y, label = lab, color = col),
            angle = 90, fontface = "bold.italic", size = 3.0,
            lineheight = 0.95, family = "serif", inherit.aes = FALSE) +
  geom_text(data = hdr1, aes(x = x, y = Inf, label = lab, color = col),
            vjust = -2.1, fontface = "bold", size = 3.2, family = "serif",
            inherit.aes = FALSE) +
  geom_text(data = hdr2, aes(x = x, y = Inf, label = lab, color = col),
            vjust = -0.7, size = 2.7, family = "serif", inherit.aes = FALSE) +
  geom_text(data = dirlab, aes(x = x, y = Inf, label = lab),
            color = "grey45", vjust = -0.8, fontface = "italic", size = 2.7,
            family = "serif", inherit.aes = FALSE) +
  geom_point(aes(fill = line_col),
             position = dodge, shape = 21, color = "white",
             size = 2.7, stroke = 0.6) +
  scale_color_identity(guide = "none") +
  scale_fill_identity(guide = "none") +
  scale_x_log10(breaks = c(0.7, 1, 1.5, 2, 2.5),
                labels = c("0.7", "1", "1.5", "2", "2.5")) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y") +
  coord_cartesian(xlim = c(0.65, 6.5), clip = "off") +
  labs(x = "Odds ratio (log scale)", y = NULL) +
  theme_minimal(base_family = "serif", base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(color = "grey25", size = 9.5),
    axis.title.x = element_text(size = 10.5, hjust = 0.29, margin = margin(t = 8)),
    axis.ticks.x = element_line(color = "grey70", linewidth = 0.3),
    axis.ticks.length.x = unit(2.5, "pt"),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing.y = unit(1.1, "lines"),
    legend.position = "none",
    panel.border = element_blank(),
    plot.margin  = margin(34, 12, 8, 210)
  )

out_dir <- "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/"

ggsave(paste0(out_dir, "Figure2_forest.png"), p,
       width = 9.3, height = 8.4, dpi = 600, bg = "white")

ggsave(paste0(out_dir, "Figure2_forest.pdf"), p,
       width = 9.3, height = 8.4, device = cairo_pdf, bg = "white")


# Figure 3


library(qgraph)

np <- readRDS("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/network_figure3.rds")

W <- np$W; sg <- np$signs; ec <- np$edgecolor; nd <- np$dugumler; p <- ncol(W)

ag_label <- c("Physical\nsomatic", "Psychological\nsomatic", "Bullying\nexposure",
              "School\nalienation", "Parental\nsupport", "BMI", "Healthy\ndiet",
              "Unhealthy\ndiet", "Physical\nactivity", "Income", "Housing\nproblems",
              "Age", "Sex", "Chronic\ndisease", "Sports\nparticipation")
b_kisa <- c("Phys. somatic", "Psych. somatic", "Bullying", "School alien.",
            "Parent. support", "BMI", "Healthy diet", "Unhealthy diet",
            "Phys. activity", "Income", "Housing", "Age", "Sex", "Chronic dis.", "Sports")

grp <- list("Somatic dimensions" = 1:2, "Psychosocial system" = 3:5,
            "Biological-demographic" = c(6,12,13,14), "Lifestyle-SES" = c(7,8,9,10,11,15))
gcol <- c("#E69F00","#56B4E9","#009E73","#CC79A7")
kat_id <- match(c("CINSIYET","kronik_hastalik","spor_etkinlik"), nd)

sgn <- sg; sgn[is.na(sgn) | sgn == 0] <- 1
W_plot <- W * sgn
ecol <- matrix("grey55", p, p)
ecol[ec == "darkgreen"] <- "#1F78B4"; ecol[ec == "red"] <- "#E31A1C"

st <- colSums(abs(W))
vs <- 6 + 4 * (st - min(st)) / (max(st) - min(st))

pie_v <- numeric(p)
for (j in 1:p) {
  r2 <- np$pred$R2[j]; ncc <- np$pred$nCC[j]
  pie_v[j] <- ifelse(!is.na(r2), r2, ifelse(!is.na(ncc), ncc, 0))
}

bmi_id <- match("BMI_z", nd)
bagli <- setdiff(1:p, bmi_id)
Wsub <- W_plot[bagli, bagli]
set.seed(7)
Lsub <- qgraph(Wsub, layout = "spring", repulsion = 1.1, DoNotPlot = TRUE)$layout
L <- matrix(NA, p, 2)
L[bagli, ] <- Lsub
L[,1] <- (L[,1] - min(L[bagli,1])) / (max(L[bagli,1]) - min(L[bagli,1])) * 2 - 1
L[,2] <- (L[,2] - min(L[bagli,2])) / (max(L[bagli,2]) - min(L[bagli,2])) * 2 - 1
L[,1] <- L[,1] * 1.35
fiz_id <- match("somatik_fiziksel", nd)
psi_id <- match("somatik_psikolojik", nd)
L[fiz_id, 1] <- L[fiz_id, 1] - 0.20
L[psi_id, 1] <- L[psi_id, 1] + 0.20
age_id <- match("YAS_YIL", nd)
udt_id <- match("sagliksiz_beslenme", nd)
bully_id <- match("zorbalik_maruziyet", nd)
L[bully_id, 1] <- L[age_id, 1] + 0.45
L[bully_id, 2] <- L[age_id, 2] + 0.40
L[bmi_id, 1] <- (L[age_id, 1] + L[udt_id, 1]) / 2
L[bmi_id, 2] <- (L[age_id, 2] + L[udt_id, 2]) / 2

curve_mat <- matrix(0, p, p)
curve_mat[fiz_id, psi_id] <- -0.6
curve_mat[psi_id, fiz_id] <- -0.6

es_all <- np$es
guvenli <- sign(es_all$CIlower) == sign(es_all$CIupper) & es_all$sample != 0
es_b <- es_all[guvenli, ]

ciz <- function() {
  layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE),
         widths = c(1.65, 1.5), heights = c(8, 1))
  par(mar = c(1, 3, 3, 2))
  qgraph(W_plot, layout = L, labels = ag_label, groups = grp, color = gcol,
         edge.color = ecol, vsize = vs, label.cex = 1.0, pie = pie_v,
         pieColor = "grey35", borders = TRUE, border.width = 1.2, legend = FALSE,
         esize = 12, cut = 0.10, minimum = 0.01, fade = TRUE,
         curve = curve_mat, curveAll = FALSE,
         title = "", rescale = TRUE, aspect = FALSE)
  mtext("A", side = 3, line = 0.5, adj = 0.02, cex = 1.4, font = 2)
  
  i1 <- match(es_b$node1, nd); i2 <- match(es_b$node2, nd)
  es_b$lab <- paste(b_kisa[i1], "-", b_kisa[i2])
  iskat <- (i1 %in% kat_id) | (i2 %in% kat_id)
  pcol <- ifelse(iskat, "grey45", ifelse(es_b$sample < 0, "#E31A1C", "#1F78B4"))
  ix <- order(abs(es_b$sample)); es_b <- es_b[ix,]; pcol <- pcol[ix]
  nb <- nrow(es_b)
  par(mar = c(4, 10.5, 3, 3.5))
  xt <- seq(-0.3, 0.5, by = 0.1)
  xr <- c(min(xt), max(xt))
  plot(NA, xlim = xr, ylim = c(0.5, nb + 0.5), yaxt = "n", xaxt = "n",
       xlab = "Edge weight (95% bootstrap CI)", ylab = "", bty = "l")
  abline(v = xt, col = "grey88", lwd = 0.7)
  abline(h = 1:nb, col = "grey92", lwd = 0.7)
  abline(v = 0, lty = 2, col = "grey55", lwd = 1.1)
  segments(es_b$CIlower, 1:nb, es_b$CIupper, 1:nb, lwd = 2.1, col = pcol)
  points(es_b$sample, 1:nb, pch = 16, cex = 1.05, col = pcol)
  text(es_b$CIupper, 1:nb, sprintf("%.2f", es_b$sample),
       pos = 4, cex = 0.62, col = "grey25", xpd = NA)
  axis(1, at = xt, labels = sprintf("%.1f", xt), cex.axis = 0.8, gap.axis = 0)
  axis(2, at = 1:nb, labels = es_b$lab, las = 1, cex.axis = 0.62)
  mtext("B", side = 3, line = 1, adj = -0.5, cex = 1.4, font = 2)
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend(x = 0.40, y = 0.5, xjust = 0.5, yjust = 0.5, ncol = 4, bty = "n", cex = 1.0,
         legend = c("Somatic dimensions", "Psychosocial system",
                    "Biological-demographic", "Lifestyle / SES",
                    "Positive edge", "Negative edge", "Sign undefined (categorical)"),
         pch = c(rep(21, 4), NA, NA, NA), pt.bg = c(gcol, NA, NA, NA), pt.cex = 2,
         lty = c(rep(NA, 4), 1, 1, 1), lwd = c(rep(NA, 4), 3, 3, 3),
         col = c(rep("black", 4), "#1F78B4", "#E31A1C", "grey55"))
}

png("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/Figure3_network.png",
    width = 14, height = 9.5, units = "in", res = 300)
ciz(); dev.off()
pdf("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/Figure3_network.pdf",
    width = 14, height = 9.5)
ciz(); dev.off()
cat("B kenar sayisi:", nrow(es_b), "\n")
cat("Figure3_network.png ve .pdf uretildi (A dar, B genis, eksen -0.3'ten)\n")


# figure s2

sp <- readRDS("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/shap_figure3.rds")

degisken_etiket <- c(
  zorbalik_maruziyet = "Bullying exposure",
  school_alienation = "School alienation",
  parental_support = "Parental support",
  BMI_z = "BMI z-score",
  saglikli_beslenme = "Healthy diet",
  sagliksiz_beslenme = "Unhealthy diet",
  fiziksel_aktivite_gun = "Physical activity",
  gelir_ordinal = "Income strain",
  konut_sorunu = "Housing problems",
  YAS_YIL = "Age",
  CINSIYETKadin = "Sex (female)",
  CINSIYET = "Sex (female)",
  kronik_hastalikVar = "Chronic disease",
  kronik_hastalik = "Chronic disease",
  spor_etkinlikVar = "Organized sport",
  spor_etkinlik = "Organized sport"
)

beeswarm_df <- function(shap_m, x_m, imp) {
  ord <- names(sort(imp, decreasing = TRUE))
  out <- data.frame()
  for (v in ord) {
    sv <- shap_m[, v]
    xv <- x_m[, v]
    xs <- (xv - min(xv)) / (max(xv) - min(xv) + 1e-9)
    out <- rbind(out, data.frame(degisken = v, shap = sv, deger = xs))
  }
  out$degisken <- factor(out$degisken, levels = rev(ord))
  out
}

beeswarm_panel <- function(df, baslik) {
  set.seed(2026)
  ggplot(df, aes(x = shap, y = degisken, color = deger)) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
    geom_jitter(height = 0.22, width = 0, size = 0.45, alpha = 0.55) +
    scale_color_gradient(low = "#2C7FB8", high = "#D7301F",
                         name = "Feature value",
                         breaks = c(0.05, 0.95), labels = c("Low", "High")) +
    scale_y_discrete(labels = function(x) degisken_etiket[x]) +
    labs(title = baslik, x = "SHAP value (impact on High-category probability)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey94", linewidth = 0.3),
          axis.text.y = element_text(size = 9, color = "black"),
          plot.title = element_text(size = 12, face = "bold"),
          legend.position = "right",
          legend.key.height = unit(1.2, "cm"))
}

df_fiz <- beeswarm_df(sp$shap_fiz, sp$x_fiz, sp$imp_fiz)
df_psi <- beeswarm_df(sp$shap_psi, sp$x_psi, sp$imp_psi)

p_fiz <- beeswarm_panel(df_fiz, "A  Physical somatic")
p_psi <- beeswarm_panel(df_psi, "B  Psychological somatic")

fig <- p_fiz + p_psi + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "right")

ggsave("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/FigureS2_shap.png",
       fig, width = 13, height = 6.5, dpi = 600, bg = "white")
ggsave("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/FigureS2_shap.pdf",
       fig, width = 13, height = 6.5)

cat("FigureS2_shap.png ve .pdf uretildi\n")
cat("Fiziksel onem sirasi:\n"); print(round(sort(sp$imp_fiz, decreasing = TRUE), 4))
cat("Psikolojik onem sirasi:\n"); print(round(sort(sp$imp_psi, decreasing = TRUE), 4))


