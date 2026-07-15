library(readxl)
library(dplyr)
library(survey)
library(lavaan)
library(MASS)
library(mice)

rm(list = ls())

select <- dplyr::select
filter <- dplyr::filter

veri_yolu <- "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/TCA2022_turetilmis_veriseti.xlsx"
df <- read_excel(veri_yolu)

df <- df %>%
  mutate(
    CINSIYET = factor(CINSIYET, levels = c("Erkek", "Kadin")),
    somatik_kategori = factor(somatik_kategori, levels = c("Dusuk", "Orta", "Yuksek"), ordered = TRUE),
    spor_etkinlik = as.integer(spor_etkinlik),
    kronik_hastalik = as.integer(kronik_hastalik),
    gelir_ordinal = as.integer(gelir_ordinal)
  )

analitik_degiskenler <- c(
  "somatik_fiziksel", "somatik_psikolojik", "somatik_genel", "somatik_kategori",
  "zorbalik_maruziyet", "okul_aidiyet", "ebeveyn_iliskisi",
  "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
  "fiziksel_aktivite_gun", "spor_etkinlik", "kronik_hastalik",
  "gelir_ordinal", "konut_sorunu",
  "CINSIYET", "YAS_YIL"
)

somatik_fiziksel_maddeler <- c("SIKLIK_BAS_AGRI_S", "SIKLIK_KARIN_AGRI_S",
                               "SIKLIK_SIRT_AGRI_S", "SIKLIK_BAS_DONME_S")
somatik_psikolojik_maddeler <- c("SIKLIK_UZGUN_HISSETME_S", "SIKLIK_SINIRLI_HUYSUZ_S",
                                 "SIKLIK_GERGIN_HISSETME_S", "SIKLIK_UYUMADA_GUCLUK_S")
somatik_maddeler <- c(somatik_fiziksel_maddeler, somatik_psikolojik_maddeler)

alienation_maddeler <- c("KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S")

parental_maddeler <- c("EBEVEYN_YARDIM_S", "EBEVEYN_IZIN_VERME_S", "EBEVEYN_ONEMSEME_S",
                       "EBEVEYN_ANLAMA_S", "EBEVEYN_CESARETLENDIRME_S", "EBEVEYN_IYI_HISSETTIRME_S")

zorbalik_maddeler <- c("SIKLIK_DENEYIM_DISLANMA_S", "SIKLIK_DENEYIM_DALGA_GECME_S",
                       "SIKLIK_DENEYIM_TEHDIT_S", "SIKLIK_DENEYIM_YOK_ETME_S",
                       "SIKLIK_DENEYIM_ITILME_S", "SIKLIK_DENEYIM_DEDIKODU_S")

cfa_ham_maddeler <- c(somatik_maddeler, alienation_maddeler)
tam_set <- unique(c(analitik_degiskenler, cfa_ham_maddeler))

cat("Full file N =", nrow(df), "\n")
cat("All required columns present:",
    all(c(tam_set, parental_maddeler, zorbalik_maddeler, "FAKTOR_FERT_1317") %in% names(df)), "\n")
eksik_kolon <- setdiff(c(tam_set, parental_maddeler, zorbalik_maddeler, "FAKTOR_FERT_1317"), names(df))
if (length(eksik_kolon) > 0) { cat("MISSING COLUMNS:\n"); print(eksik_kolon) }

analitik_orneklem <- df %>%
  filter(complete.cases(df %>% select(all_of(tam_set)))) %>%
  mutate(
    agirlik = FAKTOR_FERT_1317,
    school_alienation = 6 - rowMeans(across(all_of(alienation_maddeler)), na.rm = FALSE),
    parental_support = rowMeans(across(all_of(parental_maddeler)), na.rm = FALSE)
  )

analitik_orneklem <- as.data.frame(analitik_orneklem)

cat("\nAnalytic sample N =", nrow(analitik_orneklem), " (expected 3523)\n")
cat("Excluded N =", nrow(df) - nrow(analitik_orneklem), "\n")
cat("Weight sum =", round(sum(analitik_orneklem$agirlik), 0), "\n")

cat("\nRemaining NA counts in variables used downstream:\n")
kontrol_kolon <- c(somatik_maddeler, "somatik_fiziksel", "somatik_psikolojik",
                   "school_alienation", "parental_support", "zorbalik_maruziyet",
                   "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
                   "fiziksel_aktivite_gun", "gelir_ordinal", "konut_sorunu",
                   "YAS_YIL", "CINSIYET", "kronik_hastalik", "spor_etkinlik", "agirlik")
print(colSums(is.na(analitik_orneklem[, kontrol_kolon])))

cat("\nDirection checks (both should be positive):\n")
cat("  school_alienation vs somatik_genel:",
    round(cor(analitik_orneklem$school_alienation, analitik_orneklem$somatik_genel,
              method = "spearman"), 3), "\n")
cat("  zorbalik_maruziyet vs somatik_genel:",
    round(cor(analitik_orneklem$zorbalik_maruziyet, analitik_orneklem$somatik_genel,
              method = "spearman"), 3), "\n")
cat("  parental_support vs somatik_genel (expected negative):",
    round(cor(analitik_orneklem$parental_support, analitik_orneklem$somatik_genel,
              method = "spearman"), 3), "\n")

cat("\nSubscale summaries:\n")
cat("  somatik_fiziksel   mean =", round(mean(analitik_orneklem$somatik_fiziksel), 3),
    " sd =", round(sd(analitik_orneklem$somatik_fiziksel), 3),
    " zero =", round(100 * mean(analitik_orneklem$somatik_fiziksel == 0), 1), "%\n")
cat("  somatik_psikolojik mean =", round(mean(analitik_orneklem$somatik_psikolojik), 3),
    " sd =", round(sd(analitik_orneklem$somatik_psikolojik), 3),
    " zero =", round(100 * mean(analitik_orneklem$somatik_psikolojik == 0), 1), "%\n")

cat("\nSomatic item response levels (ordinal check):\n")
for (v in somatik_maddeler) {
  cat("  ", v, ": ", paste(sort(unique(analitik_orneklem[[v]])), collapse = " "), "\n", sep = "")
}

cat("\nBinary and factor checks:\n")
print(table(analitik_orneklem$CINSIYET))
print(table(analitik_orneklem$kronik_hastalik))
print(table(analitik_orneklem$spor_etkinlik))

ana <- analitik_orneklem

ikili <- function(x) as.integer(as.character(x) %in% c("1", "Var", "Evet", "Kadin", "TRUE"))

ana$sex_f   <- ikili(ana$CINSIYET)
ana$chronic <- ikili(ana$kronik_hastalik)
ana$sport   <- ikili(ana$spor_etkinlik)

ana$z_zorba     <- as.numeric(scale(ana$zorbalik_maruziyet))
ana$z_alien     <- as.numeric(scale(ana$school_alienation))
ana$z_parent    <- as.numeric(scale(ana$parental_support))
ana$z_bmi       <- as.numeric(scale(ana$BMI_z))
ana$z_saglikli  <- as.numeric(scale(ana$saglikli_beslenme))
ana$z_sagliksiz <- as.numeric(scale(ana$sagliksiz_beslenme))
ana$z_aktivite  <- as.numeric(scale(ana$fiziksel_aktivite_gun))
ana$z_gelir     <- as.numeric(scale(ana$gelir_ordinal))
ana$z_konut     <- as.numeric(scale(ana$konut_sorunu))
ana$z_yas       <- as.numeric(scale(ana$YAS_YIL))
ana$z_fiz       <- as.numeric(scale(ana$somatik_fiziksel))
ana$z_psi       <- as.numeric(scale(ana$somatik_psikolojik))

ana$fiz_kat <- ordered(ifelse(ana$somatik_fiziksel == 0, "Low",
                              ifelse(ana$somatik_fiziksel <= 0.75, "Moderate", "High")),
                       levels = c("Low", "Moderate", "High"))
ana$psi_kat <- ordered(ifelse(ana$somatik_psikolojik == 0, "Low",
                              ifelse(ana$somatik_psikolojik <= 1.0, "Moderate", "High")),
                       levels = c("Low", "Moderate", "High"))

ana$fiz_var <- as.integer(ana$somatik_fiziksel > 0)
ana$psi_var <- as.integer(ana$somatik_psikolojik > 0)

qf <- as.numeric(quantile(ana$somatik_fiziksel[ana$somatik_fiziksel > 0], c(1/3, 2/3)))
qp <- as.numeric(quantile(ana$somatik_psikolojik[ana$somatik_psikolojik > 0], c(1/3, 2/3)))

ana$fiz_kat4 <- ordered(ifelse(ana$somatik_fiziksel == 0, "None",
                               ifelse(ana$somatik_fiziksel <= qf[1], "Low",
                                      ifelse(ana$somatik_fiziksel <= qf[2], "Moderate", "High"))),
                        levels = c("None", "Low", "Moderate", "High"))
ana$psi_kat4 <- ordered(ifelse(ana$somatik_psikolojik == 0, "None",
                               ifelse(ana$somatik_psikolojik <= qp[1], "Low",
                                      ifelse(ana$somatik_psikolojik <= qp[2], "Moderate", "High"))),
                        levels = c("None", "Low", "Moderate", "High"))

svy <- svydesign(ids = ~1, weights = ~agirlik, data = ana)

X    <- c("z_zorba", "z_alien", "z_parent", "z_bmi", "z_saglikli", "z_sagliksiz",
          "z_aktivite", "z_gelir", "z_konut", "z_yas", "sex_f", "chronic", "sport")
X_ad <- c("Bullying", "SchoolAlien", "ParentSupport", "BMI", "HealthyDiet", "UnhealthyDiet",
          "PhysAct", "Income", "Housing", "Age", "Sex_female", "Chronic", "Sport")

cat("Nonzero tertile cutpoints physical:", round(qf, 3), "\n")
cat("Nonzero tertile cutpoints psychological:", round(qp, 3), "\n")
print(table(ana$fiz_kat))
print(table(ana$psi_kat))
print(table(ana$fiz_kat4))
print(table(ana$psi_kat4))

cat("\n===== PART 1: LATENT MIMIC MODEL =====\n\n")

olcum <- paste0("fiziksel =~ ", paste(somatik_fiziksel_maddeler, collapse = " + "), "\n",
                "psikolojik =~ ", paste(somatik_psikolojik_maddeler, collapse = " + "))
reg_f <- paste0("fiziksel ~ ",   paste0("a", seq_along(X), "*", X, collapse = " + "))
reg_p <- paste0("psikolojik ~ ", paste0("b", seq_along(X), "*", X, collapse = " + "))
fark  <- paste0("d", seq_along(X), " := a", seq_along(X), " - b", seq_along(X), collapse = "\n")
model_mimic <- paste(olcum, reg_f, reg_p, fark, sep = "\n")

mv <- ana[, c(somatik_maddeler, X, "agirlik")]

fit_mimic <- sem(model_mimic, data = mv, ordered = somatik_maddeler,
                 estimator = "WLSMV", parameterization = "delta", std.lv = TRUE)

cat("Converged:", lavInspect(fit_mimic, "converged"), "\n")
print(round(fitMeasures(fit_mimic, c("chisq.scaled", "df", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")), 4))

ps <- parameterEstimates(fit_mimic)
rf <- ps[ps$op == "~" & ps$lhs == "fiziksel", ]
rp <- ps[ps$op == "~" & ps$lhs == "psikolojik", ]
dd <- ps[ps$op == ":=", ]
rf <- rf[match(X, rf$rhs), ]
rp <- rp[match(X, rp$rhs), ]
dd <- dd[match(paste0("d", seq_along(X)), dd$label), ]

tab_mimic <- data.frame(
  Predictor = X_ad,
  Phys      = round(rf$est, 3),
  Phys_p    = round(rf$pvalue, 4),
  Psych     = round(rp$est, 3),
  Psych_p   = round(rp$pvalue, 4),
  Diff      = round(dd$est, 3),
  Diff_SE   = round(dd$se, 3),
  Diff_z    = round(dd$z, 2),
  Diff_p    = round(dd$pvalue, 4)
)
cat("\nLatent regressions and formal difference tests:\n")
print(tab_mimic, row.names = FALSE)

std <- standardizedSolution(fit_mimic)
sf  <- std[std$op == "~" & std$lhs == "fiziksel", ]
sp  <- std[std$op == "~" & std$lhs == "psikolojik", ]
sf  <- sf[match(X, sf$rhs), ]
sp  <- sp[match(X, sp$rhs), ]
cat("\nFully standardized coefficients:\n")
print(data.frame(Predictor = X_ad,
                 Phys_std  = round(sf$est.std, 3),
                 Psych_std = round(sp$est.std, 3),
                 Diff_std  = round(sf$est.std - sp$est.std, 3)), row.names = FALSE)

cat("\nLatent residual correlation:\n")
print(std[std$op == "~~" & std$lhs == "fiziksel" & std$rhs == "psikolojik",
          c("lhs", "rhs", "est.std", "se", "pvalue")], row.names = FALSE)
cat("\nR2 of latent outcomes:\n")
r2 <- lavInspect(fit_mimic, "r2")
print(round(r2[c("fiziksel", "psikolojik")], 4))
cat("\nStandardized loadings:\n")
print(std[std$op == "=~", c("lhs", "rhs", "est.std", "se", "z")], row.names = FALSE)

w_all <- lavTestWald(fit_mimic, constraints = paste0("a", seq_along(X), " == b", seq_along(X), collapse = "\n"))
w_psy <- lavTestWald(fit_mimic, constraints = "a1 == b1\na2 == b2\na3 == b3")
w_bio <- lavTestWald(fit_mimic, constraints = "a11 == b11\na12 == b12")

cat("\nWald test, all 13 paths equal across dimensions:\n")
cat("  chisq =", round(w_all$stat, 3), " df =", w_all$df,
    " p =", format.pval(w_all$p.value, digits = 4, eps = 1e-12), "\n")
cat("Psychosocial block (bullying, alienation, parental support):\n")
cat("  chisq =", round(w_psy$stat, 3), " df =", w_psy$df,
    " p =", format.pval(w_psy$p.value, digits = 4, eps = 1e-12), "\n")
cat("Biological-demographic block (sex, chronic illness):\n")
cat("  chisq =", round(w_bio$stat, 3), " df =", w_bio$df,
    " p =", format.pval(w_bio$p.value, digits = 4, eps = 1e-12), "\n")

cat("\nWeighted MIMIC attempt:\n")
fit_mimic_w <- tryCatch(
  sem(model_mimic, data = mv, ordered = somatik_maddeler, estimator = "WLSMV",
      parameterization = "delta", std.lv = TRUE, sampling.weights = "agirlik"),
  error = function(e) { cat("  not supported in this lavaan build:", conditionMessage(e), "\n"); NULL })
if (!is.null(fit_mimic_w)) {
  psw <- parameterEstimates(fit_mimic_w)
  ddw <- psw[psw$op == ":=", ]
  ddw <- ddw[match(paste0("d", seq_along(X)), ddw$label), ]
  cat("  weighted difference estimates:\n")
  print(data.frame(Predictor = X_ad, Diff_w = round(ddw$est, 3),
                   Diff_w_p = round(ddw$pvalue, 4)), row.names = FALSE)
}

cat("\n===== PART 2: CONTINUOUS OUTCOMES, NO CATEGORIZATION =====\n\n")

slim <- ana[, c(X, "agirlik", "z_fiz", "z_psi")]
slim$kimlik <- seq_len(nrow(slim))
uzun <- rbind(slim, slim)
uzun$boyut <- factor(rep(c("fiziksel", "psikolojik"), each = nrow(slim)),
                     levels = c("fiziksel", "psikolojik"))
uzun$y <- c(slim$z_fiz, slim$z_psi)

svy_uzun <- svydesign(ids = ~kimlik, weights = ~agirlik, data = uzun)
m_int  <- svyglm(as.formula(paste("y ~ boyut * (", paste(X, collapse = " + "), ")")), design = svy_uzun)
co_int <- summary(m_int)$coefficients

ana_ix <- match(X, rownames(co_int))
int_ix <- match(paste0("boyutpsikolojik:", X), rownames(co_int))

cat("Survey-weighted stacked model, both outcomes standardized:\n")
print(data.frame(
  Predictor = X_ad,
  Phys_b    = round(co_int[ana_ix, 1], 3),
  Psych_b   = round(co_int[ana_ix, 1] + co_int[int_ix, 1], 3),
  Diff      = round(co_int[int_ix, 1], 3),
  Diff_SE   = round(co_int[int_ix, 2], 3),
  Diff_t    = round(co_int[int_ix, 3], 2),
  Diff_p    = round(co_int[int_ix, 4], 4)
), row.names = FALSE)

h_f <- svyglm(as.formula(paste("fiz_var ~", paste(X, collapse = " + "))), design = svy, family = quasibinomial())
h_p <- svyglm(as.formula(paste("psi_var ~", paste(X, collapse = " + "))), design = svy, family = quasibinomial())
g_f <- svyglm(as.formula(paste("somatik_fiziksel ~", paste(X, collapse = " + "))),
              design = subset(svy, somatik_fiziksel > 0), family = Gamma(link = "log"))
g_p <- svyglm(as.formula(paste("somatik_psikolojik ~", paste(X, collapse = " + "))),
              design = subset(svy, somatik_psikolojik > 0), family = Gamma(link = "log"))

cat("\nTwo-part hurdle, logistic (any symptom) and gamma (positive part):\n")
print(data.frame(
  Predictor  = X_ad,
  Phys_OR    = round(exp(coef(h_f)[X]), 3),
  Phys_OR_p  = round(summary(h_f)$coefficients[X, 4], 4),
  Psych_OR   = round(exp(coef(h_p)[X]), 3),
  Psych_OR_p = round(summary(h_p)$coefficients[X, 4], 4),
  Phys_G     = round(exp(coef(g_f)[X]), 3),
  Phys_G_p   = round(summary(g_f)$coefficients[X, 4], 4),
  Psych_G    = round(exp(coef(g_p)[X]), 3),
  Psych_G_p  = round(summary(g_p)$coefficients[X, 4], 4)
), row.names = FALSE)

cat("\n===== PART 3: ALTERNATIVE CATEGORIZATION =====\n\n")

m3_f <- svyolr(as.formula(paste("fiz_kat ~",  paste(X, collapse = " + "))), design = svy)
m3_p <- svyolr(as.formula(paste("psi_kat ~",  paste(X, collapse = " + "))), design = svy)
m4_f <- svyolr(as.formula(paste("fiz_kat4 ~", paste(X, collapse = " + "))), design = svy)
m4_p <- svyolr(as.formula(paste("psi_kat4 ~", paste(X, collapse = " + "))), design = svy)

cat("Odds ratios, main 3-level vs nonzero-tertile 4-level:\n")
print(data.frame(
  Predictor  = X_ad,
  Phys_3lev  = round(exp(coef(m3_f)[X]), 3),
  Phys_4lev  = round(exp(coef(m4_f)[X]), 3),
  Psych_3lev = round(exp(coef(m3_p)[X]), 3),
  Psych_4lev = round(exp(coef(m4_p)[X]), 3)
), row.names = FALSE)

cat("\n===== PART 4: ORGANIZED SPORT DIAGNOSTIC =====\n\n")

m_f_adj <- svyolr(as.formula(paste("fiz_kat ~", paste(c(X, "z_psi"), collapse = " + "))), design = svy)
m_p_adj <- svyolr(as.formula(paste("psi_kat ~", paste(c(X, "z_fiz"), collapse = " + "))), design = svy)

cat("Ordinal models before and after conditioning on the other somatic dimension:\n")
print(data.frame(
  Predictor    = X_ad,
  Phys_OR      = round(exp(coef(m3_f)[X]), 3),
  Phys_OR_adj  = round(exp(coef(m_f_adj)[X]), 3),
  Psych_OR     = round(exp(coef(m3_p)[X]), 3),
  Psych_OR_adj = round(exp(coef(m_p_adj)[X]), 3)
), row.names = FALSE)
cat("\n  z_psi in physical model OR:", round(exp(coef(m_f_adj)["z_psi"]), 3), "\n")
cat("  z_fiz in psychological model OR:", round(exp(coef(m_p_adj)["z_fiz"]), 3), "\n")

dugumler <- c("z_fiz", "z_psi", "z_zorba", "z_alien", "z_parent", "z_bmi", "z_saglikli",
              "z_sagliksiz", "z_aktivite", "z_gelir", "z_konut", "z_yas", "sex_f", "chronic", "sport")
lm_f <- lm(as.formula(paste("z_fiz ~", paste(setdiff(dugumler, "z_fiz"), collapse = " + "))), data = ana)
lm_p <- lm(as.formula(paste("z_psi ~", paste(setdiff(dugumler, "z_psi"), collapse = " + "))), data = ana)

cat("\nUnregularized nodewise regression, physical node:\n")
print(round(summary(lm_f)$coefficients, 4))
cat("\nUnregularized nodewise regression, psychological node:\n")
print(round(summary(lm_p)$coefficients, 4))
cat("\nSport partial coefficients (unregularized):\n")
cat("  sport -> physical:", round(coef(lm_f)["sport"], 4),
    " p =", round(summary(lm_f)$coefficients["sport", 4], 4), "\n")
cat("  sport -> psychological:", round(coef(lm_p)["sport"], 4),
    " p =", round(summary(lm_p)$coefficients["sport", 4], 4), "\n")

cat("\n===== PART 5: WEIGHTED MIMIC AS PRIMARY MODEL =====\n\n")

cat("Weighted fit measures:\n")
print(round(fitMeasures(fit_mimic_w, c("chisq.scaled", "df", "pvalue.scaled",
                                       "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")), 4))

psw <- parameterEstimates(fit_mimic_w)
rfw <- psw[psw$op == "~" & psw$lhs == "fiziksel", ]
rpw <- psw[psw$op == "~" & psw$lhs == "psikolojik", ]
ddw <- psw[psw$op == ":=", ]
rfw <- rfw[match(X, rfw$rhs), ]
rpw <- rpw[match(X, rpw$rhs), ]
ddw <- ddw[match(paste0("d", seq_along(X)), ddw$label), ]

cat("\nWeighted latent regressions and difference tests:\n")
print(data.frame(
  Predictor = X_ad,
  Phys      = round(rfw$est, 3),
  Phys_p    = round(rfw$pvalue, 4),
  Psych     = round(rpw$est, 3),
  Psych_p   = round(rpw$pvalue, 4),
  Diff      = round(ddw$est, 3),
  Diff_SE   = round(ddw$se, 3),
  Diff_z    = round(ddw$z, 2),
  Diff_p    = round(ddw$pvalue, 4)
), row.names = FALSE)

stdw <- standardizedSolution(fit_mimic_w)
sfw  <- stdw[stdw$op == "~" & stdw$lhs == "fiziksel", ]
spw  <- stdw[stdw$op == "~" & stdw$lhs == "psikolojik", ]
sfw  <- sfw[match(X, sfw$rhs), ]
spw  <- spw[match(X, spw$rhs), ]
cat("\nWeighted standardized coefficients:\n")
print(data.frame(Predictor = X_ad,
                 Phys_std  = round(sfw$est.std, 3),
                 Psych_std = round(spw$est.std, 3),
                 Diff_std  = round(sfw$est.std - spw$est.std, 3)), row.names = FALSE)

cat("\nWeighted latent residual correlation:\n")
print(stdw[stdw$op == "~~" & stdw$lhs == "fiziksel" & stdw$rhs == "psikolojik",
           c("est.std", "se", "pvalue")], row.names = FALSE)
cat("\nWeighted R2:\n")
print(round(lavInspect(fit_mimic_w, "r2")[c("fiziksel", "psikolojik")], 4))

ww_all <- lavTestWald(fit_mimic_w, constraints = paste0("a", seq_along(X), " == b", seq_along(X), collapse = "\n"))
ww_psy <- lavTestWald(fit_mimic_w, constraints = "a1 == b1\na2 == b2\na3 == b3")
ww_bio <- lavTestWald(fit_mimic_w, constraints = "a11 == b11\na12 == b12")
ww_core <- lavTestWald(fit_mimic_w, constraints = "a1 == b1\na3 == b3\na11 == b11\na12 == b12")

cat("\nWEIGHTED Wald tests:\n")
cat("  all 13 paths equal:  chisq =", round(ww_all$stat, 3), " df =", ww_all$df,
    " p =", format.pval(ww_all$p.value, digits = 4, eps = 1e-12), "\n")
cat("  psychosocial block:  chisq =", round(ww_psy$stat, 3), " df =", ww_psy$df,
    " p =", format.pval(ww_psy$p.value, digits = 4, eps = 1e-12), "\n")
cat("  bio-demographic:     chisq =", round(ww_bio$stat, 3), " df =", ww_bio$df,
    " p =", format.pval(ww_bio$p.value, digits = 4, eps = 1e-12), "\n")
cat("  four core paths:     chisq =", round(ww_core$stat, 3), " df =", ww_core$df,
    " p =", format.pval(ww_core$p.value, digits = 4, eps = 1e-12), "\n")

cat("\nUnconditional two-factor CFA for comparison (factor correlation without predictors):\n")
model_cfa2 <- paste0("fiziksel =~ ", paste(somatik_fiziksel_maddeler, collapse = " + "), "\n",
                     "psikolojik =~ ", paste(somatik_psikolojik_maddeler, collapse = " + "))
fit_cfa2_w <- cfa(model_cfa2, data = mv, ordered = somatik_maddeler, estimator = "WLSMV",
                  parameterization = "delta", sampling.weights = "agirlik")
scfa <- standardizedSolution(fit_cfa2_w)
print(scfa[scfa$op == "~~" & scfa$lhs == "fiziksel" & scfa$rhs == "psikolojik",
           c("est.std", "se", "pvalue")], row.names = FALSE)

cat("\n===== PART 6: DESIGN EFFECT SENSITIVITY =====\n\n")

deff_tablo <- function(model, etiket) {
  co <- summary(model)$coefficients
  co <- co[X, , drop = FALSE]
  b  <- co[, 1]
  se <- co[, 2]
  out <- data.frame(
    Predictor = X_ad,
    OR        = round(exp(b), 3),
    p_deff1   = round(2 * pnorm(-abs(b / se)), 4),
    p_deff1.5 = round(2 * pnorm(-abs(b / (se * sqrt(1.5)))), 4),
    p_deff2   = round(2 * pnorm(-abs(b / (se * sqrt(2.0)))), 4),
    p_deff3   = round(2 * pnorm(-abs(b / (se * sqrt(3.0)))), 4)
  )
  cat(etiket, "\n")
  print(out, row.names = FALSE)
  cat("\n")
}
deff_tablo(m3_f, "Physical somatic, p-values under inflated SEs:")
deff_tablo(m3_p, "Psychological somatic, p-values under inflated SEs:")


cat("\n===== PART 7 (CORRECTED): SPLIT-SAMPLE REPLICATION =====\n\n")

set.seed(2026)
ana$yarim <- sample(rep(c("A", "B"), length.out = nrow(ana)))
cat("Split sizes:\n"); print(table(ana$yarim))

parental_res <- c("EBEVEYN_YARDIM_S ~~ EBEVEYN_ONEMSEME_S",
                  "EBEVEYN_ANLAMA_S ~~ EBEVEYN_CESARETLENDIRME_S")
model_par_base <- paste0("parental =~ ", paste(parental_maddeler, collapse = " + "))
model_par_mod  <- paste(c(model_par_base, parental_res), collapse = "\n")

f_par_full <- cfa(model_par_base, data = ana[, parental_maddeler], ordered = parental_maddeler,
                  estimator = "WLSMV", parameterization = "delta")
cat("\nFull sample, top modification indices of unmodified parental model:\n")
print(modindices(f_par_full, sort = TRUE, maximum.number = 6)[, c("lhs", "op", "rhs", "mi", "epc")],
      row.names = FALSE)

for (h in c("A", "B")) {
  alt <- ana[ana$yarim == h, ]
  cat("\n---- Half", h, " n =", nrow(alt), "----\n")
  
  f_par0 <- cfa(model_par_base, data = alt[, parental_maddeler], ordered = parental_maddeler,
                estimator = "WLSMV", parameterization = "delta")
  mi <- modindices(f_par0, sort = TRUE, maximum.number = 6)
  cat("Top modification indices:\n")
  print(mi[, c("lhs", "op", "rhs", "mi", "epc")], row.names = FALSE)
  mi_pair <- paste(mi$lhs, mi$op, mi$rhs)
  cat("Rank of freed pair 1:", match(parental_res[1], mi_pair), "\n")
  cat("Rank of freed pair 2:", match(parental_res[2], mi_pair), "\n")
  
  f_par1 <- cfa(model_par_mod, data = alt[, parental_maddeler], ordered = parental_maddeler,
                estimator = "WLSMV", parameterization = "delta")
  cat("Modified parental model fit:\n")
  print(round(fitMeasures(f_par1, c("chisq.scaled", "df", "cfi.scaled", "tli.scaled",
                                    "rmsea.scaled", "srmr")), 4))
  
  f_som1 <- cfa(paste0("somatik =~ ", paste(somatik_maddeler, collapse = " + ")),
                data = alt[, somatik_maddeler], ordered = somatik_maddeler,
                estimator = "WLSMV", parameterization = "delta")
  f_som2 <- cfa(model_cfa2, data = alt[, somatik_maddeler], ordered = somatik_maddeler,
                estimator = "WLSMV", parameterization = "delta")
  cat("Somatic one-factor fit:\n")
  print(round(fitMeasures(f_som1, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr")), 4))
  cat("Somatic two-factor fit:\n")
  print(round(fitMeasures(f_som2, c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr")), 4))
  cat("Scaled difference test, one vs two factor:\n")
  print(lavTestLRT(f_som1, f_som2))
  s2 <- standardizedSolution(f_som2)
  cat("Factor correlation:",
      round(s2$est.std[s2$op == "~~" & s2$lhs == "fiziksel" & s2$rhs == "psikolojik"], 3), "\n")
}

cat("\nKey odds ratios estimated separately in each half:\n")
svy_A <- svydesign(ids = ~1, weights = ~agirlik, data = ana[ana$yarim == "A", ])
svy_B <- svydesign(ids = ~1, weights = ~agirlik, data = ana[ana$yarim == "B", ])
fA <- svyolr(as.formula(paste("fiz_kat ~", paste(X, collapse = " + "))), design = svy_A)
fB <- svyolr(as.formula(paste("fiz_kat ~", paste(X, collapse = " + "))), design = svy_B)
pA <- svyolr(as.formula(paste("psi_kat ~", paste(X, collapse = " + "))), design = svy_A)
pB <- svyolr(as.formula(paste("psi_kat ~", paste(X, collapse = " + "))), design = svy_B)
print(data.frame(
  Predictor = X_ad,
  Phys_A  = round(exp(coef(fA)[X]), 3),
  Phys_B  = round(exp(coef(fB)[X]), 3),
  Psych_A = round(exp(coef(pA)[X]), 3),
  Psych_B = round(exp(coef(pB)[X]), 3)
), row.names = FALSE)

cat("\n===== PART 8: ALTERNATIVE SCORING SPECIFICATIONS =====\n\n")

f_par_fs <- cfa(model_par_mod, data = ana[, parental_maddeler], ordered = parental_maddeler,
                estimator = "WLSMV", parameterization = "delta")
f_par_noresid <- cfa(model_par_base, data = ana[, parental_maddeler], ordered = parental_maddeler,
                     estimator = "WLSMV", parameterization = "delta")
f_ali_fs <- cfa(paste0("alien =~ ", paste(alienation_maddeler, collapse = " + ")),
                data = ana[, alienation_maddeler], ordered = alienation_maddeler,
                estimator = "WLSMV", parameterization = "delta")

ana$parent_fs   <- as.numeric(scale(lavPredict(f_par_fs)[, 1]))
ana$parent_nr   <- as.numeric(scale(lavPredict(f_par_noresid)[, 1]))
ana$alien_fs    <- as.numeric(scale(-lavPredict(f_ali_fs)[, 1]))

cat("Correlation of alternative scores with the scores used in the paper:\n")
cat("  parental rowMeans vs factor score:",
    round(cor(ana$z_parent, ana$parent_fs), 3), "\n")
cat("  parental factor score with vs without residual covariances:",
    round(cor(ana$parent_fs, ana$parent_nr), 3), "\n")
cat("  alienation rowMeans vs factor score:",
    round(cor(ana$z_alien, ana$alien_fs), 3), "\n")

svy2 <- svydesign(ids = ~1, weights = ~agirlik, data = ana)
X_alt <- c("z_zorba", "alien_fs", "parent_fs", "z_bmi", "z_saglikli", "z_sagliksiz",
           "z_aktivite", "z_gelir", "z_konut", "z_yas", "sex_f", "chronic", "sport")
a3_f <- svyolr(as.formula(paste("fiz_kat ~", paste(X_alt, collapse = " + "))), design = svy2)
a3_p <- svyolr(as.formula(paste("psi_kat ~", paste(X_alt, collapse = " + "))), design = svy2)

cat("\nOdds ratios, mean scores versus latent factor scores:\n")
print(data.frame(
  Predictor    = X_ad,
  Phys_mean    = round(exp(coef(m3_f)[X]), 3),
  Phys_factor  = round(exp(coef(a3_f)[X_alt]), 3),
  Psych_mean   = round(exp(coef(m3_p)[X]), 3),
  Psych_factor = round(exp(coef(a3_p)[X_alt]), 3)
), row.names = FALSE)

cat("\n===== PART 9: FULL MEASUREMENT MODEL =====\n\n")

tum_maddeler <- c(somatik_maddeler, alienation_maddeler, parental_maddeler, zorbalik_maddeler)
model_full <- paste(
  paste0("fiziksel =~ ", paste(somatik_fiziksel_maddeler, collapse = " + ")),
  paste0("psikolojik =~ ", paste(somatik_psikolojik_maddeler, collapse = " + ")),
  paste0("alien =~ ", paste(alienation_maddeler, collapse = " + ")),
  paste0("parental =~ ", paste(parental_maddeler, collapse = " + ")),
  paste0("zorbalik =~ ", paste(zorbalik_maddeler, collapse = " + ")),
  parental_res[1], parental_res[2], sep = "\n")

fit_full <- cfa(model_full, data = ana[, tum_maddeler], ordered = tum_maddeler,
                estimator = "WLSMV", parameterization = "delta",
                sampling.weights = NULL)

cat("Full measurement model fit (alienation factor now has evaluable fit):\n")
print(round(fitMeasures(fit_full, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled",
                                    "tli.scaled", "rmsea.scaled", "srmr")), 4))

sfull <- standardizedSolution(fit_full)
cat("\nStandardized loadings:\n")
print(sfull[sfull$op == "=~", c("lhs", "rhs", "est.std", "se", "z")], row.names = FALSE)
cat("\nFactor correlations:\n")
print(sfull[sfull$op == "~~" & sfull$lhs != sfull$rhs &
              sfull$lhs %in% c("fiziksel", "psikolojik", "alien", "parental", "zorbalik") &
              sfull$rhs %in% c("fiziksel", "psikolojik", "alien", "parental", "zorbalik"),
            c("lhs", "rhs", "est.std", "se", "pvalue")], row.names = FALSE)

ave_hesap <- function(fit, fak) {
  y <- standardizedSolution(fit)
  y <- y$est.std[y$op == "=~" & y$lhs == fak]
  c(AVE = round(mean(y^2), 3), sqrtAVE = round(sqrt(mean(y^2)), 3), n_item = length(y))
}
cat("\nAVE and sqrt(AVE) per factor (Fornell-Larcker input):\n")
for (fk in c("fiziksel", "psikolojik", "alien", "parental", "zorbalik")) {
  cat("  ", fk, ": ", paste(names(ave_hesap(fit_full, fk)), ave_hesap(fit_full, fk),
                            sep = "=", collapse = "  "), "\n", sep = "")
}

cat("\n===== PART 10: MULTIPLE IMPUTATION =====\n\n")

mi_kolon <- c(somatik_maddeler, alienation_maddeler, parental_maddeler,
              "zorbalik_maruziyet", "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
              "fiziksel_aktivite_gun", "gelir_ordinal", "konut_sorunu",
              "YAS_YIL", "CINSIYET", "kronik_hastalik", "spor_etkinlik", "FAKTOR_FERT_1317")

mi_veri <- as.data.frame(df[, mi_kolon])
mi_veri$CINSIYET <- factor(mi_veri$CINSIYET, levels = c("Erkek", "Kadin"))

cat("Missing percent per variable in the full file:\n")
print(round(100 * colMeans(is.na(mi_veri)), 2))

set.seed(2026)
imp <- mice(mi_veri, m = 30, maxit = 5, method = "pmm", printFlag = FALSE)
cat("\nImputation complete. m =", imp$m, "\n")

skor_uret <- function(d) {
  d$agirlik <- d$FAKTOR_FERT_1317
  d$somatik_fiziksel   <- rowMeans(d[, somatik_fiziksel_maddeler])
  d$somatik_psikolojik <- rowMeans(d[, somatik_psikolojik_maddeler])
  d$school_alienation  <- 6 - rowMeans(d[, alienation_maddeler])
  d$parental_support   <- rowMeans(d[, parental_maddeler])
  d$sex_f   <- as.integer(d$CINSIYET == "Kadin")
  d$chronic <- as.integer(d$kronik_hastalik == 1)
  d$sport   <- as.integer(d$spor_etkinlik == 1)
  d$z_zorba     <- as.numeric(scale(d$zorbalik_maruziyet))
  d$z_alien     <- as.numeric(scale(d$school_alienation))
  d$z_parent    <- as.numeric(scale(d$parental_support))
  d$z_bmi       <- as.numeric(scale(d$BMI_z))
  d$z_saglikli  <- as.numeric(scale(d$saglikli_beslenme))
  d$z_sagliksiz <- as.numeric(scale(d$sagliksiz_beslenme))
  d$z_aktivite  <- as.numeric(scale(d$fiziksel_aktivite_gun))
  d$z_gelir     <- as.numeric(scale(d$gelir_ordinal))
  d$z_konut     <- as.numeric(scale(d$konut_sorunu))
  d$z_yas       <- as.numeric(scale(d$YAS_YIL))
  d$z_fiz       <- as.numeric(scale(d$somatik_fiziksel))
  d$z_psi       <- as.numeric(scale(d$somatik_psikolojik))
  d$fiz_kat <- ordered(ifelse(d$somatik_fiziksel == 0, "Low",
                              ifelse(d$somatik_fiziksel <= 0.75, "Moderate", "High")),
                       levels = c("Low", "Moderate", "High"))
  d$psi_kat <- ordered(ifelse(d$somatik_psikolojik == 0, "Low",
                              ifelse(d$somatik_psikolojik <= 1.0, "Moderate", "High")),
                       levels = c("Low", "Moderate", "High"))
  d
}

rubin <- function(B, SE) {
  m  <- nrow(B)
  qb <- colMeans(B)
  ub <- colMeans(SE^2)
  bb <- apply(B, 2, var)
  tt <- ub + (1 + 1/m) * bb
  se <- sqrt(tt)
  z  <- qb / se
  data.frame(est = qb, se = se, z = z, p = 2 * pnorm(-abs(z)))
}

form_f <- as.formula(paste("fiz_kat ~", paste(X, collapse = " + ")))
form_p <- as.formula(paste("psi_kat ~", paste(X, collapse = " + ")))

Bf <- matrix(NA, imp$m, length(X), dimnames = list(NULL, X))
Sf <- Bf; Bp <- Bf; Sp <- Bf
Bd <- matrix(NA, imp$m, length(X), dimnames = list(NULL, X)); Sd <- Bd

for (i in 1:imp$m) {
  d  <- skor_uret(complete(imp, i))
  sd_i <- svydesign(ids = ~1, weights = ~agirlik, data = d)
  mf <- svyolr(form_f, design = sd_i)
  mp <- svyolr(form_p, design = sd_i)
  Bf[i, ] <- coef(mf)[X]; Sf[i, ] <- summary(mf)$coefficients[X, 2]
  Bp[i, ] <- coef(mp)[X]; Sp[i, ] <- summary(mp)$coefficients[X, 2]
  
  sl <- d[, c(X, "agirlik", "z_fiz", "z_psi")]
  sl$kimlik <- seq_len(nrow(sl))
  lg <- rbind(sl, sl)
  lg$boyut <- factor(rep(c("fiziksel", "psikolojik"), each = nrow(sl)),
                     levels = c("fiziksel", "psikolojik"))
  lg$y <- c(sl$z_fiz, sl$z_psi)
  sd_l <- svydesign(ids = ~kimlik, weights = ~agirlik, data = lg)
  ml <- svyglm(as.formula(paste("y ~ boyut * (", paste(X, collapse = " + "), ")")), design = sd_l)
  cl <- summary(ml)$coefficients
  ix <- match(paste0("boyutpsikolojik:", X), rownames(cl))
  Bd[i, ] <- cl[ix, 1]; Sd[i, ] <- cl[ix, 2]
}

pf <- rubin(Bf, Sf); pp <- rubin(Bp, Sp); pd <- rubin(Bd, Sd)

cat("\nPooled odds ratios after multiple imputation, N =", nrow(df), "\n")
print(data.frame(
  Predictor   = X_ad,
  CC_Phys_OR  = round(exp(coef(m3_f)[X]), 3),
  MI_Phys_OR  = round(exp(pf$est), 3),
  MI_Phys_p   = round(pf$p, 4),
  CC_Psych_OR = round(exp(coef(m3_p)[X]), 3),
  MI_Psych_OR = round(exp(pp$est), 3),
  MI_Psych_p  = round(pp$p, 4)
), row.names = FALSE)

cat("\nPooled dissociation tests after multiple imputation:\n")
print(data.frame(
  Predictor = X_ad,
  Diff      = round(pd$est, 3),
  Diff_SE   = round(pd$se, 3),
  Diff_z    = round(pd$z, 2),
  Diff_p    = round(pd$p, 4)
), row.names = FALSE)

cat("\n===== PART 11: NETWORK STRUCTURE UNDER IMPUTATION =====\n\n")

nd <- c("z_psi", "z_zorba", "z_alien", "z_parent", "z_bmi", "z_saglikli",
        "z_sagliksiz", "z_aktivite", "z_gelir", "z_konut", "z_yas", "sex_f", "chronic", "sport")
Bnf <- matrix(NA, imp$m, length(nd), dimnames = list(NULL, nd)); Snf <- Bnf
nd2 <- c("z_fiz", nd[-1])
Bnp <- matrix(NA, imp$m, length(nd2), dimnames = list(NULL, nd2)); Snp <- Bnp

for (i in 1:imp$m) {
  d <- skor_uret(complete(imp, i))
  lf <- lm(as.formula(paste("z_fiz ~", paste(nd, collapse = " + "))), data = d)
  lp <- lm(as.formula(paste("z_psi ~", paste(nd2, collapse = " + "))), data = d)
  Bnf[i, ] <- coef(lf)[nd]; Snf[i, ] <- summary(lf)$coefficients[nd, 2]
  Bnp[i, ] <- coef(lp)[nd2]; Snp[i, ] <- summary(lp)$coefficients[nd2, 2]
}
pnf <- rubin(Bnf, Snf); pnp <- rubin(Bnp, Snp)

cat("Pooled nodewise coefficients, physical somatic node:\n")
print(data.frame(Node = nd, Est = round(pnf$est, 4), p = round(pnf$p, 4)), row.names = FALSE)
cat("\nPooled nodewise coefficients, psychological somatic node:\n")
print(data.frame(Node = nd2, Est = round(pnp$est, 4), p = round(pnp$p, 4)), row.names = FALSE)

cat("\n===== PART 12: PROFILE OF EXCLUDED PARTICIPANTS =====\n\n")

df$dahil <- factor(ifelse(complete.cases(df[, tam_set]), "Included", "Excluded"),
                   levels = c("Included", "Excluded"))
cat("Included vs excluded:\n"); print(table(df$dahil))

cat("\nAge distribution by inclusion status (column percent):\n")
print(round(prop.table(table(df$YAS_YIL, df$dahil), 2) * 100, 1))
cat("\nPercent aged 16 or 17:\n")
print(round(tapply(as.integer(df$YAS_YIL >= 16), df$dahil, mean, na.rm = TRUE) * 100, 1))
cat("\nMean age:\n")
print(round(tapply(df$YAS_YIL, df$dahil, mean, na.rm = TRUE), 2))
cat("\nSex distribution (column percent):\n")
print(round(prop.table(table(df$CINSIYET, df$dahil), 2) * 100, 1))

cat("\nPercent missing by variable block, full file:\n")
blok <- list(SomaticItems = somatik_maddeler,
             SchoolItems  = alienation_maddeler,
             ParentItems  = parental_maddeler,
             Bullying     = "zorbalik_maruziyet",
             Lifestyle    = c("BMI_z", "saglikli_beslenme", "sagliksiz_beslenme", "fiziksel_aktivite_gun"),
             SES          = c("gelir_ordinal", "konut_sorunu"))
for (b in names(blok)) {
  cat("  ", b, ": ", round(100 * mean(!complete.cases(df[, blok[[b]], drop = FALSE])), 1), "%\n", sep = "")
}

cat("\nSomatic burden in included versus excluded, where observable:\n")
print(round(tapply(df$somatik_genel, df$dahil, mean, na.rm = TRUE), 3))
print(round(tapply(df$somatik_genel, df$dahil, sd, na.rm = TRUE), 3))


print(grep("KRONIK|HASTALIK|SAGLIK|SUREKLI", names(df), value = TRUE, ignore.case = TRUE))

print(table(df$YAS_YIL, df$dahil))

cat(sum(analitik_orneklem$agirlik), sum(df$FAKTOR_FERT_1317, na.rm = TRUE))

sessionInfo()

packageVersion("mice")

as.character(packageVersion("mice"))

citation("mice")
























cat("\n===== PART 13: BIFACTOR MODEL AS A CANDIDATE STRUCTURE =====\n\n")

model_1f <- paste0("g =~ ", paste(somatik_maddeler, collapse = " + "))

model_2f <- paste0("fiziksel =~ ", paste(somatik_fiziksel_maddeler, collapse = " + "), "\n",
                   "psikolojik =~ ", paste(somatik_psikolojik_maddeler, collapse = " + "))

model_bif <- paste0(
  "g =~ ", paste(somatik_maddeler, collapse = " + "), "\n",
  "fiz_s =~ ", paste(somatik_fiziksel_maddeler, collapse = " + "), "\n",
  "psi_s =~ ", paste(somatik_psikolojik_maddeler, collapse = " + "))

fit_1f_w <- cfa(model_1f, data = mv, ordered = somatik_maddeler, estimator = "WLSMV",
                parameterization = "delta", std.lv = TRUE, sampling.weights = "agirlik")

fit_2f_w <- cfa(model_2f, data = mv, ordered = somatik_maddeler, estimator = "WLSMV",
                parameterization = "delta", std.lv = TRUE, sampling.weights = "agirlik")

fit_bif_w <- tryCatch(
  cfa(model_bif, data = mv, ordered = somatik_maddeler, estimator = "WLSMV",
      parameterization = "delta", std.lv = TRUE, orthogonal = TRUE,
      sampling.weights = "agirlik"),
  error = function(e) { cat("BIFACTOR FAILED:", conditionMessage(e), "\n"); NULL })

cat("Bifactor converged:", if (is.null(fit_bif_w)) FALSE else lavInspect(fit_bif_w, "converged"), "\n")

if (!is.null(fit_bif_w)) {
  hey <- lavInspect(fit_bif_w, "theta")
  cat("Heywood case (negative residual variance):", any(diag(hey) < 0), "\n")
  vc <- lavInspect(fit_bif_w, "cov.lv")
  cat("Latent variances:", paste(round(diag(vc), 3), collapse = " "), "\n\n")
}

fm <- c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "tli.scaled",
        "rmsea.scaled", "srmr")

uyum <- rbind(
  OneFactor = round(fitMeasures(fit_1f_w, fm), 4),
  TwoFactor = round(fitMeasures(fit_2f_w, fm), 4)
)
if (!is.null(fit_bif_w)) {
  uyum <- rbind(uyum, Bifactor = round(fitMeasures(fit_bif_w, fm), 4))
}
cat("Model fit comparison (weighted, WLSMV):\n")
print(uyum)

cat("\nScaled difference tests (nested comparisons):\n")
cat("  One-factor vs two-factor:\n")
print(lavTestLRT(fit_1f_w, fit_2f_w))
if (!is.null(fit_bif_w)) {
  cat("\n  One-factor vs bifactor:\n")
  print(lavTestLRT(fit_1f_w, fit_bif_w))
}

if (!is.null(fit_bif_w)) {
  
  cat("\n----- Standardized loadings, bifactor model -----\n")
  sb <- standardizedSolution(fit_bif_w)
  lam <- sb[sb$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
  lam_g   <- lam[lam$lhs == "g", ]
  lam_fiz <- lam[lam$lhs == "fiz_s", ]
  lam_psi <- lam[lam$lhs == "psi_s", ]
  
  tab_lam <- data.frame(
    Item      = c(somatik_fiziksel_maddeler, somatik_psikolojik_maddeler),
    Cluster   = c(rep("Physical", 4), rep("Psychological", 4)),
    General   = round(lam_g$est.std[match(c(somatik_fiziksel_maddeler,
                                            somatik_psikolojik_maddeler), lam_g$rhs)], 3),
    Gen_p     = round(lam_g$pvalue[match(c(somatik_fiziksel_maddeler,
                                           somatik_psikolojik_maddeler), lam_g$rhs)], 4),
    Specific  = round(c(lam_fiz$est.std[match(somatik_fiziksel_maddeler, lam_fiz$rhs)],
                        lam_psi$est.std[match(somatik_psikolojik_maddeler, lam_psi$rhs)]), 3),
    Spec_p    = round(c(lam_fiz$pvalue[match(somatik_fiziksel_maddeler, lam_fiz$rhs)],
                        lam_psi$pvalue[match(somatik_psikolojik_maddeler, lam_psi$rhs)]), 4)
  )
  print(tab_lam, row.names = FALSE)
  
  cat("\nMean absolute loadings:\n")
  cat("  General factor            :", round(mean(abs(tab_lam$General)), 3), "\n")
  cat("  Physical specific factor  :", round(mean(abs(tab_lam$Specific[1:4])), 3), "\n")
  cat("  Psychological specific    :", round(mean(abs(tab_lam$Specific[5:8])), 3), "\n")
  
  cat("\n----- Bifactor statistical indices -----\n")
  
  lg <- tab_lam$General
  ls <- tab_lam$Specific
  th <- 1 - lg^2 - ls^2
  
  ECV <- sum(lg^2) / (sum(lg^2) + sum(ls^2))
  
  IECV <- lg^2 / (lg^2 + ls^2)
  
  sum_g2 <- (sum(lg))^2
  sum_s2 <- (sum(ls[1:4]))^2 + (sum(ls[5:8]))^2
  sum_th <- sum(th)
  
  omega_total <- (sum_g2 + sum_s2) / (sum_g2 + sum_s2 + sum_th)
  omega_h     <- sum_g2 / (sum_g2 + sum_s2 + sum_th)
  
  sub_omega <- function(idx) {
    gg <- (sum(lg[idx]))^2
    ss <- (sum(ls[idx]))^2
    tt <- sum(th[idx])
    c(omega_s = (gg + ss) / (gg + ss + tt), omega_hs = ss / (gg + ss + tt))
  }
  os_fiz <- sub_omega(1:4)
  os_psi <- sub_omega(5:8)
  
  H_index <- function(l) 1 / (1 + 1 / sum(l^2 / (1 - l^2)))
  H_g   <- H_index(lg)
  H_fiz <- H_index(ls[1:4])
  H_psi <- H_index(ls[5:8])
  
  n_item <- 8
  tum_cift  <- n_item * (n_item - 1) / 2
  ic_cift   <- choose(4, 2) + choose(4, 2)
  PUC       <- (tum_cift - ic_cift) / tum_cift
  
  cat("  ECV (explained common variance, general factor):", round(ECV, 3), "\n")
  cat("  PUC (percent uncontaminated correlations)     :", round(PUC, 3), "\n")
  cat("  Omega (total)                                 :", round(omega_total, 3), "\n")
  cat("  OmegaH (general factor)                       :", round(omega_h, 3), "\n")
  cat("  Omega subscale, physical                      :", round(os_fiz["omega_s"], 3), "\n")
  cat("  OmegaHS, physical (unique reliability)        :", round(os_fiz["omega_hs"], 3), "\n")
  cat("  Omega subscale, psychological                 :", round(os_psi["omega_s"], 3), "\n")
  cat("  OmegaHS, psychological (unique reliability)   :", round(os_psi["omega_hs"], 3), "\n")
  cat("  H, general factor                             :", round(H_g, 3), "\n")
  cat("  H, physical specific                          :", round(H_fiz, 3), "\n")
  cat("  H, psychological specific                     :", round(H_psi, 3), "\n")
  
  cat("\n  Item-level ECV (proportion of item common variance from g):\n")
  print(data.frame(Item = tab_lam$Item, Cluster = tab_lam$Cluster,
                   IECV = round(IECV, 3)), row.names = FALSE)
  
  cat("\n  Interpretation thresholds (Reise et al. 2013; Rodriguez et al. 2016):\n")
  cat("    ECV > 0.70 AND PUC > 0.70  -> data can be treated as essentially unidimensional\n")
  cat("    PUC < 0.70                 -> unidimensionality requires ECV well above 0.70\n")
  cat("    OmegaH > 0.80              -> general factor scores are interpretable\n")
  cat("    OmegaHS > 0.30             -> subscale carries reliable unique variance\n")
  cat("    H > 0.70                   -> factor is well defined and likely to replicate\n")
  
  cat("\n===== PART 14: BIFACTOR MIMIC =====\n\n")
  
  reg_g  <- paste0("g ~ ",     paste0("c", seq_along(X), "*", X, collapse = " + "))
  reg_fs <- paste0("fiz_s ~ ", paste0("a", seq_along(X), "*", X, collapse = " + "))
  reg_ps <- paste0("psi_s ~ ", paste0("b", seq_along(X), "*", X, collapse = " + "))
  fark_s <- paste0("d", seq_along(X), " := a", seq_along(X), " - b", seq_along(X), collapse = "\n")
  
  model_bif_mimic <- paste(model_bif, reg_g, reg_fs, reg_ps, fark_s, sep = "\n")
  
  fit_bm <- tryCatch(
    sem(model_bif_mimic, data = mv, ordered = somatik_maddeler, estimator = "WLSMV",
        parameterization = "delta", std.lv = TRUE, orthogonal = TRUE,
        sampling.weights = "agirlik"),
    error = function(e) { cat("BIFACTOR MIMIC FAILED:", conditionMessage(e), "\n"); NULL })
  
  if (!is.null(fit_bm) && lavInspect(fit_bm, "converged")) {
    
    cat("Converged: TRUE\n")
    cat("Heywood case:", any(diag(lavInspect(fit_bm, "theta")) < 0), "\n\n")
    
    cat("Fit:\n")
    print(round(fitMeasures(fit_bm, fm), 4))
    
    pb <- parameterEstimates(fit_bm)
    rg <- pb[pb$op == "~" & pb$lhs == "g", ];      rg <- rg[match(X, rg$rhs), ]
    ra <- pb[pb$op == "~" & pb$lhs == "fiz_s", ];  ra <- ra[match(X, ra$rhs), ]
    rb <- pb[pb$op == "~" & pb$lhs == "psi_s", ];  rb <- rb[match(X, rb$rhs), ]
    dsp <- pb[pb$op == ":=", ]
    dsp <- dsp[match(paste0("d", seq_along(X)), dsp$label), ]
    
    cat("\nPredictors of the GENERAL factor and of the two SPECIFIC factors:\n")
    print(data.frame(
      Predictor  = X_ad,
      General    = round(rg$est, 3),
      Gen_p      = round(rg$pvalue, 4),
      PhysSpec   = round(ra$est, 3),
      PhysSpec_p = round(ra$pvalue, 4),
      PsySpec    = round(rb$est, 3),
      PsySpec_p  = round(rb$pvalue, 4),
      SpecDiff   = round(dsp$est, 3),
      SpecDiff_p = round(dsp$pvalue, 4)
    ), row.names = FALSE)
    
    wb_all <- lavTestWald(fit_bm,
                          constraints = paste0("a", seq_along(X), " == b", seq_along(X), collapse = "\n"))
    wb_core <- lavTestWald(fit_bm,
                           constraints = "a1 == b1\na3 == b3\na11 == b11\na12 == b12")
    
    cat("\nWald tests on the SPECIFIC factors:\n")
    cat("  all 13 specific paths equal: chisq =", round(wb_all$stat, 3), " df =", wb_all$df,
        " p =", format.pval(wb_all$p.value, digits = 4, eps = 1e-12), "\n")
    cat("  four core specific paths   : chisq =", round(wb_core$stat, 3), " df =", wb_core$df,
        " p =", format.pval(wb_core$p.value, digits = 4, eps = 1e-12), "\n")
    
    cat("\nR2 in the bifactor MIMIC:\n")
    r2b <- lavInspect(fit_bm, "r2")
    print(round(r2b[c("g", "fiz_s", "psi_s")], 4))
    
  } else {
    cat("\nBifactor MIMIC did not converge or produced an inadmissible solution.\n")
    cat("This is common for bifactor models with only four indicators per specific factor.\n")
    cat("Report the measurement bifactor above and note the non-convergence.\n")
  }
  
}

cat("\n===== PART 13R: BIFACTOR RE-ESTIMATED UNWEIGHTED (to match other CFAs) =====\n\n")

fit_1f_uw <- cfa(model_1f, data = mv, ordered = somatik_maddeler, estimator = "WLSMV", parameterization = "delta", std.lv = TRUE)
fit_2f_uw <- cfa(model_2f, data = mv, ordered = somatik_maddeler, estimator = "WLSMV", parameterization = "delta", std.lv = TRUE)
fit_bif_uw <- cfa(model_bif, data = mv, ordered = somatik_maddeler, estimator = "WLSMV", parameterization = "delta", std.lv = TRUE, orthogonal = TRUE)

cat("Bifactor (unweighted) converged:", lavInspect(fit_bif_uw, "converged"), "\n")
cat("Heywood case:", any(diag(lavInspect(fit_bif_uw, "theta")) < 0), "\n\n")

fm <- c("chisq.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

uw_tab <- rbind(OneFactor = round(fitMeasures(fit_1f_uw, fm), 4), TwoFactor = round(fitMeasures(fit_2f_uw, fm), 4), Bifactor = round(fitMeasures(fit_bif_uw, fm), 4))
cat("UNWEIGHTED model fit (for Table 2 and Table S7 Panels A):\n")
print(uw_tab)

cat("\n----- Unweighted bifactor loadings -----\n")
sb <- standardizedSolution(fit_bif_uw)
lam <- sb[sb$op == "=~", c("lhs", "rhs", "est.std", "pvalue")]
ord <- c(somatik_fiziksel_maddeler, somatik_psikolojik_maddeler)
lam_g <- sb[sb$op=="=~" & sb$lhs=="g", ]
lam_fiz <- sb[sb$op=="=~" & sb$lhs=="fiz_s", ]
lam_psi <- sb[sb$op=="=~" & sb$lhs=="psi_s", ]
lg <- lam_g$est.std[match(ord, lam_g$rhs)]
gp <- lam_g$pvalue[match(ord, lam_g$rhs)]
ls <- c(lam_fiz$est.std[match(somatik_fiziksel_maddeler, lam_fiz$rhs)], lam_psi$est.std[match(somatik_psikolojik_maddeler, lam_psi$rhs)])
sp <- c(lam_fiz$pvalue[match(somatik_fiziksel_maddeler, lam_fiz$rhs)], lam_psi$pvalue[match(somatik_psikolojik_maddeler, lam_psi$rhs)])
loadtab <- data.frame(Item = ord, Cluster = c(rep("Physical",4), rep("Psychological",4)), General = round(lg,3), Gen_p = round(gp,4), Specific = round(ls,3), Spec_p = round(sp,4))
print(loadtab, row.names = FALSE)

th <- 1 - lg^2 - ls^2
ECV <- sum(lg^2) / (sum(lg^2) + sum(ls^2))
IECV <- lg^2 / (lg^2 + ls^2)
sum_g2 <- (sum(lg))^2
sum_s2 <- (sum(ls[1:4]))^2 + (sum(ls[5:8]))^2
sum_th <- sum(th)
omega_total <- (sum_g2 + sum_s2) / (sum_g2 + sum_s2 + sum_th)
omega_h <- sum_g2 / (sum_g2 + sum_s2 + sum_th)
sub_omega <- function(idx){ gg<-(sum(lg[idx]))^2; ss<-(sum(ls[idx]))^2; tt<-sum(th[idx]); c((gg+ss)/(gg+ss+tt), ss/(gg+ss+tt)) }
os_fiz <- sub_omega(1:4); os_psi <- sub_omega(5:8)
H_index <- function(l) 1/(1 + 1/sum(l^2/(1-l^2)))
PUC <- (28 - 12)/28

cat("\n----- Bifactor indices (unweighted) -----\n")
cat(sprintf("  ECV = %.3f | PUC = %.3f | omega = %.3f | omegaH = %.3f\n", ECV, PUC, omega_total, omega_h))
cat(sprintf("  omegaHS physical = %.3f | omegaHS psychological = %.3f\n", os_fiz[2], os_psi[2]))
cat(sprintf("  H general = %.3f | H phys = %.3f | H psych = %.3f\n", H_index(lg), H_index(ls[1:4]), H_index(ls[5:8])))
cat("\n  Item-level ECV:\n")
print(data.frame(Item = ord, IECV = round(IECV,3)), row.names = FALSE)

cat("\n>>> Panel C (bifactor MIMIC) stays WEIGHTED, as already run in Part 14.\n")
cat(">>> Compare these unweighted values against the weighted ones to see how little moves.\n")
