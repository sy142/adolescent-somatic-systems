library(readxl)
library(dplyr)
library(naniar)
library(finalfit)
library(survey)
library(stringr)
library(lavaan)
library(psych)
library(FactoMineR)
library(factoextra)
library(coin)
library(tidyLPA)
library(MASS)
library(mgcv)
library(caret)
library(glmnet)
library(pROC)
library(MLmetrics)
library(ranger)
library(xgboost)
library(mgm)
library(bootnet)
library(qgraph)

rm(list=ls())

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

dat <- df %>% select(all_of(analitik_degiskenler))

n_toplam <- nrow(dat)
cat("Toplam orneklem N =", n_toplam, "\n\n")

eksik_tablo <- dat %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "Degisken", values_to = "n_eksik") %>%
  mutate(yuzde_eksik = round(100 * n_eksik / n_toplam, 2)) %>%
  arrange(desc(n_eksik))

cat("Degisken bazli eksik veri tablosu\n")
print(as.data.frame(eksik_tablo), row.names = FALSE)
cat("\n")

n_complete <- sum(complete.cases(dat))
cat("Tam gozlem (complete case) sayisi =", n_complete,
    " (", round(100 * n_complete / n_toplam, 2), "% )\n")
cat("En az bir eksigi olan satir sayisi =", n_toplam - n_complete,
    " (", round(100 * (n_toplam - n_complete) / n_toplam, 2), "% )\n\n")

dat <- dat %>%
  mutate(dahil_durum = factor(ifelse(complete.cases(dat), "Dahil", "Haric"),
                              levels = c("Dahil", "Haric")))

cat("Dahil vs Haric dagilimi\n")
print(table(dat$dahil_durum))
cat("\n")

karsilastirma_degiskenler <- c("YAS_YIL", "CINSIYET", "somatik_kategori",
                               "somatik_genel", "gelir_ordinal",
                               "BMI_z", "zorbalik_maruziyet", "ebeveyn_iliskisi")

cat("Dahil vs Haric karsilastirmasi (secim yanliligi degerlendirmesi)\n")
secim_tablo <- dat %>%
  summary_factorlist(
    dependent = "dahil_durum",
    explanatory = karsilastirma_degiskenler,
    p = TRUE, na_include = FALSE, cont = "mean", add_dependent_label = TRUE
  )
print(as.data.frame(secim_tablo))
cat("\n")

cat("Dahil vs Haric karsilastirmasi (medyan tabanli, dogrulama icin)\n")
secim_tablo_medyan <- dat %>%
  summary_factorlist(
    dependent = "dahil_durum",
    explanatory = karsilastirma_degiskenler,
    p = TRUE, na_include = FALSE, cont = "median", add_dependent_label = TRUE
  )
print(as.data.frame(secim_tablo_medyan))
cat("\n")

mcar_veri <- dat %>%
  select(all_of(analitik_degiskenler)) %>%
  mutate(across(where(is.factor), ~ as.integer(.)))

mcar_sonuc <- mcar_test(mcar_veri)
cat("Little MCAR testi\n")
cat("Ki-kare =", round(mcar_sonuc$statistic, 3), "\n")
cat("df =", mcar_sonuc$df, "\n")
cat("p-degeri =", format.pval(mcar_sonuc$p.value, digits = 4, eps = 1e-10), "\n")
cat("Eksik veri deseni sayisi =", mcar_sonuc$missing.patterns, "\n\n")

cat("Eksik veri deseni ozeti (ilk satirlar)\n")
desen_ozet <- dat %>%
  select(all_of(analitik_degiskenler)) %>%
  miss_var_summary()
print(as.data.frame(desen_ozet), row.names = FALSE)


analitik_orneklem <- df %>%
  filter(complete.cases(df %>% select(all_of(analitik_degiskenler))))

cat("Analitik orneklem sabitlendi\n")
cat("N (analitik) =", nrow(analitik_orneklem), "\n")
cat("Cikarilan N =", nrow(df) - nrow(analitik_orneklem), "\n\n")

cat("Analitik orneklemde kalan eksik kontrolu\n")
print(as.data.frame(
  analitik_orneklem %>%
    select(all_of(analitik_degiskenler)) %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    tidyr::pivot_longer(everything(), names_to = "Degisken", values_to = "n_eksik"))
)
cat("\n")

cat("Analitik orneklem somatik kategori dagilimi\n")
print(table(analitik_orneklem$somatik_kategori))
print(round(100 * prop.table(table(analitik_orneklem$somatik_kategori)), 2))

agirlik_adaylari <- grep("FAKTOR|AGIRLIK|WEIGHT|WGT|FERT", names(df), value = TRUE, ignore.case = TRUE)
cat("Olasi agirlik degiskenleri\n")
print(agirlik_adaylari)
cat("\n")

cat("Bu degiskenlerin ozeti\n")
for (v in agirlik_adaylari) {
  x <- df[[v]]
  cat("Degisken:", v, "\n")
  cat("  sinif:", class(x), "\n")
  if (is.numeric(x)) {
    cat("  min:", round(min(x, na.rm = TRUE), 4),
        " max:", round(max(x, na.rm = TRUE), 4),
        " ortalama:", round(mean(x, na.rm = TRUE), 4),
        " toplam:", round(sum(x, na.rm = TRUE), 2), "\n")
    cat("  eksik:", sum(is.na(x)), "\n")
    cat("  ilk degerler:", paste(round(head(x, 5), 4), collapse = ", "), "\n")
  } else {
    cat("  ilk degerler:", paste(head(x, 5), collapse = ", "), "\n")
  }
  cat("\n")
}

cat("Tum degisken adlarinda FAKTOR/STRATA/PSU/KUSE arama\n")
print(grep("STRAT|PSU|KUME|CLUSTER|BLOK|TABAKA", names(df), value = TRUE, ignore.case = TRUE))

analitik_orneklem <- analitik_orneklem %>%
  mutate(agirlik = FAKTOR_FERT_1317)

cat("Agirlik degiskeni ozeti (analitik orneklem)\n")
cat("  min:", round(min(analitik_orneklem$agirlik), 2),
    " max:", round(max(analitik_orneklem$agirlik), 2),
    " ortalama:", round(mean(analitik_orneklem$agirlik), 2),
    " toplam:", round(sum(analitik_orneklem$agirlik), 0), "\n\n")

svy_tasarim <- svydesign(
  ids = ~1,
  weights = ~agirlik,
  data = analitik_orneklem
)

cat("Survey tasarim nesnesi olusturuldu\n")
print(svy_tasarim)
cat("\n")

cat("Agirlikli somatik kategori prevalansi (yuzde, 95 CI)\n")
prev_somatik <- svymean(~somatik_kategori, svy_tasarim)
print(prev_somatik)
print(confint(prev_somatik))
cat("\n")

cat("Agirliksiz vs agirlikli somatik kategori karsilastirmasi\n")
cat("Agirliksiz:\n")
print(round(100 * prop.table(table(analitik_orneklem$somatik_kategori)), 2))
cat("Agirlikli:\n")
print(round(100 * prev_somatik, 2))


# CFA


somatik_arama <- grep("AGRI|BAS|KARIN|SIRT|DONME|UYKU|SINIR|GERGIN|UZGUN|UZUNTU|MUTSUZ|SOMATIK|SEMPTOM|SAGLIK_SIKAYET",
                      names(df), value = TRUE, ignore.case = TRUE)
cat("Somatik madde adayi degiskenler\n")
print(somatik_arama)
cat("\n")

cat("Turetilmis somatik degiskenlerin yapisi\n")
for (v in c("somatik_fiziksel", "somatik_psikolojik", "somatik_genel", "somatik_kategori")) {
  cat("Degisken:", v, " sinif:", class(df[[v]])[1], "\n")
  if (is.numeric(df[[v]])) {
    cat("  ozet:", paste(round(summary(df[[v]]), 3), collapse = " | "), "\n")
  }
}
cat("\n")

cat("Adi _S ile biten (sayisal kodlanmis) maddeler\n")
print(grep("_S$", names(df), value = TRUE))



somatik_fiziksel_maddeler <- c("SIKLIK_BAS_AGRI_S", "SIKLIK_KARIN_AGRI_S",
                               "SIKLIK_SIRT_AGRI_S", "SIKLIK_BAS_DONME_S")
somatik_psikolojik_maddeler <- c("SIKLIK_UZGUN_HISSETME_S", "SIKLIK_SINIRLI_HUYSUZ_S",
                                 "SIKLIK_GERGIN_HISSETME_S", "SIKLIK_UYUMADA_GUCLUK_S")
somatik_maddeler <- c(somatik_fiziksel_maddeler, somatik_psikolojik_maddeler)

cfa_veri <- analitik_orneklem %>% select(all_of(somatik_maddeler))

cat("Somatik madde sayisi =", length(somatik_maddeler), "\n")
cat("CFA orneklem N =", nrow(cfa_veri), "\n\n")

cat("Her maddenin kategori dagilimi (ham frekans)\n")
for (v in somatik_maddeler) {
  cat(v, "\n")
  print(table(cfa_veri[[v]], useNA = "ifany"))
  cat("\n")
}

cat("Madde duzeyinde tanimlayici istatistik (ortalama, sd, carpiklik, basiklik)\n")
print(round(describe(cfa_veri)[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")], 3))
cat("\n")

cat("Polikorik korelasyon matrisi\n")
poli <- polychoric(cfa_veri)
print(round(poli$rho, 3))
cat("\n")

cat("Madde kategori sayilari\n")
for (v in somatik_maddeler) {
  cat(v, ":", length(unique(na.omit(cfa_veri[[v]]))), "kategori\n")
}


model_tekfaktor <- '
  somatik =~ SIKLIK_BAS_AGRI_S + SIKLIK_KARIN_AGRI_S + SIKLIK_SIRT_AGRI_S + SIKLIK_BAS_DONME_S +
             SIKLIK_UZGUN_HISSETME_S + SIKLIK_SINIRLI_HUYSUZ_S + SIKLIK_GERGIN_HISSETME_S + SIKLIK_UYUMADA_GUCLUK_S
'

model_ikifaktor <- '
  fiziksel =~ SIKLIK_BAS_AGRI_S + SIKLIK_KARIN_AGRI_S + SIKLIK_SIRT_AGRI_S + SIKLIK_BAS_DONME_S
  psikolojik =~ SIKLIK_UZGUN_HISSETME_S + SIKLIK_SINIRLI_HUYSUZ_S + SIKLIK_GERGIN_HISSETME_S + SIKLIK_UYUMADA_GUCLUK_S
'

ordinal_maddeler <- somatik_maddeler

fit_tek <- cfa(model_tekfaktor, data = cfa_veri, ordered = ordinal_maddeler,
               estimator = "WLSMV", parameterization = "delta")
fit_iki <- cfa(model_ikifaktor, data = cfa_veri, ordered = ordinal_maddeler,
               estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("Tek faktor model uyum olcutleri\n")
print(round(fitMeasures(fit_tek, olcut_isimleri), 4))
cat("\n")

cat("Iki faktor model uyum olcutleri\n")
print(round(fitMeasures(fit_iki, olcut_isimleri), 4))
cat("\n")

cat("Iki faktor model standart yukler ve parametreler\n")
print(standardizedSolution(fit_iki) %>% filter(op %in% c("=~", "~~")), row.names = FALSE)
cat("\n")

cat("Olceklenmis ki-kare fark testi (tek faktor vs iki faktor)\n")
print(lavTestLRT(fit_tek, fit_iki))

cat("\n")

cat("Iki faktor model guvenirlik (ordinal alpha ve omega)\n")
cat("Fiziksel faktor:\n")
print(psych::omega(cfa_veri[, somatik_fiziksel_maddeler], nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot)
print(psych::alpha(polychoric(cfa_veri[, somatik_fiziksel_maddeler])$rho)$total$raw_alpha)
cat("Psikolojik faktor:\n")
print(psych::omega(cfa_veri[, somatik_psikolojik_maddeler], nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot)
print(psych::alpha(polychoric(cfa_veri[, somatik_psikolojik_maddeler])$rho)$total$raw_alpha)


okul_maddeler <- c("KATILMA_OKUL_ARKADAS_S", "KATILMA_OKUL_AIDIYET_S",
                   "KATILMA_OKUL_SEVILME_S", "KATILMA_OKUL_DISLANMA_S",
                   "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S",
                   "KATILMA_OKUL_ENDISE_S")

okul_veri_ham <- analitik_orneklem %>% select(all_of(okul_maddeler))

cat("Okul aidiyeti ham madde sayisi =", length(okul_maddeler), "\n")
cat("Orneklem N =", nrow(okul_veri_ham), "\n\n")

cat("Her ham maddenin kategori dagilimi\n")
for (v in okul_maddeler) {
  cat(v, "\n")
  print(table(okul_veri_ham[[v]], useNA = "ifany"))
  cat("\n")
}

cat("Ham madde tanimlayici istatistik\n")
print(round(describe(okul_veri_ham)[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")], 3))
cat("\n")

cat("Ham polikorik korelasyon matrisi (reverse oncesi yon teshisi icin)\n")
okul_poli_ham <- polychoric(okul_veri_ham)
print(round(okul_poli_ham$rho, 3))
cat("\n")

olumlu_maddeler <- c("KATILMA_OKUL_ARKADAS_S", "KATILMA_OKUL_AIDIYET_S", "KATILMA_OKUL_SEVILME_S")
olumsuz_maddeler <- c("KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S",
                      "KATILMA_OKUL_YALNIZ_S", "KATILMA_OKUL_ENDISE_S")

cat("Olumlu maddelerin olumsuz maddelerle ortalama korelasyonu (yon kontrolu)\n")
capraz <- okul_poli_ham$rho[olumlu_maddeler, olumsuz_maddeler]
cat("Ortalama capraz korelasyon =", round(mean(capraz), 3), "\n")
cat("Eger negatifse maddeler zaten ayni yonde degil, reverse gerekli\n")
cat("Eger pozitifse ham maddeler zaten ayni yone kodlanmis (reverse yapilmis)\n\n")

cat("Olumlu blok ic korelasyon ortalamasi =",
    round(mean(okul_poli_ham$rho[olumlu_maddeler, olumlu_maddeler][lower.tri(okul_poli_ham$rho[olumlu_maddeler, olumlu_maddeler])]), 3), "\n")
cat("Olumsuz blok ic korelasyon ortalamasi =",
    round(mean(okul_poli_ham$rho[olumsuz_maddeler, olumsuz_maddeler][lower.tri(okul_poli_ham$rho[olumsuz_maddeler, olumsuz_maddeler])]), 3), "\n\n")

cat("Ham veri uzerinde tum 7 madde duz ordinal alpha\n")
print(round(psych::alpha(okul_poli_ham$rho)$total$raw_alpha, 4))
cat("\n")

cat("psych alpha otomatik yon uyarisi (check.keys ile)\n")
print(psych::alpha(okul_veri_ham, check.keys = TRUE)$keys)

okul_cfa_veri <- analitik_orneklem %>% select(all_of(okul_maddeler))

model_okul_7tek <- '
  okul =~ KATILMA_OKUL_ARKADAS_S + KATILMA_OKUL_AIDIYET_S + KATILMA_OKUL_SEVILME_S +
          KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S +
          KATILMA_OKUL_ENDISE_S
'

model_okul_7iki <- '
  aidiyet =~ KATILMA_OKUL_ARKADAS_S + KATILMA_OKUL_AIDIYET_S + KATILMA_OKUL_SEVILME_S
  olumsuz =~ KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S + KATILMA_OKUL_ENDISE_S
'

model_okul_6iki <- '
  aidiyet =~ KATILMA_OKUL_ARKADAS_S + KATILMA_OKUL_AIDIYET_S + KATILMA_OKUL_SEVILME_S
  olumsuz =~ KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S
'

model_okul_6tek <- '
  okul =~ KATILMA_OKUL_ARKADAS_S + KATILMA_OKUL_AIDIYET_S + KATILMA_OKUL_SEVILME_S +
          KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S
'

maddeler_7 <- okul_maddeler
maddeler_6 <- setdiff(okul_maddeler, "KATILMA_OKUL_ENDISE_S")

fit_okul_7tek <- cfa(model_okul_7tek, data = okul_cfa_veri, ordered = maddeler_7,
                     estimator = "WLSMV", parameterization = "delta")
fit_okul_7iki <- cfa(model_okul_7iki, data = okul_cfa_veri, ordered = maddeler_7,
                     estimator = "WLSMV", parameterization = "delta")
fit_okul_6iki <- cfa(model_okul_6iki, data = okul_cfa_veri, ordered = maddeler_6,
                     estimator = "WLSMV", parameterization = "delta")
fit_okul_6tek <- cfa(model_okul_6tek, data = okul_cfa_veri, ordered = maddeler_6,
                     estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("Model 1: 7 madde tek faktor\n")
print(round(fitMeasures(fit_okul_7tek, olcut_isimleri), 4))
cat("\n")

cat("Model 2: 7 madde iki faktor (ENDISE olumsuz blokta)\n")
print(round(fitMeasures(fit_okul_7iki, olcut_isimleri), 4))
cat("\n")

cat("Model 3: 6 madde iki faktor (ENDISE atildi)\n")
print(round(fitMeasures(fit_okul_6iki, olcut_isimleri), 4))
cat("\n")

cat("Model 4: 6 madde tek faktor (ENDISE atildi)\n")
print(round(fitMeasures(fit_okul_6tek, olcut_isimleri), 4))
cat("\n")

cat("Model 3 (tercih edilen aday) standart yukler ve faktor korelasyonu\n")
print(standardizedSolution(fit_okul_6iki) %>% filter(op %in% c("=~", "~~")), row.names = FALSE)
cat("\n")

cat("Model 3 vs Model 4 olceklenmis ki-kare fark testi\n")
print(lavTestLRT(fit_okul_6tek, fit_okul_6iki))
cat("\n")

aidiyet_maddeler <- c("KATILMA_OKUL_ARKADAS_S", "KATILMA_OKUL_AIDIYET_S", "KATILMA_OKUL_SEVILME_S")
olumsuz3_maddeler <- c("KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S")

cat("Alt boyut ordinal alpha (6 madde modeli)\n")
cat("Aidiyet (3 madde):\n")
print(round(psych::alpha(polychoric(okul_cfa_veri[, aidiyet_maddeler])$rho)$total$raw_alpha, 4))
cat("Olumsuz deneyim (3 madde):\n")
print(round(psych::alpha(polychoric(okul_cfa_veri[, olumsuz3_maddeler])$rho)$total$raw_alpha, 4))


maddeler_5 <- c("KATILMA_OKUL_ARKADAS_S", "KATILMA_OKUL_AIDIYET_S",
                "KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S")

okul_cfa_veri5 <- analitik_orneklem %>% select(all_of(maddeler_5))

model_okul_5tek <- '
  okul =~ KATILMA_OKUL_ARKADAS_S + KATILMA_OKUL_AIDIYET_S +
          KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S
'

model_okul_5iki <- '
  aidiyet =~ KATILMA_OKUL_ARKADAS_S + KATILMA_OKUL_AIDIYET_S
  olumsuz =~ KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S
'

fit_okul_5tek <- cfa(model_okul_5tek, data = okul_cfa_veri5, ordered = maddeler_5,
                     estimator = "WLSMV", parameterization = "delta")
fit_okul_5iki <- cfa(model_okul_5iki, data = okul_cfa_veri5, ordered = maddeler_5,
                     estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("Model 5tek: 5 madde tek faktor\n")
print(round(fitMeasures(fit_okul_5tek, olcut_isimleri), 4))
cat("\n")

cat("Model 5iki: 5 madde iki faktor\n")
print(round(fitMeasures(fit_okul_5iki, olcut_isimleri), 4))
cat("\n")

cat("Iki faktor model standart yukler ve faktor korelasyonu\n")
print(standardizedSolution(fit_okul_5iki) %>% filter(op %in% c("=~", "~~")), row.names = FALSE)
cat("\n")

cat("Tek faktor model standart yukler\n")
print(standardizedSolution(fit_okul_5tek) %>% filter(op == "=~"), row.names = FALSE)
cat("\n")

cat("Iki faktor model konverjans ve admissibility kontrolu\n")
cat("Konverge etti mi:", lavInspect(fit_okul_5iki, "converged"), "\n")
cat("Heywood (negatif varyans) var mi:", any(lavInspect(fit_okul_5iki, "theta") %>% diag() < 0), "\n\n")

cat("Tek vs iki faktor olceklenmis ki-kare fark testi\n")
print(lavTestLRT(fit_okul_5tek, fit_okul_5iki))
cat("\n")

aidiyet2_maddeler <- c("KATILMA_OKUL_ARKADAS_S", "KATILMA_OKUL_AIDIYET_S")
olumsuz3_maddeler <- c("KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S")

cat("Alt boyut guvenirlikleri\n")
cat("Aidiyet (2 madde) ordinal alpha:\n")
print(round(psych::alpha(polychoric(okul_cfa_veri5[, aidiyet2_maddeler])$rho)$total$raw_alpha, 4))
cat("Aidiyet (2 madde) Spearman-Brown (2 madde icin uygun):\n")
print(round(psych::alpha(polychoric(okul_cfa_veri5[, aidiyet2_maddeler])$rho)$total$std.alpha, 4))
cat("Olumsuz deneyim (3 madde) ordinal alpha:\n")
print(round(psych::alpha(polychoric(okul_cfa_veri5[, olumsuz3_maddeler])$rho)$total$raw_alpha, 4))

alienation_maddeler <- c("KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S")

alienation_veri <- analitik_orneklem %>% select(all_of(alienation_maddeler))

cat("School alienation madde sayisi =", length(alienation_maddeler), "\n")
cat("Orneklem N =", nrow(alienation_veri), "\n\n")

model_alienation <- '
  alienation =~ KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S
'

fit_alienation <- cfa(model_alienation, data = alienation_veri, ordered = alienation_maddeler,
                      estimator = "WLSMV", parameterization = "delta")

cat("Just-identified tek faktor model (df = 0, uyum doygun)\n")
cat("Konverge etti mi:", lavInspect(fit_alienation, "converged"), "\n")
cat("df =", fitMeasures(fit_alienation, "df"), "\n\n")

cat("Standart yukler\n")
print(standardizedSolution(fit_alienation) %>% filter(op == "=~"), row.names = FALSE)
cat("\n")

cat("Guvenirlik\n")
alienation_poli <- polychoric(alienation_veri)
cat("Ordinal alpha:\n")
print(round(psych::alpha(alienation_poli$rho)$total$raw_alpha, 4))
cat("McDonald omega (poly):\n")
print(round(psych::omega(alienation_veri, nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot, 4))
cat("\n")

cat("Polikorik korelasyonlar\n")
print(round(alienation_poli$rho, 3))
cat("\n")

analitik_orneklem <- analitik_orneklem %>%
  mutate(
    school_alienation = rowMeans(across(all_of(alienation_maddeler)), na.rm = FALSE)
  )

cat("Yeni school_alienation skoru ozeti\n")
print(round(describe(analitik_orneklem$school_alienation)[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")], 3))
cat("\n")

cat("Eksik kontrolu (analitik orneklemde tam olmali)\n")
cat("Eksik =", sum(is.na(analitik_orneklem$school_alienation)), "\n\n")

cat("Eski turetilmis okul_aidiyet ile yeni school_alienation korelasyonu\n")
print(round(cor(analitik_orneklem$okul_aidiyet, analitik_orneklem$school_alienation,
                use = "complete.obs", method = "spearman"), 3))
cat("\n")


ek_ham_maddeler <- c("KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S")

tam_set <- c(analitik_degiskenler, ek_ham_maddeler)

analitik_orneklem <- df %>%
  filter(complete.cases(df %>% select(all_of(tam_set)))) %>%
  mutate(
    agirlik = FAKTOR_FERT_1317,
    school_alienation_ham = rowMeans(across(all_of(ek_ham_maddeler)), na.rm = FALSE),
    school_alienation = 6 - school_alienation_ham
  )

cat("Guncellenmis analitik orneklem N =", nrow(analitik_orneklem), "\n")
cat("Onceki N = 3591, kayip =", 3591 - nrow(analitik_orneklem), "\n\n")

cat("school_alienation (ters cevrilmis, yuksek = yuksek yabancilasma) ozeti\n")
print(round(describe(analitik_orneklem$school_alienation)[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")], 3))
cat("Eksik =", sum(is.na(analitik_orneklem$school_alienation)), "\n\n")

svy_tasarim <- svydesign(ids = ~1, weights = ~agirlik, data = analitik_orneklem)

cat("Survey tasarim yeniden kuruldu, N =", nrow(svy_tasarim$variables), "\n\n")

cat("Guncellenmis somatik kategori dagilimi\n")
print(table(analitik_orneklem$somatik_kategori))
print(round(100 * prop.table(table(analitik_orneklem$somatik_kategori)), 2))
cat("\n")

cat("school_alienation ile somatik_genel ham korelasyonu (yon kontrolu)\n")
print(round(cor(analitik_orneklem$school_alienation, analitik_orneklem$somatik_genel,
                use = "complete.obs", method = "spearman"), 3))
cat("Pozitif olmali: yuksek yabancilasma yuksek somatik yuk ile gitmeli\n")

analitik_degiskenler <- c(
  "somatik_fiziksel", "somatik_psikolojik", "somatik_genel", "somatik_kategori",
  "zorbalik_maruziyet", "okul_aidiyet", "ebeveyn_iliskisi",
  "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
  "fiziksel_aktivite_gun", "spor_etkinlik", "kronik_hastalik",
  "gelir_ordinal", "konut_sorunu",
  "CINSIYET", "YAS_YIL"
)

somatik_maddeler <- c("SIKLIK_BAS_AGRI_S", "SIKLIK_KARIN_AGRI_S", "SIKLIK_SIRT_AGRI_S",
                      "SIKLIK_BAS_DONME_S", "SIKLIK_UZGUN_HISSETME_S", "SIKLIK_SINIRLI_HUYSUZ_S",
                      "SIKLIK_GERGIN_HISSETME_S", "SIKLIK_UYUMADA_GUCLUK_S")

alienation_maddeler <- c("KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S")

cfa_ham_maddeler <- c(somatik_maddeler, alienation_maddeler)

tam_set <- unique(c(analitik_degiskenler, cfa_ham_maddeler))

analitik_orneklem <- df %>%
  filter(complete.cases(df %>% select(all_of(tam_set)))) %>%
  mutate(
    agirlik = FAKTOR_FERT_1317,
    school_alienation = 6 - rowMeans(across(all_of(alienation_maddeler)), na.rm = FALSE)
  )

svy_tasarim <- svydesign(ids = ~1, weights = ~agirlik, data = analitik_orneklem)

cat("KONSOLIDE ANALITIK ORNEKLEM\n")
cat("N =", nrow(analitik_orneklem), "\n")
cat("Tam ornekiem disinda kalan =", nrow(df) - nrow(analitik_orneklem), "\n")
cat("Agirlik toplami =", round(sum(analitik_orneklem$agirlik), 0), "\n\n")

cat("Tum analitik ve CFA maddelerinde eksik kontrolu\n")
print(colSums(is.na(analitik_orneklem %>% select(all_of(tam_set)))))
cat("\n")

cat("school_alienation eksik =", sum(is.na(analitik_orneklem$school_alienation)), "\n\n")

cat("Somatik kategori dagilimi (agirliksiz)\n")
print(table(analitik_orneklem$somatik_kategori))
print(round(100 * prop.table(table(analitik_orneklem$somatik_kategori)), 2))
cat("\n")

cat("Somatik kategori agirlikli prevalans (yuzde, 95 CI)\n")
prev_somatik <- svymean(~somatik_kategori, svy_tasarim)
print(round(100 * prev_somatik, 2))
print(round(100 * confint(prev_somatik), 2))

somatik_fiziksel_maddeler <- c("SIKLIK_BAS_AGRI_S", "SIKLIK_KARIN_AGRI_S",
                               "SIKLIK_SIRT_AGRI_S", "SIKLIK_BAS_DONME_S")
somatik_psikolojik_maddeler <- c("SIKLIK_UZGUN_HISSETME_S", "SIKLIK_SINIRLI_HUYSUZ_S",
                                 "SIKLIK_GERGIN_HISSETME_S", "SIKLIK_UYUMADA_GUCLUK_S")

cfa_veri <- analitik_orneklem %>% select(all_of(somatik_maddeler))

model_ikifaktor <- '
  fiziksel =~ SIKLIK_BAS_AGRI_S + SIKLIK_KARIN_AGRI_S + SIKLIK_SIRT_AGRI_S + SIKLIK_BAS_DONME_S
  psikolojik =~ SIKLIK_UZGUN_HISSETME_S + SIKLIK_SINIRLI_HUYSUZ_S + SIKLIK_GERGIN_HISSETME_S + SIKLIK_UYUMADA_GUCLUK_S
'
model_tekfaktor <- '
  somatik =~ SIKLIK_BAS_AGRI_S + SIKLIK_KARIN_AGRI_S + SIKLIK_SIRT_AGRI_S + SIKLIK_BAS_DONME_S +
             SIKLIK_UZGUN_HISSETME_S + SIKLIK_SINIRLI_HUYSUZ_S + SIKLIK_GERGIN_HISSETME_S + SIKLIK_UYUMADA_GUCLUK_S
'

fit_iki <- cfa(model_ikifaktor, data = cfa_veri, ordered = somatik_maddeler,
               estimator = "WLSMV", parameterization = "delta")
fit_tek <- cfa(model_tekfaktor, data = cfa_veri, ordered = somatik_maddeler,
               estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("SOMATIK OLCEK (N =", nrow(cfa_veri), ")\n")
cat("Iki faktor uyum\n")
print(round(fitMeasures(fit_iki, olcut_isimleri), 4))
cat("Tek faktor uyum\n")
print(round(fitMeasures(fit_tek, olcut_isimleri), 4))
cat("\n")

cat("Iki faktor standart yukler ve faktor korelasyonu\n")
print(standardizedSolution(fit_iki) %>% filter(op %in% c("=~", "~~")) %>%
        select(lhs, op, rhs, est.std, se, z, pvalue, ci.lower, ci.upper), row.names = FALSE)
cat("\n")

cat("Tek vs iki faktor fark testi\n")
print(lavTestLRT(fit_tek, fit_iki))
cat("\n")

cat("Somatik faktor guvenirlikleri\n")
cat("Fiziksel ordinal alpha:", round(psych::alpha(polychoric(cfa_veri[, somatik_fiziksel_maddeler])$rho)$total$raw_alpha, 4), "\n")
cat("Fiziksel omega:", round(psych::omega(cfa_veri[, somatik_fiziksel_maddeler], nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot, 4), "\n")
cat("Psikolojik ordinal alpha:", round(psych::alpha(polychoric(cfa_veri[, somatik_psikolojik_maddeler])$rho)$total$raw_alpha, 4), "\n")
cat("Psikolojik omega:", round(psych::omega(cfa_veri[, somatik_psikolojik_maddeler], nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot, 4), "\n")
cat("\n")

alienation_veri <- analitik_orneklem %>% select(all_of(alienation_maddeler))
model_alienation <- '
  alienation =~ KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S
'
fit_alienation <- cfa(model_alienation, data = alienation_veri, ordered = alienation_maddeler,
                      estimator = "WLSMV", parameterization = "delta")

cat("SCHOOL ALIENATION OLCEK (N =", nrow(alienation_veri), ", just-identified df=0)\n")
cat("Standart yukler\n")
print(standardizedSolution(fit_alienation) %>% filter(op == "=~") %>%
        select(lhs, op, rhs, est.std, se, z, pvalue, ci.lower, ci.upper), row.names = FALSE)
cat("Alienation ordinal alpha:", round(psych::alpha(polychoric(alienation_veri)$rho)$total$raw_alpha, 4), "\n")
cat("Alienation omega:", round(psych::omega(alienation_veri, nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot, 4), "\n")

mcar_degiskenler <- c(
  "somatik_fiziksel", "somatik_psikolojik", "somatik_genel", "somatik_kategori",
  "zorbalik_maruziyet", "ebeveyn_iliskisi", "school_alienation",
  "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
  "fiziksel_aktivite_gun", "spor_etkinlik", "kronik_hastalik",
  "gelir_ordinal", "konut_sorunu", "CINSIYET", "YAS_YIL"
)


eksik_set <- c(
  "somatik_fiziksel", "somatik_psikolojik", "somatik_genel", "somatik_kategori",
  "zorbalik_maruziyet", "ebeveyn_iliskisi", "okul_aidiyet",
  "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
  "fiziksel_aktivite_gun", "spor_etkinlik", "kronik_hastalik",
  "gelir_ordinal", "konut_sorunu", "CINSIYET", "YAS_YIL",
  "KATILMA_OKUL_DISLANMA_S", "KATILMA_OKUL_YABANCI_S", "KATILMA_OKUL_YALNIZ_S"
)

dat_eksik <- df %>%
  mutate(
    CINSIYET = factor(CINSIYET, levels = c("Erkek", "Kadin")),
    somatik_kategori = factor(somatik_kategori, levels = c("Dusuk", "Orta", "Yuksek"), ordered = TRUE)
  ) %>%
  select(all_of(eksik_set))

n_toplam <- nrow(dat_eksik)
cat("Toplam N (df) =", n_toplam, "\n\n")

cat("Degisken bazli eksik veri tablosu\n")
eksik_tablo <- dat_eksik %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "Degisken", values_to = "n_eksik") %>%
  mutate(yuzde_eksik = round(100 * n_eksik / n_toplam, 2)) %>%
  arrange(desc(n_eksik))
print(as.data.frame(eksik_tablo))
cat("\n")

n_complete <- sum(complete.cases(dat_eksik))
cat("Tam gozlem =", n_complete, " (", round(100 * n_complete / n_toplam, 2), "% )\n")
cat("En az bir eksik =", n_toplam - n_complete, " (", round(100 * (n_toplam - n_complete) / n_toplam, 2), "% )\n\n")

dat_eksik <- dat_eksik %>%
  mutate(dahil_durum = factor(ifelse(complete.cases(dat_eksik), "Dahil", "Haric"),
                              levels = c("Dahil", "Haric")))

cat("Dahil vs Haric dagilimi\n")
print(table(dat_eksik$dahil_durum))
cat("\n")

karsilastirma_degiskenler <- c("YAS_YIL", "CINSIYET", "somatik_kategori",
                               "somatik_genel", "gelir_ordinal",
                               "BMI_z", "zorbalik_maruziyet", "ebeveyn_iliskisi")

cat("Dahil vs Haric karsilastirmasi (mean tabanli)\n")
print(as.data.frame(
  dat_eksik %>% summary_factorlist(
    dependent = "dahil_durum", explanatory = karsilastirma_degiskenler,
    p = TRUE, na_include = FALSE, cont = "mean", add_dependent_label = TRUE)))
cat("\n")

cat("Dahil vs Haric karsilastirmasi (median tabanli dogrulama)\n")
print(as.data.frame(
  dat_eksik %>% summary_factorlist(
    dependent = "dahil_durum", explanatory = karsilastirma_degiskenler,
    p = TRUE, na_include = FALSE, cont = "median", add_dependent_label = TRUE)))
cat("\n")

mcar_veri <- dat_eksik %>%
  select(all_of(eksik_set)) %>%
  mutate(across(where(is.factor), ~ as.integer(.)))
mcar_sonuc <- mcar_test(mcar_veri)
cat("Little MCAR testi (20 degisken)\n")
cat("Ki-kare =", round(mcar_sonuc$statistic, 3), "\n")
cat("df =", mcar_sonuc$df, "\n")
cat("p =", format.pval(mcar_sonuc$p.value, digits = 4, eps = 1e-10), "\n")
cat("Desen sayisi =", mcar_sonuc$missing.patterns, "\n")

ave_cr_hesapla <- function(fit, faktor_adi) {
  std <- standardizedSolution(fit)
  yukler <- std %>% filter(op == "=~" & lhs == faktor_adi) %>% pull(est.std)
  ave <- mean(yukler^2)
  cr <- sum(yukler)^2 / (sum(yukler)^2 + sum(1 - yukler^2))
  c(AVE = round(ave, 4), CR = round(cr, 4), sqrtAVE = round(sqrt(ave), 4), n_madde = length(yukler))
}

cat("SOMATIK iki faktor AVE / CR\n")
cat("Fiziksel:\n"); print(ave_cr_hesapla(fit_iki, "fiziksel"))
cat("Psikolojik:\n"); print(ave_cr_hesapla(fit_iki, "psikolojik"))
cat("\n")

fkor <- standardizedSolution(fit_iki) %>%
  filter(op == "~~" & lhs == "fiziksel" & rhs == "psikolojik") %>% pull(est.std)
cat("Somatik faktorler arasi korelasyon r =", round(fkor, 4), "\n")
cat("Fornell-Larcker: her faktorun sqrt(AVE) degeri r =", round(fkor, 4), "degerinden buyuk olmali\n")
cat("\n")

cat("Somatik HTMT (discriminant validity)\n")
print(round(semTools::htmt(model_ikifaktor, data = cfa_veri, ordered = somatik_maddeler), 4))
cat("\n")

cat("SCHOOL ALIENATION AVE / CR\n")
print(ave_cr_hesapla(fit_alienation, "alienation"))
cat("\n")

cat("Ozet tablo icin tum degerler\n")
cat("Fiziksel:    AVE, CR yukarida\n")
cat("Psikolojik:  AVE, CR yukarida\n")
cat("Alienation:  AVE, CR yukarida\n")


ebeveyn_maddeler <- c("EBEVEYN_YARDIM_S", "EBEVEYN_IZIN_VERME_S", "EBEVEYN_ONEMSEME_S",
                      "EBEVEYN_ANLAMA_S", "EBEVEYN_CESARETLENDIRME_S", "EBEVEYN_IYI_HISSETTIRME_S",
                      "EBEVEYN_BEBEK_GIBI_DAVRANMA_S", "EBEVEYN_KONTROL_S")

zorbalik_maddeler <- c("SIKLIK_DENEYIM_DISLANMA_S", "SIKLIK_DENEYIM_DALGA_GECME_S",
                       "SIKLIK_DENEYIM_TEHDIT_S", "SIKLIK_DENEYIM_YOK_ETME_S",
                       "SIKLIK_DENEYIM_ITILME_S", "SIKLIK_DENEYIM_DEDIKODU_S")

cat("Ebeveyn maddeleri df icinde var mi:\n")
print(ebeveyn_maddeler %in% names(df))
cat("Zorbalik maddeleri df icinde var mi:\n")
print(zorbalik_maddeler %in% names(df))
cat("\n")

cat("Ebeveyn ham maddeleri eksik (df, 4072 uzerinden):\n")
print(colSums(is.na(df %>% select(any_of(ebeveyn_maddeler)))))
cat("Zorbalik ham maddeleri eksik:\n")
print(colSums(is.na(df %>% select(any_of(zorbalik_maddeler)))))
cat("\n")

cat("Ebeveyn ham madde ornek dagilimi (ilk 2):\n")
print(table(df[[ebeveyn_maddeler[1]]], useNA = "ifany"))
print(table(df[["EBEVEYN_BEBEK_GIBI_DAVRANMA_S"]], useNA = "ifany"))
cat("\n")

cat("Zorbalik ham madde ornek dagilimi (ilk 1):\n")
print(table(df[[zorbalik_maddeler[1]]], useNA = "ifany"))

ebeveyn_olumlu <- c("EBEVEYN_YARDIM_S", "EBEVEYN_IZIN_VERME_S", "EBEVEYN_ONEMSEME_S",
                    "EBEVEYN_ANLAMA_S", "EBEVEYN_CESARETLENDIRME_S", "EBEVEYN_IYI_HISSETTIRME_S")
ebeveyn_ters <- c("EBEVEYN_BEBEK_GIBI_DAVRANMA_S", "EBEVEYN_KONTROL_S")

ebeveyn_ham <- analitik_orneklem %>% select(all_of(c(ebeveyn_olumlu, ebeveyn_ters)))

cat("EBEVEYN yon teshisi (reverse oncesi polychoric)\n")
ebeveyn_poli_ham <- polychoric(ebeveyn_ham)
print(round(ebeveyn_poli_ham$rho, 2))
cat("\nOlumlu-ters capraz korelasyon ortalamasi =",
    round(mean(ebeveyn_poli_ham$rho[ebeveyn_olumlu, ebeveyn_ters]), 3), "\n")
cat("Negatifse ters maddeler reverse edilmemis demektir\n\n")

ebeveyn_cfa <- analitik_orneklem %>%
  select(all_of(c(ebeveyn_olumlu, ebeveyn_ters))) %>%
  mutate(across(all_of(ebeveyn_ters), ~ 6 - .))

ebeveyn_maddeler_tum <- c(ebeveyn_olumlu, ebeveyn_ters)

model_ebeveyn <- paste0("ebeveyn =~ ", paste(ebeveyn_maddeler_tum, collapse = " + "))

fit_ebeveyn <- cfa(model_ebeveyn, data = ebeveyn_cfa, ordered = ebeveyn_maddeler_tum,
                   estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("EBEVEYN tek faktor uyum (reverse sonrasi)\n")
print(round(fitMeasures(fit_ebeveyn, olcut_isimleri), 4))
cat("\nEBEVEYN standart yukler\n")
print(standardizedSolution(fit_ebeveyn) %>% filter(op == "=~") %>%
        select(rhs, est.std, se, z, pvalue), row.names = FALSE)
cat("\n")

zorbalik_cfa <- analitik_orneklem %>% select(all_of(zorbalik_maddeler))
model_zorbalik <- paste0("zorbalik =~ ", paste(zorbalik_maddeler, collapse = " + "))
fit_zorbalik <- cfa(model_zorbalik, data = zorbalik_cfa, ordered = zorbalik_maddeler,
                    estimator = "WLSMV", parameterization = "delta")

cat("ZORBALIK tek faktor uyum\n")
print(round(fitMeasures(fit_zorbalik, olcut_isimleri), 4))
cat("\nZORBALIK standart yukler\n")
print(standardizedSolution(fit_zorbalik) %>% filter(op == "=~") %>%
        select(rhs, est.std, se, z, pvalue), row.names = FALSE)
cat("\n")

ave_cr_hesapla <- function(fit, faktor_adi) {
  yukler <- standardizedSolution(fit) %>% filter(op == "=~" & lhs == faktor_adi) %>% pull(est.std)
  ave <- mean(yukler^2)
  cr <- sum(yukler)^2 / (sum(yukler)^2 + sum(1 - yukler^2))
  c(AVE = round(ave, 4), CR = round(cr, 4), sqrtAVE = round(sqrt(ave), 4), n_madde = length(yukler))
}

cat("EBEVEYN AVE/CR:\n"); print(ave_cr_hesapla(fit_ebeveyn, "ebeveyn"))
cat("ZORBALIK AVE/CR:\n"); print(ave_cr_hesapla(fit_zorbalik, "zorbalik"))
cat("\n")

cat("EBEVEYN guvenirlik (reverse sonrasi):\n")
cat("Ordinal alpha:", round(psych::alpha(polychoric(ebeveyn_cfa)$rho)$total$raw_alpha, 4), "\n")
cat("Omega:", round(psych::omega(ebeveyn_cfa, nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot, 4), "\n")
cat("ZORBALIK guvenirlik:\n")
cat("Ordinal alpha:", round(psych::alpha(polychoric(zorbalik_cfa)$rho)$total$raw_alpha, 4), "\n")
cat("Omega:", round(psych::omega(zorbalik_cfa, nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot, 4), "\n")

parental_maddeler <- c("EBEVEYN_YARDIM_S", "EBEVEYN_IZIN_VERME_S", "EBEVEYN_ONEMSEME_S",
                       "EBEVEYN_ANLAMA_S", "EBEVEYN_CESARETLENDIRME_S", "EBEVEYN_IYI_HISSETTIRME_S")

parental_cfa <- analitik_orneklem %>% select(all_of(parental_maddeler))

model_parental <- paste0("parental =~ ", paste(parental_maddeler, collapse = " + "))

fit_parental <- cfa(model_parental, data = parental_cfa, ordered = parental_maddeler,
                    estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("PARENTAL SUPPORT (6 madde) tek faktor uyum\n")
print(round(fitMeasures(fit_parental, olcut_isimleri), 4))
cat("\nStandart yukler\n")
print(standardizedSolution(fit_parental) %>% filter(op == "=~") %>%
        select(rhs, est.std, se, z, pvalue), row.names = FALSE)
cat("\n")

ave_cr_hesapla <- function(fit, faktor_adi) {
  yukler <- standardizedSolution(fit) %>% filter(op == "=~" & lhs == faktor_adi) %>% pull(est.std)
  ave <- mean(yukler^2)
  cr <- sum(yukler)^2 / (sum(yukler)^2 + sum(1 - yukler^2))
  c(AVE = round(ave, 4), CR = round(cr, 4), sqrtAVE = round(sqrt(ave), 4), n_madde = length(yukler))
}
cat("PARENTAL SUPPORT AVE/CR:\n"); print(ave_cr_hesapla(fit_parental, "parental"))
cat("\n")

cat("PARENTAL SUPPORT guvenirlik:\n")
cat("Ordinal alpha:", round(psych::alpha(polychoric(parental_cfa)$rho)$total$raw_alpha, 4), "\n")
cat("Omega:", round(psych::omega(parental_cfa, nfactors = 1, poly = TRUE, plot = FALSE)$omega.tot, 4), "\n")
cat("\n")

analitik_orneklem <- analitik_orneklem %>%
  mutate(parental_support = rowMeans(across(all_of(parental_maddeler)), na.rm = FALSE))

cat("Yeni parental_support skoru ozeti\n")
print(round(describe(analitik_orneklem$parental_support)[, c("n","mean","sd","min","max","skew","kurtosis")], 3))
cat("Eksik =", sum(is.na(analitik_orneklem$parental_support)), "\n\n")

cat("Eski ebeveyn_iliskisi ile yeni parental_support korelasyonu (Spearman)\n")
print(round(cor(analitik_orneklem$ebeveyn_iliskisi, analitik_orneklem$parental_support,
                method = "spearman", use = "complete.obs"), 3))


cat("PARENTAL SUPPORT modifikasyon indeksleri (en yuksek 10)\n")
mi_parental <- modificationIndices(fit_parental, sort. = TRUE)
print(head(mi_parental, 10))
cat("\n")

cat("Madde icerikleri hatirlatma:\n")
cat("YARDIM: ailem ihtiyacim kadar yardim eder\n")
cat("IZIN_VERME: sevdigim seyleri yapmama izin verir\n")
cat("ONEMSEME: beni onemsediklerini gosterir\n")
cat("ANLAMA: sorunlarimi ve endiselerimi anlamaya calisir\n")
cat("CESARETLENDIRME: kendi kararlarimi vermem icin cesaretlendirir\n")
cat("IYI_HISSETTIRME: uzgun oldugumda beni iyi hissettirir\n")

model_parental_mod <- '
  parental =~ EBEVEYN_YARDIM_S + EBEVEYN_IZIN_VERME_S + EBEVEYN_ONEMSEME_S +
              EBEVEYN_ANLAMA_S + EBEVEYN_CESARETLENDIRME_S + EBEVEYN_IYI_HISSETTIRME_S
  EBEVEYN_YARDIM_S ~~ EBEVEYN_ONEMSEME_S
'

fit_parental_mod <- cfa(model_parental_mod, data = parental_cfa, ordered = parental_maddeler,
                        estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("PARENTAL SUPPORT modifiye model (YARDIM ~~ ONEMSEME serbest)\n")
print(round(fitMeasures(fit_parental_mod, olcut_isimleri), 4))
cat("\nStandart yukler ve serbest kovaryans\n")
print(standardizedSolution(fit_parental_mod) %>% filter(op %in% c("=~","~~") & lhs != rhs) %>%
        select(lhs, op, rhs, est.std, se, z, pvalue), row.names = FALSE)
cat("\n")

cat("AVE/CR (modifiye model, sadece yuklerden)\n")
yukler_mod <- standardizedSolution(fit_parental_mod) %>% filter(op == "=~") %>% pull(est.std)
cat("AVE =", round(mean(yukler_mod^2), 4), "\n")
cat("CR =", round(sum(yukler_mod)^2 / (sum(yukler_mod)^2 + sum(1 - yukler_mod^2)), 4), "\n")

model_parental_mod2 <- '
  parental =~ EBEVEYN_YARDIM_S + EBEVEYN_IZIN_VERME_S + EBEVEYN_ONEMSEME_S +
              EBEVEYN_ANLAMA_S + EBEVEYN_CESARETLENDIRME_S + EBEVEYN_IYI_HISSETTIRME_S
  EBEVEYN_YARDIM_S ~~ EBEVEYN_ONEMSEME_S
  EBEVEYN_ANLAMA_S ~~ EBEVEYN_CESARETLENDIRME_S
'

fit_parental_mod2 <- cfa(model_parental_mod2, data = parental_cfa, ordered = parental_maddeler,
                         estimator = "WLSMV", parameterization = "delta")

olcut_isimleri <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                    "cfi.scaled", "tli.scaled", "rmsea.scaled",
                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")

cat("PARENTAL SUPPORT iki kovaryansli model\n")
print(round(fitMeasures(fit_parental_mod2, olcut_isimleri), 4))
cat("\nStandart yukler ve serbest kovaryanslar\n")
print(standardizedSolution(fit_parental_mod2) %>% filter(op %in% c("=~","~~") & lhs != rhs) %>%
        select(lhs, op, rhs, est.std, se, z, pvalue), row.names = FALSE)
cat("\n")

yukler_mod2 <- standardizedSolution(fit_parental_mod2) %>% filter(op == "=~") %>% pull(est.std)
cat("AVE =", round(mean(yukler_mod2^2), 4), "\n")
cat("CR =", round(sum(yukler_mod2)^2 / (sum(yukler_mod2)^2 + sum(1 - yukler_mod2^2)), 4), "\n")

cat("\nKalan modifikasyon indeksleri (eklenenlerden sonra)\n")
print(head(modificationIndices(fit_parental_mod2, sort. = TRUE), 5))

analitik_orneklem <- analitik_orneklem %>%
  mutate(parental_support = rowMeans(across(all_of(parental_maddeler)), na.rm = FALSE))

svy_tasarim <- svydesign(ids = ~1, weights = ~agirlik, data = analitik_orneklem)

cat("Kontrol: parental_support var mi:", "parental_support" %in% names(svy_tasarim$variables), "\n")
cat("Eksik =", sum(is.na(analitik_orneklem$parental_support)), "\n\n")

cat("=== DUYARLILIK: OLCUM MODELLERI FIML ile TAM ORNEKLEM (Section 2 destegi) ===\n")
cat("Ana modeller WLSMV-ordinal-completecase. Bu kontrol MLR (surekli) ile,\n")
cat("tahminci sabit; sadece eksik-veri islemi degisiyor:\n")
cat("  (a) listwise complete-case   (b) FIML tam orneklem\n\n")

olcum_modelleri <- list(
  Somatic_twofactor = '
    fiziksel   =~ SIKLIK_BAS_AGRI_S + SIKLIK_KARIN_AGRI_S + SIKLIK_SIRT_AGRI_S + SIKLIK_BAS_DONME_S
    psikolojik =~ SIKLIK_UZGUN_HISSETME_S + SIKLIK_SINIRLI_HUYSUZ_S + SIKLIK_GERGIN_HISSETME_S + SIKLIK_UYUMADA_GUCLUK_S
  ',
  School_alienation = '
    alienation =~ KATILMA_OKUL_DISLANMA_S + KATILMA_OKUL_YABANCI_S + KATILMA_OKUL_YALNIZ_S
  ',
  Parental_support = '
    parental =~ EBEVEYN_YARDIM_S + EBEVEYN_IZIN_VERME_S + EBEVEYN_ONEMSEME_S +
                EBEVEYN_ANLAMA_S + EBEVEYN_CESARETLENDIRME_S + EBEVEYN_IYI_HISSETTIRME_S
    EBEVEYN_YARDIM_S ~~ EBEVEYN_ONEMSEME_S
    EBEVEYN_ANLAMA_S ~~ EBEVEYN_CESARETLENDIRME_S
  ',
  Peer_victimization = '
    zorbalik =~ SIKLIK_DENEYIM_DISLANMA_S + SIKLIK_DENEYIM_DALGA_GECME_S +
                SIKLIK_DENEYIM_TEHDIT_S + SIKLIK_DENEYIM_YOK_ETME_S +
                SIKLIK_DENEYIM_ITILME_S + SIKLIK_DENEYIM_DEDIKODU_S
  '
)

fit_olcut <- c("cfi.robust", "tli.robust", "rmsea.robust", "srmr")

fiml_karsilastir <- function(model_adi, model_syntax) {
  cat("\n#####", model_adi, "#####\n")
  
  fit_lw   <- cfa(model_syntax, data = df, estimator = "MLR", missing = "listwise")
  fit_fiml <- cfa(model_syntax, data = df, estimator = "MLR", missing = "fiml")
  
  n_lw <- lavInspect(fit_lw, "nobs"); n_fiml <- lavInspect(fit_fiml, "nobs")
  cat(sprintf("N: listwise complete-case = %d ; FIML tam orneklem = %d\n", n_lw, n_fiml))
  
  df0 <- fitMeasures(fit_lw, "df")
  if (df0 == 0) {
    cat("(Model just-identified, df=0: uyum olcutleri bilgilendirici degil; yuklere bakilir)\n")
  } else {
    cat("\nUyum (listwise vs FIML):\n")
    print(rbind(listwise = round(fitMeasures(fit_lw, fit_olcut), 3),
                FIML     = round(fitMeasures(fit_fiml, fit_olcut), 3)))
  }
  
  cat("\nStandart yukler (listwise vs FIML):\n")
  l_lw   <- standardizedSolution(fit_lw)   %>% dplyr::filter(op == "=~") %>% dplyr::select(lhs, rhs, est.std)
  l_fiml <- standardizedSolution(fit_fiml) %>% dplyr::filter(op == "=~") %>% dplyr::select(lhs, rhs, est.std)
  birlesik <- merge(l_lw, l_fiml, by = c("lhs", "rhs"), suffixes = c("_lw", "_fiml"))
  birlesik$fark         <- round(birlesik$est.std_fiml - birlesik$est.std_lw, 3)
  birlesik$est.std_lw   <- round(birlesik$est.std_lw, 3)
  birlesik$est.std_fiml <- round(birlesik$est.std_fiml, 3)
  print(birlesik, row.names = FALSE)
  cat("En buyuk mutlak yuk farki =", round(max(abs(birlesik$fark)), 3), "\n")
  
  fk <- standardizedSolution(fit_fiml) %>% dplyr::filter(op == "~~" & lhs != rhs)
  if (nrow(fk) > 0) {
    cat("\nFaktor korelasyonu / serbest kovaryanslar (FIML):\n")
    print(fk %>% dplyr::select(lhs, rhs, est.std) %>% dplyr::mutate(est.std = round(est.std, 3)), row.names = FALSE)
  }
  invisible(NULL)
}

invisible(Map(fiml_karsilastir, names(olcum_modelleri), olcum_modelleri))

cat("\n=== OZET ===\n")
cat("Yuk farklari kucukse (orn. <0.05) ve faktor yapisi ayni kaliyorsa,\n")
cat("tam orneklemi FIML ile dahil etmek conclusionu degistirmiyor demektir.\n")
cat("Beklenti: somatik ve zorbalik maddelerinde eksik ~0 oldugundan listwise=FIML;\n")
cat("asil kontrol alienation (ve kismen parental) faktorlerinde.\n")


secim_surekli <- c("YAS_YIL", "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
                   "fiziksel_aktivite_gun", "gelir_ordinal", "konut_sorunu")
secim_kategorik <- c("CINSIYET", "somatik_kategori", "spor_etkinlik", "kronik_hastalik")

cat("=== TABLO 1: AGIRLIKLI ORNEKLEM OZELLIKLERI ===\n\n")

cat("--- Surekli degiskenler: agirlikli ortalama (SD), medyan [min-max] ---\n")
for (v in secim_surekli) {
  frm <- as.formula(paste0("~", v))
  m <- coef(svymean(frm, svy_tasarim, na.rm = TRUE))
  s <- SE(svymean(frm, svy_tasarim, na.rm = TRUE))
  varc <- svyvar(frm, svy_tasarim, na.rm = TRUE)
  sd_w <- sqrt(as.numeric(varc))
  med <- coef(svyquantile(frm, svy_tasarim, quantiles = 0.5, na.rm = TRUE))
  mn <- min(analitik_orneklem[[v]], na.rm = TRUE)
  mx <- max(analitik_orneklem[[v]], na.rm = TRUE)
  cat(sprintf("%s: mean=%.2f, SD=%.2f, median=%.2f [min %.2f, max %.2f]\n",
              v, as.numeric(m), sd_w, as.numeric(med), mn, mx))
}
cat("\n")

cat("--- Kategorik degiskenler: agirlikli yuzde (%) ---\n")
for (v in secim_kategorik) {
  frm <- as.formula(paste0("~factor(", v, ")"))
  tab <- svymean(frm, svy_tasarim, na.rm = TRUE)
  cat(v, ":\n")
  print(round(100 * tab, 2))
  cat("  agirliksiz n:\n")
  print(table(analitik_orneklem[[v]]))
  cat("\n")
}

cat("=== OLCEK SKORLARI (agirlikli ortalama+-SD, medyan [min-max]) ===\n\n")
olcek_skorlari <- c("somatik_fiziksel", "somatik_psikolojik", "somatik_genel",
                    "school_alienation", "parental_support", "zorbalik_maruziyet")
for (v in olcek_skorlari) {
  frm <- as.formula(paste0("~", v))
  m <- as.numeric(coef(svymean(frm, svy_tasarim, na.rm = TRUE)))
  sd_w <- sqrt(as.numeric(svyvar(frm, svy_tasarim, na.rm = TRUE)))
  med <- as.numeric(coef(svyquantile(frm, svy_tasarim, quantiles = 0.5, na.rm = TRUE)))
  mn <- min(analitik_orneklem[[v]], na.rm = TRUE)
  mx <- max(analitik_orneklem[[v]], na.rm = TRUE)
  cat(sprintf("%s: %.2f +- %.2f, median %.2f [%.2f, %.2f]\n", v, m, sd_w, med, mn, mx))
}
cat("\n")

cat("=== AGIRLIKSIZ OZET (Supplementary Section 1 paragrafi icin) ===\n\n")
cat("--- Surekli: ham ortalama (SD), medyan [min-max] ---\n")
print(round(describe(analitik_orneklem[, secim_surekli])[, c("n","mean","sd","median","min","max")], 2))
cat("\n--- Kategorik: ham n (%) ---\n")
for (v in secim_kategorik) {
  cat(v, ":\n")
  t <- table(analitik_orneklem[[v]])
  print(t)
  print(round(100 * prop.table(t), 1))
  cat("\n")
}

# MCA

mca_degiskenler <- c("zorbalik_maruziyet", "school_alienation", "parental_support")

cat("=== MCA ICIN UC PSIKOSOSYAL DEGISKEN: DAGILIM KESIF ===\n\n")

for (v in mca_degiskenler) {
  x <- analitik_orneklem[[v]]
  cat("---", v, "---\n")
  cat("ozet:", paste(round(summary(x), 3), collapse = " | "), "\n")
  cat("sd:", round(sd(x), 3), " carpiklik:", round(psych::skew(x), 3),
      " basiklik:", round(psych::kurtosi(x), 3), "\n")
  cat("persentiller (10/25/33/50/67/75/90):\n")
  print(round(quantile(x, c(.10, .25, .333, .50, .667, .75, .90)), 3))
  cat("\n")
}

cat("=== ZORBALIK detayli (cok carpik, ozel dikkat) ===\n")
cat("Tam 0 olanlarin orani:\n")
cat("  zorbalik == 0:", round(100 * mean(analitik_orneklem$zorbalik_maruziyet == 0), 1), "%\n")
cat("  zorbalik > 0 :", round(100 * mean(analitik_orneklem$zorbalik_maruziyet > 0), 1), "%\n")
cat("Sifir olmayan degerlerin dagilimi:\n")
print(round(quantile(analitik_orneklem$zorbalik_maruziyet[analitik_orneklem$zorbalik_maruziyet > 0],
                     c(.25, .50, .75, .90)), 3))
cat("\nZorbalik histogram (kabaca, deger araliklari):\n")
print(table(cut(analitik_orneklem$zorbalik_maruziyet,
                breaks = c(-0.001, 0, 0.5, 1, 2, 4),
                labels = c("0", "(0-0.5]", "(0.5-1]", "(1-2]", "(2-4]"))))
cat("\n")

cat("=== SCHOOL ALIENATION dagilim (1-5) ===\n")
print(round(quantile(analitik_orneklem$school_alienation, c(.25, .333, .50, .667, .75)), 3))
cat("Histogram:\n")
print(table(cut(analitik_orneklem$school_alienation,
                breaks = c(0.99, 1.5, 2, 2.5, 3, 5),
                labels = c("[1-1.5]", "(1.5-2]", "(2-2.5]", "(2.5-3]", "(3-5]"))))
cat("\n")

cat("=== PARENTAL SUPPORT dagilim (1-5) ===\n")
print(round(quantile(analitik_orneklem$parental_support, c(.25, .333, .50, .667, .75)), 3))
cat("Histogram:\n")
print(table(cut(analitik_orneklem$parental_support,
                breaks = c(0.99, 3, 3.5, 4, 4.5, 5),
                labels = c("[1-3]", "(3-3.5]", "(3.5-4]", "(4-4.5]", "(4.5-5]"))))
cat("\n")

cat("=== Uc degisken arasi korelasyon (Spearman) ===\n")
print(round(cor(analitik_orneklem[, mca_degiskenler], method = "spearman", use = "complete.obs"), 3))

mca_dat <- analitik_orneklem %>%
  mutate(
    zorba_kat = case_when(
      zorbalik_maruziyet == 0 ~ "None",
      zorbalik_maruziyet > 0 & zorbalik_maruziyet <= 0.333 ~ "Low",
      zorbalik_maruziyet > 0.333 ~ "High"
    ),
    alien_kat = case_when(
      school_alienation < 2 ~ "Low",
      school_alienation == 2 ~ "Moderate",
      school_alienation > 2 ~ "High"
    ),
    parent_kat = case_when(
      parental_support < 4 ~ "Low",
      parental_support == 4 ~ "Moderate",
      parental_support > 4 ~ "High"
    )
  ) %>%
  mutate(
    zorba_kat = factor(zorba_kat, levels = c("None", "Low", "High")),
    alien_kat = factor(alien_kat, levels = c("Low", "Moderate", "High")),
    parent_kat = factor(parent_kat, levels = c("Low", "Moderate", "High"))
  )

cat("=== KATEGORI DAGILIMLARI ===\n")
cat("Zorbalik (peer victimization):\n")
print(table(mca_dat$zorba_kat))
print(round(100 * prop.table(table(mca_dat$zorba_kat)), 1))
cat("\nSchool alienation:\n")
print(table(mca_dat$alien_kat))
print(round(100 * prop.table(table(mca_dat$alien_kat)), 1))
cat("\nParental support:\n")
print(table(mca_dat$parent_kat))
print(round(100 * prop.table(table(mca_dat$parent_kat)), 1))
cat("\n")

cat("=== Capraz tablolar (kategoriler iliskili mi) ===\n")
cat("Zorbalik x Alienation:\n")
print(table(mca_dat$zorba_kat, mca_dat$alien_kat))
cat("\nAlienation x Parental:\n")
print(table(mca_dat$alien_kat, mca_dat$parent_kat))
cat("\n")

mca_giris <- mca_dat %>% select(zorba_kat, alien_kat, parent_kat)

mca_sonuc <- MCA(mca_giris, graph = FALSE)

cat("=== MCA OZDEGERLER VE ACIKLANAN INERTIA ===\n")
print(round(mca_sonuc$eig, 3))
cat("\n")

cat("=== Boyutlara degisken katkilari ===\n")
cat("Dim 1 ve Dim 2 kategori koordinatlari:\n")
print(round(mca_sonuc$var$coord[, 1:2], 3))
cat("\nDim 1 ve Dim 2 katkilari (%):\n")
print(round(mca_sonuc$var$contrib[, 1:2], 2))
cat("\n")

cat("=== Boyut-degisken iliskisi (her degiskenin her boyutla R2) ===\n")
print(round(mca_sonuc$var$eta2[, 1:2], 3))


mca_dat <- mca_dat %>%
  mutate(psikososyal_eksen = mca_sonuc$ind$coord[, 1])

analitik_orneklem <- analitik_orneklem %>%
  mutate(
    zorba_kat = mca_dat$zorba_kat,
    alien_kat = mca_dat$alien_kat,
    parent_kat = mca_dat$parent_kat,
    psikososyal_eksen = mca_dat$psikososyal_eksen
  )

cat("=== PSIKOSOSYAL EKSEN (MCA Dim 1) birey skoru ozeti ===\n")
print(round(describe(analitik_orneklem$psikososyal_eksen)[, c("n","mean","sd","min","max","skew","kurtosis")], 3))
cat("\n")

cat("Yon kontrolu: eksen yuksek = psikososyal adversite mi?\n")
cat("psikososyal_eksen ile school_alienation korelasyonu:",
    round(cor(analitik_orneklem$psikososyal_eksen, analitik_orneklem$school_alienation, method = "spearman"), 3), "\n")
cat("psikososyal_eksen ile zorbalik korelasyonu:",
    round(cor(analitik_orneklem$psikososyal_eksen, analitik_orneklem$zorbalik_maruziyet, method = "spearman"), 3), "\n")
cat("psikososyal_eksen ile parental_support korelasyonu:",
    round(cor(analitik_orneklem$psikososyal_eksen, analitik_orneklem$parental_support, method = "spearman"), 3), "\n")
cat("Beklenti: alienation(+), zorbalik(+), parental(-) => yuksek eksen = yuksek adversite\n\n")

cat("Eksen ile somatik_genel ham iliski (on-bakis, henuz model degil):\n")
cat("Spearman:", round(cor(analitik_orneklem$psikososyal_eksen, analitik_orneklem$somatik_genel, method = "spearman"), 3), "\n")

dir.create("C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar", showWarnings = FALSE)
saveRDS(mca_sonuc, "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/mca_sonuc.rds")
saveRDS(analitik_orneklem, "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/analitik_orneklem_mca.rds")
cat("MCA sonuc ve guncel analitik orneklem Datalar klasorune kaydedildi.\n")


cat("=== FIGUR ICIN MCA KOORDINATLARI ===\n\n")

cat("--- Ozdegerler (eksen yuzdeleri) ---\n")
print(round(mca_sonuc$eig[1:2, ], 2))
cat("\n")

cat("--- Kategori koordinatlari (Dim1, Dim2) + katki ---\n")
kat_koord <- data.frame(
  kategori = rownames(mca_sonuc$var$coord),
  Dim1 = round(mca_sonuc$var$coord[, 1], 4),
  Dim2 = round(mca_sonuc$var$coord[, 2], 4),
  katki1 = round(mca_sonuc$var$contrib[, 1], 2),
  katki2 = round(mca_sonuc$var$contrib[, 2], 2)
)
print(kat_koord, row.names = FALSE)
cat("\n")

cat("--- Birey koordinat ozeti (bulut araligi) ---\n")
cat("Dim1 araligi:", round(range(mca_sonuc$ind$coord[,1]), 3), "\n")
cat("Dim2 araligi:", round(range(mca_sonuc$ind$coord[,2]), 3), "\n")
cat("\n")

cat("--- Birey koordinatlarini somatik kategoriye gore ozetle ---\n")
birey_df <- data.frame(
  Dim1 = mca_sonuc$ind$coord[,1],
  Dim2 = mca_sonuc$ind$coord[,2],
  somatik = analitik_orneklem$somatik_kategori
)


print(birey_df %>% group_by(somatik) %>%
        summarise(n = n(),
                  Dim1_ort = round(mean(Dim1), 3),
                  Dim2_ort = round(mean(Dim2), 3)))
cat("\n")

cat("--- Birey bulutu CSV olarak kaydet (figur scripti icin) ---\n")
write.csv(birey_df,
          "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/mca_birey_koord.csv",
          row.names = FALSE)
write.csv(kat_koord,
          "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/mca_kategori_koord.csv",
          row.names = FALSE)
cat("Koordinatlar Datalar klasorune kaydedildi (mca_birey_koord.csv, mca_kategori_koord.csv)\n")

cat("=== MCA SENSITIVITY ANALIZLERI ===\n\n")

cat("--- (2) GREENACRE/BENZECRI DUZELTILMIS INERTIA ---\n")
Q <- 3
ham_eig <- mca_sonuc$eig[, 1]
esik <- 1/Q
ust_esik <- ham_eig[ham_eig > esik]
benzecri <- (Q/(Q-1))^2 * (ust_esik - esik)^2
benzecri_pct <- 100 * benzecri / sum(benzecri)
cat("Esik (1/Q =", round(esik,3), ") ustundeki boyutlar dikkate alinir\n")
cat("Ham ilk iki boyut inertia %:", round(100*ham_eig[1:2]/sum(ham_eig), 2), "\n")
cat("Benzecri duzeltilmis inertia % (esik ustu boyutlar):\n")
print(round(benzecri_pct, 2))
cat("Duzeltilmis Dim1 kumulatif:", round(benzecri_pct[1], 1), "%\n")
cat("Duzeltilmis Dim1+Dim2 kumulatif:", round(sum(benzecri_pct[1:2]), 1), "%\n\n")

cat("--- (3) MCA DIM1 vs BASIT STANDARDIZE ORTALAMA SKOR ---\n")
basit_skor <- analitik_orneklem %>%
  transmute(
    z_alien = scale(school_alienation)[,1],
    z_zorba = scale(zorbalik_maruziyet)[,1],
    z_parent_ters = scale(-parental_support)[,1]
  ) %>%
  rowMeans()
cat("Basit adversite skoru = ortalama(z_alienation, z_zorbalik, z_(-parental))\n")
cat("MCA Dim1 ile basit skor Pearson r:",
    round(cor(analitik_orneklem$psikososyal_eksen, basit_skor), 3), "\n")
cat("MCA Dim1 ile basit skor Spearman r:",
    round(cor(analitik_orneklem$psikososyal_eksen, basit_skor, method = "spearman"), 3), "\n\n")

cat("--- (1) KATEGORILENDIRME DUYARLILIGI ---\n")

mca_alt <- analitik_orneklem %>%
  mutate(
    zorba_kat2 = factor(ifelse(zorbalik_maruziyet == 0, "None", "Any"),
                        levels = c("None","Any")),
    alien_kat2 = cut(school_alienation,
                     breaks = quantile(school_alienation, c(0, .5, 1)),
                     labels = c("LowMid","High"), include.lowest = TRUE),
    parent_kat2 = cut(parental_support,
                      breaks = quantile(parental_support, c(0, .5, 1)),
                      labels = c("Low","HighMid"), include.lowest = TRUE)
  )

cat("Alternatif kategori dagilimlari:\n")
print(table(mca_alt$zorba_kat2))
print(table(mca_alt$alien_kat2))
print(table(mca_alt$parent_kat2))
cat("\n")

mca_alt_giris <- mca_alt %>% select(zorba_kat2, alien_kat2, parent_kat2)
mca_alt_sonuc <- MCA(mca_alt_giris, graph = FALSE)

alt_dim1 <- mca_alt_sonuc$ind$coord[, 1]

cat("Alternatif MCA Dim1 ile orijinal MCA Dim1 korelasyonu:\n")
cat("Pearson r:", round(cor(analitik_orneklem$psikososyal_eksen, alt_dim1), 3), "\n")
cat("Spearman r:", round(abs(cor(analitik_orneklem$psikososyal_eksen, alt_dim1, method = "spearman")), 3), "\n")
cat("(Yon MCA tarafindan keyfi atanir; mutlak deger onemli)\n\n")

cat("Alternatif MCA ham Dim1 inertia %:",
    round(100*mca_alt_sonuc$eig[1,1]/sum(mca_alt_sonuc$eig[,1]), 2), "\n\n")

cat("--- Ozet: uc kontrol bir arada ---\n")
cat("1. Kategorilendirme duyarliligi: orijinal vs alternatif Dim1 |r| =",
    round(abs(cor(analitik_orneklem$psikososyal_eksen, alt_dim1, method = "spearman")), 3), "\n")
cat("2. Greenacre duzeltilmis Dim1 inertia:", round(benzecri_pct[1], 1), "%\n")
cat("3. MCA Dim1 vs basit ortalama r:",
    round(cor(analitik_orneklem$psikososyal_eksen, basit_skor), 3), "\n")

cat("=== LINEAR-BY-LINEAR ASSOCIATION (ordinal trend) TESTLERI ===\n\n")

analitik_orneklem <- analitik_orneklem %>%
  mutate(
    somatik_ord = ordered(somatik_kategori, levels = c("Dusuk","Orta","Yuksek"))
  )

cat("--- (A) Uc psikososyal kategori x somatik kategori (ordinal x ordinal) ---\n\n")

for (v in c("zorba_kat","alien_kat","parent_kat")) {
  cat("###", v, "x somatik_kategori\n")
  tab <- table(analitik_orneklem[[v]], analitik_orneklem$somatik_ord)
  print(tab)
  lbl <- lbl_test(as.table(tab))
  cat("Linear-by-linear association:\n")
  cat("  Z =", round(statistic(lbl), 3), "\n")
  cat("  p =", format.pval(pvalue(lbl), digits = 4, eps = 1e-10), "\n")
  cat("  Satir yuzdesi (her psikososyal kategoride somatik dagilimi):\n")
  print(round(100 * prop.table(tab, 1), 1))
  cat("\n")
}

cat("--- (B) MCA Dim1 (psikososyal adversite ekseni) x somatik kategori ---\n\n")
cat("Dim1'i tersiyelere bolup ordinal trend (gosterim icin):\n")
analitik_orneklem <- analitik_orneklem %>%
  mutate(eksen_tertil = ordered(ntile(psikososyal_eksen, 3),
                                labels = c("T1_dusuk_adversite","T2","T3_yuksek_adversite")))
tab_eksen <- table(analitik_orneklem$eksen_tertil, analitik_orneklem$somatik_ord)
print(tab_eksen)
lbl_eksen <- lbl_test(as.table(tab_eksen))
cat("Linear-by-linear association (eksen tertil x somatik):\n")
cat("  Z =", round(statistic(lbl_eksen), 3), "\n")
cat("  p =", format.pval(pvalue(lbl_eksen), digits = 4, eps = 1e-10), "\n")
cat("  Satir yuzdesi:\n")
print(round(100 * prop.table(tab_eksen, 1), 1))
cat("\n")

cat("--- (C) Surekli Dim1 ile somatik_genel iliskisi (referans) ---\n")
cat("Spearman r:", round(cor(analitik_orneklem$psikososyal_eksen,
                             analitik_orneklem$somatik_genel, method = "spearman"), 3), "\n")
cat("Jonckheere-Terpstra trend (Dim1 ortalamasi somatik kategoriye gore artiyor mu):\n")
jt <- independence_test(psikososyal_eksen ~ somatik_ord, data = analitik_orneklem,
                        teststat = "quadratic")
print(jt)

# LPA

lpa_aday <- c("BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
              "fiziksel_aktivite_gun", "spor_etkinlik", "gelir_ordinal",
              "konut_sorunu", "kronik_hastalik")

cat("=== LPA ADAY DEGISKENLER: DAGILIM KESIF ===\n\n")
print(round(describe(analitik_orneklem[, lpa_aday])[, c("n","mean","sd","median","min","max","skew","kurtosis")], 3))
cat("\n")

cat("=== Binary degiskenler dagilimi ===\n")
cat("spor_etkinlik:\n"); print(table(analitik_orneklem$spor_etkinlik))
cat("kronik_hastalik:\n"); print(table(analitik_orneklem$kronik_hastalik))
cat("\n")

cat("=== Surekli gostergelerin korelasyonu (Pearson) ===\n")
surekli_aday <- c("BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
                  "fiziksel_aktivite_gun", "gelir_ordinal", "konut_sorunu")
print(round(cor(analitik_orneklem[, surekli_aday], use = "complete.obs"), 3))
cat("\n")

cat("=== Surekli gostergelerin Spearman korelasyonu (carpik degiskenler icin) ===\n")
print(round(cor(analitik_orneklem[, surekli_aday], use = "complete.obs", method = "spearman"), 3))
cat("\n")

analitik_orneklem <- analitik_orneklem %>%
  mutate(
    z_bmi = scale(BMI_z)[,1],
    z_saglikli = scale(saglikli_beslenme)[,1],
    z_sagliksiz = scale(sagliksiz_beslenme)[,1],
    z_aktivite = scale(fiziksel_aktivite_gun)[,1],
    z_gelir = scale(gelir_ordinal)[,1],
    z_konut = scale(konut_sorunu)[,1]
  )

cat("=== Standardize gostergeler eklendi ===\n")
print(round(describe(analitik_orneklem[, c("z_bmi","z_saglikli","z_sagliksiz","z_aktivite","z_gelir","z_konut")])[, c("mean","sd","min","max","skew","kurtosis")], 2))

lpa_gostergeler <- c("z_bmi", "z_saglikli", "z_sagliksiz", "z_aktivite", "z_gelir", "z_konut")

lpa_veri <- analitik_orneklem %>% select(all_of(lpa_gostergeler))

cat("=== LPA: 1-6 PROFIL, MODEL KARSILASTIRMASI ===\n")
cat("Gostergeler:", paste(lpa_gostergeler, collapse = ", "), "\n")
cat("N =", nrow(lpa_veri), "\n\n")

set.seed(20260609)

lpa_model1 <- lpa_veri %>%
  estimate_profiles(1:6, variances = "equal", covariances = "zero")
cat("--- Model 1: esit varyans, sifir kovaryans (en kisitli, klasik LPA) ---\n")
print(get_fit(lpa_model1) %>% select(Classes, LogLik, AIC, BIC, SABIC, Entropy, BLRT_p))
cat("\n")

lpa_model2 <- lpa_veri %>%
  estimate_profiles(1:6, variances = "varying", covariances = "zero")
cat("--- Model 2: serbest varyans, sifir kovaryans ---\n")
print(get_fit(lpa_model2) %>% select(Classes, LogLik, AIC, BIC, SABIC, Entropy, BLRT_p))
cat("\n")

lpa_model3 <- lpa_veri %>%
  estimate_profiles(1:6, variances = "equal", covariances = "equal")
cat("--- Model 3: esit varyans, esit kovaryans ---\n")
print(get_fit(lpa_model3) %>% select(Classes, LogLik, AIC, BIC, SABIC, Entropy, BLRT_p))
cat("\n")

lpa_model6 <- lpa_veri %>%
  estimate_profiles(1:6, variances = "varying", covariances = "varying")
cat("--- Model 6: serbest varyans, serbest kovaryans (en esnek) ---\n")
print(get_fit(lpa_model6) %>% select(Classes, LogLik, AIC, BIC, SABIC, Entropy, BLRT_p))
cat("\n")

cat("=== EN DUSUK BIC OZETI (tum modeller, tum sinif sayilari) ===\n")
tum_fit <- bind_rows(
  get_fit(lpa_model1) %>% mutate(VarKov = "equal var, zero cov"),
  get_fit(lpa_model2) %>% mutate(VarKov = "varying var, zero cov"),
  get_fit(lpa_model3) %>% mutate(VarKov = "equal var, equal cov"),
  get_fit(lpa_model6) %>% mutate(VarKov = "varying var, varying cov")
) %>% select(VarKov, Classes, BIC, SABIC, Entropy, BLRT_p) %>%
  arrange(BIC)
print(head(tum_fit, 12))
cat("\n")
cat("En dusuk BIC:\n")
print(tum_fit[which.min(tum_fit$BIC), ])

# Ordinal regresyon

cat("=== HALKA 4: YORDAYICI VE SONUC HAZIRLIGI ===\n\n")

cat("--- Somatik iki boyut surekli dagilim (sonuc adaylari) ---\n")
print(round(describe(analitik_orneklem[, c("somatik_fiziksel","somatik_psikolojik","somatik_genel")])[, c("mean","sd","median","min","max","skew","kurtosis")], 3))
cat("\n")

cat("--- Fiziksel ve psikolojik icin uc-kategori dagilimi denemesi ---\n")
cat("Fiziksel skor = 0 olanlar:", round(100*mean(analitik_orneklem$somatik_fiziksel==0),1), "%\n")
cat("Psikolojik skor = 0 olanlar:", round(100*mean(analitik_orneklem$somatik_psikolojik==0),1), "%\n\n")

analitik_orneklem <- analitik_orneklem %>%
  mutate(
    CINSIYET = factor(CINSIYET, levels = c("Erkek","Kadin")),
    spor_etkinlik = factor(spor_etkinlik, levels = c(0,1)),
    kronik_hastalik = factor(kronik_hastalik, levels = c(0,1)),
    fiz_kat = ordered(case_when(
      somatik_fiziksel == 0 ~ "Low",
      somatik_fiziksel > 0 & somatik_fiziksel <= 0.75 ~ "Moderate",
      somatik_fiziksel > 0.75 ~ "High"), levels = c("Low","Moderate","High")),
    psi_kat = ordered(case_when(
      somatik_psikolojik == 0 ~ "Low",
      somatik_psikolojik > 0 & somatik_psikolojik <= 1.0 ~ "Moderate",
      somatik_psikolojik > 1.0 ~ "High"), levels = c("Low","Moderate","High"))
  )

cat("Fiziksel kategori dagilimi:\n"); print(table(analitik_orneklem$fiz_kat))
print(round(100*prop.table(table(analitik_orneklem$fiz_kat)),1))
cat("\nPsikolojik kategori dagilimi:\n"); print(table(analitik_orneklem$psi_kat))
print(round(100*prop.table(table(analitik_orneklem$psi_kat)),1))
cat("\n")

svy_tasarim <- svydesign(ids = ~1, weights = ~agirlik, data = analitik_orneklem)

yordayicilar <- "zorbalik_maruziyet + school_alienation + parental_support + z_bmi + z_saglikli + z_sagliksiz + z_aktivite + z_gelir + z_konut + YAS_YIL + CINSIYET + kronik_hastalik + spor_etkinlik"

cat("=== YORDAYICILAR ===\n", yordayicilar, "\n\n")

cat("=== Multicollinearity on-kontrol (VIF, basit lineer) ===\n")
vif_model <- lm(as.formula(paste("somatik_genel ~", yordayicilar)), data = analitik_orneklem)
print(round(car::vif(vif_model), 2))
cat("\n")

cat("=== MODEL A: GENEL SOMATIK KATEGORI (referans, survey-weighted ordinal) ===\n")
model_genel <- svyolr(as.formula(paste("somatik_kategori ~", yordayicilar)), design = svy_tasarim)
print(summary(model_genel))
cat("\n")

cat("=== Odds oranlari ve %95 CI (genel model) ===\n")
or_tab <- data.frame(
  OR = round(exp(coef(model_genel)), 3),
  alt = round(exp(confint(model_genel)[,1]), 3),
  ust = round(exp(confint(model_genel)[,2]), 3)
)
print(or_tab)

yordayicilar <- "zorbalik_maruziyet + school_alienation + parental_support + z_bmi + z_saglikli + z_sagliksiz + z_aktivite + z_gelir + z_konut + YAS_YIL + CINSIYET + kronik_hastalik + spor_etkinlik"

or_cikar <- function(model, etiket) {
  co <- coef(model)
  ci <- confint(model)
  yordayici_isim <- names(co)
  data.frame(
    Model = etiket,
    Yordayici = yordayici_isim,
    OR = round(exp(co), 3),
    alt = round(exp(ci[,1]), 3),
    ust = round(exp(ci[,2]), 3),
    z = round(summary(model)$coefficients[yordayici_isim, "t value"], 2),
    row.names = NULL
  )
}

cat("=== MODEL FIZIKSEL (survey-weighted ordinal) ===\n")
model_fiz <- svyolr(as.formula(paste("fiz_kat ~", yordayicilar)), design = svy_tasarim)
print(summary(model_fiz))
cat("\n--- Fiziksel OR + 95 CI ---\n")
or_fiz <- or_cikar(model_fiz, "Physical")
print(or_fiz)
cat("\n")

cat("=== MODEL PSIKOLOJIK (survey-weighted ordinal) ===\n")
model_psi <- svyolr(as.formula(paste("psi_kat ~", yordayicilar)), design = svy_tasarim)
print(summary(model_psi))
cat("\n--- Psikolojik OR + 95 CI ---\n")
or_psi <- or_cikar(model_psi, "Psychological")
print(or_psi)
cat("\n")

cat("=== YAN YANA KARSILASTIRMA (fiziksel vs psikolojik OR) ===\n")
karsilastirma <- data.frame(
  Yordayici = or_fiz$Yordayici,
  OR_fiziksel = or_fiz$OR,
  CI_fiziksel = paste0("[", or_fiz$alt, ", ", or_fiz$ust, "]"),
  OR_psikolojik = or_psi$OR,
  CI_psikolojik = paste0("[", or_psi$alt, ", ", or_psi$ust, "]")
)
print(karsilastirma, row.names = FALSE)
cat("\n")

saveRDS(model_fiz, "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/model_fiziksel.rds")
saveRDS(model_psi, "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/model_psikolojik.rds")
saveRDS(model_genel, "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/model_genel.rds")
cat("Uc model Datalar klasorune kaydedildi.\n")


yordayicilar <- "zorbalik_maruziyet + school_alienation + parental_support + z_bmi + z_saglikli + z_sagliksiz + z_aktivite + z_gelir + z_konut + YAS_YIL + CINSIYET + kronik_hastalik + spor_etkinlik"

cat("=== PROPORTIONAL ODDS VARSAYIMI KONTROLU (survey-uyumlu, Harrell yaklasimi) ===\n")
cat("Her sonuc icin iki ikili esik: (>=Moderate) ve (=High)\n")
cat("Yordayici katsayilari iki esikte benzerse proportional odds desteklenir\n\n")

po_kontrol <- function(katdegisken, etiket) {
  analitik_orneklem$y_ge_mod <- as.integer(as.integer(analitik_orneklem[[katdegisken]]) >= 2)
  analitik_orneklem$y_eq_high <- as.integer(as.integer(analitik_orneklem[[katdegisken]]) >= 3)
  svy <- svydesign(ids = ~1, weights = ~agirlik, data = analitik_orneklem)
  
  m1 <- svyglm(as.formula(paste("y_ge_mod ~", yordayicilar)), design = svy, family = quasibinomial())
  m2 <- svyglm(as.formula(paste("y_eq_high ~", yordayicilar)), design = svy, family = quasibinomial())
  
  k1 <- coef(m1)[-1]; k2 <- coef(m2)[-1]
  karsilastir <- data.frame(
    Yordayici = names(k1),
    logit_geMod = round(k1, 3),
    logit_eqHigh = round(k2, 3),
    fark = round(k1 - k2, 3),
    row.names = NULL
  )
  cat("---", etiket, "---\n")
  print(karsilastir, row.names = FALSE)
  cat("Ortalama mutlak fark =", round(mean(abs(k1 - k2)), 3), "\n")
  cat("En buyuk mutlak fark =", round(max(abs(k1 - k2)), 3),
      " (", names(which.max(abs(k1 - k2))), ")\n\n")
}

po_kontrol("fiz_kat", "FIZIKSEL SOMATIK")
po_kontrol("psi_kat", "PSIKOLOJIK SOMATIK")
po_kontrol("somatik_kategori", "GENEL SOMATIK")

cat("=== Yorum ===\n")
cat("Iki esikteki katsayilar yakinsa (kucuk fark) proportional odds makul.\n")
cat("Buyuk ve tutarli isaret degisiklikleri varsa varsayim sorunlu olabilir.\n")


cat("=== DUYARLILIK 1: PSIKOSOSYAL EKSEN (MCA Dim1) KOMPOZIT MODEL ===\n")
cat("Uc psikososyal degisken yerine tek MCA adversite skoru\n\n")

yordayici_kompozit <- "psikososyal_eksen + z_bmi + z_saglikli + z_sagliksiz + z_aktivite + z_gelir + z_konut + YAS_YIL + CINSIYET + kronik_hastalik + spor_etkinlik"

model_fiz_komp <- svyolr(as.formula(paste("fiz_kat ~", yordayici_kompozit)), design = svy_tasarim)
model_psi_komp <- svyolr(as.formula(paste("psi_kat ~", yordayici_kompozit)), design = svy_tasarim)

cat("--- FIZIKSEL, kompozit psikososyal eksen ---\n")
ck <- coef(model_fiz_komp); ci <- confint(model_fiz_komp)
cat("psikososyal_eksen OR:", round(exp(ck["psikososyal_eksen"]),3),
    "[", round(exp(ci["psikososyal_eksen",1]),3), ",", round(exp(ci["psikososyal_eksen",2]),3), "]\n")
cat("--- PSIKOLOJIK, kompozit psikososyal eksen ---\n")
ck2 <- coef(model_psi_komp); ci2 <- confint(model_psi_komp)
cat("psikososyal_eksen OR:", round(exp(ck2["psikososyal_eksen"]),3),
    "[", round(exp(ci2["psikososyal_eksen",1]),3), ",", round(exp(ci2["psikososyal_eksen",2]),3), "]\n\n")

cat("Karsilastirma: ayri modeldeki uc psikososyal degisken hala ayni yonde mi\n")
cat("(ayri model: zorbalik+, alienation+, parental- idi; kompozit eksen + olmali)\n\n")

cat("=== DUYARLILIK 2: GAM ile NON-LINEERLIK (BMI ve YAS) ===\n")
cat("BMI ve yas icin spline; egri non-lineer mi (Chen 2024 U-egrisi)\n\n")

analitik_orneklem$fiz_say <- as.integer(analitik_orneklem$fiz_kat)
analitik_orneklem$psi_say <- as.integer(analitik_orneklem$psi_kat)

gam_fiz <- gam(somatik_fiziksel ~ s(BMI_z) + s(YAS_YIL, k=5) + zorbalik_maruziyet +
                 school_alienation + parental_support + z_saglikli + z_sagliksiz +
                 z_aktivite + z_gelir + z_konut + CINSIYET + kronik_hastalik + spor_etkinlik,
               data = analitik_orneklem, weights = agirlik, method = "REML")

gam_psi <- gam(somatik_psikolojik ~ s(BMI_z) + s(YAS_YIL, k=5) + zorbalik_maruziyet +
                 school_alienation + parental_support + z_saglikli + z_sagliksiz +
                 z_aktivite + z_gelir + z_konut + CINSIYET + kronik_hastalik + spor_etkinlik,
               data = analitik_orneklem, weights = agirlik, method = "REML")

cat("--- FIZIKSEL GAM: smooth terimlerin anlamliligi ve edf ---\n")
print(summary(gam_fiz)$s.table)
cat("\n--- PSIKOLOJIK GAM: smooth terimlerin anlamliligi ve edf ---\n")
print(summary(gam_psi)$s.table)
cat("\n")

cat("Yorum: edf ~1 ise iliski dogrusal (non-lineerlik yok); edf belirgin >1 ve p<0.05 ise non-lineer\n")
cat("BMI icin edf>1 + anlamli => U-egrisi/non-lineerlik var demektir (Chen 2024)\n\n")

cat("BMI smooth egri degerleri (gorsel/yorum icin birkac nokta):\n")
bmi_grid <- data.frame(
  BMI_z = quantile(analitik_orneklem$BMI_z, c(.05,.25,.5,.75,.95)),
  zorbalik_maruziyet = mean(analitik_orneklem$zorbalik_maruziyet),
  school_alienation = mean(analitik_orneklem$school_alienation),
  parental_support = mean(analitik_orneklem$parental_support),
  z_saglikli=0, z_sagliksiz=0, z_aktivite=0, z_gelir=0, z_konut=0,
  YAS_YIL = mean(analitik_orneklem$YAS_YIL),
  CINSIYET = factor("Erkek", levels=c("Erkek","Kadin")),
  kronik_hastalik = factor("0", levels=c("0","1")),
  spor_etkinlik = factor("0", levels=c("0","1"))
)
bmi_grid$tahmin_fiz <- predict(gam_fiz, newdata = bmi_grid)
print(round(bmi_grid[, c("BMI_z","tahmin_fiz")], 3))

z_to_p <- function(z) round(2 * pnorm(-abs(z)), 3)

cat("FIZIKSEL model p-degerleri:\n")
print(z_to_p(summary(model_fiz)$coefficients[,"t value"]))
cat("\nPSIKOLOJIK model p-degerleri:\n")
print(z_to_p(summary(model_psi)$coefficients[,"t value"]))


# Machine Learning

cat("=== ML ADIM 1: SONUC VE YORDAYICI YAPISI KESIF ===\n\n")

cat("--- (A) SONUC DEGISKENLERI: surekli somatik skorlar ---\n")
sonuc_surekli <- c("somatik_fiziksel", "somatik_psikolojik")
print(round(describe(analitik_orneklem[, sonuc_surekli])[, c("n","mean","sd","median","min","max","skew","kurtosis")], 3))
cat("\n")

cat("--- Sifir-yigilma (zero-inflation) ve dagilim sekli ---\n")
for (v in sonuc_surekli) {
  x <- analitik_orneklem[[v]]
  cat(v, ":\n")
  cat("  sifir orani:", round(100*mean(x==0),1), "%\n")
  cat("  benzersiz deger sayisi:", length(unique(x)), "\n")
  cat("  deger dagilimi (ilk 10 deger):\n")
  print(round(head(sort(table(x), decreasing=TRUE), 10) / length(x) * 100, 1))
  cat("\n")
}

cat("--- (B) SONUC: kategorik formlar (zaten kurulu) ---\n")
cat("Fiziksel kategori:\n"); print(table(analitik_orneklem$fiz_kat))
cat("Psikolojik kategori:\n"); print(table(analitik_orneklem$psi_kat))
cat("\n")

cat("--- (C) YORDAYICI SETI: tip ve dagilim ---\n")
ml_yordayicilar <- c("zorbalik_maruziyet", "school_alienation", "parental_support",
                     "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
                     "fiziksel_aktivite_gun", "gelir_ordinal", "konut_sorunu",
                     "YAS_YIL", "CINSIYET", "kronik_hastalik", "spor_etkinlik")

cat("Yordayici sayisi:", length(ml_yordayicilar), "\n")
cat("Surekli/sirali yordayicilar:\n")
surekli_y <- c("zorbalik_maruziyet","school_alienation","parental_support","BMI_z",
               "saglikli_beslenme","sagliksiz_beslenme","fiziksel_aktivite_gun",
               "gelir_ordinal","konut_sorunu","YAS_YIL")
print(round(describe(analitik_orneklem[, surekli_y])[, c("mean","sd","min","max","skew","kurtosis")], 2))
cat("\nIkili (binary) yordayicilar:\n")
cat("CINSIYET:"); print(table(analitik_orneklem$CINSIYET))
cat("kronik_hastalik:"); print(table(analitik_orneklem$kronik_hastalik))
cat("spor_etkinlik:"); print(table(analitik_orneklem$spor_etkinlik))
cat("\n")

cat("--- (D) ORNEKLEM/DEGISKEN ORANI (overfitting riski) ---\n")
cat("N =", nrow(analitik_orneklem), "; yordayici =", length(ml_yordayicilar),
    "; oran =", round(nrow(analitik_orneklem)/length(ml_yordayicilar),1), "gozlem/degisken\n\n")

cat("--- (E) SONUC-YORDAYICI on-iliski (Spearman, surekli sonuc) ---\n")
cat("Fiziksel skor ile yordayicilar:\n")
for (v in surekli_y) {
  cat(sprintf("  %s: %.3f\n", v, cor(analitik_orneklem$somatik_fiziksel, analitik_orneklem[[v]], method="spearman")))
}
cat("Psikolojik skor ile yordayicilar:\n")
for (v in surekli_y) {
  cat(sprintf("  %s: %.3f\n", v, cor(analitik_orneklem$somatik_psikolojik, analitik_orneklem[[v]], method="spearman")))
}

cat("=== ML ADIM 2: VERI HAZIRLIK VE TRAIN/TEST AYRIMI ===\n\n")


cat("--- (A) ML veri cercevesi ---\n")
ml_df <- analitik_orneklem[, c("fiz_kat", "psi_kat", ml_yordayicilar)]
ml_df$CINSIYET <- factor(ml_df$CINSIYET)
ml_df$kronik_hastalik <- factor(ml_df$kronik_hastalik, levels = c(0, 1), labels = c("Yok", "Var"))
ml_df$spor_etkinlik <- factor(ml_df$spor_etkinlik, levels = c(0, 1), labels = c("Yok", "Var"))
cat("Boyut:", nrow(ml_df), "x", ncol(ml_df), "\n")
cat("Eksik hucre sayisi:", sum(is.na(ml_df)), "\n")
cat("Sinif etiketleri gecerli mi (make.names):",
    all(levels(ml_df$fiz_kat) == make.names(levels(ml_df$fiz_kat))),
    all(levels(ml_df$psi_kat) == make.names(levels(ml_df$psi_kat))), "\n\n")

cat("--- (B) Stratified 70/30 split (set.seed = 2026) ---\n")
set.seed(2026)
idx_fiz <- createDataPartition(ml_df$fiz_kat, p = 0.70, list = FALSE)
train_fiz <- ml_df[idx_fiz, ]
test_fiz  <- ml_df[-idx_fiz, ]

set.seed(2026)
idx_psi <- createDataPartition(ml_df$psi_kat, p = 0.70, list = FALSE)
train_psi <- ml_df[idx_psi, ]
test_psi  <- ml_df[-idx_psi, ]

cat("Fiziksel: train n =", nrow(train_fiz), "; test n =", nrow(test_fiz), "\n")
cat("Psikolojik: train n =", nrow(train_psi), "; test n =", nrow(test_psi), "\n\n")

cat("--- (C) Sinif dagilimlari (oran, %) ---\n")
cat("Fiziksel train:\n"); print(round(100 * prop.table(table(train_fiz$fiz_kat)), 1))
cat("Fiziksel test:\n");  print(round(100 * prop.table(table(test_fiz$fiz_kat)), 1))
cat("Psikolojik train:\n"); print(round(100 * prop.table(table(train_psi$psi_kat)), 1))
cat("Psikolojik test:\n");  print(round(100 * prop.table(table(test_psi$psi_kat)), 1))
cat("\n")

cat("--- (D) Ortak CV semasi (10-fold, train icinde) ---\n")
set.seed(2026)
cv_ctrl <- trainControl(method = "cv", number = 10,
                        classProbs = TRUE,
                        summaryFunction = multiClassSummary,
                        savePredictions = "final")
cat("CV: 10-fold, classProbs = TRUE, multiClassSummary (AUC/Accuracy/Kappa)\n")
cat("Ayni cv_ctrl uc algoritmada da kullanilacak\n")


cat("=== ML ADIM 3: ELASTIC NET BASELINE (glmnet) ===\n\n")


enet_grid <- expand.grid(alpha = seq(0, 1, by = 0.25),
                         lambda = 10^seq(-4, 0, length.out = 25))
cat("Tuning grid:", nrow(enet_grid), "kombinasyon (alpha x lambda)\n\n")

cat("--- (A) Fiziksel sonuc ---\n")

set.seed(2026)

enet_fiz <- train(fiz_kat ~ ., data = train_fiz[, c("fiz_kat", ml_yordayicilar)],
                  method = "glmnet", family = "multinomial",
                  trControl = cv_ctrl, tuneGrid = enet_grid,
                  metric = "AUC", preProcess = c("center", "scale"))
cat("En iyi hiperparametreler:\n"); print(enet_fiz$bestTune)
cat("CV performansi (en iyi):\n")
print(round(enet_fiz$results[rownames(enet_fiz$bestTune), c("AUC","Accuracy","Kappa")], 3))
cat("\n")

cat("--- (B) Psikolojik sonuc ---\n")
set.seed(2026)
enet_psi <- train(psi_kat ~ ., data = train_psi[, c("psi_kat", ml_yordayicilar)],
                  method = "glmnet", family = "multinomial",
                  trControl = cv_ctrl, tuneGrid = enet_grid,
                  metric = "AUC", preProcess = c("center", "scale"))
cat("En iyi hiperparametreler:\n"); print(enet_psi$bestTune)
cat("CV performansi (en iyi):\n")
print(round(enet_psi$results[rownames(enet_psi$bestTune), c("AUC","Accuracy","Kappa")], 3))
cat("\n")

cat("--- (C) Test seti performansi ---\n")
test_performans <- function(model, test_df, sonuc_adi) {
  gercek <- test_df[[sonuc_adi]]
  tahmin_sinif <- predict(model, newdata = test_df)
  tahmin_prob <- predict(model, newdata = test_df, type = "prob")
  cm <- confusionMatrix(tahmin_sinif, gercek)
  mroc <- multiclass.roc(gercek, tahmin_prob)
  data.frame(AUC = round(as.numeric(mroc$auc), 3),
             Accuracy = round(as.numeric(cm$overall["Accuracy"]), 3),
             Kappa = round(as.numeric(cm$overall["Kappa"]), 3))
}

cat("Fiziksel (test):\n")
perf_enet_fiz <- test_performans(enet_fiz, test_fiz, "fiz_kat")
print(perf_enet_fiz)
cat("Psikolojik (test):\n")
perf_enet_psi <- test_performans(enet_psi, test_psi, "psi_kat")
print(perf_enet_psi)
cat("\n")

cat("--- (D) Confusion matrix (test) ---\n")
cat("Fiziksel:\n")
print(confusionMatrix(predict(enet_fiz, newdata = test_fiz), test_fiz$fiz_kat)$table)
cat("Psikolojik:\n")
print(confusionMatrix(predict(enet_psi, newdata = test_psi), test_psi$psi_kat)$table)

cat("=== ML ADIM 4: RANDOM FOREST (ranger) ===\n\n")

rf_grid <- expand.grid(mtry = c(2, 3, 4, 6, 8),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(5, 10, 20, 50))
cat("Tuning grid:", nrow(rf_grid), "kombinasyon (mtry x splitrule x min.node.size)\n")
cat("num.trees = 1000 (sabit)\n\n")

cat("--- (A) Fiziksel sonuc ---\n")
set.seed(2026)
rf_fiz <- train(fiz_kat ~ ., data = train_fiz[, c("fiz_kat", ml_yordayicilar)],
                method = "ranger", num.trees = 1000,
                trControl = cv_ctrl, tuneGrid = rf_grid,
                metric = "AUC", importance = "permutation")
cat("En iyi hiperparametreler:\n"); print(rf_fiz$bestTune)
cat("CV performansi (en iyi):\n")
print(round(rf_fiz$results[rownames(rf_fiz$bestTune), c("AUC","Accuracy","Kappa")], 3))
cat("\n")

cat("--- (B) Psikolojik sonuc ---\n")
set.seed(2026)
rf_psi <- train(psi_kat ~ ., data = train_psi[, c("psi_kat", ml_yordayicilar)],
                method = "ranger", num.trees = 1000,
                trControl = cv_ctrl, tuneGrid = rf_grid,
                metric = "AUC", importance = "permutation")
cat("En iyi hiperparametreler:\n"); print(rf_psi$bestTune)
cat("CV performansi (en iyi):\n")
print(round(rf_psi$results[rownames(rf_psi$bestTune), c("AUC","Accuracy","Kappa")], 3))
cat("\n")

cat("--- (C) Test seti performansi ---\n")
cat("Fiziksel (test):\n")
perf_rf_fiz <- test_performans(rf_fiz, test_fiz, "fiz_kat")
print(perf_rf_fiz)
cat("Psikolojik (test):\n")
perf_rf_psi <- test_performans(rf_psi, test_psi, "psi_kat")
print(perf_rf_psi)
cat("\n")

cat("--- (D) Permutation importance (train, en iyi model) ---\n")
cat("Fiziksel:\n")
print(round(sort(rf_fiz$finalModel$variable.importance, decreasing = TRUE), 4))
cat("Psikolojik:\n")
print(round(sort(rf_psi$finalModel$variable.importance, decreasing = TRUE), 4))


cat("=== ML ADIM 5 DUZELTME: PREDICT MATRIS/VEKTOR UYUMU ===\n\n")

xgb_prob_matrix <- function(fit, x_mat, sinif_isimleri) {
  pr <- predict(fit, xgb.DMatrix(x_mat))
  if (is.matrix(pr)) {
    pm <- as.data.frame(pr)
  } else {
    pm <- as.data.frame(matrix(pr, ncol = length(sinif_isimleri), byrow = TRUE))
  }
  colnames(pm) <- sinif_isimleri
  pm
}

xgb_cv_tune <- function(train_df, sonuc_adi, aday_grid) {
  y_factor <- train_df[[sonuc_adi]]
  y_num <- as.integer(y_factor) - 1
  x_mat <- model.matrix(reformulate(ml_yordayicilar), data = train_df)[, -1]
  set.seed(2026)
  folds <- createFolds(y_factor, k = 10)
  cv_auc <- numeric(nrow(aday_grid))
  for (i in seq_len(nrow(aday_grid))) {
    params <- list(objective = "multi:softprob", num_class = 3,
                   max_depth = aday_grid$max_depth[i], eta = aday_grid$eta[i],
                   subsample = aday_grid$subsample[i],
                   colsample_bytree = aday_grid$colsample_bytree[i],
                   min_child_weight = aday_grid$min_child_weight[i],
                   nthread = 2)
    fold_auc <- numeric(10)
    for (k in 1:10) {
      va_idx <- folds[[k]]
      dtr <- xgb.DMatrix(x_mat[-va_idx, ], label = y_num[-va_idx])
      fit <- xgb.train(params, dtr, nrounds = aday_grid$nrounds[i], verbose = 0)
      pm <- xgb_prob_matrix(fit, x_mat[va_idx, ], levels(y_factor))
      fold_auc[k] <- as.numeric(multiclass.roc(y_factor[va_idx], pm)$auc)
    }
    cv_auc[i] <- mean(fold_auc)
  }
  best_i <- which.max(cv_auc)
  best_params <- list(objective = "multi:softprob", num_class = 3,
                      max_depth = aday_grid$max_depth[best_i], eta = aday_grid$eta[best_i],
                      subsample = aday_grid$subsample[best_i],
                      colsample_bytree = aday_grid$colsample_bytree[best_i],
                      min_child_weight = aday_grid$min_child_weight[best_i],
                      nthread = 2)
  set.seed(2026)
  final_fit <- xgb.train(best_params,
                         xgb.DMatrix(x_mat, label = y_num),
                         nrounds = aday_grid$nrounds[best_i], verbose = 0)
  list(fit = final_fit, best = aday_grid[best_i, ], cv_auc = cv_auc[best_i],
       x_train = x_mat, y_levels = levels(y_factor))
}

xgb_test_perf <- function(xgb_obj, test_df, sonuc_adi) {
  y_factor <- test_df[[sonuc_adi]]
  x_mat <- model.matrix(reformulate(ml_yordayicilar), data = test_df)[, -1]
  pm <- xgb_prob_matrix(xgb_obj$fit, x_mat, xgb_obj$y_levels)
  tahmin_sinif <- factor(xgb_obj$y_levels[max.col(pm)], levels = xgb_obj$y_levels)
  cm <- confusionMatrix(tahmin_sinif, y_factor)
  mroc <- multiclass.roc(y_factor, pm)
  list(perf = data.frame(AUC = round(as.numeric(mroc$auc), 3),
                         Accuracy = round(as.numeric(cm$overall["Accuracy"]), 3),
                         Kappa = round(as.numeric(cm$overall["Kappa"]), 3)),
       cm = cm$table)
}

cat("--- Kontrol: predict cikti tipi ---\n")
set.seed(2026)
kontrol_x <- model.matrix(reformulate(ml_yordayicilar), data = train_fiz)[, -1]
kontrol_y <- as.integer(train_fiz$fiz_kat) - 1
kontrol_fit <- xgb.train(list(objective = "multi:softprob", num_class = 3, max_depth = 2, eta = 0.1),
                         xgb.DMatrix(kontrol_x, label = kontrol_y), nrounds = 10, verbose = 0)
kontrol_pr <- predict(kontrol_fit, xgb.DMatrix(kontrol_x[1:5, ]))
cat("Sinif:", class(kontrol_pr)[1], "; boyut:", paste(dim(kontrol_pr), collapse = " x "), "\n")
cat("Satir toplamlari (1 olmali):\n")
if (is.matrix(kontrol_pr)) print(round(rowSums(kontrol_pr), 3)) else print(round(rowSums(matrix(kontrol_pr, ncol = 3, byrow = TRUE)), 3))
cat("\n")

cat("--- (A) Fiziksel sonuc ---\n")
xgb_fiz <- xgb_cv_tune(train_fiz, "fiz_kat", xgb_grid)
cat("En iyi aday (CV AUC =", round(xgb_fiz$cv_auc, 3), "):\n")
print(xgb_fiz$best, row.names = FALSE)
cat("\n")

cat("--- (B) Psikolojik sonuc ---\n")
xgb_psi <- xgb_cv_tune(train_psi, "psi_kat", xgb_grid)
cat("En iyi aday (CV AUC =", round(xgb_psi$cv_auc, 3), "):\n")
print(xgb_psi$best, row.names = FALSE)
cat("\n")

cat("--- (C) Test seti performansi ---\n")
res_xgb_fiz <- xgb_test_perf(xgb_fiz, test_fiz, "fiz_kat")
res_xgb_psi <- xgb_test_perf(xgb_psi, test_psi, "psi_kat")
perf_xgb_fiz <- res_xgb_fiz$perf
perf_xgb_psi <- res_xgb_psi$perf
cat("Fiziksel (test):\n"); print(perf_xgb_fiz)
cat("Psikolojik (test):\n"); print(perf_xgb_psi)
cat("\n")

cat("--- (D) Confusion matrix (test) ---\n")
cat("Fiziksel:\n"); print(res_xgb_fiz$cm)
cat("Psikolojik:\n"); print(res_xgb_psi$cm)
cat("\n")

cat("--- (E) Uc model karsilastirma tablosu (test seti, NIHAI) ---\n")
karsilastirma <- rbind(
  cbind(Sonuc = "Fiziksel", Model = "Elastic net", perf_enet_fiz),
  cbind(Sonuc = "Fiziksel", Model = "Random forest", perf_rf_fiz),
  cbind(Sonuc = "Fiziksel", Model = "XGBoost", perf_xgb_fiz),
  cbind(Sonuc = "Psikolojik", Model = "Elastic net", perf_enet_psi),
  cbind(Sonuc = "Psikolojik", Model = "Random forest", perf_rf_psi),
  cbind(Sonuc = "Psikolojik", Model = "XGBoost", perf_xgb_psi))
print(karsilastirma, row.names = FALSE)


cat("=== ML ADIM 6 DUZELTME: SUTUN ADI UYUMU ===\n\n")

cat("--- Teshis: SHAP sutunlari vs test matrisi sutunlari ---\n")
cat("SHAP fiz sutun sayisi:", ncol(shap_fiz), "; test fiz sutun sayisi:", ncol(x_test_fiz), "\n")
cat("SHAP'ta olup test'te olmayan:\n")
print(setdiff(colnames(shap_fiz), colnames(x_test_fiz)))
cat("Test'te olup SHAP'ta olmayan:\n")
print(setdiff(colnames(x_test_fiz), colnames(shap_fiz)))
cat("\n")

ortak_fiz <- intersect(colnames(shap_fiz), colnames(x_test_fiz))
ortak_psi <- intersect(colnames(shap_psi), colnames(x_test_psi))
shap_fiz_m <- shap_fiz[, ortak_fiz, drop = FALSE]
shap_psi_m <- shap_psi[, ortak_psi, drop = FALSE]
cat("Ortak sutun sayisi (13 olmali): fiz =", length(ortak_fiz), "; psi =", length(ortak_psi), "\n\n")

cat("--- (A) Ortalama |SHAP| siralamasi: FIZIKSEL (High) ---\n")
imp_fiz <- sort(colMeans(abs(shap_fiz_m)), decreasing = TRUE)
print(round(imp_fiz, 4))
cat("\n")

cat("--- (B) Ortalama |SHAP| siralamasi: PSIKOLOJIK (High) ---\n")
imp_psi <- sort(colMeans(abs(shap_psi_m)), decreasing = TRUE)
print(round(imp_psi, 4))
cat("\n")

cat("--- (C) Yon kontrolu: SHAP-degisken korelasyonu (isaret) ---\n")
cat("Fiziksel:\n")
for (v in names(imp_fiz)[1:6]) {
  cat(sprintf("  %s: r = %.3f\n", v, cor(x_test_fiz[, v], shap_fiz_m[, v], method = "spearman")))
}
cat("Psikolojik:\n")
for (v in names(imp_psi)[1:6]) {
  cat(sprintf("  %s: r = %.3f\n", v, cor(x_test_psi[, v], shap_psi_m[, v], method = "spearman")))
}
cat("\n")

cat("--- (D) Figur scripti icin RDS kaydi (guncel) ---\n")
shap_paket <- list(shap_fiz = shap_fiz_m, shap_psi = shap_psi_m,
                   x_fiz = x_test_fiz[, ortak_fiz], x_psi = x_test_psi[, ortak_psi],
                   imp_fiz = imp_fiz, imp_psi = imp_psi,
                   perf = karsilastirma)
saveRDS(shap_paket, "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/shap_figure3.rds")
cat("Kaydedildi: Datalar/shap_figure3.rds\n")

# network analizi

cat("=== NETWORK ADIM 1: MGM KESIF (15 dugum, agirliksiz) ===\n\n")



cat("--- (A) Ag veri matrisi ve tip tanimi ---\n")
net_dugumler <- c("somatik_fiziksel", "somatik_psikolojik",
                  "zorbalik_maruziyet", "school_alienation", "parental_support",
                  "BMI_z", "saglikli_beslenme", "sagliksiz_beslenme",
                  "fiziksel_aktivite_gun", "gelir_ordinal", "konut_sorunu",
                  "YAS_YIL", "CINSIYET", "kronik_hastalik", "spor_etkinlik")

net_df <- analitik_orneklem[, net_dugumler]
net_df$CINSIYET <- as.integer(as.factor(net_df$CINSIYET))
net_df$kronik_hastalik <- as.integer(net_df$kronik_hastalik) + 1L - min(as.integer(net_df$kronik_hastalik))
net_df$spor_etkinlik <- as.integer(net_df$spor_etkinlik) + 1L - min(as.integer(net_df$spor_etkinlik))
net_df$kronik_hastalik <- as.integer(as.factor(analitik_orneklem$kronik_hastalik))
net_df$spor_etkinlik <- as.integer(as.factor(analitik_orneklem$spor_etkinlik))

tip <- c("g","g","g","g","g","g","g","g","g","g","g","g","c","c","c")
seviye <- c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2)

surekli_idx <- which(tip == "g")
net_mat <- as.matrix(net_df)
net_mat[, surekli_idx] <- scale(net_mat[, surekli_idx])

cat("Boyut:", nrow(net_mat), "x", ncol(net_mat), "\n")
cat("Tip dagilimi: Gaussian =", sum(tip=="g"), "; kategorik =", sum(tip=="c"), "\n")
cat("Kategorik dugum seviyeleri:\n")
for (j in which(tip=="c")) cat("  ", net_dugumler[j], ":", paste(sort(unique(net_mat[,j])), collapse=","), "\n")
cat("Eksik:", sum(is.na(net_mat)), "\n\n")

cat("--- (B) MGM kestirimi (EBIC, gamma=0.25) ---\n")
set.seed(2026)
fit_mgm <- mgm(data = net_mat, type = tip, level = seviye,
               lambdaSel = "EBIC", lambdaGam = 0.25,
               ruleReg = "AND", pbar = FALSE)

cat("Kenar agirliklari matrisi (mutlak, yuvarli):\n")
W <- fit_mgm$pairwise$wadj
dimnames(W) <- list(net_dugumler, net_dugumler)
print(round(W, 3))
cat("\n")

cat("--- (C) Kenar ozeti ---\n")
ut <- W[upper.tri(W)]
cat("Olasi kenar sayisi:", length(ut), "\n")
cat("Sifir-olmayan kenar sayisi:", sum(ut != 0), "\n")
cat("Ag yogunlugu:", round(mean(ut != 0), 3), "\n")
cat("En guclu 12 kenar:\n")
kenar_liste <- which(W != 0 & upper.tri(W), arr.ind = TRUE)
if (nrow(kenar_liste) > 0) {
  kd <- data.frame(dugum1 = net_dugumler[kenar_liste[,1]],
                   dugum2 = net_dugumler[kenar_liste[,2]],
                   agirlik = round(W[kenar_liste], 3),
                   isaret = sign(fit_mgm$pairwise$signs[kenar_liste]))
  kd <- kd[order(-abs(kd$agirlik)), ]
  print(head(kd, 12), row.names = FALSE)
}
cat("\n")

cat("--- (D) Iki somatik dugumun komsulari (tez kontrolu) ---\n")
for (s in c("somatik_fiziksel","somatik_psikolojik")) {
  si <- which(net_dugumler == s)
  komsu <- which(W[si, ] != 0)
  cat(s, "komsulari:\n")
  if (length(komsu) > 0) {
    for (k in komsu) cat(sprintf("  %s: %.3f\n", net_dugumler[k], W[si,k]))
  } else cat("  (yok)\n")
  cat("\n")
}

cat("--- (E) Dugum predictability (R2 / CC) ---\n")
pred <- predict(fit_mgm, net_mat)
cat("Dugum bazli aciklanabilirlik:\n")
pr_tab <- data.frame(dugum = net_dugumler,
                     olcut = sapply(pred$errors[,2:ncol(pred$errors)], function(x) NA)[1:length(net_dugumler)])
print(pred$errors, row.names = FALSE)
cat("\n")

cat("--- (F) Centrality (strength) ---\n")
qg <- qgraph(W, layout = "spring", DoNotPlot = TRUE)
cent <- centrality(qg)
ct <- data.frame(dugum = net_dugumler, strength = round(cent$OutDegree, 3))
ct <- ct[order(-ct$strength), ]
print(ct, row.names = FALSE)

cat("=== NETWORK ADIM 2: STABILITE (bootnet, nonparametric + case-drop) ===\n\n")

set.seed(2026)
net_data <- as.data.frame(net_mat)

cat("--- (A) bootnet uyumlu yeniden kestirim (estimateNetwork, mgm) ---\n")
net_est <- estimateNetwork(net_data, default = "mgm",
                           type = tip, level = seviye,
                           criterion = "EBIC", tuning = 0.25)
cat("Kenar sayisi (estimateNetwork):", sum(net_est$graph[upper.tri(net_est$graph)] != 0), "\n\n")

cat("--- (B) Nonparametric bootstrap (kenar guven araliklari) ---\n")
boot_edge <- bootnet(net_est, nBoots = 1000, nCores = 2,
                     type = "nonparametric", statistics = c("edge","strength"))
cat("Tamamlandi. Ozet:\n")
print(summary(boot_edge, statistics = "edge")[order(-abs(summary(boot_edge, statistics = "edge")$sample)), ][1:12, c("node1","node2","sample","CIlower","CIupper")], row.names = FALSE)
cat("\n")

cat("--- (C) Case-dropping bootstrap (centrality stabilite) ---\n")
boot_case <- bootnet(net_est, nBoots = 1000, nCores = 2,
                     type = "case", statistics = "strength")
cat("CS-coefficient (strength), hedef cor=0.7:\n")
print(corStability(boot_case))
cat("\n")

cat("--- (D) Ozet karar metrikleri ---\n")
es <- summary(boot_edge, statistics = "edge")
cat("Sifir-CI icermeyen (guvenli) kenar sayisi:\n")
guvenli <- sum(sign(es$CIlower) == sign(es$CIupper) & es$sample != 0, na.rm = TRUE)
cat("  ", guvenli, "/", sum(es$sample != 0), "kenar 0'i CI disinda tutuyor\n")



cat("=== NETWORK ADIM 3: FIGUR ICIN RDS KAYDI ===\n\n")

es_df <- as.data.frame(summary(boot_edge, statistics = "edge"))
es_df <- es_df[, c("node1","node2","sample","CIlower","CIupper")]
es_df <- es_df[order(-abs(es_df$sample)), ]

net_paket <- list(W = fit_mgm$pairwise$wadj,
                  signs = fit_mgm$pairwise$signs,
                  edgecolor = fit_mgm$pairwise$edgecolor,
                  dugumler = net_dugumler,
                  pred = pred$errors,
                  es = es_df,
                  cs_strength = 0.75)
saveRDS(net_paket, "C:/Users/Salim/Desktop/makaleler/Derya TUIK/Ece/Makale/Datalar/network_figure3.rds")
cat("Kaydedildi: Datalar/network_figure3.rds\n")


cat("=== KATEGORIK KENAR YONLERI (mgm nodewise) ===\n\n")


cat("--- Kategorik dugumlere dokunan guvenli kenarlarin yonu ---\n")
cat("Yon, surekli komsunun kategorik degiskenin ikinci kategorisindeki\n")
cat("kosullu ortalamasina gore okunur (pozitif: 2. kategori > 1. kategori)\n\n")

kat_dugum <- c("CINSIYET", "kronik_hastalik", "spor_etkinlik")
kat_seviye <- list(CINSIYET = c("Erkek","Kadin"),
                   kronik_hastalik = c("Yok","Var"),
                   spor_etkinlik = c("Yok","Var"))

yon_bul <- function(kat_ad, surekli_ad) {
  ki <- which(net_dugumler == kat_ad)
  si <- which(net_dugumler == surekli_ad)
  grup <- net_mat[, ki]
  m1 <- mean(net_mat[grup == sort(unique(grup))[1], si])
  m2 <- mean(net_mat[grup == sort(unique(grup))[2], si])
  fark <- m2 - m1
  lvl <- kat_seviye[[kat_ad]]
  yon_txt <- if (fark > 0) paste0(lvl[2], " > ", lvl[1]) else paste0(lvl[1], " > ", lvl[2])
  data.frame(kategorik = kat_ad, surekli = surekli_ad,
             agirlik = round(W[ki, si], 3),
             yon = yon_txt,
             fark_z = round(fark, 3))
}

guvenli_kat_kenar <- list(
  c("CINSIYET","somatik_fiziksel"),
  c("CINSIYET","fiziksel_aktivite_gun"),
  c("kronik_hastalik","somatik_fiziksel"),
  c("spor_etkinlik","somatik_psikolojik"),
  c("spor_etkinlik","fiziksel_aktivite_gun"),
  c("spor_etkinlik","school_alienation"),
  c("spor_etkinlik","parental_support"),
  c("spor_etkinlik","gelir_ordinal"),
  c("CINSIYET","spor_etkinlik")
)

W <- np$W
yon_tablo <- do.call(rbind, lapply(guvenli_kat_kenar, function(e) yon_bul(e[1], e[2])))
yon_tablo <- yon_tablo[order(-abs(yon_tablo$agirlik)), ]
print(yon_tablo, row.names = FALSE)
