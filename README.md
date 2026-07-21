# Adolescent Somatic Symptoms Under Psychosocial Adversity: Differential Links of Risk and Protective Factors to Physical and Psychological Burden in Türkiye

This repository contains the analysis code and aggregate outputs for the study
"Adolescent Somatic Symptoms Under Psychosocial Adversity: Differential Links of Risk and Protective Factors to Physical and Psychological Burden in Türkiye."

## Overview

The study is a secondary analysis of the self-report module of the 2022 Türkiye
Child Survey (ages 13–17; Turkish Statistical Institute). The analytic chain
comprises confirmatory factor analysis, multiple correspondence analysis, latent
profile analysis, survey-weighted ordinal regression, machine learning (elastic
net, random forest, and extreme gradient boosting with SHAP), and a mixed graphical
network model.

## Data availability

The analysis uses microdata from the 2022 Türkiye Child Survey, which are **not
publicly available** owing to confidentiality restrictions imposed by the Turkish
Statistical Institute (TURKSTAT). Researchers wishing to access the data may submit
a formal application through TURKSTAT's official data access procedures. **No raw or
individual-level data are included in this repository.** Only aggregate, non-identifiable outputs 
(network weights, category-level coordinates, and the Table S16 design-effect sensitivity results) 
and rendered figures are provided."

## Repository structure

- R/ — analysis and figure scripts
- outputs/aggregate/ — network weights, category-level MCA coordinates, and the Table S16 design-effect sensitivity results (safe to share)
- outputs/figures/ — rendered figures (PNG/PDF)
- data/ — empty; the restricted microdata is placed here locally and is not tracked

## Software and reproducibility

All analyses were performed in R (version 4.5.2). The full list of packages and
their versions is given in Supplementary Table S21 of the article. Random seeds
were fixed before all stochastic procedures: a common seed governed the training
and test partition together with the cross-validation and bootstrap routines, and
a separate seed governed the latent profile enumeration.

Scripts use a configurable data path; before running, set the path to the local
microdata file. Figures that depend on individual-level data (the SHAP summary and
the MCA individual cloud) require access to the restricted microdata. The network
figure can be reproduced from outputs/aggregate/network_figure3.rds without any
individual-level data.

## Citation

Azim, D., Kara, S. B., Güvey, M. E., Aydemir, E., Gündem, S., & Yılmaz, S. (2026).
Adolescent Somatic Symptoms Under Psychosocial Adversity: Differential Links of Risk and Protective Factors to Physical and Psychological Burden in Türkiye.
[Journal]. [DOI when available]

## License

Code is released under the MIT License (see LICENSE).

## Contact

Salim Yılmaz — salim.yilmaz@acibadem.edu.tr
