# Illuminating gendered pathways: Exploring energy service demands in rural and peri-urban Nigeria

Replication code for the article published in *Energy Research & Social Science*.

> Lich, U. (2026). Illuminating gendered pathways: Exploring energy service demands
> in rural and peri-urban Nigeria. *Energy Research & Social Science*.
> DOI: to be added upon publication

This repository contains the R and Stata code that produces all figures and tables
in the article and its Supplementary Material. The underlying data are openly
available via Harvard Dataverse (see **Data** below) and are not stored here.

---

## Data

The scripts expect the data files to be downloaded from Harvard Dataverse and
placed in the folders shown below. None of the data are included in this
repository.

| Source | DOI | Files | Place in |
|---|---|---|---|
| Quantitative household survey | [10.7910/DVN/GTNEJD](https://doi.org/10.7910/DVN/GTNEJD) | `peoplesun_hh_anon.csv`, `peoplesun_hhapps_anon.csv` | `Quant_data/Raw/` |
| Qualitative materials (anonymised) | [10.7910/DVN/GYDWW1](https://doi.org/10.7910/DVN/GYDWW1) | `PeopleSun User Research_sentiment analysis.xlsx` | `Qual_data/Raw/` |

A curated access point for PeopleSuN datasets is provided via the
[Open Energy Platform](https://openenergyplatform.org/database/topic/demand?query=&tags=peoplesun).
A visual dashboard for the qualitative materials is available via
[Rural Senses](https://data.ruralsenses.com/peoplesun); free registration is
required for access.

---

## Folder structure

```
.
├── Quant_data/
│   ├── Raw/      <- downloaded survey data (see above)
│   ├── New/      <- intermediate files, created by the scripts
│   └── Stata/    <- input and output of PCAs.do
├── Qual_data/
│   └── Raw/      <- downloaded qualitative data (see above)
├── Output/       <- all figures and tables, created by the scripts
├── 1_Data_preparation.R
├── 2_PCAs.R
├── PCAs.do
├── 3_Descriptives.R
├── 4_Regressions.R
├── 5_Regression_plots_tables.R
├── 6_Ranking_analysis.R
└── 7_Value_maps.R
```

`Quant_data/Stata/` comes with the repository and already contains the Stata
output files. `Quant_data/New/` and `Output/` are created automatically on the
first run. `Quant_data/Raw/` and `Qual_data/Raw/` must be created by hand, and
the downloaded data placed in them.

---

## Requirements

**R** 4.6.0 with the packages listed at the top of each script. All are
available from CRAN.

**Stata** 15 (only needed for `PCAs.do`, see below) with the user-written package
`polychoric` by Kolenikov:

```stata
ssc install polychoric
```

The scripts use the [`here`](https://here.r-lib.org/) package to locate files
relative to the project root. Open the `.Rproj` file, or set the working
directory to the repository folder, before running any script.

---

## How to run

Run the scripts in numerical order. Script 2 is interrupted by a step in Stata.

1. **`1_Data_preparation.R`** — cleans the survey data, harmonises the LGA
   assignment within enumeration areas, and writes `hh_clean.rds`,
   `apps_clean.rds` and `Regression_1.rds`.

2. **`2_PCAs.R`, first part** — builds the input for the decision-making index
   and writes `Quant_data/Stata/emp_PCA_1.dta`. The script then stops with a
   message asking for the Stata output.

3. **`PCAs.do`** — runs the polychoric PCA for the women's decision-making index
   and writes `pca_fe_dec.dta`, `pca_eigenvalues_onefactor.dta` and
   `polychoricpca_decision.txt`. Set `global root` at the top of the file to the
   path of this repository before running.

   *These three output files are included in the repository, so this step can be
   skipped if Stata is not available.*

4. **`2_PCAs.R`, remainder** — reads the Stata output, computes the
   survey-weighted wealth index, and writes `regression_data.rds`.

5. **`3_Descriptives.R`** — weighted summary statistics of energy access and
   appliance ownership.

6. **`4_Regressions.R`** — fits all logit, conditional logit and Poisson models
   (11 appliances × 4 specifications) and saves them to `all_results.rds`.
   This step takes about a minute.

7. **`5_Regression_plots_tables.R`** — produces all regression figures and
   tables from `all_results.rds`, so that plotted estimates and tabulated
   estimates cannot diverge.

8. **`6_Ranking_analysis.R`** — appliance rankings from the User Perceived Value
   Game, by gender and by group composition.

9. **`7_Value_maps.R`** — value prevalence per appliance and gender, and the
   heatmaps of gendered value rankings.

---

## Notes on the data

Two properties of the qualitative file are worth knowing, as both are handled
explicitly in the scripts:

**Granularity.** The Dataverse file is at *extract* level: each row is one text
extract, and a single paragraph can contain several extracts. Script 6 reduces
the data to one row per paragraph before computing item worth, because the
ranking exercise operates at paragraph level. Script 7 deliberately keeps all
extracts, because value prevalence is computed per paragraph inside
`topic_weights_paragraph()`.

**Column type guessing.** The first 6,290 rows are individual interviews, in
which `Group Type` is empty. `read_excel()` guesses column types from the first
1,000 rows by default and would read the column as logical, silently discarding
all group values. Both scripts therefore pass `guess_max = 20000`.

---

## Verification

Script 7 prints tile counts at the end. These should reproduce the figures given
in the Supplementary Material:

```
Available tiles (>0%): 760
  Women: 338 | Men: 422
```

---

## Licence

Code released under the MIT Licence (see `LICENSE`). The data are licensed
separately; see the terms on Harvard Dataverse.

---

## Funding

This research is based upon results generated through the PeopleSuN project,
conducted as a consortium led by the Reiner Lemoine Institut, Berlin. The
project was financed by the German Federal Ministry of Education and Research
(BMBF; since May 2025 the Federal Ministry of Research, Technology and Space,
BMFTR) within "CLIENT II — International Partnerships for Sustainable
Innovations" (grant number 03SF0606A).

---

## Contact

Ulli Lich, University of Siegen — lich@wiwi.uni-siegen.de
ORCID: [0009-0004-4543-7983](https://orcid.org/0009-0004-4543-7983)
