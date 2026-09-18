
***************************************************************
*Ulli Lich*
*Illuminating gendered pathways: exploring energy service demands in rural and peri-urban Nigeria*
*PCAs in Stata*
***************************************************************



*Setup
clear all
version 15
set more off

*Requires the user-written package 'polychoric' (Kolenikov):
*ssc install polychoric

global root "PATH/TO/REPOSITORY"
cd "$root/Quant_data/Stata"



*Decision making PCA*
***********************************************************

*Import data and label variables
use "emp_PCA_1", clear

*Data preparation
label define female_edu_lbl 0 "None" 1 "Primary" 2 "Secondary" 3 "Higher Secondary" 4 "Post-Secondary"
label values fem_edu_cat female_edu_lbl

*Remove incomplete cases
keep if !missing(gen_head, dec_expensive, dec_clothes, dec_market, dec_dur, dec_stove, fem_edu_cat, weighting)

*Check weighting variable
summarize weighting, detail
count if weighting == 0  // Weights should be >0

*save output in log
capture log close _all
log using "polychoricpca_decision.smcl", replace name(pcadec)

*Polychoric PCA
polychoricpca gen_head dec_expensive dec_clothes dec_market dec_dur dec_stove fem_edu_cat [pweight=weighting], ///
    pw score(pc_scores) nscore(1)
matrix E = r(eigenvalues) // Capture eigenvalues right after PCA

log close pcadec
translate "polychoricpca_decision.smcl" "polychoricpca_decision.txt", replace
*end log


*Standardise first component
summarize pc_scores1 [aw=weighting], detail
local score_mean = r(mean)
local score_sd = r(sd)
gen emp = (pc_scores1 - `score_mean') / `score_sd'

*Verify weighted mean≈0, sd≈1 and no missings in PCA sample
summarize emp [aw=weighting], detail  // Should have mean≈0, SD≈1
count if missing(emp)  // Must be 0

*Save results
keep HHID emp
save "pca_fe_dec.dta", replace

*Save eigenvalues dataset
matrix E_rowsum = J(1, colsof(E), 1) * E'
local sum_eig = E_rowsum[1,1]

preserve
clear
svmat E
rename E1 eigenvalue
keep in 1
gen factor_number = 1
gen proportion_explained = eigenvalue / `sum_eig'
save "pca_eigenvalues_onefactor.dta", replace
restore



