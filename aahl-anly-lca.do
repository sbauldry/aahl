*** Purpose: analyze predictors of lifestyles and mortality
*** Author: S Bauldry
*** Date: July 1, 2019

*** Defining program to calculate entropy
capture program drop ent
program ent, rclass
  args nc 

  qui predict cp* if e(sample), classposteriorpr
  forval k = 1/`nc' {
    qui gen sum_p_lnp_`k' = cp`k'*ln(cp`k')
  }
  qui egen sum_p_lnp = rowtotal(sum_p_lnp_*)
  qui sum sum_p_lnp
  local E = 1 + `r(sum)'/( e(N)*ln(`nc') )
  return scalar E = `E'
  di "Entropy = " `E'
  drop cp* sum_p_lnp_* sum_p_lnp
end




*** Set working directory
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-8

*** Load prepared data
use aahl-data, replace


*** Unconditional models for men
est clear

* 2-class men
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if !fem, ///
  lclass(C 2) nodvheader nonrtolerance
mat mb2 = e(b)

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (2: 3.drk <- _cons@-15) if !fem, lclass(C 2) nodvheader from(mb2)
est sto mu2
qui ent 2
local me2 = r(E)

* 3-class men
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if !fem, ///
  lclass(C 3) nodvheader 
est sto mu3
qui ent 3
local me3 = r(E)

* 4-class men
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if !fem, ///
  lclass(C 4) nodvheader nonrtolerance
mat umb4 = e(b)

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (1: 2.smk <- _cons@-15) (2: 3.smk <- _cons@-15) (3: 3.drk <- _cons@-15) ///
  (3: fvg <- _cons@15) if !fem, lclass(C 4) nodvheader from(umb4)
est sto mu4
qui ent 4
local me4 = r(E)

* 5-class men
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if !fem, ///
  lclass(C 5) nodvheader nonrtolerance
mat umb5 = e(b) 

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (1: 2.drk <- _cons@15) (1: 3.drk <- _cons@15) (2: 2.smk <- _cons@-15) ///
  (2: 3.drk <- _cons@-15) (2: exr <- _cons@-15) (3: fvg <- _cons@15)    ///
  (4: 3.drk <- _cons@-15) (5: 3.smk <- _cons@-15) (5: exr <- _cons@-15) ///
  if !fem, lclass(C 5) nodvheader from(umb5) 
est sto mu5
qui ent 5
local me5 = r(E)



*** Unconditional models for women
* 2-class women
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem, ///
  lclass(C 2) nodvheader
est sto fu2
qui ent 2
local fe2 = r(E)

* 3-class women
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem, ///
  lclass(C 3) nodvheader nonrtolerance
mat fb3 = e(b)

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) (2: 3.drk <- _cons@-15) ///
  if fem, lclass(C 3) nodvheader from(fb3)
est sto fu3
qui ent 3
local fe3 = r(E)

* 4-class women
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem, ///
  lclass(C 4) nodvheader nonrtolerance
mat fb4 = e(b)

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (1: 3.smk <- _cons@15) (2: 3.drk <- _cons@-15) (4: 2.drk <- _cons@15) ///
  (4: 3.drk <- _cons@15) if fem, lclass(C 4) nodvheader from(fb4) 
est sto fu4
qui ent 4
local fe4 = r(E)

* 5-class women
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem, ///
  lclass(C 5) nodvheader nonrtolerance
mat fb5 = e(b)

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (1: exr <- _cons@-15) (2: 3.smk <- _cons@-15) (2: 3.drk <- _cons@-15) ///
  if fem, lclass(C 5) nodvheader from(fb5)
est sto fu5
qui ent 5
local fe5 = r(E)




****** Model fit statistics
est stats mu2 mu3 mu4 mu5
dis "Male Entropies: " as res %3.2f `me2' " " as res %3.2f `me3' " " ///
  as res %3.2f `me4' " " as res %3.2f `me5'
  
est stats fu2 fu3 fu4 fu5
dis "Female Entropies: " as res %3.2f `fe2' " " as res %3.2f `fe3' " " ///
  as res %3.2f `fe4' " " as res %3.2f `fe5'



  
****** Examining results from best model 
*** 3-class men
est res mu3

* class distribution
estat lcprob, nose

* item response probabilites
estat lcmean, nose
mat mirp = r(b)

* assigning cases
predict mpp* if e(sample), classposteriorpr
egen mxmpp = rowmax(mpp*)
gen cls = 1 if mpp1 == mxmpp & e(sample)
replace cls = 2 if mpp2 == mxmpp & e(sample)
replace cls = 3 if mpp3 == mxmpp & e(sample)


*** 3-class women
est res fu3

* class distribution
estat lcprob, nose

* item response probabilites
estat lcmean, nose
mat firp = r(b)

* assigning cases
predict fpp* if e(sample), classposteriorpr
egen mxfpp = rowmax(fpp*)
replace cls = 1 if fpp1 == mxfpp & e(sample)
replace cls = 2 if fpp2 == mxfpp & e(sample)
replace cls = 3 if fpp3 == mxfpp & e(sample)


*** labeling and matching class for men and women
recode cls (1 = 1) (2 = 3) (3 = 2) if fem
lab def lst 1 "SD" 2 "UHD" 3 "H"
lab val cls lst



*** Graphing item response probabilities
tempfile pf
postutil clear
postfile pf fem id cls est using `pf', replace
foreach i in 0 9 18 {
  forval j = 1/9 {
    local k = `i' + `j'
	post pf (0) (`j') (`i') (mirp[1,`k'])
	post pf (1) (`j') (`i') (firp[1,`k'])
  }
}
postclose pf

preserve
use `pf', replace
recode cls (0 = 1) (9 = 3) (18 = 2) if !fem
recode cls (0 = 1) (9 = 2) (18 = 3) if fem
lab val cls lst

tempfile g1 g2 g3 g4 g5 g6 g7 g8
graph bar (sum) est if cls == 1 & !fem, over(id, ///
  relabel(1 `" "Y" "Ex" "' 2 `" "Y" "FV" "' 3 `" "Y" "SD" "'         ///
  4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" 8 `" "M" "Drink" "' 9 "H"))  ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) ytit("probability")       ///
  tit("Smoker/Drinker (16%)") saving(`g1')
  
graph bar (sum) est if cls == 2 & !fem, over(id, ///
  relabel(1 `" "Y" "Ex" "' 2 `" "Y" "FV" "' 3 `" "Y" "SD" "'         ///
  4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" 8 `" "M" "Drink" "' 9 "H"))  ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) ytit("probability")       ///
  tit("Unhealthy Diet (47%)") saving(`g2')
  
graph bar (sum) est if cls == 3 & !fem, over(id, ///
  relabel(1 `" "Y" "Ex" "' 2 `" "Y" "FV" "' 3 `" "Y" "SD" "'         ///
  4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" 8 `" "M" "Drink" "' 9 "H"))  ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) ytit("probability")       ///
  tit("Healthy (37%)")  saving(`g3')
  
graph bar (sum) est if cls == 1 & fem, over(id, ///
  relabel(1 `" "Y" "Ex" "' 2 `" "Y" "FV" "' 3 `" "Y" "SD" "'         ///
  4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" 8 `" "M" "Drink" "' 9 "H"))  ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) ytit("probability")       ///
  tit("Smoker/Drinker (7%)") saving(`g4')
  
graph bar (sum) est if cls == 2 & fem, over(id, ///
  relabel(1 `" "Y" "Ex" "' 2 `" "Y" "FV" "' 3 `" "Y" "SD" "'         ///
  4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" 8 `" "M" "Drink" "' 9 "H"))  ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) ytit("probability")       ///
  tit("Unhealthy Diet (29%)") saving(`g5')
  
graph bar (sum) est if cls == 3 & fem, over(id, ///
  relabel(1 `" "Y" "Ex" "' 2 `" "Y" "FV" "' 3 `" "Y" "SD" "'         ///
  4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" 8 `" "M" "Drink" "' 9 "H"))  ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) ytit("probability")       ///
  tit("Healthy (64%)")  saving(`g6')
  
graph combine "`g1'" "`g2'" "`g3'", rows(1) tit("Male Health Lifestyles") ///
  saving(`g7')
graph combine "`g4'" "`g5'" "`g6'", rows(1) tit("Female Health Lifestyles") ///
  saving(`g8')
  
graph combine "`g7'" "`g8'", rows(2)
graph export aahl-fig1.pdf, replace
restore


*** Saving class assignments for additional analysis
est clear
drop mpp* fpp* mxfpp* mxmpp*
save aahl-data-2, replace
