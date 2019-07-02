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
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 0, ///
  lclass(C 2) nodvheader nonrtolerance
mat mb2 = e(b)

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (2: 3.drk <- _cons@-15) if fem == 0, lclass(C 2) nodvheader from(mb2)
est sto mu2
ent 2


* 3-class men
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 0, ///
  lclass(C 3) nodvheader 
est sto mu3
ent 3


* 4-class men
qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 0, ///
  lclass(C 4) nodvheader nonrtolerance
mat umb4 = e(b)

qui gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (1: 2.smk <- _cons@-15) (2: 3.smk <- _cons@-15) (3: 3.drk <- _cons@-15) ///
  (3: fvg <- _cons@15) if fem == 0, lclass(C 4) nodvheader from(umb4)
est sto mu4
ent 4

est stats mu2 mu3 mu4


*** 3-class men
est res mu3

* class distribution
estat lcprob

* item response probabilites
estat lcmean
mat mirp = r(b)

* assigning cases
predict mpp* if e(sample), classposteriorpr
egen mxmpp = rowmax(mpp*)
gen cls = 1 if mpp1 == mxmpp & e(sample)
replace cls = 2 if mpp2 == mxmpp & e(sample)
replace cls = 3 if mpp3 == mxmpp & e(sample)



*** Unconditional models for women
* 2-class women
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 1, ///
  lclass(C 2) nodvheader
est sto fu2
ent 2

* 3-class women
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 1, ///
  lclass(C 3) nodvheader nonrtolerance
mat fb2 = e(b)

gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) (2: 3.drk <- _cons@-15) ///
  if fem == 1, lclass(C 3) nodvheader from(fb2)
est sto fu3
ent 3

* 4-class women
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 1, ///
  lclass(C 4) nodvheader nonrtolerance
mat fb4 = e(b)

gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) (1: 3.smk <- _cons@15) ///
  (2: 3.drk <- _cons@-15) (4: 2.drk <- _cons@15) (4: 3.drk <- _cons@15)  ///
  if fem == 1, lclass(C 4) nodvheader from(fb4) 
est sto fu4
ent 4

est stats fu2 fu3 fu4


*** 3-class women
est res fu3

* class distribution
estat lcprob

* item response probabilites
estat lcmean
mat firp = r(b)

* assigning cases
predict fpp* if e(sample), classposteriorpr
egen mxfpp = rowmax(fpp*)
replace cls = 1 if fpp1 == mxfpp & e(sample)
replace cls = 2 if fpp2 == mxfpp & e(sample)
replace cls = 3 if fpp3 == mxfpp & e(sample)


*** labeling and matching class for men and women
recode cls (1 = 1) (2 = 3) (3 = 2) if fem
recode cls (1 = 1) (2 = 3) (3 = 2) if !fem
lab def lst 1 "UHS" 2 "UHD" 3 "HD"
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
recode cls (0 = 1) (9 = 2) (18 = 3) if !fem

graph bar (sum) est if cls == 1, over(id)
restore


*** Saving class assignments for additional analysis
est clear
drop mpp* fpp* mxfpp* mxmpp*
save aahl-data-2, replace
