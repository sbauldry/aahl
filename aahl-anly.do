*** Purpose: analyze predictors of lifestyles and mortality
*** Author: S Bauldry
*** Date: July 1, 2019

*** Defining program to calculate entropy
capture program drop ent
program ent
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
* 2-class men
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 0, lclass(C 2) ///
  nodvheader nonrtolerance
mat mb2 = e(b)

gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) (2: 3.drk <- _cons@-15) ///
  if fem == 0, lclass(C 2) nodvheader from(mb2)
est sto um2
ent 2

* 3-class men
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 0, lclass(C 3) ///
  nodvheader 
est sto um33
ent 3

* 4-class men
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) if fem == 0, lclass(C 4) ///
  nodvheader nonrtolerance
mat umb4 = e(b)

gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) (1: 2.smk <- _cons@-15) ///
  (2: 3.smk <- _cons@-15) (3: 3.drk <- _cons@-15) (3: fvg <- _cons@15) ///
  if fem == 0, lclass(C 4) nodvheader from(umb4)
est sto um44
ent 4

est stats um2 um3 um4


*** Conditional models for men
* 2-class men
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (C <- age edu i.occ sss dib cvd) if fem == 0, lclass(C 2) nodvheader ///
  nonrtolerance
mat cmb2 = e(b)

gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (C <- age edu i.occ sss dib cvd) if fem == 0, lclass(C 2) nodvheader ///
  from(cmb2)
est sto cm2
ent 2

* 3-class men
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (C <- age edu i.occ sss dib cvd) if fem == 0, lclass(C 3) nodvheader
est sto cm3
ent 3

* 4-class men
gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) ///
  (C <- age edu i.occ sss dib cvd) if fem == 0, lclass(C 4) nodvheader ///
  nonrtolerance em(iter(5)) iter(100)
mat cmb4 = e(b)

gsem (smk drk <-, mlogit) (exr fvg sbv <-, logit) (2: 3.drk <- _cons@-15) ///
  (C <- age edu i.occ sss dib cvd) if fem == 0, lclass(C 4) nodvheader ///
  from(cmb4)
est sto cm4
ent 4

est stat cm2 cm3 cm4






*** Extract class assignments
capture program drop ReadClass
program ReadClass
	args nv nc cl fn tn
	clear
	infile str5 v1-v`nv' id cp1-cp`nc' `cl' using `fn'
	keep id `cl'
	save `tn', replace
end

*** Reading data from Mplus
ReadClass 5 3 ls  aahl-anal-7-mplus/aahl-f1-3-cp.txt d1
ReadClass 5 3 ls  aahl-anal-7-mplus/aahl-m1-3-cp.txt d2

*** Merging with original data
use d1, replace
append using d2
merge 1:1 id using aahl-data
drop _merge

*** Recode lifestyles
recode ls (1 = 1) (2 = 3) (3 = 2) if fem == 1
recode ls (1 = 1) (2 = 3) (3 = 2) if fem == 0
lab def lst 1 "UHS" 2 "UHD" 3 "HD"
lab val ls lst


*** Run multiple imputation for covariates
mi set wide
mi reg impute smk drk fvg sbv edu inc sss dib
mi imp chain (logit) fvg sbv (ologit) dib smk drk edu (regress) inc sss = ///
  i.ls age fem cvd i.occ i.exr dth agd, add(25) augment rseed(93122)
save aahl-mi-data, replace
			 
	
*** Descriptive statistics
*** Note: F-test from regression is equivalent to t-test
foreach x of varlist exr fvg sbv age edu inc sss dds ltd dsb cvd dth {
	qui mi est: mean `x' if !fem
	mat b2 = e(b_mi) 
	local b2 = b2[1,1]
	
	qui mi est: mean `x' if fem
	mat b3 = e(b_mi)
	local b3 = b3[1,1]
	
	qui mi est: regress `x' i.fem
	local p = e(p_mi)
	
	dis "`x' " as res %5.2f `b2' " " as res %5.2f `b3' " " as res %5.3f `p'
}

foreach x of varlist smk drk occ dib {
	qui mi est: prop `x' if fem == 0
	mat b2 = e(b_mi)
	
	qui mi est: prop `x' if fem == 1
	mat b3 = e(b_mi)
	
	qui mi est: mlogit `x' i.fem
	local p = e(p_mi)
	
	forval i = 1/3 {
		local b2 = b2[1,`i']
		local b3 = b3[1,`i']
		
		dis "`x' `i' " as res %5.2f `b2' " " as res %5.2f `b3' ///
		  " " as res %5.3f `p'
	}
}


	
	
*** predictors of class membership

eststo clear

qui mi est, post: mlogit ls age edu inc i.occ sss i.dib cvd ///
  if !fem
eststo m1

qui mi est, post: mlogit ls age edu inc i.occ sss i.dib cvd ///
  if !fem, base(2)
eststo m2

qui mi est, post: mlogit ls age edu inc i.occ sss i.dib cvd if fem
eststo m3

qui mi est, post: mlogit ls age edu inc i.occ sss i.dib cvd ///
  if fem, base(2)
eststo m4

esttab m1 m2 using t1-mal.csv, replace wide b(%5.2f) ci(%5.2f) nodep nonum ///
  nogap unstack eform
  
esttab m3 m4 using t1-fem.csv, replace wide b(%5.2f) ci(%5.2f) nodep nonum ///
  nogap unstack eform
  

*** predictors of mortality
mi stset agd, failure(dth)

eststo clear

mi est, post: stcox b3.ls age edu inc i.occ sss i.dib cvd if !fem
eststo m1

mi est, post: stcox b3.ls age edu inc i.occ sss i.dib cvd if fem
eststo m2

esttab m1 m2 , replace wide b(%5.2f) ci(%5.2f) nodep nonum ///
  nogap unstack eform
  
  
using t2.csv



