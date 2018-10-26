*** Purpose: analyze predictors of lifestyles and mortality
*** Author: S Bauldry
*** Date: October 26, 2018

*** Set working directory
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-7

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
ReadClass 5 4 ls  aahl-anal-7-mplus/aahl-f1-4-cp.txt d1
ReadClass 5 3 ls  aahl-anal-7-mplus/aahl-m1-3-cp.txt d2

*** Merging with original data
use d1, replace
append using d2
merge 1:1 id using aahl-data
drop _merge

*** Recode lifestyles
recode ls (1 = 1) (2 = 4) (3 = 3) (4 = 2) if fem == 1
recode ls (1 = 1) (2 = 3) (3 = 2) if fem == 0
lab def lst 1 "UHS" 2 "UHD" 3 "HD" 4 "H"
lab val ls lst


*** Run multiple imputation for covariates
drop ed2
mi set wide
mi reg impute smk drk exr fvg sbv edu inc occ sss dds ltd dsb dib edr ldr
mi imp chain (logit) exr fvg sbv edr ldr (ologit) dib smk drk              ///
             (mlogit) occ (regress) edu inc sss dds ltd dsb = i.ls age ///
			 fem cvd dth agd, add(25) augment rseed(931225)
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

mi est, post: stcox b3.ls age edu ed2 inc i.occ sss dds ltd dsb i.dib cvd if !fem
eststo m1

mi est, post: stcox b3.ls age edu ed2 inc i.occ sss dds ltd dsb i.dib cvd if fem
eststo m2

esttab m1 m2 , replace wide b(%5.2f) ci(%5.2f) nodep nonum ///
  nogap unstack eform
  
  
using t2.csv



