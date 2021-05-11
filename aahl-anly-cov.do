*** Purpose: analyze predictors of lifestyles and mortality
*** Author: S Bauldry
*** Date: July 2, 2019

*** Setting directory
local dk "~/desktop"


*** Margins wrappers for MI
capture program drop mimrg1
program mimrg1, eclass properties(mi)
  args lst vs sam
  mlogit `lst' `vs' if `sam'
  margins, dydx(*) post
end

capture program drop mimrg2
program mimrg2, eclass properties(mi)
  args dv vs sam
  logit `dv' `vs' if `sam'
  margins, dydx(*) post
end



*** Load prepared data with class assignments
use "`dk'/aahl-data-2", replace


*** Combining lifestyle measure
gen lst = flst
recode mlst (1 = 1) (2 = 3) (3 = 2)
replace lst = mlst if mi(lst)

*** MI to addressing missing data in covariates
mi set wide
mi reg impute inc sss dib 
mi imp chain (ologit) dib (regress) inc sss  = age cvd i.occ ba i.lst, ///
  add(25) augment rseed(931225) by(fem)
save "`dk'/aahl-mi-data-2", replace


*** Descriptives
* health lifestyle indicators
qui tab smk, gen(smk)
qui tab drk, gen(drk)
bysort fem: sum smk1-smk3 drk1-drk3 exr fvg sbv

* covariates
foreach x of varlist age ba inc sss cvd {
  qui mi est: mean `x' if !fem
  mat mt`x' = e(b_mi)
  local m`x' = mt`x'[1,1]
  
  qui mi est: mean `x' if fem
  mat ft`x' = e(b_mi)
  local f`x' = ft`x'[1,1]
  
  dis "`x': " as res %5.3f `m`x'' " " as res %5.3f `f`x''
}

mi est: prop occ if !fem
mi est: prop occ if fem

mi est: prop dib if !fem
mi est: prop dib if fem



*** predictors of health lifestyle membership
tab mlst
mi est: mimrg1 mlst "age ba inc i.occ sss i.dib cvd" "fem == 0"

tab flst
mi est: mimrg1 flst "age ba inc i.occ sss i.dib cvd" "fem == 1"



*** relationship between health lifestyles and bmi/obesity
mi est: mimrg2 obe3 "i.mlst age ba inc i.occ sss i.dib cvd" "fem == 0"
mi est: mimrg2 obe3 "i.flst age ba inc i.occ sss i.dib cvd" "fem == 1"



mi est: logit obe3 i.lst age ba inc i.occ sss i.dib cvd if fem == 1
