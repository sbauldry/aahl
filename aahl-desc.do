*** Purpose: descriptive statistics for analysis of health lifestyles
*** Author:  S Bauldry 
*** Date: July 2, 2019


*** Set working directory and read prepared data
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-8
use aahl-mi-data-2, replace

* health lifestyle indicators
qui tab smk, gen(smk)
qui tab drk, gen(drk)
bysort fem: sum smk1-smk3 drk1-drk3 exr fvg sbv

* covariates 
foreach x of varlist age ba inc sss dds ltd cvd dth {
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
