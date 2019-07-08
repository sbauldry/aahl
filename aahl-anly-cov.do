*** Purpose: analyze predictors of lifestyles and mortality
*** Author: S Bauldry
*** Date: July 2, 2019


*** Margins wrapper for MI
capture program drop mimrg
program mimrg, eclass properties(mi)
  args vs f
  mlogit cls `vs' if fem == `f'
  margins, dydx(*) post
end


*** Set working directory
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-8

*** Load prepared data with class assignments
use aahl-data-2, replace


*** MI to addressing missing data in covariates
mi set wide
mi reg impute ba inc occ sss dib dds ltd
mi imp chain (logit) ba (ologit) dib (mlogit) occ (regress) inc sss dds ltd ///
  = i.cls age cvd dth agd, add(25) augment rseed(931225) by(fem)
save aahl-mi-data-2, replace


*** predictors of class membership
mi est: mimrg "age ba inc i.occ sss dds ltd i.dib cvd" 0
mi est: mimrg "age ba inc i.occ sss dds ltd i.dib cvd" 1
  
mi est: mimrg "age ba inc i.occ sss dds i.dib cvd" 0
mi est: mimrg "age ba inc i.occ sss ltd i.dib cvd" 0

*** predictors of mortality
mi stset agd, failure(dth)

eststo clear

mi est, post: stcox b3.cls age ba inc i.occ sss dds ltd i.dib cvd if !fem
eststo m1

mi est, post: stcox b3.cls age ba inc i.occ sss dds ltd i.dib cvd if fem
eststo m2

esttab m1 m2 using ~/desktop/t3.csv, b(%9.3f) se(%9.3f) eform compress nogaps


mi est, post: stcox b1.cls age ba inc i.occ sss dds ltd i.dib cvd if !fem
mi est, post: stcox b1.cls age ba inc i.occ sss dds ltd i.dib cvd if fem
