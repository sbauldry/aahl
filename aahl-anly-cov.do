*** Purpose: analyze predictors of lifestyles and mortality
*** Author: S Bauldry
*** Date: July 2, 2019


*** Set working directory
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-8

*** Load prepared data with class assignments
use aahl-data-2, replace


*** MI to addressing missing data in covariates
drop smk drk exr fvg sbv
mi set wide
mi reg impute edu inc occ sss dib dds ltd dsb
mi imp chain (ologit) edu dib (mlogit) occ (regress)inc sss dds ltd dsb ///
  = i.cls age cvd dth agd, add(25) augment rseed(931225) by(fem)



*** predictors of class membership
eststo clear

qui mi est, post: mlogit cls age edu inc i.occ sss i.dib cvd if !fem
eststo m1

qui mi est, post: mlogit cls age edu inc i.occ sss i.dib cvd if !fem, base(2)
eststo m2

qui mi est, post: mlogit cls age edu inc i.occ sss i.dib cvd if fem
eststo m3

qui mi est, post: mlogit cls age edu inc i.occ sss i.dib cvd if fem, base(2)
eststo m4

esttab m1 m2 m3 m4 using t1.csv, replace wide b(%5.2f) se(%5.2f) nodep nonum ///
  nogap unstack eform
  


*** predictors of mortality
mi stset agd, failure(dth)

eststo clear

mi est, post: stcox b3.cls age edu inc i.occ sss i.dib cvd if !fem
eststo m1

mi est, post: stcox b3.cls age edu inc i.occ sss i.dib cvd if fem
eststo m2

esttab m1 m2 using t2.csv, replace wide b(%5.2f) se(%5.2f) nodep nonum ///
  nogap unstack eform
