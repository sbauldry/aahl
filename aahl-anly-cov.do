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
drop smk drk exr fvg sbv
mi set wide
mi reg impute edu inc occ sss dib dds ltd dsb
mi imp chain (ologit) edu dib (mlogit) occ (regress)inc sss dds ltd dsb ///
  = i.cls age cvd dth agd, add(25) augment rseed(931225) by(fem)



*** predictors of class membership
mi est: mimrg "age i.edu inc i.occ sss dds ltd dsb i.dib cvd" 0
mi est: mimrg "age i.edu inc i.occ sss dds ltd dsb i.dib cvd" 1
  

*** predictors of mortality
mi stset agd, failure(dth)

mi est: stcox b3.cls age i.edu inc i.occ sss dds ltd dsb i.dib cvd if !fem
mi est: stcox b3.cls age i.edu inc i.occ sss dds ltd dsb i.dib cvd if fem
