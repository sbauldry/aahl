*** Purpose: Identify health lifestyles
*** Author:  S Bauldry
*** Date:    May 11, 2021


*** Setting directory
local dk "~/desktop"


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


*** Defining program for LCAs
capture program drop lca
program lca
  args sam ncl enm cst
  
  qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) `cst' if `sam', ///
       lclass(C `ncl') nodvheader iter(100)
     est sto `enm'
     ent `ncl'  	
end




*** Loading data
use "`dk'/aahl-data", replace



*** Identifying best fitting models
est clear

* Men
qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) if !fem, lclass(C 2)
est sto m2
ent 2 

qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) ///
  (2: fvg <- _cons@15) (3: fvg <- _cons@-15) if !fem, lclass(C 3)
est sto m3
ent 3

qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) ///
  (2: 3.smk <- _cons@-15) (3: sbv <- _cons@-15) if !fem, lclass(C 4)
est sto m4
ent 4

qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) ///
  (2: sbv <- _cons@-15) (3: 3.smk <- _cons@-15) (4: 2.smk <- _cons@15) ///
  (4: 3.smk <- _cons@15) (4: fvg <- _cons@-15) if !fem, lclass(C 5)
est sto m5
ent 5

est stats m2 m3 m4 m5


* Women
qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) if fem, lclass(C 2)
est sto f2
ent 2

qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) ///
  (2: 3.drk <- _cons@-15) if fem, lclass(C 3)
est sto f3
ent 3

qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) ///
  (2: 3.drk <- _cons@-15) if fem, lclass(C 4)
est sto f4
ent 4

qui gsem (smk drk <- , mlogit) (exr fvg sbv <- , logit) ///
  (1: exr <- _cons@-15) (2: 3.drk <- _cons@-15) (2: fvg <- _cons@-15)  ///
  (4: 3.drk <- _cons@-15) (5: 3.smk <- _cons@-15) (5: exr <- _cons@15) ///
  (5: sbv <- _cons@15) if fem, lclass(C 5)
est sto f5
ent 5



*** Examining distributions and item response probabilities

* 3-class men
est res m3
estat lcprob, nose
estat lcmean, nose
mat irp1 = r(table)

predict mp* if e(sample), classposteriorpr
egen mxmp = rowmax(mp*)
gen     mlst = 1 if mp1 == mxmp & e(sample)
replace mlst = 2 if mp2 == mxmp & e(sample)
replace mlst = 3 if mp3 == mxmp & e(sample)


*** 4-class women
est res f4
estat lcprob, nose
estat lcmean, nose
mat irp2 = r(table)

predict fp* if e(sample), classposteriorpr
egen mxfp = rowmax(fp*)
gen     flst = 1 if fp1 == mxfp & e(sample)
replace flst = 2 if fp2 == mxfp & e(sample)
replace flst = 3 if fp3 == mxfp & e(sample)
replace flst = 4 if fp4 == mxfp & e(sample)


*** Saving class assignments for additional analysis
est clear
drop mp* mxmp fp* mxfp
save "`dk'/aahl-data-2", replace
