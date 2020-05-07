*** Purpose: Identify health lifestyles
*** Author:  S Bauldry
*** Date:    May 5, 2020


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




*** Set working directory and loading data
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-9
use aahl-data, replace


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
mat irp = r(b)

predict mp* if e(sample), classposteriorpr
egen mxmp = rowmax(mp*)
gen     mlst = 1 if mp1 == mxmp & e(sample)
replace mlst = 2 if mp2 == mxmp & e(sample)
replace mlst = 3 if mp3 == mxmp & e(sample)

preserve
clear
svmat irp
gen id = 1
reshape long irp, i(id) j(v)
gen lst = 1 if _n < 10
replace lst = 2 if _n < 19 & mi(lst)
replace lst = 3 if mi(lst)
replace v = v - 9 if _n >= 10
replace v = v - 9 if _n >= 19

tempfile g1 g2 g3
graph bar (sum) irp if lst == 1, over(v, relabel(1 `" "Y" "Ex" "' ///
  2 `" "Y" "FV" "' 3 `" "Y" "SD" "' 4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" ///
  8 `" "M" "Drink" "' 9 "H")) ylab(0(0.2)1, angle(h) grid gstyle(dot)) ///
  ytit("probability") tit("Unhealthy Smoker (19%)") scheme(s1color) ///
  saving(`g1')
  
graph bar (sum) irp if lst == 3, over(v, relabel(1 `" "Y" "Ex" "' ///
  2 `" "Y" "FV" "' 3 `" "Y" "SD" "' 4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" ///
  8 `" "M" "Drink" "' 9 "H")) ylab(0(0.2)1, angle(h) grid gstyle(dot)) ///
  ytit("probability") tit("Unhealthy Diet (27%)") scheme(s1color) ///
  saving(`g2')
  
graph bar (sum) irp if lst == 2, over(v, relabel(1 `" "Y" "Ex" "' ///
  2 `" "Y" "FV" "' 3 `" "Y" "SD" "' 4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" ///
  8 `" "M" "Drink" "' 9 "H")) ylab(0(0.2)1, angle(h) grid gstyle(dot)) ///
  ytit("probability") tit("Healthy Diet (55%)") scheme(s1color) ///
  saving(`g3')
  
graph combine "`g1'" "`g2'" "`g3'", scheme(s1color)
graph export ~/desktop/aahl-fig1.pdf, replace
restore


*** 4-class women
est res f4
estat lcprob, nose
estat lcmean, nose
mat irp = r(b)

predict fp* if e(sample), classposteriorpr
egen mxfp = rowmax(fp*)
gen     flst = 1 if fp1 == mxfp & e(sample)
replace flst = 2 if fp2 == mxfp & e(sample)
replace flst = 3 if fp3 == mxfp & e(sample)
replace flst = 4 if fp4 == mxfp & e(sample)

preserve
clear
svmat irp
gen id = 1
reshape long irp, i(id) j(v)
gen lst = 1 if _n < 10
replace lst = 2 if _n < 19 & mi(lst)
replace lst = 3 if _n < 28 & mi(lst)
replace lst = 4 if mi(lst)
replace v = v - 9 if _n >= 10
replace v = v - 9 if _n >= 19
replace v = v - 9 if _n >= 28

tempfile g1 g2 g3 g4
graph bar (sum) irp if lst == 1, over(v, relabel(1 `" "Y" "Ex" "' ///
  2 `" "Y" "FV" "' 3 `" "Y" "SD" "' 4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" ///
  8 `" "M" "Drink" "' 9 "H")) ylab(0(0.2)1, angle(h) grid gstyle(dot)) ///
  ytit("probability") tit("Unhealthy Smoker (10%)") scheme(s1color) ///
  saving(`g1')
  
graph bar (sum) irp if lst == 2, over(v, relabel(1 `" "Y" "Ex" "' ///
  2 `" "Y" "FV" "' 3 `" "Y" "SD" "' 4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" ///
  8 `" "M" "Drink" "' 9 "H")) ylab(0(0.2)1, angle(h) grid gstyle(dot)) ///
  ytit("probability") tit("Unhealthy Diet (11%)") scheme(s1color) ///
  saving(`g2')
  
graph bar (sum) irp if lst == 3, over(v, relabel(1 `" "Y" "Ex" "' ///
  2 `" "Y" "FV" "' 3 `" "Y" "SD" "' 4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" ///
  8 `" "M" "Drink" "' 9 "H")) ylab(0(0.2)1, angle(h) grid gstyle(dot)) ///
  ytit("probability") tit("Healthy Diet (77%)") scheme(s1color) ///
  saving(`g3')
  
graph bar (sum) irp if lst == 4, over(v, relabel(1 `" "Y" "Ex" "' ///
  2 `" "Y" "FV" "' 3 `" "Y" "SD" "' 4 "N" 5 `" "P" "Smoke" "' 6 "C" 7 "A" ///
  8 `" "M" "Drink" "' 9 "H")) ylab(0(0.2)1, angle(h) grid gstyle(dot)) ///
  ytit("probability") tit("Most Healthy (2%)") scheme(s1color) ///
  saving(`g4')
  
graph combine "`g1'" "`g2'" "`g3'" "`g4'", scheme(s1color)
graph export ~/desktop/aahl-fig2.pdf, replace
restore



*** Saving class assignments for additional analysis
est clear
drop mp* mxmp fp* mxfp
save aahl-data-2, replace


