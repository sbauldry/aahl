*** Purpose: descriptive statistics for analysis of health lifestyles
*** Author:  S Bauldry 
*** Date:    October 1, 2018


*** Set working directory and read prepared data
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-7
use aahl-data, replace

* value labels
lab def sx 0 "male" 1 "female", replace
lab val fem sx

lab def yn 0 "N" 1 "Y", replace
lab val exr fvg sbv yn

lab def sm 1 "N" 2 "P" 3 "C", replace
lab val smk sm

lab def dr 1 "N" 2 "M" 3 "H", replace
lab val drk dr

lab def oc 1 "M/P" 2 "S/S" 3 "C/P", replace
lab val occ oc

* univariate descriptives
capture program drop vd
program vd
  args vr tit
  
  preserve
  keep if !mi(`vr')
  
  qui sum fem if fem
  local nf = r(N)
  local nm = _N - r(N)
  gen pr = 1/`nf' if fem
  replace pr = 1/`nm' if !fem
  
  graph bar (sum) pr, over(`vr') by(fem) ytit("proportion") ///
    ylab(0(.2)1, angle(h) grid gstyle(dot)) tit("`tit'") ///
	saving("~/desktop/`vr'", replace)
  restore
end

vd smk "smoking"
vd drk "drinking"
vd exr "exercise"
vd fvg "fruit/veg"
vd sbv "sugar bev"

graph combine ~/desktop/smk.gph ~/desktop/drk.gph ~/desktop/exr.gph ///
  ~/desktop/fvg.gph ~/desktop/sbv.gph

 
hist edu, discrete by(fem) xtit("education") xlab(, grid gstyle(dot)) ///
  ylab(, angle(h) grid gstyle(dot)) tit("education") ///
  saving(~/desktop/edu, replace)
  
hist inc, discrete by(fem) xtit("log income") xlab(, grid gstyle(dot)) ///
  ylab(, angle(h) grid gstyle(dot)) tit("log income") ///
  saving(~/desktop/inc, replace)
  
hist sss, discrete by(fem) xtit("social status") xlab(, grid gstyle(dot)) ///
  ylab(, angle(h) grid gstyle(dot)) tit("social status") ///
  saving(~/desktop/sss, replace)

vd occ "occupation"

graph combine ~/desktop/edu.gph ~/desktop/inc.gph ~/desktop/sss.gph ///
  ~/desktop/occ.gph 
  
  
hist age, by(fem) xtit("age") xlab(50(20)100, grid gstyle(dot)) ///
  ylab(, angle(h) grid gstyle(dot)) tit("age") saving(~/desktop/age, replace)
  




* for manuscript

qui tab smk, gen(smk)
qui tab drk, gen(drk)

sum smk1-smk3 drk1-drk3 exr fvg sbv age ed2 ed3 db2 db3 cvd dth
sum smk1-smk3 drk1-drk3 exr fvg sbv age ed2 ed3 db2 db3 cvd dth if male == 1
sum smk1-smk3 drk1-drk3 exr fvg sbv age ed2 ed3 db2 db3 cvd dth if male == 0


* for visualization
gen po = 1/_

gen one = 1
graph bar (sum) one, over(smk) by(fem) 
