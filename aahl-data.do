*** Purpose: prepare JHS data for analysis of health lifestyles
*** Author:  S Bauldry 
*** Date:    October 1, 2018

*** Setting working directory
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-7

*** Extract variables
use subjid male age1 fmlyinc1 edu3cat alcw1 currentsmoker1 eversmoker1  ///
    idealhealthpa1 nutrition3cat1 diab3cat1 cvdhx1 death lastdate       ///
	occupation1 brthyr brthmo using                                     ///
	"~/dropbox/research/data/JHS/aahl-JHS-Total", replace
	
merge 1:1 subjid using "~/dropbox/research/data/JHS/aahl-JHS-data2", ///
  keepusing(pdsa18a ds_fruveg ds_swtbev)
drop _merge

merge 1:1 subjid using "~/dropbox/research/data/JHS/aahl-JHS-data3", ///
  keepusing(pdsa2a)
drop _merge

mi unset, asis


*** preparing variables for analysis
gen smk = .
replace smk = 1 if ever == 0 & current == 0
replace smk = 2 if ever == 1 & current == 0
replace smk = 3 if ever == 1 & current == 1
lab var smk "smoke"
lab def sm 1 "never" 2 "past" 3 "current"
lab val smk sm

gen drk = .
replace drk = 3 if male & alcw > 14 & !mi(alcw)
replace drk = 2 if male & alcw <= 14
replace drk = 3 if !male & alcw > 7 & !mi(alcw)
replace drk = 2 if !male & alcw <= 7
replace drk = 1 if alcw == 0
lab var drk "drinks/week"
lab def drk 1 "none" 2 "1-14 m, 1-7 f" 3 ">14 m, >7 f"
lab val drk drk

rename idealhealthpa1 exr
lab val exr yn

rename (ds_fruveg ds_swtbev) (fvg sbv)

replace fmlyinc1 = "2500"  if fmlyinc1 == "A"
replace fmlyinc1 = "6500"  if fmlyinc1 == "B"
replace fmlyinc1 = "10000" if fmlyinc1 == "C"
replace fmlyinc1 = "14000" if fmlyinc1 == "D"
replace fmlyinc1 = "18000" if fmlyinc1 == "E"
replace fmlyinc1 = "22500" if fmlyinc1 == "F"
replace fmlyinc1 = "30000" if fmlyinc1 == "G"
replace fmlyinc1 = "42500" if fmlyinc1 == "H"
replace fmlyinc1 = "62500" if fmlyinc1 == "I"
replace fmlyinc1 = "87500" if fmlyinc1 == "J"
replace fmlyinc1 = "125000" if fmlyinc1 == "K"
destring fmlyinc1, replace
gen inc = ln(fmlyinc1)
lab var inc "ln family income"

* setting framing, military, sick, unemployed, homemaker, retired, student, and
* other to missing (N = 52; < 1%)
recode occupation1 (1 = 1) (2 3 = 2) (5 6 = 3) (4 7/13 = .), gen(occ)
lab def oc 1 "management/professional" 2 "service/sales" ///
           3 "construction/production" 
lab val occ oc

rename pdsa18a edu

rename (diab3cat cvdhx age1 death lastdate pdsa2a) (dib cvd age dth ldt sss)

gen fem = (male == 0) if !mi(male)

*** calculating age at death or censoring
gen bdt = mdy(brthmo, 15, brthyr)
gen agd = (ldt - bdt)/364.25

*** keeping analysis sample and variables
keep if !mi(occ)
keep if age >= 50
sort subjid
gen id = _n
order id smk drk exr fvg sbv age fem edu inc occ sss cvd dth agd
keep id-agd
save aahl-data, replace

*** saving lifestyle indicators for LCA in Mplus
keep id smk drk exr fvg sbv fem
recode _all (. = -9)
outsheet using aahl-data-mplus.txt, replace comma nolab nonames


