*** Purpose: extract results from LCA
*** Author:  S Bauldry 
*** Date:    October 24, 2018

*** Setting working directory
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-7/aahl-anal-7-mplus

*** Program to extract fit statistics
capture program drop ExtFit
program ExtFit
  args OutFile PostFile sex ncl
  
  qui insheet using "`OutFile'", clear
  
  qui gen flag     = regexm(v1, "Number of observations")
  qui replace flag = regexm(v1, "Number of Free Parameters") if flag == 0
  qui replace flag = regexm(v1, "Akaike") if flag == 0
  qui replace flag = regexm(v1, "Bayesian") if flag == 0
  qui replace flag = regexm(v1, "Entropy") if flag == 0
  
  qui gen flag2     = regexm(v1, "Chi-Square Test of Model Fit") 
  qui replace flag = 1 if flag2[_n - 7] == 1
  qui replace flag = 1 if flag2[_n - 8] == 1
  qui replace flag = 1 if flag2[_n - 9] == 1
  
  qui gen flag3    = regexm(v1, "VUONG-LO-MENDELL")
  qui replace flag = 1 if flag3[_n - 6] == 1  
  
  qui keep if flag
	
  qui replace v1 = regexr(v1, "Number of observations", "")
  qui replace v1 = regexr(v1, "Number of Free Parameters", "")
  qui replace v1 = regexr(v1, "Akaike \(AIC\)", "")
  qui replace v1 = regexr(v1, "Bayesian \(BIC\)", "")
  qui replace v1 = regexr(v1, "Degrees of Freedom", "")
  qui replace v1 = regexr(v1, "P-Value", "")
  qui replace v1 = regexr(v1, "Value", "")
  qui replace v1 = regexr(v1, "Entropy", "")
  
  qui destring v1, replace
  
  forval i = 1/9 {
    local p`i' = v1[`i']
  }
	
  post `PostFile' ("`sex'") (`ncl') (`p1') (`p2') (`p3') (`p4') (`p5') ///
    (`p6') (`p7') (`p8') (`p9')
end

*** Extract fit statistics
postutil clear
tempfile d1
postfile pf1 str2 sex ncl n np aic bic chi df p e vlmrp using `d1', replace
foreach s in m f {
  forval i = 2/5 {
    ExtFit "aahl-`s'1-`i'.out" "pf1" "`s'" `i'
  }
}
postclose pf1

*** Examine fit statistics
use `d1', replace
sort ncl

* Female
list ncl chi df p vlmrp bic e if sex == "f", clean

* Male
list ncl chi df p vlmrp bic e if sex == "m", clean



*** Program to extract class counts and CIRPs
capture program drop ExtCIRP
program ExtCIRP
  args OutFile PostFile sex ncl

  qui insheet using "`OutFile'", clear
  
  qui gen flag  = 0
  qui gen flag1 = regexm(v1, "Class Counts and Proportions")
  
  forval i = 1/`ncl' {
    local j = `i' + 2
    qui replace flag = 1 if flag1[_n - `j'] == 1 & flag == 0
  }
  
  qui keep if flag
  
  qui split v1, gen(x) parse("") destring
  
  forval i = 1/`ncl' {
    local n`i' = x2[`i']
  }
  if (`ncl' == 3) {
    local n4 = .
  }
  
  
  
  post `PostFile' ("`sex'") (`ncl') (`n1') (`n2') (`n3') (`n4')
end

*** Extract class counts and CIRPS for best fitting models
postutil clear
tempfile d2
postfile pf2 str2 sex ncl n1 n2 n3 n4 using `d2', replace

ExtCIRP "aahl-f1-4.out" "pf2" f 4
ExtCIRP "aahl-m1-3.out" "pf2" m 3

postclose pf2

*** Examine class counts and CIRPs
use `d2', replace
list

  
