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



*** Program to extract class counts
capture program drop ExtCount
program ExtCount
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

ExtCount "aahl-f1-4.out" "pf2" f 4
ExtCount "aahl-f1-3.out" "pf2" f 3
ExtCount "aahl-m1-3.out" "pf2" m 3

postclose pf2

*** Examine class counts and CIRPs
use `d2', replace
list


*** Program to extract and graph CIRPs
capture program drop ExtCIRP
program ExtCIRP
  args OutFile ncl
  
  qui insheet using "`OutFile'", clear
  
  qui gen flag1     = regexm(v1, "RESULTS IN PROBABILITY SCALE")
  qui replace flag1 = regexm(v1, "LATENT CLASS ODDS RATIO") if flag1 == 0
  qui gen flag1a    = sum(flag1)
  qui keep if flag1a == 1
  
  qui split v1, gen(x) parse("")
  keep if x2 == "1" | x2 == "2" | x2 == "3"
  
  qui rename x3 p
  qui destring p, replace
  qui keep p
  
  qui gen cls = 0
  if (`ncl' == 3) {
    forval i = 1/3 {
	  local j = `i'*12
	  qui replace cls = `i' if _n <= `j' & cls == 0
	}
  }
  else if (`ncl' == 4) {
    forval i = 1/4 {
	  local j = `i'*12
	  qui replace cls = `i' if _n <= `j' & cls == 0
	}
  }
  
  bysort cls: gen id = _n
  qui reshape wide p, i(id) j(cls)
  
  set obs 19
  replace id = 3.5 in 13
  replace id = 6.5 in 14
  replace id = 7.5 in 15 
  replace id = 8.5 in 16
  replace id = 9.5 in 17
  replace id = 10.5 in 18
  replace id = 11.5 in 19
  sort id
end

*** Extracting results and creating graphs

* Women 4-class model
ExtCIRP "aahl-f1-4.out" 4
tempfile g1 g2 g3 g4

graph bar (sum) p1, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ylab(, angle(h) grid gstyle(dot))      ///
  ytit("probability") tit("Class 1 (N = 169)") saving(`g1', replace)

graph bar (sum) p2, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ylab(, angle(h) grid gstyle(dot))      ///
  ytit("probability") tit("Class 2 (N = 39)") saving(`g2', replace)

graph bar (sum) p3, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ylab(, angle(h) grid gstyle(dot))      ///
  ytit("probability") tit("Class 3 (N = 1743)") saving(`g3', replace)

graph bar (sum) p4, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ylab(, angle(h) grid gstyle(dot))      ///
  ytit("probability") tit("Class 4 (N = 253)") saving(`g4', replace)
  
graph combine "`g1'" "`g2'" "`g3'" "`g4'"


* Women 3-class model
ExtCIRP "aahl-f1-3.out" 3
tempfile g1 g2 g3

graph bar (sum) p1, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ytit("probability")                    ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) tit("Class 1 (N = 123)")          ///
  saving(`g1', replace)

graph bar (sum) p2, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ytit("probability")                    ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) tit("Class 2 (N = 1656)")         ///
  saving(`g2', replace)

graph bar (sum) p3, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes"))  ytit("probability")                   ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) tit("Class 3 (N = 425)")          ///
  saving(`g3', replace)

graph combine "`g1'" "`g2'" "`g3'"

  
* Men 3-class model
ExtCIRP "aahl-m1-3.out" 3
tempfile g1 g2 g3

graph bar (sum) p1, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ytit("probability")                    ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) tit("Class 1 (N = 201)")          ///
  saving(`g1', replace)

graph bar (sum) p2, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes")) ytit("probability")                    ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) tit("Class 2 (N = 643)")          ///
  saving(`g2', replace)

graph bar (sum) p3, over(id, relabel(1 "N" 2 `""P" "{bf:Smoke}"' 3 "C" 4 " " ///
  5 "N" 6 `" "L""{bf:Drink}"' 7 "H" 8 " " 9 "No" 10 `"" " "{bf:Exer}"'       ///
  11 "Yes" 12 " " 13 "No" 14 `"" " "{bf:Frt/Veg}"' 15 "Yes" 16 " " 17 "No"   ///
  18 `"" " "{bf:Sug Bev}"' 19 "Yes"))  ytit("probability")                   ///
  ylab(0(0.2)1, angle(h) grid gstyle(dot)) tit("Class 3 (N = 344)")          ///
  saving(`g3', replace)

graph combine "`g1'" "`g2'" "`g3'" 
  
