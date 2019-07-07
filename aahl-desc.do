*** Purpose: descriptive statistics for analysis of health lifestyles
*** Author:  S Bauldry 
*** Date: July 2, 2019


*** Set working directory and read prepared data
cd ~/dropbox/research/hlthineq/aahl/aahl-work/aahl-anal-8
use aahl-mi-data-2, replace

* health lifestyle indicators
tab smk fem, col
tab drk fem, col
tab exr fem, col
tab fvg fem, col
tab sbv fem, col







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
