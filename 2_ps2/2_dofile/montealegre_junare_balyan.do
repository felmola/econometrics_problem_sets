//*#############################################################################
//* 0. Set working directory and log.
//*#############################################################################
clear all
set more off
cd "A:\_maestria_unibo_(operacional)\4_econometrics_1\4_problem_sets\2_ps2"

log using 3_log\log, replace

//*#############################################################################
//* A.
//*#############################################################################

use 1_data\dataset_1.dta

//*=============================================================================
//* A.1.
//*=============================================================================
//*----- Calculate the mean value for each TV over all sample.


//*----- Calculate mean values of each target variable (TV).

bysort state_code: egen mean_etr = mean(expos_to_robots)
bysort state_code: egen mean_emp = mean(d_emppriv_1990_2011)

//*----- Remove duplicated data to display min and max values.

quietly by state_code:  gen dup = cond(_N==1,0,_n)
drop if dup > 1

//*----- Identify the states that have the min and max values for each TV.

sort mean_etr
gen min_etr_state = 1 if _n == 1
gen max_etr_state = 1 if _n == _N
table state_code (min_etr_state), statistic (mean mean_etr) nototals
table state_code (max_etr_state), statistic (mean mean_etr) nototals

sort mean_emp
gen min_emp_state = 1 if _n == 1
gen max_emp_state = 1 if _n == _N
table state_code (min_emp_state), statistic (mean mean_emp) nototals
table state_code (max_emp_state), statistic (mean mean_emp) nototals

//*----- Calculate the mean value for each TV over state means.

summarize mean_etr
summarize mean_emp

//*----- Two-way catterplot with regression line.

clear all
use 1_data\dataset_1.dta

graph twoway (lfitci d_emppriv_1990_2011 expos_to_robots, xtitle("Exposure to robots")) (scatter d_emppriv_1990_2011 expos_to_robots, msize(tiny) legend(size(small) label(3 "Change in the share of private employment over total population. 1990-2011.")))
 
graph save "scatter_line.gph", replace
graph save 4_graphs\scatter_line.gph, replace
graph export 4_graphs\scatter_line.png, as(png) replace
graph close

//*#############################################################################
//* B.
//*#############################################################################

clear all
use 1_data\dataset_1.dta

//*=============================================================================
//* B.2. Simple linear regression.
//*=============================================================================

reg d_emppriv_1990_2011 expos_to_robots, rob
outreg2 using 5_tables\reg1, tex replace

//*=============================================================================
//* B.3. Regression with additional controls.
//*=============================================================================

reg d_emppriv_1990_2011 expos_to_robots ipums_logpop_1990 ipums_female_1990 ipums_above65_1990 ipums_highschool_1990 ipums_somecollege_1990 ipums_college_1990 ipums_masters_1990 ipums_white_1990 ipums_black_1990 ipums_hispanic_1990 ipums_asian_1990, rob
outreg2 using 5_tables\reg2, tex replace

//*----- Dropping some variables because of multicollinearity.

reg d_emppriv_1990_2011 expos_to_robots ipums_logpop_1990 ipums_female_1990 ipums_above65_1990 ipums_somecollege_1990 ipums_college_1990 ipums_masters_1990 ipums_black_1990 ipums_hispanic_1990 ipums_asian_1990, rob
outreg2 using 5_tables\reg2, tex replace

//*=============================================================================
//* B.4. Regression with the square of expos_to_robots.
//*=============================================================================

gen expos_2 = expos_to_robots*expos_to_robots
reg d_emppriv_1990_2011 expos_to_robots expos_2 ipums_logpop_1990 ipums_female_1990 ipums_above65_1990 ipums_somecollege_1990 ipums_college_1990 ipums_masters_1990 ipums_black_1990 ipums_hispanic_1990 ipums_asian_1990, rob
outreg2 using 5_tables\reg3, tex replace

//*=============================================================================
//* B.5. Testing the marginal effect of expos_to_robots (and its cuadratic term).
//*=============================================================================
//*----- Evaluating the marginal effect of exposure to robots at the mean of expos_to_robots.

summarize expos_to_robots
scalar mean_etr = r(mean)
di mean_etr
lincom _b[expos_to_robots] + (2 * _b[expos_2] * mean_etr)

//*=============================================================================
//* B.6. Heteroskedasticity tests.
//*----- Runnign full regression without rob.

reg d_emppriv_1990_2011 expos_to_robots expos_2 ipums_logpop_1990 ipums_female_1990 ipums_above65_1990 ipums_somecollege_1990 ipums_college_1990 ipums_masters_1990 ipums_black_1990 ipums_hispanic_1990 ipums_asian_1990
rvfplot, yline(0)
estat imtest, white

//*#############################################################################
//* C.
//*#############################################################################

clear all
use 1_data\dataset_2.dta
summarize

//*=============================================================================
//* C.1. Creating a dummy variable for whites.
//*=============================================================================

codebook race
gen white = 0 if race != 5
replace white = 1 if race == 5
tab race white

//*=============================================================================
//* C.2. Regression.
//*=============================================================================

reg d_yrwage_ln_1990_2011 c.expos_to_robots##i.female white##i.education, rob
outreg2 using 5_tables\reg4, tex replace

//*=============================================================================
//* C.3. Chow test for testing differences between whites and non-whites.
//*=============================================================================

//* Not included.

//*=============================================================================
//* C.4	Test of the difference in marginal effect of robots on men and women.
//*=============================================================================

test 1.female#c.expos_to_robots

//*=============================================================================
//* C.5. Regression with dummy variables for education levels.
//*=============================================================================

tabulate education, generate(edu)
reg d_yrwage_ln_1990_2011 expos_to_robots female edu2 edu3 edu4 edu5 

//*=============================================================================
//* C.6. Regression with interaction between robots and dummies for education.
//*=============================================================================

reg d_yrwage_ln_1990_2011 c.expos_to_robots##i.education female race, rob
outreg2 using 5_tables\reg5, tex replace

//*#############################################################################
//* n. Close log.
//*#############################################################################

log close
translate 3_log\log.smcl 3_log\log.pdf, replace

//*#############################################################################