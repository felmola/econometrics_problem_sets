/*
Todo: Enable log again
*/

//*#############################################################################
//* 0. Set working directory and log.
//*#############################################################################
clear all
set more off
cd "A:\_maestria_unibo_(operacional)\4_econometrics_1\4_problem_sets\2_ps2"

//log using 3_log\log, replace

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

//*#############################################################################
//* n. Close log.
//*#############################################################################

//log close
//translate 3_log\log.smcl 3_log\log.pdf, replace

//*#############################################################################















//*#############################################################################
//* 0. Set working directory and log.
//*#############################################################################
//*=============================================================================
//* 1.1.
//*=============================================================================
//*-----------------------------------------------------------------------------
//* 1.1.1.
//*-----------------------------------------------------------------------------
