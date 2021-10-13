*###############################################################################
* 0. Set working directory.
*###############################################################################
clear all
set more off
dir "A:\_maestria_unibo_(operacional)\4_econometrics_1\4_problem_sets\1_ps1"

*###############################################################################
* 1. Question 1.
*###############################################################################
*===============================================================================
* 1.1. Generate sample from the random variables and its population parameters.
*===============================================================================
set obs 100 // Set the number of  observations for the random sample.
set seed 1015 // Set the seed for the pseudo-random number generator.

matrix means = (10,15,15,10) //Vector of means for the drawnorm().
matrix varcov = (1,0.6,0,0.2\0.6,1,0,0.3\0,0,1,0\0.2,0.3,0,1) //Matrix of var-cov for the drawnorm().
matrix list means // Displays a matrix.
matrix list varcov

drawnorm y x1 x2 x3, cov(varcov) means(means) //Generates a sample of the random variables with the specified parameters.
list y in 1/10
summarize // Calculate mean and sd for the data.

gen constant = 1

mata

//------------------------------------------------------------------------------
// 1.1.a. Generate population values of the coefficients of X.
//------------------------------------------------------------------------------
//----- Calculate the vector of coeficcients for X.

means = (10,15,15,10)' //Vector of means for the drawnorm().
varcov = (1,0.6,0,0.2\0.6,1,0,0.3\0,0,1,0\0.2,0.3,0,1) //Matrix of var-cov for the drawnorm().
means
varcov

varcov_x = varcov[2..4,2..4]
varcov_x

cov_y = (varcov[1,2],varcov[1,3],varcov[1,4])'
cov_y

beta = invsym(varcov_x) * (cov_y)
beta

//----- Calculate B0.

mean_x = (15,15,10)'
mean_x

beta_0 = 10 - (mean_x'*beta)
beta_0

//------------------------------------------------------------------------------
// 1.1.b. Interpretation of B1.
//------------------------------------------------------------------------------

// b1 is the marginal effect of the regresor x1 on the expected valuer of y. 
// Since b1=0, the marginal effecct of x1 on y is zero.

//------------------------------------------------------------------------------
// 1.1.c. OLS Estimator.
//------------------------------------------------------------------------------

st_view(y=.,.,"y")
st_view(x=.,.,("x1","x2","x3","constant"))

beta_hat=invsym(x'x)*(x'y)
beta_hat

//------------------------------------------------------------------------------
// 1.1.d. SST, SSE, SSR.
//------------------------------------------------------------------------------

st_view(y=.,.,"y")
st_view(x=.,.,("x1","x2","x3","constant"))

mean_vector_y = J(rows(y),1,mean(y))
mean_vector_y
sst = (y-mean_vector_y)'(y-mean_vector_y)
sst

vector_y_hat = x * beta_hat
vector_y_hat
sse = (vector_y_hat - mean_vector_y)'(vector_y_hat - mean_vector_y)
sse

vector_u_hat = y - vector_y_hat
vector_u_hat
ssr = (vector_u_hat)'(vector_u_hat)
ssr

sst
sse + ssr

//------------------------------------------------------------------------------
// 1.1.e. r2 and adjusted r2.
//------------------------------------------------------------------------------

r_squared = sse/sst
r_squared

adjusted_r_squared = 1 - ((ssr / (rows(x) - cols(x))) / (sst / (rows(x) - 1)))
adjusted_r_squared

//------------------------------------------------------------------------------
// 1.1.f. OLS residuals and fitted values of y.
//------------------------------------------------------------------------------

vector_y_hat

vector_u_hat

//------------------------------------------------------------------------------
// 1.1.g. Sample average of the OLS residuals and sample covariance between 
/// regressors and the residuals.
//------------------------------------------------------------------------------

mean_vector_u_hat = mean(vector_u_hat)
mean_vector_u_hat

cov_xu_hat = x'vector_u_hat
cov_xu_hat

// TODO: The mean of the residuals and the covariance between the residuals and the 
// regressors is virtually zero.

//------------------------------------------------------------------------------
// 1.1.h. Comparison betweenthe average fitted value of y and the average value
/// of y.
//------------------------------------------------------------------------------

mean_y = mean(y)

mean_vector_y_hat = mean(vector_y_hat)

mean_vector_y_hat - mean_y

// A summary of the key values to compare with the OLS regression output.

beta_hat
sst
sse
ssr
r_squared
adjusted_r_squared

end

*===============================================================================
* 1.2. OLS regression in STATA and comparison with results from MATA.
*===============================================================================

reg y x*
corr y x*
// The results are exactly the same.

*===============================================================================
* 1.3. 1000 random samples from the joint distribution above.
*===============================================================================

capture program drop random_sample 

program define random_sample, rclass // Define the name of the program.
	drop _all
	scalar drop _all
	matrix drop _all
	set more off
	set obs 100 // Set the number of observations in the sample.
	matrix varcov = (1,0.6,0,0.2\0.6,1,0,0.3\0,0,1,0\0.2,0.3,0,1) //
	matrix  means = (10,15,15,10)'    //Vector of means for the drawnorm().
	drawnorm y x1 x2 x3, cov(varcov) means(means)

	reg y x1 x2 x3 
	// Store regression coefficients in r() in order to return them in the simulation
	return scalar beta_0 = _b[_cons]  //_b[namevariable]
	return scalar beta_1 = _b[x1]
    return scalar beta_2 = _b[x2]
	return scalar beta_3 = _b[x3]
	// End of program 
end

*-------------------------------------------------------------------------------
* 1.3.a. Estimation of parameters from 1000 replications.
*-------------------------------------------------------------------------------

simulate ///
beta_0_hat = r(beta_0) ///
beta_1_hat = r(beta_1)  ///
beta_2_hat = r(beta_2) ///
beta_3_hat = r(beta_3), reps(1000)  ///
saving(0_data\coefficient_estimators, replace) seed(1015): random_sample

*-------------------------------------------------------------------------------
* 1.3.b. Unbiasness of the estimators of beta (against parameter beta).
*-------------------------------------------------------------------------------

egen mean_beta_0_hat = mean(beta_0_hat)
egen mean_beta_1_hat = mean(beta_1_hat)
egen mean_beta_2_hat = mean(beta_2_hat)
egen mean_beta_3_hat = mean(beta_3_hat)

gen diff_b_0 = beta_0_hat - mean_beta_0_hat
gen diff_b_1 = beta_1_hat - mean_beta_1_hat
gen diff_b_2 = beta_2_hat - mean_beta_2_hat
gen diff_b_3 = beta_3_hat - mean_beta_3_hat

foreach var in beta_0_hat beta_1_hat beta_2_hat beta_3_hat{
summarize `var'
}

foreach var in diff_b_0 diff_b_1 diff_b_2 diff_b_3{
summarize `var'
}

*-------------------------------------------------------------------------------
* 1.3.c. beta_hat distribution plots
*-------------------------------------------------------------------------------

foreach var  of varlist beta_0_hat beta_1_hat beta_2_hat beta_3_hat{
	histogram `var', normal name(`var', replace)
	local graphnames `graphnames' `var'
}

graph combine `graphnames'
graph save 3_graphs\betas.gph, replace

//TODO: Comment: They look pretty normal, no pun intended.

*###############################################################################
* 2. Question 2.
*###############################################################################
*===============================================================================
* 2.1. Load dataset in STATA and MATA and calculate the regression model in MATA.
*===============================================================================
clear all
use 0_data\ps1_group15

summarize

gen constant = 1
//TODO: Compute estimators in mata using regression sub-vectors.
mata

st_view(y = .,.,"hourswm")
st_view(x = .,.,("morekids","agem1","agefstm","blackm","hispm","othracem","educm","constant"))

beta = invsym(x'x) * (x'y)
beta

// I cannot compute the partitioned regression in mata because of the inability
// to create an identity matrix of dimension = n (Insuficiente memory).

end

*===============================================================================
* 2.2. Calculate the regression model in STATA and compare it with the one
* obtained in MATA.
*===============================================================================


// The results are the same.

*===============================================================================
* 2.3.a. 
*===============================================================================
cls
//----- First, regress x1 onto the other regressors (without x2), and save the
// residuals.
reg morekids agem1 agefstm blackm hispm othracem  
predict residuals_1, residuals

//----- Second, regress x2 onto the other regressors (without x1), and save the
// residuals.
reg educm agem1 agefstm blackm hispm othracem  
predict residuals_2, residuals

//----- Third, regress y onto both residuals and compare the results with the
// full model.
reg hourswm residuals_1 residuals_2

reg hourswm morekids educm agem1 agefstm blackm hispm othracem 



//TODO: Activate the log.



































