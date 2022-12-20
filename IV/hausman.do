clear 
bcuse card

// run OLS
regress lwage educ exper expersq
estimates store ols

// run 2SLS
ivregress 2sls lwage exper expersq (educ = nearc4 nearc2)
estimates store iv

// Hausman test
hausman iv ols
