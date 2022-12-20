clear
bcuse card

// single iv
ivregress 2sls lwage (educ = nearc4), vce(robust)

// multiple ivs
ivregress 2sls lwage (educ = nearc2 nearc4), vce(robust)

// check how 2SLS works
reg educ nearc2 nearc4
predict educhat, xb
regress lwage educhat, vce(robust) // SE does not take into account the 1st stage
