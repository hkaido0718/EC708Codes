clear
bcuse card

// Uncomment the line below if condivreg is not installed
// net install st0033_2.pkg 

// 2sls
ivregress 2sls lwage (educ = nearc2), vce(robust)
ivregress 2sls lwage (educ = nearc4), vce(robust)

// robust methods
condivreg lwage (educ = nearc2), ar
condivreg lwage (educ = nearc4), ar 
