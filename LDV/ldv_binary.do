// This following codes are based on https://stats.oarc.ucla.edu/stata/dae/probit-regression/

use https://stats.idre.ucla.edu/stat/stata/dae/binary.dta, clear

// Logit & Probit 
probit admit gre gpa i.rank 
logit admit gre gpa i.rank 

// Calculate the APE and PEAs
probit admit gre gpa i.rank 
margins , dydx(gpa) // APE
margins , dydx(gpa) atmeans // PEA
margins , dydx(gpa) atmeans at(4.rank=1)

// LPM
regress admit gre gpa i.rank 
