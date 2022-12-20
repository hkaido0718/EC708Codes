
clear
bcuse wagepan // wage panel data from Wooldridge's book

xtset nr year  // set indiv and time
xtreg lwage exper expersq, fe vce(robust)

xtreg lwage exper expersq educ, fe vce(robust) 
xtreg lwage exper expersq educ, re

reg lwage exper expersq educ, vce(robust)
reg lwage exper expersq educ, vce(cluster nr)
