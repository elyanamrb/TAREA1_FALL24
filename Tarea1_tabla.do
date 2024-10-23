clear all
cls

cd "C:\Users\artur\Dropbox (Personal)\ITAM-Clases\Microeconometria_aplicada\Tareas\Fall 2024\TAREA1_FALL24"

u philantropy.dta, clear

* Crear variables

* Outcomes
g log_worth = log(finalworth)
g worth_gdppc = (finalworth*1000000) / (gdp_country*1000/population_country)
su finalworth, d
g worth_75 = (finalworth>r(p75))
g worth_gdp = 100*(finalworth / (gdp_country*1000))

* Industrias
tab industries, g(ind)
ren ind17 I_tech
ren ind6 I_fin
ren ind5 I_retail
ren ind11 I_manuf
ren ind7 I_food
g I_other = 1- I_tech- I_fin- I_retail- I_manuf- I_food
g indus = 1*I_tech + 2*I_fin + 3*I_retail + 4*I_manuf + 5*I_food + 6*I_other

* Otras explicativas
g log_gdp = log(gdp_country)
g hi_philant = (philanthropyscore>1)
tab maritalstatus, g(marit)
g M_couple =  marit4 + marit2+ marit3+ marit9
g M_separ = marit1+ marit5+ marit8
ren marit6 M_single
g M_other = 1- M_couple-M_separ-M_single
ren total_tax_rate_country tax_rate
g age2 = age^2
g fem_selfm = female*selfmade

reg finalworth age age2 selfmade female log_gdp tax_rate hi_philant bachelor master doctorate M_*, vce(robust)
outreg2 using tabla_ols.tex, bd(2) sd(4) text replace

reg log_worth age selfmade female log_gdp tax_rate hi_philant bachelor master doctorate M_*, vce(robust)
outreg2 using tabla_ols.tex, bd(2) sd(4) text append

reg worth_gdp age selfmade female log_gdp tax_rate hi_philant bachelor master doctorate M_*, vce(robust)
outreg2 using tabla_ols.tex, bd(2) sd(4) text append

reg worth_75 age age2 selfmade female log_gdp tax_rate hi_philant bachelor master doctorate M_*, vce(robust)
outreg2 using tabla_ols.tex, bd(2) sd(4) text append

reg rank age selfmade female fem_selfm log_gdp tax_rate hi_philant bachelor master doctorate M_*, vce(robust)
outreg2 using tabla_ols.tex, bd(2) sd(4) tex append

