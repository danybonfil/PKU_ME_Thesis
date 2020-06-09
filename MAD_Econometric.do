
****************************************
* Template for "MAD Agents: Low stakes, still mistakes?" 
* June 18, 2019 - Version 2
* Daniel Bonfil | PHBS
* 1801214680
****************************************


****************************************
* Organization
*	ANOVAS
*	Correlation and Sensibility
*	Performance Meand and SD
*	Linear Regression Continuous Measures
*	Ordered Probit Categorical Measures
*	Time Dimension Analysis
****************************************


****************************************

*ANOVA Models in Stata

*Run separately, somehow esttab duplicates results on word***
*#############################################################

clear all
set more off
cd "C:\Users\danbo\Desktop\Economic Research\STATA\TablesDocs\Anovas"
pwd

*One way (ESTTAB doesnt work for O-W Anova)
clear all
set more off

import delimited C:\Users\danbo\Desktop\MturkR\BaseF\base.pop.cutoff_EX.csv, numericcols(7) clear

**Antes del KW Test, Anovas de los Base y NB Treatments (Acá vamos a hacer un anova a través de grupos (orden de juegos) solo para ver distribuciones
tabulate grupo, generate(grupod)
tabulate treat, generate(treatd)
tabulate treatgen, generate(treatgend)

oneway fracmax grupo
oneway nback grupo
oneway stroop grupo
oneway iowa grupo
oneway fracmax treatgend1 
*Ellos solo tienen ABS por eso oneway
anova fracmax treatgend1 treatgend4
anova nback treatgend1 treatgend4
anova stroop treatgend1 treatgend4
anova iowa treatgend1 treatgend4

******####KW
*Groups together
clear all
set more off

import delimited C:\Users\danbo\Desktop\MturkR\BaseF\base.pop.cutoff_EX.csv, numericcols(7) clear
* Strings as Factors y hacer dummies de Grupo
encode grupo, gen(grupod)
tabulate grupod

* Strings as Factors y hacer dummies de Treat
encode treat, gen(treatd)
tabulate treatd
encode treatgen, gen(treatgend)
tabulate treatgend

*####Kruskal Wallis Test (Non-parametric Anova) "Dont assume Normal Distribution"####
*By individual treatment
eststo clear
local kruskal "fracmax nback stroop iowa"
local n: word count `kruskal'

local i=1
foreach var of local kruskal{
kwallis `var', by(grupod)
local pvalue: display %05.4f chi2tail(r(df), r(chi2_adj))
mat r`i'=  `pvalue'
local ++i
}

forval j=2/`n'{
local all "`all' \r`j'"
}
mat R= r1`all'
mat rownames R = `kruskal'
mat colnames R= "P-value"
esttab mat(R) using 1.groupgame.rtf, title(P-Values - By Group)
eststo clear

*######## Absolute - By Groups NSI
clear all
set more off

import delimited C:\Users\danbo\Desktop\MturkR\BaseF\Gen\base.pop.cutoff.g1.abs.csv, numericcols(7) clear

* Strings as Factors y hacer dummies de Treat
encode treat, gen(treatd)
tabulate treatd
encode treatgen, gen(treatgend)
tabulate treatgend

*Here as in the paper only checking the anovas for groups in abs and nb.
*####Before going to grouped anovas, check how it works individually: Group 1 means Game 1 is N, 2 is S and 3 is I. Repeat for Groups 2 and 3
kwallis nback, by(treatd)
anova fracmax treatd

*####Kruskal Wallis Test (Non-parametric Anova) "Dont assume Normal Distribution"####
*By individual treatment
eststo clear
local kruskal "fracmax nback stroop iowa"
local n: word count `kruskal'

local i=1
foreach var of local kruskal{
kwallis `var', by(treatd)
local pvalue: display %05.4f chi2tail(r(df), r(chi2_adj))
mat r`i'=  `pvalue'
local ++i
}

forval j=2/`n'{
local all "`all' \r`j'"
}
mat R= r1`all'
mat rownames R = `kruskal'
mat colnames R= "P-value"
esttab mat(R) using 2.g1abs.rtf, title(P-Values - Group 1 - Absolute) 
eststo clear

*######## Absolute - By Groups SIN
clear all
set more off

import delimited C:\Users\danbo\Desktop\MturkR\BaseF\Gen\base.pop.cutoff.g2.abs.csv, numericcols(7) clear

* Strings as Factors y hacer dummies de Treat
encode treat, gen(treatd)
tabulate treatd
encode treatgen, gen(treatgend)
tabulate treatgend

*####Kruskal Wallis Test (Non-parametric Anova) "Dont assume Normal Distribution"####
*By individual treatment
eststo clear
local kruskal "fracmax nback stroop iowa"
local n: word count `kruskal'

local i=1
foreach var of local kruskal{
kwallis `var', by(treatd)
local pvalue: display %05.4f chi2tail(r(df), r(chi2_adj))
mat r`i'=  `pvalue'
local ++i
}

forval j=2/`n'{
local all "`all' \r`j'"
}
mat R= r1`all'
mat rownames R = `kruskal'
mat colnames R= "P-value"
esttab mat(R) using 3.g2abs.rtf, title(P-Values - Group 2 - Absolute) 
eststo clear

*######## Absolute - By Groups INS
clear all
set more off

import delimited C:\Users\danbo\Desktop\MturkR\BaseF\Gen\base.pop.cutoff.g3.abs.csv, numericcols(7) clear

* Strings as Factors y hacer dummies de Treat
encode treat, gen(treatd)
tabulate treatd
encode treatgen, gen(treatgend)
tabulate treatgend

*####Kruskal Wallis Test (Non-parametric Anova) "Dont assume Normal Distribution"####
*By individual treatment
eststo clear
local kruskal "fracmax nback stroop iowa"
local n: word count `kruskal'

local i=1
foreach var of local kruskal{
kwallis `var', by(treatd)
local pvalue: display %05.4f chi2tail(r(df), r(chi2_adj))
mat r`i'=  `pvalue'
local ++i
}

forval j=2/`n'{
local all "`all' \r`j'"
}
mat R= r1`all'
mat rownames R = `kruskal'
mat colnames R= "P-value"
esttab mat(R) using 4.g3abs.rtf, title(P-Values - Group 3 - Absolute) 
eststo clear

*######## No Bonus - By Groups NSI
clear all
set more off
import delimited C:\Users\danbo\Desktop\MturkR\BaseF\Gen\base.pop.cutoff.g1.nb.csv, numericcols(7) clear

* Strings as Factors y hacer dummies de Treat
encode treat, gen(treatd)
tabulate treatd
encode treatgen, gen(treatgend)
tabulate treatgend

*####Kruskal Wallis Test (Non-parametric Anova) "Dont assume Normal Distribution"####
*By individual treatment
eststo clear
local kruskal "fracmax nback stroop iowa"
local n: word count `kruskal'

local i=1
foreach var of local kruskal{
kwallis `var', by(treatd)
local pvalue: display %05.4f chi2tail(r(df), r(chi2_adj))
mat r`i'=  `pvalue'
local ++i
}

forval j=2/`n'{
local all "`all' \r`j'"
}
mat R= r1`all'
mat rownames R = `kruskal'
mat colnames R= "P-value"
esttab mat(R) using 5.g1nb.rtf, title(P-Values - Group 1 - No Bonus) 
eststo clear

*######## No Bonus - By Groups SIN
clear all
set more off

import delimited C:\Users\danbo\Desktop\MturkR\BaseF\Gen\base.pop.cutoff.g2.nb.csv, numericcols(7) clear

* Strings as Factors y hacer dummies de Treat
encode treat, gen(treatd)
tabulate treatd
encode treatgen, gen(treatgend)
tabulate treatgend

*####Kruskal Wallis Test (Non-parametric Anova) "Dont assume Normal Distribution"####
*By individual treatment
eststo clear
local kruskal "fracmax nback stroop iowa"
local n: word count `kruskal'

local i=1
foreach var of local kruskal{
kwallis `var', by(treatd)
local pvalue: display %05.4f chi2tail(r(df), r(chi2_adj))
mat r`i'=  `pvalue'
local ++i
}

forval j=2/`n'{
local all "`all' \r`j'"
}
mat R= r1`all'
mat rownames R = `kruskal'
mat colnames R= "P-value"
esttab mat(R) using 6.g2nb.rtf, title(P-Values - Group 2 - No Bonus) 
eststo clear

*######## No Bonus - By Groups INS
clear all
set more off

import delimited C:\Users\danbo\Desktop\MturkR\BaseF\Gen\base.pop.cutoff.g3.nb.csv, numericcols(7) clear

* Strings as Factors y hacer dummies de Treat
encode treat, gen(treatd)
tabulate treatd
encode treatgen, gen(treatgend)
tabulate treatgend

*####Kruskal Wallis Test (Non-parametric Anova) "Dont assume Normal Distribution"####
*By individual treatment
eststo clear
local kruskal "fracmax nback stroop iowa"
local n: word count `kruskal'

local i=1
foreach var of local kruskal{
kwallis `var', by(treatd)
local pvalue: display %05.4f chi2tail(r(df), r(chi2_adj))
mat r`i'=  `pvalue'
local ++i
}

forval j=2/`n'{
local all "`all' \r`j'"
}
mat R= r1`all'
mat rownames R = `kruskal'
mat colnames R= "P-value"
esttab mat(R) using 7.g3nb.rtf, title(P-Values - Group 3 - No Bonus) 
eststo clear

*#############################################################
* Correl

clear all
set more off
cd "C:\Users\danbo\Desktop\Economic Research\STATA\TablesDocs\CorSens"
pwd

*age.1	education.1	income.1	religion.1	mturkexp.1	mturkappr.1	mturkhits.1	gender.1	OtherGender.1	
*location.1	OtherLocation.1	ethnicity.1	OtherEthnicity.1	political.1	be_aware.1	endquestion.1	secon2.1	subseq2.1
*enjoy.1	satisfied.1	difficulty.1	timing.1	compajust.1	compensation.1

*age	education	income	religion	mturkexp	mturkappr	mturkhits	gender	OtherGender.1	
*location	OtherLocation.1	develop	ethnicity	OtherEthnicity.1	political	secondbonus	subseq2bonus	
*enjoy	satisfied	difficulty	timing	compajust	compensation

*Create Dummy Variables per Cutoff, run them separately and use *esttab* to show all results together
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.csv, numericcols(10) clear
*import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.VARS.csv, numericcols(6) clear

gen bonlevxgender = bonlev*gender
gen bonlevxage = bonlev*age
gen bonlevxedu = bonlev*education
gen bonlevxincome = bonlev*income
gen bonlevxreligion = bonlev*religion
gen bonlevxethn = bonlev*ethnicity
gen bonlevxpolitics = bonlev*political
gen bonlevxlocation = bonlev*location
gen bonlevxcntrydev = bonlev*develop

gen bonlevxmtexp = bonlev*mturkexp
gen bonlevxmtapp = bonlev*mturkappr
gen bonlevxmtehit = bonlev*mturkhits

gen bonlevxenj = bonlev*enjoy
gen bonlevxsat = bonlev*satisfied
gen bonlevxdiff = bonlev*difficulty
gen bonlevxtimi = bonlev*timing
gen bonlevxcompajust = bonlev*compajust
gen bonlevxcomp = bonlev*compensation

*pwcorr type1 age sex edu income religion politics volunteering choice cntrydev csrlvl if country=="USA", star(0.01) print(0.05)
asdoc pwcorr bonlev fracmax nback stroop iowa time tnback tstroop tiowa, star(0.001) print(0.05) nonum replace
asdoc pwcorr bonlev fracmax nback stroop iowa time tnback tstroop tiowa age	education	income	gender	develop religion	mturkexp	mturkappr	mturkhits, star(0.001) print(0.05) nonum replace save(gamesvars.rtf)
asdoc pwcorr bonlev fracmax nback stroop iowa time tnback tstroop tiowa age	education	income	religion	mturkexp	mturkappr	mturkhits	gender	location	develop	ethnicity	political	enjoy	satisfied	difficulty	timing	compajust	compensation, star(0.001) print(0.05) nonum replace save(allvars.rtf)

*sin ethn
eststo clear
eststo: regress fracmax bonlev
eststo: regress fracmax bonlev bonlev*age
eststo: regress fracmax bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp 
eststo: regress fracmax bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation
esttab using 0.fxcrossprod.rtf, p r2 ar2 bic aic title("Fracmax: Influence of demographics on Overall Performance") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")

*sin eth, robust
eststo clear
eststo: regress fracmax bonlev, robust
eststo: regress fracmax bonlev bonlev*age, robust
eststo: regress fracmax bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp, robust
eststo: regress fracmax bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation, robust
esttab using 0.robfxcrossprod.rtf, p r2 ar2 bic aic title("Fracmax: Influence of demographics on Overall Performance") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")

*sin ethn
eststo clear
eststo: regress nback bonlev
eststo: regress nback bonlev bonlev*age
eststo: regress nback bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp 
eststo: regress nback bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation
esttab using 1.nbcrossprod.rtf, p r2 ar2 bic aic title("NBack: Influence of demographics on Correct%") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")
 
*sin eth, robust
eststo clear
eststo: regress nback bonlev, robust
eststo: regress nback bonlev bonlev*age, robust
eststo: regress nback bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp, robust
eststo: regress nback bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation, robust
esttab using 2.robnbcrossprod.rtf, p r2 ar2 bic aic title("NBack: Influence of demographics on Correct%") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")

*sin ethn
eststo clear
eststo: regress stroop bonlev
eststo: regress stroop bonlev bonlev*age
eststo: regress stroop bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp 
eststo: regress stroop bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation
esttab using 3.stcrossprod.rtf, p r2 ar2 bic aic title("Stroop: Influence of demographics on Incorrect%") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")
 
*sin eth, robust
eststo clear
eststo: regress stroop bonlev, robust
eststo: regress stroop bonlev bonlev*age, robust
eststo: regress stroop bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp, robust
eststo: regress stroop bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation, robust
esttab using 4.robstcrossprod.rtf, p r2 ar2 bic aic title("Stroop: Influence of demographics on Incorrect%") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")

*sin ethn
eststo clear
eststo: regress iowa bonlev
eststo: regress iowa bonlev bonlev*age
eststo: regress iowa bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp 
eststo: regress iowa bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation
esttab using 5.iocrossprod.rtf, p r2 ar2 bic aic title("Iowa: Influence of demographics on ROI%") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")

*sin eth, robust
eststo clear
eststo: regress iowa bonlev, robust
eststo: regress iowa bonlev bonlev*age, robust
eststo: regress iowa bonlev bonlevxgender bonlevxage bonlevxedu bonlevxincome bonlevxreligion bonlevxpolitics bonlevxcntrydev bonlevxmtexp bonlevxmtapp bonlevxmtehit bonlevxenj bonlevxsat bonlevxdiff bonlevxtimi bonlevxcompajust bonlevxcomp, robust
eststo: regress iowa bonlev bonlevxgender gender bonlevxage age bonlevxedu education bonlevxincome income bonlevxreligion religion bonlevxpolitics political bonlevxcntrydev develop bonlevxmtexp mturkexp bonlevxmtapp mturkappr bonlevxmtehit mturkhits bonlevxenj enjoy bonlevxsat satisfied bonlevxdiff difficulty bonlevxtimi timing bonlevxcompajust compajust bonlevxcomp compensation, robust
esttab using 6.robiocrossprod.rtf, p r2 ar2 bic aic title("Iowa: Influence of demographics on ROI%") mtitles("Single Parameter" "Simple Regression" "All Crossproducts" "All Variables & Crossproducts") addnote("Note: Crossproducts of demographic variable with Bonus Levels")


*#############################################################

* Mean Fracmax per Game, individual and generic

clear all
set more off
cd "C:\Users\danbo\Desktop\Economic Research\STATA\TablesDocs\MeanPerf"
pwd


*Create Dummy Variables per Cutoff, run them separately and use *esttab* to show all results together
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.ex.csv, numericcols(7) clear
estpost tabstat fracmax, by(treatgen) stat(mean sd n)
esttab using 1EXfrac.cofrac.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Max Earnings/Percentage Cutoff) 
estpost tabstat nback, by(treatgen) stat(mean sd n)
esttab using 2EXnb.conb.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%NBack Performance/Percentage Cutoff)
estpost tabstat stroop, by(treatgen) stat(mean sd n)
esttab using 3EXsp.cosp.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Stroop Performance/Percentage Cutoff)
estpost tabstat iowa, by(treatgen) stat(mean sd n)
esttab using 4EXio.coio.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Iowa Performance/Percentage Cutoff)

*Create Dummy Variables per Cutoff, run them separately and use *esttab* to show all results together
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.vg.csv, numericcols(7) clear
estpost tabstat fracmax, by(treatgen) stat(mean sd n)
esttab using 1VGfrac.cofrac.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Max Earnings/Percentage Cutoff) 
estpost tabstat nback, by(treatgen) stat(mean sd n)
esttab using 2VGnb.conb.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%NBack Performance/Percentage Cutoff)
estpost tabstat stroop, by(treatgen) stat(mean sd n)
esttab using 3VGsp.cosp.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Stroop Performance/Percentage Cutoff)
estpost tabstat iowa, by(treatgen) stat(mean sd n)
esttab using 4VGio.coio.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Iowa Performance/Percentage Cutoff)

*Create Dummy Variables per Cutoff, run them separately and use *esttab* to show all results together
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.go.csv, numericcols(7) clear
estpost tabstat fracmax, by(treatgen) stat(mean sd n)
esttab using 1GOfrac.cofrac.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Max Earnings/Percentage Cutoff) 
estpost tabstat nback, by(treatgen) stat(mean sd n)
esttab using 2GOnb.conb.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%NBack Performance/Percentage Cutoff)
estpost tabstat stroop, by(treatgen) stat(mean sd n)
esttab using 3GOsp.cosp.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Stroop Performance/Percentage Cutoff)
estpost tabstat iowa, by(treatgen) stat(mean sd n)
esttab using 4GOio.coio.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Iowa Performance/Percentage Cutoff)

*Create Dummy Variables per Cutoff, run them separately and use *esttab* to show all results together
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.fa.csv, numericcols(7) clear
estpost tabstat fracmax, by(treatgen) stat(mean sd n)
esttab using 1FAfrac.cofrac.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Max Earnings/Percentage Cutoff) 
estpost tabstat nback, by(treatgen) stat(mean sd n)
esttab using 2FAnb.conb.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%NBack Performance/Percentage Cutoff)
estpost tabstat stroop, by(treatgen) stat(mean sd n)
esttab using 3FAsp.cosp.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Stroop Performance/Percentage Cutoff)
estpost tabstat iowa, by(treatgen) stat(mean sd n)
esttab using 4FAio.coio.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Iowa Performance/Percentage Cutoff)

*Create Dummy Variables per Cutoff, run them separately and use *esttab* to show all results together
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.csv, numericcols(7) clear

*Table 2
estpost tabstat fracmax, by(cutofffracmax) stat(mean sd n)
esttab using 1frac.cofrac.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Max Earnings/Percentage Cutoff) 
estpost tabstat nback, by(cutoffnback) stat(mean sd n)
esttab using 2nb.conb.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%NBack Performance/Percentage Cutoff)
estpost tabstat stroop, by(cutoffstroop) stat(mean sd n)
esttab using 3sp.cosp.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Stroop Performance/Percentage Cutoff)
estpost tabstat iowa, by(cutoffiowa) stat(mean sd n)
esttab using 4io.coio.rtf, cells("mean(fmt(a3)) sd count") nomtitle nonumber title(%Iowa Performance/Percentage Cutoff)

*Table 2
*tabstat fracmax, by(cutofffracmax) stat(mean sd n)
*tabstat nback, by(cutoffnback) stat(mean sd n)
*tabstat stroop, by(cutoffstroop) stat(mean sd n)
*tabstat iowa, by(cutoffiowa) stat(mean sd n)

*Table 3
estpost tabstat fracmax nback stroop iowa, by(treatgen) statistics(mean sd) columns(statistics) listwise
esttab . using 5fnsi.tg.rtf, cells("mean(fmt(a3)) sd") title(% Max Earnings / Performance by game and g.treatment) 
estpost tabstat fracmax nback stroop iowa, by(treat) statistics(mean sd) columns(statistics) listwise
esttab . using 6fnsi.ti.rtf, cells("mean(fmt(a3)) sd") title(% Max Earnings / Performance by game and i.treatment)

*Table 3
*tabstat fracmax nback stroop iowa, by(treatgen) stat(mean sd) long format save
*tabstat fracmax nback stroop iowa, by(treat) stat(mean sd) long format
*tabstat fracmax nback stroop iowa, by(treat) stat(mean) long format
*tabstat fracmax nback stroop iowa, by(treat) stat(sd) long format

*tabstat fracmax nback stroop iowa, by(treatgen) statistics(mean sd n) columns(statistics)
*estpost tabstat fracmax nback stroop iowa, by(treatgen) statistics(mean sd n) columns(statistics) listwise
*esttab . using tabstat_1.rtf, cells("mean(fmt(a3)) sd count")

*#############################################################

* Ordered Probit and Logit Models in Stata

clear all
set more off
cd "C:\Users\danbo\Desktop\Economic Research\STATA\TablesDocs\OrderedProbit"
pwd

*Create Dummy Variables per Cutoff, run them separately and use *esttab* to show all results together
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.csv, numericcols(9) clear
*Create Dummy Variables (For RHS, being able to choose RG use tabulate)
*To choose reference group, do not include that dummy in regression!
tabulate grupo, generate(grupod)
tabulate treat, generate(treatd)
tabulate treatgen, generate(treatgend)
tabulate treatmag, generate(treatmagd)
tabulate treatpay, generate(treatpayd)
tabulate cutofffracmax, generate(cutofffracmax_D)
tabulate cutoffnback, generate(cutoffnback_D)
tabulate cutoffstroop, generate(cutoffstroop_D)
tabulate cutoffiowa, generate(cutoffiowa_D)

*3) #####_MEV
*Create Dummy Variables (For LHS, being able to use all dummies at once use encode)
encode cutofffracmax, gen(cutofffracmaxD)
tabulate cutofffracmaxD
encode cutoffnback, gen(cutoffnbackD)
tabulate cutoffnbackD
encode cutoffstroop, gen(cutoffstroopD)
tabulate cutoffstroopD
encode cutoffiowa, gen(cutoffiowaD)
tabulate cutoffiowaD

*#######Table 4####### 
*The results of a linear regression with robust S.E.s (Left continuous Fracmax, right dummies) Abajo solo prueba con dummy fracmax
*in which the dependent measure was the performance across 
*all six games and the independent variables were dummies for the mid and high incentive levels

*##Individual Treatments (A1 being reference group)
eststo clear
eststo: regress fracmax treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using inda1.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Individual Treatments - AB1 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##Individual Treatments (NB1 being reference group)
eststo clear
eststo: regress fracmax treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using indn1.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Individual Treatments - NB1 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##Generic Treatments (A1 being reference group)
eststo clear
eststo: regress fracmax treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using gena1.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Generic Treatments - ABS Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##Generic Treatments (NB1 being reference group)
eststo clear
eststo: regress fracmax treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using genn1.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Generic Treatments - NB Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##Mag Treatments (AB being reference group)
eststo clear
eststo: regress fracmax treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using magam.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Magnitude Treatments - Absolute Magnitude Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##Mag Treatments (NB1 being reference group)
eststo clear
eststo: regress fracmax treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using magcm.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Magnitude Treatments - Change in Magnitude Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##PAY Treatments (1.¢6 being reference group)
eststo clear
eststo: regress fracmax treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using pay6.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Incentive Levels - ¢6 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##PAY Treatments (4.¢15 being reference group)
eststo clear
eststo: regress fracmax treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using pay15.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Incentive Levels - ¢15 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*PRUEBAS ##PAY Treatments (3.¢12 being reference group)
eststo clear
eststo: regress fracmax treatpayd1 treatpayd2 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatpayd1 treatpayd2 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatpayd1 treatpayd2 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatpayd1 treatpayd2 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Incentive Levels - ¢15 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)
*##PAY Treatments (2.¢9 being reference group)
eststo clear
eststo: regress fracmax treatpayd1 treatpayd3 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback treatpayd1 treatpayd3 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop treatpayd1 treatpayd3 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa treatpayd1 treatpayd3 treatpayd4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Linear Regression: Incentive Levels - ¢15 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Linear Regression includes Robust S.E.s" "Significant Differences marked:") ///
stats(F r2 df_m, labels("F-Statistic" "R-Squared" "DoF")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

regress fracmax nback stroop iowa, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)


*###Table 5### Somewhat contrary to our expectations, however, the pattern of results (Left Dummy Fracmax, Right Dummy Treatment Games Performance in Cutoffs)
*held across tasks differing both in terms of dif?culty and the types 
*of skills they require (see Figure 1 (b)–(d)). 

*The likelihood ratio chi-square of 24.18 with a p-value of 0.0000 tells
*us that our model as a whole is statistically significant, as compared 
*to the null model with no predictors.  
*The pseudo-R-squared of 0.0326 is also given.

*So for fracmax*, we would say that for a one unit increase in fracmax*
*(i.e., going from 0 to 1), we expect a 1.05 increase in the log odds 
*of being in a higher level of cutoff, given all of the other variables 
*in the model are held constant.

*To test for the signifcance 
*of observed differences, we analysed the data separately for each of the 
*games with an ordered probit in which the dependent measure was performance 
*in a game (measured as fraction of maximum possible earnings) and the 
*independent variables were dummies for the two incentive levels low and mid.

*((The six different games might have different cut-points. Therefore, running 
*separate specifcations for each game enables different cut-points for each.))

*OVERALL FRACMAX _exD
*Individual AB1
*FRACMAX 
eststo clear
eststo: oprobit cutofffracmaxD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using opia1.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Individual Treatments - AB1 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*Individual NB1
eststo clear
eststo: oprobit cutofffracmaxD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using opin1.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Individual Treatments - NB1 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*Grupo GEN AB
eststo clear
eststo: oprobit cutofffracmaxD treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatgend2 treatgend3 treatgend4, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using opgab.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Generic Treatments - AB Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*Grupo GEN NB
eststo clear
eststo: oprobit cutofffracmaxD treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatgend1 treatgend2 treatgend3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using opgnb.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Generic Treatments - NB Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*Grupo MAG AB
eststo clear
eststo: oprobit cutofffracmaxD treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatmagd1, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using opamab.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Magnitude Treatments - Absolute Magnitude Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*Grupo MAG NB
eststo clear
eststo: oprobit cutofffracmaxD treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatmagd2, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using opcmab.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Magnitude Treatments - Change in Magnitude Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##PAY Treatments (1.¢6 being reference group)
eststo clear
eststo: oprobit cutofffracmaxD treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatpayd2 treatpayd3 treatpayd4, robust 
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using oppay6.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Incentive Levels - ¢6 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##PAY Treatments (1.¢15 being reference group)
eststo clear
eststo: oprobit cutofffracmaxD treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*NBACK CUTOFF PERFORMANCE
eststo: oprobit cutoffnbackD treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*STROOP CUTOFF PERFORMANCE
eststo: oprobit cutoffstroopD treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*IOWA CUTOFF PERFORMANCE
eststo: oprobit cutoffiowaD treatpayd1 treatpayd2 treatpayd3, robust
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using oppay15.rtf, ///
label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace se ///
title(Ordered Probit: Incentive Levels - ¢15 Reference Group) ///
nonumbers mtitles("Fracmax" "NBack" "Stroop" "Iowa") ///
addnotes("Ordered Probit Model includes Robust S.E.s" "Significant Differences marked:") ///
stats(N k_cat df_m r2_p ll chi2 p, labels("Observations" "Categories" "DoF" "Pseudo R-Squared" "Log Likelihood" "Chi2" "Significance")) varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)

*##### MARGINAL EFFECTS AND PREDICTED PROBABILITIES #####
cd "C:\Users\danbo\Desktop\Economic Research\STATA\TablesDocs\OrderedProbit\ME"
pwd
*Marginal Effects (Hay que hacer un probit por cada RHS Dummy para poder sacar los ME de c/u)
*PAY 6
oprobit cutofffracmaxD treatpayd2 treatpayd3 treatpayd4, robust 
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using pay6f.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatpayd2 treatpayd3 treatpayd4, robust 
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using pay6n.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatpayd2 treatpayd3 treatpayd4, robust 
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using pay6s.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatpayd2 treatpayd3 treatpayd4, robust 
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using pay6i.rtf, word replace ctitle(Marginal Effects - 4Fair)
*Pay 15
oprobit cutofffracmaxD treatpayd1 treatpayd2 treatpayd3, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using pay15f.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatpayd1 treatpayd2 treatpayd3, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using pay15n.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatpayd1 treatpayd2 treatpayd3, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using pay15s.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatpayd1 treatpayd2 treatpayd3, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using pay15i.rtf, word replace ctitle(Marginal Effects - 4Fair)

*MAG CM
oprobit cutofffracmaxD treatmagd1, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using cmf.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatmagd1, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using cmn.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatmagd1, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using cms.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatmagd1, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using cmi.rtf, word replace ctitle(Marginal Effects - 4Fair)
*MAG AM
oprobit cutofffracmaxD treatmagd2, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using amf.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatmagd2, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using amn.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatmagd2, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using ams.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatmagd2, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using ami.rtf, word replace ctitle(Marginal Effects - 4Fair)

*ME de GEN ABS
oprobit cutofffracmaxD treatgend2 treatgend3 treatgend4, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using mega1.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatgend2 treatgend3 treatgend4, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using mega2.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatgend2 treatgend3 treatgend4, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using mega3.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatgend2 treatgend3 treatgend4, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using mega4.rtf, word replace ctitle(Marginal Effects - 4Fair)
*ME de GEN NB
oprobit cutofffracmaxD treatgend1 treatgend2 treatgend3, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using megn1.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatgend1 treatgend2 treatgend3, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using megn2.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatgend1 treatgend2 treatgend3, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using megn3.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatgend1 treatgend2 treatgend3, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using megn4.rtf, word replace ctitle(Marginal Effects - 4Fair)

*Individual Treatments
*ME de ABS1
oprobit cutofffracmaxD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using meina1.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using meina2.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using meina3.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using meina4.rtf, word replace ctitle(Marginal Effects - 4Fair)

*ME de NB1
oprobit cutofffracmaxD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using meinn1.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutofffracmaxD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using meinn2.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutofffracmaxD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using meinn3.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutofffracmaxD treatd1 treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using meinn4.rtf, word replace ctitle(Marginal Effects - 4Fair)

*Games Only Absolute
*ME NBACK de ABS1
oprobit cutoffnbackD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using meinbacka1.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutoffnbackD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using meinbacka2.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutoffnbackD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using meinbacka3.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutoffnbackD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using meinbacka4.rtf, word replace ctitle(Marginal Effects - 4Fair)

*ME STROOP de ABS1
oprobit cutoffstroopD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using meinstroopa1.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutoffstroopD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using meinstroopa2.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutoffstroopD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using meinstroopa3.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutoffstroopD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using meinstroopa4.rtf, word replace ctitle(Marginal Effects - 4Fair)

*ME IOWA de ABS1
oprobit cutoffiowaD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(1)) post
outreg2 using meiniowaa1.rtf, word replace ctitle(Marginal Effects - 1Excellent)
oprobit cutoffiowaD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(2)) post
outreg2 using meiniowaa2.rtf, word replace ctitle(Marginal Effects - 2Very Good)
oprobit cutoffiowaD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(3)) post
outreg2 using meiniowaa3.rtf, word replace ctitle(Marginal Effects - 3Good)
oprobit cutoffiowaD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
margins, dydx(*) atmeans predict(outcome(4)) post
outreg2 using meiniowaa4.rtf, word replace ctitle(Marginal Effects - 4Fair)

*#### Predicted Probabilities
* Dependent variable has 3 categories denoted 1,2,3
oprobit cutofffracmaxD treatgend2 treatgend3 treatgend4, robust
* Ordered probit model predicted probabilities
predict p1oprobit, pr outcome(1)
predict p2oprobit, pr outcome(2)
predict p3oprobit, pr outcome(3)
predict p4oprobit, pr outcome(4)
summarize p1oprobit p2oprobit p3oprobit p4oprobit
tabulate cutofffracmaxD

* Dependent variable has 3 categories denoted 1,2,3
oprobit cutoffnbackD treatgend1 treatgend2 treatgend3, robust
* Ordered probit model predicted probabilities
predict p1oprobit, pr outcome(1)
predict p2oprobit, pr outcome(2)
predict p3oprobit, pr outcome(3)
predict p4oprobit, pr outcome(4)
summarize p1oprobit p2oprobit p3oprobit p4oprobit
tabulate cutoffnbackD

* Dependent variable has 3 categories denoted 1,2,3
oprobit cutoffstroopD treatgend1 treatgend2 treatgend3, robust
* Ordered probit model predicted probabilities
predict p1oprobit, pr outcome(1)
predict p2oprobit, pr outcome(2)
predict p3oprobit, pr outcome(3)
predict p4oprobit, pr outcome(4)
summarize p1oprobit p2oprobit p3oprobit p4oprobit
tabulate cutoffstroopD

* Dependent variable has 3 categories denoted 1,2,3
oprobit cutoffiowaD treatgend1 treatgend2 treatgend3, robust
* Ordered probit model predicted probabilities
predict p1oprobit, pr outcome(1)
predict p2oprobit, pr outcome(2)
predict p3oprobit, pr outcome(3)
predict p4oprobit, pr outcome(4)
summarize p1oprobit p2oprobit p3oprobit p4oprobit
tabulate cutoffiowaD

*####
*Notas:

*(Versión Marshall) Prefieron la mía porque encode en Byte, no hace vectores individuales
*tabulate treat, generate(treatd)
*regress fracmax treatd, robust
*regress fracmax treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
*parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)

*Solo probar el cutoff
*regress cutofffracmaxd treatd, robust
*parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*regress cutofffracmaxd 1.treatd 2.treatd 3.treatd 4.treatd 5.treatd 6.treatd 7.treatd 8.treatd 9.treatd 10.treatd 11.treatd 12.treatd 13.treatd 14.treatd, robust
*parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)

*El performance no cambia a través de los Cutoffs, solo el fracmax cambia cuando los modificas
*NBACK CUTOFF PERFORMANCE // lo mismo stroop o iowa
*eststo: oprobit cutoffnbackd_1e2vgD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
*parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*eststo: oprobit cutoffnbackd_1vg2eD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
*parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*eststo: oprobit cutoffnbackd_vgD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
*parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*eststo: oprobit cutoffnbackd_exD treatd2 treatd3 treatd4 treatd5 treatd6 treatd7 treatd8 treatd9 treatd10 treatd11 treatd12 treatd13 treatd14, robust
*parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
*esttab, label compress star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(F r2 df_m df_r) title(NBack Individual Treatments - AB1 Reference Group) nonumbers mtitles("1e2vg" "1vg2e" "Very Good" "Excellent") addnote("")
*eststo clear


*#############################################################

***Quality and Effort Performance Measure por Grupo ***
* Colinearidad con Bono2 y Delta2; realizar mismo analizis que Yin por partes

clear all
set more off
cd "C:\Users\danbo\Desktop\Economic Research\STATA\TablesDocs\Performance\Final"
pwd

*############ PERFORMANCE MEASURES OF TIME
*Grupo 1 ####
* NSI    ####
import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.g1.csv, numericcols(9) clear
encode treat, gen(treatd)
tabulate treatd

eststo: regress iowa stroop iowabonos delta3 nback, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress iowa stroop stroopbonos delta1 nback, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using 1.QNSI_b.rtf, label compress beta star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(F r2 df_m df_r) title(NSI: Quality - Iowa Performance) nonumbers mtitles("Delta 3-2" "Delta 3-1") addnote("") varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)
eststo clear

eststo: regress tiowa tstroop iowabonos delta3 tnback, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress tiowa tstroop stroopbonos delta1 tnback, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using 1.ENSI_b.rtf, label compress beta star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(F r2 df_m df_r) title(NSI: Effort - Iowa Performance) nonumbers mtitles("Delta 3-2" "Delta 3-1") addnote("") varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)
eststo clear

*############
*Grupo 2 ####
* SIN    ####
clear all
set more off

import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.g2.csv, numericcols(9) clear
encode treat, gen(treatd)
tabulate treatd

eststo: regress nback iowa nbackbonos delta3 stroop, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress nback iowa iowabonos delta1 stroop, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using 2.QSIN_b.rtf, label compress beta star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(F r2 df_m df_r) title(SIN: Quality - NBack Performance) nonumbers mtitles("Delta 3-2" "Delta 3-1") addnote("") varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)
eststo clear

eststo: regress tnback tiowa nbackbonos delta3 tstroop, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress tnback tiowa iowabonos delta1 tstroop, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using 2.ESIN_b.rtf, label compress beta star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(F r2 df_m df_r) title(SIN: Effort - NBack Performance) nonumbers mtitles("Delta 3-2" "Delta 3-1") addnote("") varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)
eststo clear

*############
*Grupo 3 ####
* INS    ####
clear all
set more off

import delimited C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.g3.csv, numericcols(9) clear
encode treat, gen(treatd)
tabulate treatd

eststo: regress stroop nback stroopbonos delta3 iowa, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress stroop nback nbackbonos delta1 iowa, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using 3.QINS_b.rtf, label compress beta star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(F r2 df_m df_r) title(INS: Quality - Stroop Performance) nonumbers mtitles("Delta 3-2" "Delta 3-1") addnote("") varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)
eststo clear

eststo: regress tstroop tnback stroopbonos delta3 tiowa, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
eststo: regress tstroop tnback nbackbonos delta1 tiowa, beta
parmest, list(,) stars(0.1 0.05 0.01 0.001) format(estimate min* max* %8.2f)
esttab using 3.EINS_b.rtf, label compress beta star(+ 0.10 * 0.05 ** 0.01 *** 0.001) scalars(F r2 df_m df_r) title(INS: Effort - Stroop Performance) nonumbers mtitles("Delta 3-2" "Delta 3-1") addnote("") varlabels(,blist(cut1:_cons "{hline @width}{break}")) eqlabels(none)
eststo clear


*#############################################################

*END

*#############################################################
