// Set globals for paths to output directories
global tables ""
global logs ""
global data ""
global dofiles ""
global figures ""

clear all

set more off

cap log close
log using "$logs/1psidresults.log" , replace

set matsize 11000
set maxvar 20000

// Create global variable list for tables and regressions
global xlist "birthyear female mom_hoh income1 income2 income3 income4 incomeage01 incomeage02 incomeage03 incomeage04 incomeage11 incomeage12 incomeage13 incomeage14 incomeage21 incomeage22 incomeage23 incomeage24 momempage0 momempage1 momempage2 hhsize_age4 eldestchild lbw momed2 daded2 black"

/********************************************************************
		MINOR DATA CLEANING PRIOR TO CREATING P-SCORE WEIGHTS
*****************************************************************/

// First, some general cleaning and variable creation

use "$data/psid_clean.dta", clear

replace earncpi_2325 = . if birthyear > 1985 // Only use earnings for individuals who are 25 by 2010 (the last year of our data is 2011, which reports 2010 earnings).

** Determine whether child was eligible for Head Start for "eligible" target group. Define eligible as having one year of income under 150% of poverty threshold
merge 1:1 pernum id1968 using $data/pov_thresh_for_psidfe.dta // merge on family-specific poverty thresholds created by PSID
drop _m
egen mincinc = rowmin(cincage2_relpov   cincage3_relpov cincage4_relpov cincage5_relpov )
gen hselig = mincinc<=1.5
replace hselig = . if cincage2_relpov ==. & cincage4_relpov ==. & cincage3_relpov ==. & cincage5_relpov ==.
replace hselig = 1 if headstart ==1 // assume head start attendees are eligible
replace hselig = 0 if birthyear<1962

egen maxafdc = rowmax(afdcage2 afdcage3 afdcage4)

replace hsel = 1 if maxafdc ==1 & hsel==0 & birthyear>=1962

** Indicators for groups of interest
gen blk = black
gen wht = white
gen anyrace = 1
gen switch = hs_switcher
gen BFEweight = 1
gen inv_famhs_var = .

** Assign means to remaining missing and generate a flag for doing so
foreach y of varlist $xlist {
		gen missing`y' =.
		egen `y'mean = mean(`y')
		replace missing`y'=1 if `y'==.
		replace `y' = `y'mean if `y'==.
		replace missing`y'=0 if missing`y' ==.
		global xmissing $xmissing missing`y'
	}

replace sib=0 if mom_unique ==.

set more off

** Predict Head Start and completing college using controls other than family size for horse race between family size and predictions
reg headstart birthyear female black momed2 daded2 mom_hoh income1 income2 income3 income4 incomeage0* incomeage1* incomeage2* momempage0 momempage1 momempage2 eldestchild lbw missing*
predict pred_headstart, xb

reg fin_college birthyear female black momed2 daded2 mom_hoh income1 income2 income3 income4 incomeage0* incomeage1* incomeage2* momempage0 momempage1 momempage2 eldestchild lbw missing*
predict pred_college, xb

// Create variables for p-score estimation and reweighting

** Create family-level means of key covariates
foreach cov in birthyear female momed2 income3 income4 {
	bysort mom_unique: egen m`cov' = mean(`cov')
}

** Create "Broken or Fixed Effect Weight" (Gibbons, Suarez, Urbancic, 2018) that undoes variance weighting of FE
foreach race in blk wht {

	*Estimate within-family variance net of regression controls
	qui xi: reg headstart preschool missing* $xlist i.mom_unique if  `race'==1 [aweight=weight2]
	predict hs_r, r
	gen r2 = hs_r^2
	bysort mom_unique: egen famhs_var = mean(r2)
	replace inv_famhs_var = 1 / famhs_var if `race'==1

	replace BFEweight = inv_famhs_var if hs_switcher == 1 & `race'==1 // note: this will be equal to 1 for non-switchers

    drop famhs_var hs_r r2
}

** Indicator for having anyone that is Head Start eligible -- use as potential target for p-score reweighting
bysort mom_unique: egen anyhsel = max(hsel)
replace anyhsel = . if mom_unique ==.

drop hsel*

rename (anyhsel) (hsel)  

** Group 5-or-more sibling families
gen numsibs_simp = numsibs
replace numsibs_simp = 5 if numsibs >=5 & numsibs !=.

// Group 1 + 2 child families in order to include 1-child families in the creation of weights
gen numsibs_group = numsibs_simp
replace numsibs_group = 2 if numsibs_group ==1

// Polynomials in number of kids
gen numsibs_sq = numsibs_simp^2

/****************************************************************
		Create P-Score Weights for Reweighting
****************************************************************/

local icov birthyear female momed2 income3 income4

** Targets that don't include singletons for alternative method where we model singleton treatment effects with extrapolation from siblings
gen hsel_nos = hsel if numsibs_simp !=1
gen hs_nos = headstart if numsibs_simp !=1

foreach sample in sib hsel headstart switch hs_nos hsel_nos { // targets of interest: siblings, head-start-eligible, head-start-participants
foreach race in blk wht {

	* weight for post-regression weighting (no BFE adjustment)
    gen lscwt_`sample'_grp`race' = .

	* weight for in-regression weighting (incl. BFE adjustment)
	gen lscwtbf_`sample'_grp`race' = .

    local predlistgrp ""

	// group of four - target or switcher or both
	cap drop t_sw t_sw*
	gen t_sw = 1 if `sample' ==1 & hs_switcher ==1
	replace t_sw = 2 if `sample' ==1 & hs_switcher !=1
	replace t_sw = 3 if `sample' !=1 & hs_switcher ==1
	replace t_sw = 4 if `sample' !=1 & hs_switcher !=1

	// indicator for which of the "in the target" categories we have
	local cat1and2 0
	local cat1only 0
	local cat2only 0
	local cat1and3 0
	local cat3only 0

** 1. Make list of predicted variables -- code is dynamic in case there aren't four categories, but if you know how many categories there are you can simplify this by setting predlistgrp with the names of the variables you want to predict from mlogit

    // Figure out which types of target we have
	tab t_sw if `race'==1 & (t_sw ==1 | t_sw ==2)
	local rows12 = r(r)
	if `rows12'==2 local cat1and2 = 1
	tab t_sw if t_sw ==1  & `race'==1
	local rows1 = r(r)
	if `rows1' ==1 & `rows12'==1 local cat1only =1
	if `rows1' <1 & `rows12'==1 local cat2only =1

	// Figure out which types of switchers we have
	tab t_sw if `race'==1 & (t_sw ==1 | t_sw ==3)
	local rows13 = r(r)
	if `rows13'==2 local cat1and3 = 1
	if `rows1' <1 & `rows13'==1 local cat3only =1
	tab t_sw if t_sw <=3 & `race'==1
	local rows123 = r(r)

** Create a list of 1-4 variables to be predicted from mlogit (# of variables depends on the number of categories in t_sw)

	* Determine number of categories in t_sw
	tab t_sw if `race'==1, g(t_sw)
	local maxcat = r(r)

	* Local with list of variables to be predicted
	forvalues cat = 1/`maxcat' {
		local predlistgrp `predlistgrp' lsc_`sample'_grp`race'`cat'
	}

** 2. Run mlogit to get predicted probabilities

	mlogit t_sw i.numsibs_group `icov' if `race'==1 , iterate(50)
	predict `predlistgrp' if `race' ==1  & e(sample)==1 // don't predict if dropped from estimation

** Set probability = own status if dropped from estimation
	forvalues cat = 1/`maxcat' {
		replace lsc_`sample'_grp`race'`cat' = 1 if e(sample) == 0 & `race'==1 & t_sw`cat'==1
		replace lsc_`sample'_grp`race'`cat' = 0 if e(sample) == 0 & `race'==1 & t_sw`cat'!=1
	}

** 3. To create weights, set numerator as pr(target) & denominator as pr(switch). Again, this is written to be dynamic. If you know which of the four categories you have in your data, you can shorten this to two lines that define the numerator as the sum of pr(switch + target) and pr(no switch + target) and the denominator as the sum of pr(switch + target) and pr(switch + not target)
	gen numerator = .
	gen denominator = .
	if `cat1and2' ==1 replace numerator = lsc_`sample'_grp`race'1 + lsc_`sample'_grp`race'2 // numerator = pr(target+switch) + pr(target + no switch) if both categories exist
	if `cat1only' ==1 | `cat2only'==1 replace numerator = lsc_`sample'_grp`race'1 // numerator = pr(target+switch) or pr(target + no switch) if only one category exists
	if `cat1and3'==1 & `rows123' ==3 replace  denominator = lsc_`sample'_grp`race'1 + lsc_`sample'_grp`race'3 // denominator = pr(target + switch) + pr(not target + switch) if both categories exist
	if `cat1and3'==1 & `rows123' <3 replace  denominator = lsc_`sample'_grp`race'1 + lsc_`sample'_grp`race'2  // denominator = pr(target + switch) + pr(not target + switch) if both categories exist
	if `cat3only'==1 & `cat2only' ==1 replace denominator =  lsc_`sample'_grp`race'2  // denominator = pr(not target + switch) if only one category exists
	if `cat1and3'==0 & `cat3only'==0 & `rows1' <2 replace  denominator =  lsc_`sample'_grp`race'1 // denominator = pr(target + switch) if only one category exists

	replace lscwt_`sample'_grp`race' = numerator/denominator if `race'==1

    rename numerator pr_`sample'_grp`race'
    rename denominator  pr_sw_`sample'_grp`race'

** 4. Multiply by BFE weight for one-step estimation
	replace lscwtbf_`sample'_grp`race'= lscwt_`sample'_grp`race'*BFEweight

}
}

// Save dataset
tempfile lscwt
save "`lscwt'"

/********************************************************************
		TABLE 1, FIGURE B.1
*****************************************************************/

// Literature review tables graphs
use $data/litreview.dta, replace

// Generate total sample
gen one = 1
la var one "Total"

// More variable configuration
rename binaryoutcome* binaryoutcome
rename binaryindependentvariable binaryindep
rename mainindependentvariables mainindep
rename yearpublished year
la var year "Year Publication"
destring binaryoutcome, replace
encode journal, g(journal_n)
gen binaryoutindep = binaryindep ==1 & binaryoutcome==1
destring siblingsn, g(siblingsn_n) force
rename totaln obs

// Summarize outcomes
gen out_educ = strpos(mainoutcomes, "school")>0 | strpos(mainoutcomes, "college")>0 | strpos(mainoutcomes, "educ")>0 | strpos(mainoutcomes, "Educ")>0
la var out_educ "Schooling/Attainment"
gen out_score =  strpos(mainoutcomes, "score")>0 | strpos(mainoutcomes, "IQ")>0 | strpos(mainoutcomes, "GPA")>0 | strpos(mainoutcomes, "cogn")>0
la var out_score "Test Score"
gen out_birthwt = strpos(mainoutcomes, "weight")>0 | strpos(mainoutcomes, "parenatal")>0
la var out_birthwt "Birth Weight"
gen out_earn = strpos(mainoutcomes, "wage")>0 | strpos(mainoutcomes, "earn")>0 | strpos(mainoutcomes, "Emp")>0 | strpos(mainoutcomes, "Wealth")>0  | strpos(mainoutcomes, "Earn")>0 | strpos(mainoutcomes, "income")>0 | strpos(mainoutcomes, "work")>0
la var out_earn "Employment/Earnings"
gen out_behav = strpos(mainoutcomes, "discipl")>0 | strpos(mainoutcomes, "behav")>0 |  strpos(mainoutcomes, "crim")>0
la var out_behav "Behavioral Issues/Crime"
gen out_health = strpos(mainoutcomes, "height")>0 | strpos(mainoutcomes, "BMI")>0  | strpos(mainoutcomes, "Health")>0  | strpos(mainoutcomes, "health")>0
la var out_health "Health"
gen indep_birthwt = strpos(mainindep, "weight")>0
la var indep_birthwt "Birth Weight"
gen indep_pubprog = strpos(mainindep, "wic")>0 | strpos(mainindep, "AFDC")>0
la var indep_pubprog "Means-Tested Public Program"
gen indep_pre = strpos(mainindep, "preschool")>0 | strpos(mainindep, "Head Start")>0 | strpos(mainindep, "head start")>0 | strpos(mainindep, "school")>0
la var indep_pre "Schooling"
gen indep_death = strpos(mainindep, "death")>0
la var indep_death "Death of Family Member"
gen indep_bomb = strpos(mainindep, "expl")>0 | strpos(mainindep, "radiation")>0
la var indep_bomb "Bombing/Radiation"
gen indep_emp = strpos(mainindep, "hours work")>0 | strpos(mainindep, "unemp")>0 | strpos(mainindep, "Income")>0
la var indep_emp "Employment"
gen indep_ord = strpos(mainindep, "order")>0 | strpos(mainindep, "first-born")>0
la var indep_ord "Birth order"
gen indep_hlth = strpos(mainindep, "asthma")>0 | strpos(mainindep, "Health")>0  | strpos(mainindep, "Height")>0  | strpos(mainindep, "obesity")>0 | strpos(mainindep, "schiz")>0
la var indep_hlth "Health"
gen indep_par = strpos(mainindep, "mother's ed")>0 | strpos(mainindep, "mother has less than high")>0  | strpos(mainindep, "had a child before age 20")>0  | strpos(mainindep, "teenmother")>0
la var indep_par "Parental Traits"
gen test = journal_n
label define sibslabel 2 "2" 3 "3" 4 "4" 5 "5+"

/* Table 1 */
estpost tabstat one binaryindep binaryoutcome, by(journal_n) stat(sum) col(stat)
estimates store simp
esttab simp using "$tables/table1.tex", main(sum %1.0f)  replace  nostar  label unstack nonumber nonote compress  nogaps  nonotes 	///
collabels("AEJ:App AER AER PP JOLE JPubE QJE")
estpost tabstat binaryindep binaryoutcome, by(journal_n)
esttab  using "$tables/table1.tex", replace
eststo clear
foreach var in binaryindep binaryoutcome binaryoutindep one {
	estpost tabstat `var', by(journal_n) stat(sum)
	estimates store `var'
}
// Add to Table 1
esttab  binaryindep binaryoutcome binaryoutindep one using "$tables/table1.tex", cells(sum) ///
varlabels(`e(labels)') varwidth(20) noobs replace mtitle("Binary Indep." "Binary Dep." "Both Binary" "Total") nodepvar nonum collabels("") unstack compress label postfoot("")
eststo clear
estpost tabstat out_educ out_score out_birthwt out_earn out_behav out_health, stat(sum) col(stat)
esttab  using "$tables/table1.tex", cells(sum) append noobs nodepvar nonum collabels("") ///
nomtitle unstack compress label order(out_educ  out_score out_earn out_birthwt out_health out_behav ) ///
varlabels( ,  blist(out_educ "\midrule \it{\underline{Common Dependent Variables}} \\")) prehead("") posthead("") postfoot("")
estpost tabstat indep_birthwt indep_pubprog indep_pre indep_death indep_bomb indep_emp indep_ord indep_hlth indep_par, stat(sum) col(stat)
esttab  using "$tables/table1.tex", cells(sum) append noobs nodepvar nonum collabels("") ///
nomtitle unstack compress label order(indep_pre indep_birthwt  indep_hlth  indep_par indep_emp indep_ord indep_pubprog indep_death indep_bomb)  ///
varlabels( ,  blist(indep_pre "\midrule \it{\underline{Common Independent Variables}} \\") elist(indep_bomb "\\ \midrule \it{\underline{Observations by Sample}} \\")) prehead("") posthead("") postfoot("")
estpost tabstat siblingsn_n obs, stat(p10 p25 p50 p75 p90)
esttab  using "$tables/table1.tex", append cells("siblingsn_n(fmt(%9.0fc)) obs(fmt(%9.0fc))")  collabels("Siblings N" "Total N" ) nonum nomtitle noobs ///
prehead("") posthead("") postfoot("")
estpost tabstat siblingsn_n obs if binaryindep, stat(p10 p25 p50 p75 p90)
estpost tabstat year, stat(min max)
esttab using "$tables/table1.tex", append  nonum nomtitle noobs collabels("" ) postfoot("") cells("min max") prehead("") posthead("") varlabel(year "Year Publication Min/Max")
estpost tabstat balance, stat(sum)
esttab using "$tables/table1.tex", append  nonum nomtitle noobs collabels("" ) cells("sum") prehead("") posthead("") varlabel(balance "Articles with Balance Table if Binary Indep.")

/* Figure B.1 */
// Graph of number of articles per year
sum gscholar_cites, detail
collapse (count) freq= binaryindep (mean) gscholar_cites, by(year)
sum freq if year>=2002
sum gscholar_cites, detail
tab year, sum(gscholar_cites)
gr tw bar freq year, xlabel(2000(4)2016) ytitle("Count") xtitle("Year of Publication") ylabel(0(1)6) scheme(s1mono) color(green) yline(0, lc(green))
list

gr export $figures/figureB1_a_psid.eps, replace
cd $figures
!ps2pdf -dEPSCrop $figures/figureB1_a_psid.eps
!rm -f $figures/figureB1_a_psid.eps
cd $logs

// Add arrow and text
gen arrowx1 = 2004
gen arrowx2 = 2003
gen arrowy1 = 900
gen arrowy2 = 800

// Graph of mean citations per year
gr tw bar gscholar_cites year, xlabel(2000(4)2016) ytitle("Mean Citations") xtitle("Year of Publication")  scheme(s1mono) color(blue) yline(0, lc(blue)) || ///
pcarrow arrowy1 arrowx1 arrowy2 arrowx2, lcolor(black) mcolor(black) text(950 2004.5 "GTC (2002)", size(small)) legend(off)

gr export $figures/figureB1_b_psid.eps, replace
cd $figures
!ps2pdf -dEPSCrop $figures/figureB1_b_psid.eps
!rm -f $figures/figureB1_b_psid.eps

/********************************************************************
                TABLES 2, B.2, B.9
*****************************************************************/

** TABLE B.9: TEST: are these characteristics different within families for children that attended Head Start?
foreach var in lbw disabled mom_hoh cincage1 cincage2 momempage1 momempage2 {

	use "$data/psid_clean", clear
	replace earncpi_2325 = . if birthyear > 1985

	* Exclude `var' from list of controls
	foreach control in birthyear female black momed2 daded2 mom_hoh income1 income2 income3 income4 ///
	incomeage01 incomeage02 incomeage03 incomeage04 incomeage11 incomeage12 incomeage13 incomeage14 ///
	incomeage21 incomeage22 incomeage23 incomeage24 ///
	momempage0 momempage1 momempage2 hhsize_age4 eldestchild lbw {

	local `control' = "`control'"
		if "`var'" == "`control'" local `control' = ""

	}

	if "`var'"	== "cincage2" {
		local incomeage21 = ""
		local incomeage22 = ""
		local incomeage23 = ""
		local incomeage24 = ""
	}

	if "`var'"	== "cincage1" {
		local incomeage11 = ""
		local incomeage12 = ""
		local incomeage13 = ""
		local incomeage14 = ""
	}

	global vlist "`birthyear' `female' `black' `momed2' `daded2' `mom_hoh' `income1' `income2' `income3' `income4'"
	global vlist "$vlist `incomeage01' `incomeage02' `incomeage03' `incomeage04' `incomeage11' `incomeage12' `incomeage13' `incomeage14' "
	global vlist "$vlist `incomeage21' `incomeage22' `incomeage23' `incomeage24' `momempage0' `momempage1' `momempage2' `hhsize_age4' `eldestchild' `lbw'"

	// Assign means to remaining missing and generate a flag for doing so
	foreach y of varlist $vlist {
		gen missing`y' =.
		egen `y'mean = mean(`y')
		replace missing`y'=1 if `y'==.
		replace `y' = `y'mean if `y'==.
		replace missing`y'=0 if missing`y' ==.
	}

	replace sib=0 if mom_unique ==.

	set more off

	// Linear regressions on variable `var'
	estimates clear
	eststo COL1: reg `var' headstart preschool birthyear female black missing*  $vlist [aweight=weight2],  vce(cluster id1968)
	estadd ysumm
	eststo COL2: reg `var' headstart preschool birthyear female black missing*  $vlist ///
		if sib==1  [aweight=weight2], vce(cluster mom_unique)
	estadd ysumm
	eststo COL3: xtivreg2 `var' headstart preschool  missing* $vlist ///
		 if sib==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)
	estadd ysumm
		 	eststo COL4: xtivreg2 `var' headstart preschool  missing* $vlist ///
		if sib==1&black==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)
	estadd ysumm
	eststo COL5: xtivreg2 `var' headstart preschool  missing* $vlist ///
		 if sib==1&black==0 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)
	estadd ysumm
	// Titles of variable `var'
	if "`var'" == "lbw" local title "Low birth weight"
	if "`var'" == "mom_hoh" local title "Single mom at age 4"
	if "`var'" == "momempage1" local title "Mom working at age 1"
	if "`var'" == "momempage2" local title "Mom working at age 2"
	if "`var'" == "cincage1" local title "Family income (age 1) (CPI-adjusted)"
	if "`var'" == "cincage2" local title "Family income (age 2) (CPI-adjusted)"
	if "`var'" == "disabled" local title "Disabled"

    /* Table B.9 */
	if "`var'" == "lbw" ///
	esttab using "${tables}/tableB9_psid.tex", replace keep(headstart preschool) ///
	se ar2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant ///
	stats(N, labels("Observations") fmt( 0)) mtitle("All" "Sibs" "Mom FE" "Blk, FE" "Wht, FE") ///
	label postfoot("") varlabels( , blist(headstart "\midrule \it{\underline{`title'}} \\ "))
	if "`var'" == "mom_hoh" | "`var'" == "cincage1" | "`var'" == "cincage2" | "`var'" =="momempage1" |"`var'" == "disabled" ///
	esttab using "${tables}/tableB9_psid.tex", append keep(headstart preschool) ///
	se ar2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant ///
	stats(N, labels("Observations") fmt( 0)) nomtitle ///
	label prehead("") posthead("") postfoot("") varlabels( , blist(headstart "\midrule \it{\underline{`title'}} \\ "))
	if "`var'" == "momempage2" ///
	esttab using "${tables}/tableB9_psid.tex", append keep(headstart preschool) ///
	se ar2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant ///
	stats(N, labels("Observations") fmt( 0)) nomtitle ///
	label prehead("") posthead("") varlabels( , blist(headstart "\midrule \it{\underline{`title'}} \\ "))

}

** PREP FOR TABLE 2
use "`lscwt'", replace

gen hs = headstart
rename lsc*headstart* lsc*hs*

// Label variables
local racelabel "Whites"
local targetlabel "Head Start"

// Inverse p-score if switcher
gen hsinvpscwht_ifsw = 1/lscwt_hs_grpwht if wht ==1
la var hsinvpscwht_ifsw "Pr(switch)/Pr(Head Start), Whites"
gen hsinvpscblk_ifsw = 1/lscwt_hs_grpblk if blk ==1
la var hsinvpscblk_ifsw "Pr(switch)/Pr(Head Start), Blacks"

// Save file to merge later
keep hsinvpscwht_ifsw hsinvpscblk_ifsw pid mom_unique switch
tempfile invpsc
save "`invpsc'", replace

** TABLE 2 AND TABLE B2

use "$data/psid_clean", clear

preserve

* Indicator for families with any child that attends head start
gen temp = headstart if headstart==1
bysort mom_unique: egen anyheadstart= mean(temp)
replace anyheadstart=. if mom_unique ==.

gen hs_switcher_any = hs_switcher if anyheadstart ==1
gen hs_switcher_hsonly = hs_switcher if headstart ==1

// Modify full xlist - don't include birthyear or splines
global xlist2 "female black momed2 daded2 mom_hoh cinc momempage0 momempage1 momempage2 hhsize_age4 lbw"

// Mean: switcher families
estpost tabstat $xlist2 if hs_switcher ==1 [aweight=weight2] , stat(mean sd semean n count sum) col(stat)
	est store switcher_mean

// Mean: non-switcher families
estpost tabstat $xlist2 if hs_switcher ==0 [aweight=weight2] , stat(mean sd semean n count sum) col(stat)
	est store nonswitcher_mean

// Mean non-switcher families, hs ==1
estpost tabstat $xlist2 if hs_switcher ==0 & anyheadstart==1 [aweight=weight2] , stat(mean sd semean n count sum) col(stat)
	est store nonswitcher_hs_mean

* Test: are characteristics different for switchers and non-switchers?
foreach var of varlist $xlist2 {

	reg `var' hs_switcher [aweight=weight2]
	lincom hs_switcher
	test hs_switcher
	local pv = r(p)

	gen `var'_ttest_sw = `pv'
	rename `var' `var'_old
	rename  `var'_ttest_sw  `var'
}

estpost tabstat $xlist2 if hs_switcher ==1|hs_switcher==0, stat(mean sd semean n count sum) col(stat)
	est store test_switch

* Test: are characteristics different for switchers/non-switchers among families with a Head Start participant?
foreach var of varlist $xlist2 {
	rename `var' `var'_ttest_sw
	rename `var'_old `var'
		reg `var' hs_switcher_any [aweight=weight2]
	lincom hs_switcher_any
	test hs_switcher_any
	local pv = r(p)
		rename `var' `var'_old
	gen `var'_ttest_swany = `pv'
	rename `var'_ttest_swany `var'
}

estpost tabstat $xlist2 if hs_switcher !=. & anyheadstart==1, stat(mean sd semean n count sum) col(stat)
	est store test_switch_any

foreach var of varlist $xlist2 {
	rename `var' `var'_ttest_swany
	rename `var'_old `var'
}

* Test: are characteristics different for switchers and non-switchers conditional on controls?
foreach var of varlist $xlist2 {

	// Exclude dep variable from controls
	foreach control in birthyear female black momed2 daded2 mom_hoh income1 income2 income3 income4 ///
	incomeage01 incomeage02 incomeage03 incomeage04 incomeage11 incomeage12 incomeage13 incomeage14 ///
	incomeage21 incomeage22 incomeage23 incomeage24 ///
	momempage0 momempage1 momempage2 hhsize_age4 eldestchild lbw {

		local `control' = "`control'"
		if "`var'" == "`control'" local `control' = ""
	}

	if "`var'"	== "cinc" {
		local income1 = ""
		local income2 = ""
		local income3 = ""
		local income4 = ""
	}

	global clist "`birthyear' `female' `black' `momed2' `daded2' `mom_hoh' `income1' `income2' `income3' `income4' `incomeage01' `incomeage02' `incomeage03' `incomeage04' `incomeage11' `incomeage12' `incomeage13' `incomeage14' `incomeage21' `incomeage22' `incomeage23' `incomeage24' `momempage0' `momempage1' `momempage2' `hhsize_age4' `eldestchild' `lbw'"

	// Regression of switcher on variable
	reg `var' hs_switcher $clist [aweight=weight2] , vce(cluster id1968)
	local `var'_beta = _b[hs_switcher]
	local `var'_se = _se[hs_switcher]
	local `var'_pval = (2 * ttail(e(df_r), abs(_b[hs_switcher]/_se[hs_switcher])))
	rename `var' `var'_old
	gen `var' = ``var'_beta'

}

estpost tabstat $xlist2 [aweight=weight2] , stat(mean sd semean n count sum) col(stat)
	est store betas

foreach var of varlist $xlist2 {
	rename `var' `var'_beta
	gen `var' = ``var'_pval'
}

estpost tabstat $xlist2 [aweight=weight2] , stat(mean sd semean n count sum) col(stat)
	est store tstats

restore

    /* Table B.2 */
	esttab  switcher_mean nonswitcher_hs_mean test_switch_any ///
		using "$tables/tableB2_psid.tex", replace main(mean %10.3f) nostar ///
			nodepvar label unstack nonote compress  nogaps  ///
            nonotes  mtitles("Switch" "Non-Switch, HS=1" "P-Val. (1)=(3)")
    	merge 1:1 pid mom_unique using "`invpsc'"
	drop _m

    // Partial statistics to add to table of individual covariates
	estpost tabstat hsinvpscwht_ifsw hsinvpscblk_ifsw if switch ==1 [aweight=weight2], stat(mean sd semean n count sum) col(stat)
		estimates store hs_sw_partial

	estpost tabstat hsinvpscwht_ifsw hsinvpscblk_ifsw if switch ==0  [aweight=weight2], stat(mean sd semean n count sum) col(stat)
		estimates store hs_nsw_partial

	estpost tabstat hsinvpscwht_ifsw hsinvpscblk_ifsw if switch ==0 & headstart ==1 [aweight=weight2], stat(mean sd semean n count sum) col(stat)
		estimates store hs_nswhs_partial

    /* Table 2 */
	esttab  switcher_mean nonswitcher_mean test_switch betas tstats ///
		using "$tables/table2_psid.tex", replace main(mean %10.3f) nostar ///
			nodepvar label unstack nonote compress  nogaps  ///
            nonotes  mtitles("Switch" "Non-Switch" "P-Val. (1)=(2)" "Beta Switch" "P-Val (4)")	postfoot("") ///
			varlabels( , blist(female "\midrule \it{\underline{A. Individual Covariates}} \\ "))

    esttab  hs_sw_partial hs_nsw_partial ///
		using "$tables/table2_psid.tex", append main(mean %10.3f) aux(sd %10.2f) nostar ///
			nodepvar label unstack nonote compress   noobs nogaps varlabels( , blist(hsinvpscwht_ifsw "\midrule \it{\underline{B. Inverse Selection into Identification Wts. }} \\ "))  ///
            nonotes  nonum nomtitle	prehead("") posthead("")

drop *invpsc* switch

/********************************************************************
                    TABLES 3, B.1
*****************************************************************/

use $data/psid_clean.dta, clear

// Graph share in preschool, headstart, neither
forvalues child = 1/5 {
	gen child`child' = numsibs==`child'
	la var child`child' "`child' child family"
}

gen child5plus = numsibs>=5
replace child5plus = 0 if numsibs==.

la var child5plus "5+ child family"

gen childunknown = numsibs==.
la var childunknown "Unknown siblings"

// Make inputs to regression weights for each family size using Angrist + Pischke MHE formula: function of (Pr(HS)|family size)*(Variance|family size)

* Calculate within family variance with no controls
qui xi: reg headstart i.mom_unique [aweight=weight2]
predict hs_resid, r
replace hs_resid = 0 if hs_resid <.000001 & hs_resid>= -.000001
gen hs_res_var = .

** Probability of head start for each family size
foreach child in 1 2 3 4 5plus unknown {
	// Probability of head start for each family size
	sum headstart [aweight=weight2] if child`child'==1
	local pr_hs_child`child' = r(mean)
	sum headstart [aweight=weight2] if child`child'==1 & hs_switcher==1
	local pr_hs_child`child'_sw = r(mean)

	sum hs_resid [aweight=weight2] if child`child'==1 & hs_switcher==1
	local hs_res_varchild`child' = r(Var)

	sum child`child' [aweight=weight2]
	local child`child'wt = r(mean)
	sum child`child' [aweight=weight2] if sib==1
	local child`child'sibwt = r(mean)
	sum child`child' [aweight=weight2] if sib==1  & hs_switcher==1
	local child`child'switchwt = r(mean)
}

// Create regression weights for sibling, switcher and all samples
foreach weight in sib switch all {

	if "`weight'" == "sib" local end ""
	if "`weight'" == "switch" local end "_sw"
	if "`weight'" == "all" local end ""
	if "`weight'" == "sib" {
		local denominator = `pr_hs_child2`end''*(1-`pr_hs_child2`end'')*`child2`weight'wt' + ///
					`pr_hs_child3`end''*(1-`pr_hs_child3`end'')*`child3`weight'wt' + ///
					`pr_hs_child4`end''*(1-`pr_hs_child4`end'')*`child4`weight'wt' + ///
					`pr_hs_child5plus`end''*(1-`pr_hs_child5plus`end'')*`child5plus`weight'wt'
	}
	if "`weight'" == "switch" { // for switchers: use within family variance of Head Start
		local denominator = `hs_res_varchild2'*`child2`weight'wt' + ///
					`hs_res_varchild3'*`child3`weight'wt' + ///
					`hs_res_varchild4'*`child4`weight'wt' + ///
					`hs_res_varchild5plus'*`child5plus`weight'wt'
	}
	if "`weight'" == "all" {
		local denominator = `pr_hs_child1'*(1-`pr_hs_child1')*`child1wt' + ///
					`pr_hs_child2'*(1-`pr_hs_child2')*`child2wt' + ///
					`pr_hs_child3'*(1-`pr_hs_child3')*`child3wt' + ///
					`pr_hs_child4'*(1-`pr_hs_child4')*`child4wt' + ///
					`pr_hs_child5plus'*(1-`pr_hs_child5plus')*`child5pluswt' + ///
					`pr_hs_childunknown'*(1-`pr_hs_childunknown')*`childunknownwt'
	}
	if "`weight'" == "all" local weight ""
	foreach child in 1 2 3 4 5plus unknown {
		if "`weight'" == "switch" local apweight_num = `hs_res_varchild`child''*`child`child'`weight'wt'
		if "`weight'" != "switch" local apweight_num = `pr_hs_child`child'`end''*(1-`pr_hs_child`child'`end'')*`child`child'`weight'wt' // for switchers: use within family variance of Head Start
		gen apwt_child`child'`weight' = `apweight_num'/`denominator'
		replace  apwt_child`child'`weight'  = 0 if  apwt_child`child'`weight'  ==.
	}
}

// Generate shares of each type of family in sibling sample and switcher sample
gen childall = .
gen childsw = .
gen childsib = .
la var childall "All Sample"
la var childsib "Siblings Sample"
la var childsw "Switchers Sample"
foreach sib in 1 2 3 4 5plus unknown {
	replace childall = child`sib'
	replace childsib = child`sib' if sib ==1
	replace childsw = child`sib' if sib==1 & hs_devmean!=0
	estpost tabstat childall childsib childsw [aweight=weight2], stat(mean sd semean n count sum) col(stat)
	est store ALL_nchild`sib'
}


/* Table 3 */

*Panel A: Share of each family size by sample
esttab  ALL_nchild1 ALL_nchild2  ALL_nchild3 ALL_nchild4 ALL_nchild5plus ALL_nchildunknown ///
			using "$tables/table3_psid.tex", replace main(mean %10.3f)  nostar ///
			nodepvar label unstack nonumber nonote compress  nogaps  ///
			mgroups("Number of Children in Family:", pattern(1 0 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			nonotes  mtitles("1" "2" "3" "4" "5 +" "Unknown") ///
			varlabels( , blist(childall "\midrule \it{\underline{A. Share of Sample}} \\ ")) noobs postfoot("")

*Panel B: Variance in Head Start by family size and sample
foreach sib in 1 2 3 4 5plus unknown {
	replace childall = headstart if child`sib' ==1
	replace childsib = headstart if sib ==1 & child`sib'==1
	replace childsw = hs_resid if sib==1 & hs_resid!=0 & child`sib'==1
	estpost tabstat childall childsib childsw [aweight=weight2], stat(mean sd semean n count sum v) col(stat)
	est store ALL_nchild`sib'
}
esttab  ALL_nchild1 ALL_nchild2  ALL_nchild3 ALL_nchild4 ALL_nchild5plus ALL_nchildunknown ///
		using "$tables/table3_psid.tex", append main(variance %10.3f) nostar ///
		nodepvar label unstack nonumber nonote compress  nogaps  ///
		nonotes  nomtitle noobs ///
		varlabels( , blist(childall "\midrule \it{\underline{B. Variance in Head Start}} \\ ")) prehead("") posthead("") postfoot("")

*Panel C: Regression weights by family size and sample
foreach sib in 1 2 3 4 5plus unknown {
	replace childall = apwt_child`sib'
	replace childsib = apwt_child`sib'sib if sib ==1
	replace childsw = apwt_child`sib'switch if sib==1 & hs_devmean!=0
	estpost tabstat childall childsib childsw [aweight=weight2], stat(mean sd semean n count sum) col(stat)
	est store ALL_nchild`sib'
}
esttab  ALL_nchild1 ALL_nchild2  ALL_nchild3 ALL_nchild4 ALL_nchild5plus ALL_nchildunknown ///
		using "$tables/table3_psid.tex", append main(mean %10.3f) nostar ///
		nodepvar label unstack nonumber nonote compress  nogaps  ///
		nonotes  nomtitle noobs ///
		varlabels( , blist(childall "\midrule \it{\underline{C. Regression weights}} \\ ")) prehead("") posthead("")

/* Table B.1 */

keep if sib==1
bysort mom_unique: egen share_hs_family = mean(headstart)

// Generate switchers
gen switch = share_hs >0 & share_hs <1
gen all_hs = share_hs==1
gen no_hs = share_hs==0
la var no_hs "No Participants in HS in Family"
la var all_hs "All Participants in HS in Family"
la var switch "Share HS Switchers"

// Generate number of siblings
gen numsibs_simp = numsibs
replace numsibs_simp = 5 if numsibs>5
label define sibslabel 2 "2" 3 "3" 4 "4" 5 "5+"
label values numsibs_simp sibslabel

// Table B.1: Average share of family in head start, all in hs, none in hs by number of children
preserve
collapse share_hs all_hs no_hs switch, by(numsibs_simp mom_unique) //family level observation

label values numsibs_simp sibslabel

estpost tabstat share_hs switch all_hs no_hs , by(numsibs_simp) stat(mean sd semean n count sum) col(stat)
est store shares_all_none_by_sibs

esttab shares_all_none_by_sibs using "$tables/tableB1_psid.tex", replace main(mean %10.3f) nostar ///
			mgroups("Number of Children in Family:", pattern(1 0 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			nodepvar label unstack nonumber nonote compress  nogaps  ///
			nonotes  nomtitle noobs ///
			coeflabels(share_hs_family "Share of Family in Head Start ($\pi$)" all_hs "All Participants in HS in Family" no_hs "No Participants in HS in Family" switch "Share with Switching")
restore

/********************************************************************
					TABLE 6, B.13 - B.15
*****************************************************************/

foreach var in hsgrad college zsc_econ30 zsc_health30 {

	eststo clear

	** Code to reweight family-level betas for Tables 6, B13-B15
	foreach race in wht blk {

        use "`lscwt'", clear

		qui tab mom_unique if hs_switcher==1, g(m)
		local maxid = r(r)

	** Generate interaction between momid and headstart for estimating family-level betas
		forvalues id = 1/`maxid' {
			qui gen hsxm`id' = headstart*m`id'
			replace hsxm`id'=0 if hsxm`id'==.
		}
		gen hsxm_other = headstart*(1-hs_switcher)

	** Store base FE regression coefficients for comparison to weighted betas
		xtivreg2 `var' headstart preschool  missing* $xlist ///
				 if sib==1&`race'==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

		sum `var' [aweight= weight2] if e(sample) ==1 & `race'==1
		local targetmean = r(mean)
		gen wtdb =_b[headstart]
		gen wtdse = _se[headstart]
		local obs = e(N)

		estpost tabstat wtdb wtdse if `race'==1, stat(mean sd semean n count sum) col(stat)
		estimates store sibfe_`race'
		estadd scalar estobs = `obs'
		estadd scalar targetmean = `targetmean'
		drop wtdb wtdse

	** Estimate family-level betas with interacted regressions
		eststo `race': xtivreg2 `var' hsxm* preschool  missing* $xlist ///
			 if `race' ==1 [aweight= weight2], fe i(mom_unique) cluster(mom_unique)

		* Regression sample indicator
		gen sample = e(sample) if hs_switcher ==1 & `race'==1

		* Create variable with each family's share of switcher sample
		bysort mom_unique: egen tfam = total(sample)
		replace tfam = . if hs_switcher==0
		egen totsw = total(sample)
		gen share = tfam/totsw if hs_switcher ==1 & `race'==1

		* For comparison: family-level FFE regression weights = share of sample * conditional variance
		gen condvar = 1/BFEweight if sample==1
		gen sharecondvar = share*condvar if sample==1

		* Save variable with family-specific beta
		gen beta = .
		forvalues id = 1/`maxid' {
			cap qui replace beta = _b[hsxm`id'] if m`id'==1
		}

        if "`race'" == "wht"{
            tempfile withweights_`var'wht
            save "`withweights_`var'wht'"
        }
		
        drop sample


		/***************************************************************
		REWEIGHTING FAMILY-LEVEL BETAS
		****************************************************************/
		
		preserve

			* Placeholder for weighted beta
			gen wtdb = .
			gen wtdse = .

			* Create target means for each sample
			foreach sample in sib hsel switch headstart hsel_nos hs_nos{
				sum `var' [aweight = weight2] if `sample' ==1 & `race'==1
				local targetmean`sample' = r(mean)
			}

			** Collapse weights to family-level. Note: in order to keep all members of family, assign same race to family if anyone has that race
			
			bysort mom_unique: egen new`race' = max(`race')
			replace `race' = new`race'
			drop new`race'

			collapse `var' beta share sharecondvar tfam totsw wtdb wtdse (mean) lsc*wt_* weight2, by(mom_unique  switch hs_switcher `race' )

			* Create FFE weight to compare with the representative weight later
                egen totsharevar = total(sharecondvar) // need to normalize sharecondvar to get FFE weight
                gen ffewt = sharecondvar/totsharevar

			* Aggregate family-level betas for each target using weights
			foreach wt in nowt wt {
            foreach sample in sib hsel headstart hsel_nos hs_nos {

                if "`wt'" == "wt" {
					local spec "grp"
					gen wt_temp = share*lscwt_`sample'_`spec'`race'*weight2 if `race' == 1 // family-level weight = share of sample*pscore weight* sampling weight
				}
				
                if "`wt'" == "nowt" {
					local spec ""
					gen wt_temp = share if `race' == 1 // family-level weight = share of sample -- no pscore wieght  (only fix cond'l variance weighting)
				}
				
                egen totwt = total(wt_temp)
                gen norm_`wt'_temp = wt_temp/totwt  
                gen norm_`wt'_`sample'_`spec'`race' = wt_temp/totwt // need to divide by total weight so weights sum to one

                // Final beta = weighted average of betas
                gen wtdb_temp = beta*(wt_temp/totwt)
                egen `wt'db_`sample'_`spec' = total(wtdb_temp)

                mean beta  [pw = norm_`wt'_temp] // can use this to get conventional s.e.'s, but in the paper we bootstrap s.e.'s
				matrix myv = e(V)
				matrix myb = e(b)
				local beta = myb[1,1]
				local vce = myv[1,1]
				local se = sqrt(`vce')
				gen `wt'dse_`sample'_`spec' =`se' // placeholder s.e. for table, which is manually replaced with bootstrapped s.e.'s

				drop *temp totwt

				la var `wt'db_`sample'_`spec' "Wtd. B: Target = `sample', controls = `spec'"

				** Make tables with weighted betas and placeholder s.e.'s
				replace wtdb = `wt'db_`sample'_`spec'
				replace wtdse = `wt'dse_`sample'_`spec'

				la var wtdb "Beta"
				la var wtdse "S.E."

				estpost tabstat wtdb wtdse if `race'==1, stat(mean sd semean n count sum) col(stat)
				estimates store `wt'db_`sample'_`spec'`race'
				estadd scalar estobs = `obs'
				estadd scalar targetmean = `targetmean`sample''

		}

			// Save weights for Figure 3
            if "`race'" == "wht" & "`var'" == "college" & "`wt'" == "wt"{
                tempfile weights_whtcollege
                save "`weights_whtcollege'"
            }

            if "`wt'" == "nowt" {
                /* Table B.16 */
                if "`var'" == "college" & "`race'" == "wht"  ///
                esttab sibfe_wht nowtdb_hsel_wht nowtdb_headstart_wht nowtdb_sib_wht using ///
                "${tables}/tableB16_psid.tex", replace  ///
                main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
                stats(targetmean, labels("Y Mean in Target") fmt( 3))   ///
                postfoot("") prehead("") posthead("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{A. Some College}} \\ "))  ///
                mtitle("Baseline" "HS Eligible" "Participants" "Siblings"  ) ///
                mgroups("FFE" "Reweighted, Target = ", pattern(1 1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))

                if "`var'" =="zsc_econ30" & "`race'" == "wht" ///
                esttab sibfe_wht nowtdb_hsel_wht nowtdb_headstart_wht nowtdb_sib_wht using ///
                "${tables}/tableB16_psid.tex", append ///
                main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
                stats(targetmean, labels("Y Mean in Target") fmt( 3)) nomtitle ///
                prehead("") posthead("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{B. Economic Sufficiency Index, Age 30}} \\ "))
            }

        }
			drop tfam totsw

		restore

	} // end race

			// RELOAD DATA TO OUTPUT TABLES
			use "`lscwt'", clear

			// Split outcomes - TABLES WITHOUT SECOND EXTRAPOLATION STEP

            /* Table 6 */
			if "`var'" == "college"  ///
			esttab sibfe_wht wtdb_hsel_grpwht wtdb_headstart_grpwht wtdb_sib_grpwht using ///
			"${tables}/table6_psid.tex", replace  ///
			main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
			stats(targetmean, labels("Y Mean in Target") fmt( 3))   ///
			postfoot("") prehead("") posthead("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{A. Some College}} \\ "))  ///
			mtitle("Baseline" "HS Eligible" "Participants"  ) ///
			mgroups("FFE" "Reweighted, Target = ", pattern(1 1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))

			if "`var'" =="zsc_econ30" ///
			esttab sibfe_wht wtdb_hsel_grpwht wtdb_headstart_grpwht wtdb_sib_grpwht using ///
			"${tables}/table6_psid.tex", append ///
			main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
			stats(targetmean, labels("Y Mean in Target") fmt( 3)) nomtitle ///
			 prehead("") posthead("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{B. Economic Sufficiency Index, Age 30}} \\ "))

 			foreach race in wht blk {

            /* Tables B.13 and B.14 */
            if "`race'" == "wht" local name "tableB13"
            if "`race'" == "blk" local name "tableB14"
            if "`race'" == "wht" local panelname "B."
            if "`race'" == "blk" local panelname "D."
			if "`var'" == "hsgrad" ///
			esttab sibfe_`race'  wtdb_hsel_grp`race' wtdb_headstart_grp`race' wtdb_sib_grp`race'  using ///
			"${tables}/`name'_psid.tex", replace  ///
			main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
			stats(targetmean, labels("Y Mean in Target") fmt( 3)) mtitle("Baseline" "HS Eligible" "Participants"  ) ///
			postfoot("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{A. High School Graduation}} \\ ")) ///
			 mgroups("FFE" "Reweighted, Target = ", pattern(1 1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))

            if "`var'" == "college" & "`race'" == "blk" ///
			esttab sibfe_blk wtdb_hsel_grpblk wtdb_headstart_grpblk wtdb_sib_grpblk using ///
			"${tables}/`name'_psid.tex", append  ///
			main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
			stats(targetmean, labels("Y Mean in Target") fmt( 3)) nomtitle ///
			prehead("") posthead("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{B. Some College}} \\ "))

            if "`var'" == "zsc_econ30" & "`race'" == "blk" ///
			esttab sibfe_blk wtdb_hsel_grpblk wtdb_headstart_grpblk wtdb_sib_grpblk using ///
			"${tables}/`name'_psid.tex", append  ///
			main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
			stats(targetmean, labels("Y Mean in Target") fmt( 3)) nomtitle ///
			prehead("") posthead("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{C. Economic Sufficiency Index, Age 30}} \\ "))

			if "`var'" == "zsc_health30" ///
			esttab sibfe_`race' wtdb_hsel_grp`race' wtdb_headstart_grp`race' wtdb_sib_grp`race' using ///
			"${tables}/`name'_psid.tex", append  ///
			main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
			stats(targetmean, labels("Y Mean in Target") fmt( 3)) nomtitle ///
			prehead("") posthead("") varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`panelname' Good Health Index, Age 30}} \\ "))

			}

			
/**************************************************
		FIGURE 1, TABLES 4, B.11 and B17
***************************************************/

   	if "`var'" != "zsc_health30" {
    eststo clear

	// PREP FOR TABLES B11 AND TABLE 4: Generate indicators for number of kids in family and fraction attending

	// Create fraction of siblings who attended headstart over total siblings per family
	bysort mom_unique: egen numhs = total(headstart)
	replace numhs = . if sib==0
	gen frachs = numhs/numsibs

		// Variables for children who attended high school
		forvalues child = 1/4 {
			gen numhs`child' = numhs==`child'
			la var numhs`child' "`child' attended HS"
		}
			gen numhs5plus = numhs>=5
			la var numhs5plus "5+ attended HS"

		// Variables for number of children per family
		forvalues child = 1/4 {
			gen child`child' = numsibs==`child'
			la var child`child' "`child' child family"
		}
		gen child5plus = numsibs>=5
		replace child5plus = 0 if numsibs==.

		// Variables for number of children who attended Head Start
		foreach child in 1 2 3 4 5plus {
			gen hsxnumhs`child' = headstart*numhs`child'
			la var hsxnumhs`child' "Head Start x `child' Attended HS"

			gen hsxchild`child' = headstart*child`child'
			la var hsxchild`child' "Head Start x `child' child family"

			gen psxchild`child' = preschool*child`child'
			la var psxchild`child' "Preschool x `child' child family"

			gen hsxchild`child'xfrac = hsxchild`child'*frachs
			la var hsxchild`child'xfrac "Head Start x `child' child family x Fraction"
		}

		// Variable for fraction of children who attended Head Start
		gen hsxfrac = headstart*frachs
		la var hsxfrac "Head Start x Fraction attend"

		/* Table B.11 */
		if "`var'" == "college" { // Family FE regression - don't include 1 child families
		eststo white: xtivreg2 `var'   hsxchild2 hsxchild3 hsxchild4 hsxchild5plus ///
								 hsxchild2xfrac hsxchild3xfrac hsxchild4xfrac hsxchild5plusxfrac ///
								 child2 child3 child4 child5plus ///
								psxchild2 psxchild3 psxchild4 psxchild5plus   ///
									frachs ///
									missing* $xlist  if sib==1 & white==1  [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

		eststo white2: xtivreg2 `var'   hsxchild2 hsxchild3 hsxchild4 hsxchild5plus ///
								 hsxfrac ///
								 child2 child3 child4 child5plus ///
								psxchild2 psxchild3 psxchild4 psxchild5plus   ///
									frachs ///
									missing* $xlist  if sib==1 & white==1  [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

        esttab  white white2  using "${tables}/tableB11_psid.tex", replace keep(hsx*) ///
		se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
		stats(N, labels("Observations") fmt(0)) nomtitle  ///
		label
        }

drop hsxchild* psxchild* child*

// PREP FOR TABLE 4: CALCULATE EFFECTIVE NUMBER OF OBS. AND STORE AS LOCALS
preserve
	keep if white == 1
	// For comparison, generate total switchers by race
		gen temp = hs_switcher if sib==1
		egen totswitcherwhite = total(temp)
		bys mom_unique: egen totfswwhite = total(temp)
		drop temp*

	// Generate family means of covariates
		foreach fvar in birthyear female mom_hoh income1 income2 income3 income4 momempage0 momempage1 momempage2 hhsize_age4 eldestchild lbw momed2 daded2 black {
			bys mom_unique: egen fm`fvar' = mean(`fvar') if  sib==1
		}

	// Get variance of Head Start residualized of family means
		reg headstart fm* if sib==1 [aw=weight2]
		predict reswhite if  sib==1 , r
		qui sum reswhite
		gen vcewhite = (r(sd))^2*(r(N)-1)/r(N)

	// Get N_g -1
		bys mom_unique: egen nfamwhite = total(hs_switcher)
		gen dfwhite = nfamwhite -1 if nfamwhite>0

	// Get variance of Head Start within families
		local n = 0
	local n = `n'+1
	qui xi: reg headstart i.mom_unique if sib==1 [aw=weight2]
	predict r`n'white if sib==1, r
	bys mom_unique: egen sd`n'white = sd(r`n'white)
	gen vce`n'white = (sd`n'^2)*(dfwhite/nfamwhite)

	// Effective obs using vce of residuals
	gen vratio`n'white = (vce`n'white/vcewhite) if sib==1
	gen temp_eff_obs`n' = vratio`n'white*df
	bysort mom_unique: gen eff_obs`n'white = temp_eff_obs`n' if _n==1
	egen toteff_obs`n'white= total(eff_obs`n'white)
	bysort mom_unique: gen temp_vcedf`n' = vce`n'*df if _n==1
	drop temp*

	// Effective obs using vce of 2-kid families
	gen vratio`n'white_2fam = (vce`n'white/0.125)
	gen temp_eff_obs`n'white = vratio`n'white_2fam*df
	bysort mom_unique: gen eff_obs`n'white_2fam = temp_eff_obs`n'white if _n==1
	egen toteff_obs`n'white_2fam= total(eff_obs`n'white_2fam)
	drop temp*

	// For comparison, generate total effective obervations by race
	bysort mom_unique: gen temp_totdf`n' = dfwhite if _n==1
	egen totdf`n'white =total(temp_totdf`n')
	drop temp*
	sum toteff_obs1white
	local effobshswhite = r(mean)
	sum toteff_obs1white_2fam
	local effobshswhite_2fam = r(mean)
	sum totswitcherwhite
	local numswitchwhite = r(mean)

restore

// TABLE 4 REGRESSIONS: INTERACTIONS WITH NUMBER OF CHILDREN IN FAMILY

    if "`var'" == "college" {

	// Head Start coefficients by number of children in family
	use "`lscwt'", clear
	eststo clear

	preserve

		keep if white ==1

		// Number of switchers by family size
		forvalues child = 1/4 {
			gen child`child' = numsibs==`child'
			la var child`child' "`child' child family"
		}
		gen child5plus = numsibs>=5
		replace child5plus = 0 if numsibs==.
		la var child5plus "5+ child family"
		gen childunknown = numsibs==.
		la var childunknown "Unknown siblings"
		gen child4plus = numsibs>=4
		replace child4plus = 0 if numsibs==.
		la var child4plus "4+ child family"
		gen childupto3 = numsibs>=1 & numsibs<=3
		replace childupto3 = 0 if numsibs==.
		la var childupto3 "1-3 child family"

		// Interaction
		eststo clear

		// Round to 0 if very close
		replace hs_switcher = 0 if mom_unique ==. // define families with unknown number of children as non-switcher

		foreach child in 1 2 3 4 5plus unknown {

			// Number of siblings in each family for children in Headstart
			gen hsxchild`child' = headstart*child`child'
			la var hsxchild`child' "Head Start x `child' child family"
			gen psxchild`child' = preschool*child`child'
			la var psxchild`child' "Preschool x `child' child family"
			gen hsxchild`child'_sw = hsxchild`child'*(hs_switcher==1)
			la var hsxchild`child'_sw "HS x `child' child family"
			gen hsxchild`child'_nsw = hsxchild`child'*(hs_switcher==0)
			la var hsxchild`child'_nsw "HS x `child' child family"

			// Weights
			sum child`child' [aweight=weight2]
			local child`child'wt = r(mean)
			sum child`child' [aweight=weight2] if sib==1
			local child`child'sibwt = r(mean)
			sum child`child' [aweight=weight2] if sib==1 & hs_devmean !=0
			local child`child'switchwt = r(mean)

			// Probability of head start for each family size
			sum headstart [aweight=weight2] if child`child'==1
			local pr_hs_child`child' = r(mean)
			sum headstart [aweight=weight2] if child`child'==1 & hs_switcher==1
			local pr_hs_child`child'_sw = r(mean)
		}

			eststo: reg `var' hsxchild1 hsxchild2 hsxchild3 hsxchild4 hsxchild5plus hsxchildunknown ///
									psxchild1 psxchild2 psxchild3 psxchild4 psxchild5plus psxchildunknown ///
									child1 child2 child3 child4 child5plus childunknown ///
									birthyear female black missing*  $xlist [aweight=weight2],  vce(cluster id1968)

            // Weighted average of coefficients.  Weights = distribution in whole sample 
			lincom hsxchild1*`child1wt' + hsxchild2*`child2wt' + hsxchild3*`child3wt' + hsxchild4*`child4wt' + hsxchild5plus*`child5pluswt'  + hsxchildunknown*`childunknownwt'
			local coeffall = r(estimate)
			local coeffall_se = r(se)

			// Weights = distribution among isbs 
			lincom hsxchild1*`child1sibwt' + hsxchild2*`child2sibwt' + hsxchild3*`child3sibwt' + hsxchild4*`child4sibwt' + hsxchild5plus*`child5plussibwt'  + hsxchildunknown*`childunknownsibwt'
			local coeffsib = r(estimate)
			local coeffsib_se = r(se)

			// Weights = distribution among switchers 
			lincom  hsxchild1*`child1switchwt' + hsxchild2*`child2switchwt' + hsxchild3*`child3switchwt' + hsxchild4*`child4switchwt' + hsxchild5plus*`child5plusswitchwt'  + hsxchildunknown*`childunknownswitchwt'
			local coeffswitch = r(estimate)
			local coeffswitch_se = r(se)

			// Create regression weights for 2+ families
			foreach weight in sib switch {
				if "`weight'" == "sib" local end ""
				if "`weight'" == "switch" local end "_sw"
				cap lincom 	hsxchild2*`pr_hs_child2`end''*(1-`pr_hs_child2`end'')*`child2`weight'wt' + ///
					hsxchild3*`pr_hs_child3`end''*(1-`pr_hs_child3`end'')*`child3`weight'wt' + ///
					hsxchild4*`pr_hs_child4`end''*(1-`pr_hs_child4`end'')*`child4`weight'wt' + ///
					hsxchild5plus*`pr_hs_child5plus`end''*(1-`pr_hs_child5plus`end'')*`child5plus`weight'wt'

				cap local numerator = r(estimate)
				cap local denominator = `pr_hs_child2`end''*(1-`pr_hs_child2`end'')*`child2`weight'wt' + ///
									`pr_hs_child3`end''*(1-`pr_hs_child3`end'')*`child3`weight'wt' + ///
									`pr_hs_child4`end''*(1-`pr_hs_child4`end'')*`child4`weight'wt' + ///
									`pr_hs_child5plus`end''*(1-`pr_hs_child5plus`end'')*`child5plus`weight'wt'
				cap local ap_wtd_coeff`weight' = `numerator'/`denominator'
			}

			// Average of cefficients using regression weights for all families
			lincom 	hsxchild1*`pr_hs_child1'*(1-`pr_hs_child1')*`child1wt' + ///
					hsxchild2*`pr_hs_child2'*(1-`pr_hs_child2')*`child2wt' + ///
					hsxchild3*`pr_hs_child3'*(1-`pr_hs_child3')*`child3wt' + ///
					hsxchild4*`pr_hs_child4'*(1-`pr_hs_child4')*`child4wt' + ///
					hsxchild5plus*`pr_hs_child5plus'*(1-`pr_hs_child5plus')*`child5pluswt' + ///
					hsxchildunknown*`pr_hs_childunknown'*(1-`pr_hs_childunknown')*`childunknownwt'
				local numerator = r(estimate)
				local denominator = `pr_hs_child1'*(1-`pr_hs_child1')*`child1wt' + ///
									`pr_hs_child2'*(1-`pr_hs_child2')*`child2wt' + ///
									`pr_hs_child3'*(1-`pr_hs_child3')*`child3wt' + ///
									`pr_hs_child4'*(1-`pr_hs_child4')*`child4wt' + ///
									`pr_hs_child5plus'*(1-`pr_hs_child5plus')*`child5pluswt' + ///
									`pr_hs_childunknown'*(1-`pr_hs_childunknown')*`childunknownwt'
				local ap_wtd_coeff = `numerator'/`denominator'

			 estadd scalar ap_wtd_coeff = `ap_wtd_coeff'
			 estadd scalar ap_wtd_coeffswitch = `ap_wtd_coeffswitch'
			 estadd scalar ap_wtd_coeffsib = `ap_wtd_coeffsib'

			// Family FE regression - don't include 1 child families
			eststo: xtivreg2 `var' hsxchild2 hsxchild3 hsxchild4 hsxchild5plus ///
										psxchild2 psxchild3 psxchild4 psxchild5plus ///
									child1 child2 child3 child4 child5plus childunknown ///
										missing* $xlist  if sib==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

			// Weighted ave using Angrist and Pischke formula in MHE page 75
			foreach weight in sib switch {
				if "`weight'" == "sib" local end ""
				if "`weight'" == "switch" local end "_sw"
				cap lincom 	hsxchild2*`pr_hs_child2`end''*(1-`pr_hs_child2`end'')*`child2`weight'wt' + ///
						hsxchild3*`pr_hs_child3`end''*(1-`pr_hs_child3`end'')*`child3`weight'wt' + ///
						hsxchild4*`pr_hs_child4`end''*(1-`pr_hs_child4`end'')*`child4`weight'wt' + ///
						hsxchild5plus*`pr_hs_child5plus`end''*(1-`pr_hs_child5plus`end'')*`child5plus`weight'wt'
				cap local numerator = r(estimate)
				cap local denominator = `pr_hs_child2`end''*(1-`pr_hs_child2`end'')*`child2`weight'wt' + ///
									`pr_hs_child3`end''*(1-`pr_hs_child3`end'')*`child3`weight'wt' + ///
									`pr_hs_child4`end''*(1-`pr_hs_child4`end'')*`child4`weight'wt' + ///
									`pr_hs_child5plus`end''*(1-`pr_hs_child5plus`end'')*`child5plus`weight'wt'
				cap local ap_wtd_coeff`weight' = `numerator'/`denominator'
				foreach x in 2 3 4 5plus {
					cap local apweight_num = `pr_hs_child`x'`end''*(1-`pr_hs_child`x'`end'')*`child`x'`weight'wt'
					cap local apweight = `apweight_num'/`denominator'
				}
			}

			estadd scalar ap_wtd_coeffsib = `ap_wtd_coeffsib'
            estadd scalar ap_wtd_coeffswitch = `ap_wtd_coeffswitch'
			estadd scalar effobshs_2fam = `effobshswhite_2fam'
			estadd scalar effobshs = `effobshswhite'
			estadd scalar numswitch = `numswitchwhite'

            /* Table 4 */
			
			esttab 
			
			esttab using "${tables}/table4_psid.tex", replace keep(hsx*) ///
			se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
			stats(N ap_wtd_coeff ap_wtd_coeffsib ap_wtd_coeffswitch effobshs effobshs_2fam numswitch, ///
			labels("Observations" "All" "Siblings" "Switchers" "Effective Obs. (CX Indivs.)" "Effective Obs. (Indivs. 2-Person Fams.)" "Switchers") ///
			fmt(0 3 3 3 3 3 3 3 1 0)) mtitle("CX" "FE") ///
			label	coeflabels(hsxchild1 "Head Start x 1 child family" hsxchild2 "Head Start x 2 child family" hsxchild3 "Head Start x 3 child family" ///
			hsxchild4 "Head Start x 4 child family" hsxchild5plus "Head Start x 5+ child family" hsxchildunknown "Head Start x Unknown child family")

			
	// Table B.17: Horse race between family size and other covariates.  Terciles of predicted head start/college distribution
	eststo clear

		// Terciles of predicted head start distribution
		centile pred_headstart, centile(33 66)
		gen pred_headstart1 = pred_headstart < r(c_1)
		gen pred_headstart2 =  pred_headstart>= r(c_1) & pred_headstart < r(c_2)
		gen pred_headstart3 =  pred_headstart>= r(c_2) & pred_headstart !=.
		replace pred_headstart3 = . if pred_headstart==.

		// Terciles of predicted college distribution
		centile pred_college, centile(33 66) // predicted finish college
		gen pred_college1 = pred_college < r(c_1)
		gen pred_college2 =  pred_college >= r(c_1) & pred_college < r(c_2)
		gen pred_college3 =  pred_college>= r(c_2) & pred_college !=.
		replace pred_college3 = . if pred_college==.

		// Variables for interaction
			foreach child in upto3 4plus {

				gen hsxchild`child' = headstart*child`child'
				la var hsxchild`child' "Head Start x `child' child family"

				gen psxchild`child' = preschool*child`child'
				la var psxchild`child' "Preschool x `child' child family"

			}

			foreach pred in headstart college {
			forvalues centile = 1/2 {
				if "`pred'" == "headstart" local label "Head Start"
				if "`pred'" == "college" local label "Finish College"

				gen hsxpred`pred'`centile' = headstart*pred_`pred'`centile'
				la var hsxpred`pred'`centile' "Head Start x Tercile `centile' Predicted `label'"

				gen psxpred`pred'`centile' = preschool*pred_`pred'`centile'
				la var psxpred`pred'`centile' "Preschool x Tercile `centile' Predicted `label'"

			}
			}

					eststo intfam: xi: xtivreg2 `var' headstart hsxchild4plus ///
												psxchildupto3 psxchild4plus ///
												child1 child2 child3 child4 child5plus ///
												if sib==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

					// Interaction b/w predicted head start/ college and headstart
					eststo intpredhs: xi: xtivreg2 `var' hsxpredheadstart1 hsxpredheadstart2 headstart ///
												psxpredheadstart1 psxpredheadstart2 preschool ///
												pred_headstart1 pred_headstart2 pred_headstart3 ///
												if sib==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

					eststo intpredcol: xi: xtivreg2 `var' hsxpredcollege1 hsxpredcollege2 headstart ///
												psxpredcollege1 psxpredcollege2 preschool ///
												pred_college1 pred_college2 pred_college3 ///
												if sib==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

					// Horse race b/w predicted head start/ college and family size
					eststo intfampredhs: xi: xtivreg2 `var' hsxchild4plus ///
												psxchildupto3 psxchild4plus ///
												child1 child2 child3 child4 child5plus ///
												hsxpredheadstart1 hsxpredheadstart2 headstart ///
												psxpredheadstart1 psxpredheadstart2 preschool ///
												pred_headstart1 pred_headstart2 pred_headstart3 ///
												if sib==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

					eststo intfampredcol: xi: xtivreg2 `var' hsxchild4plus ///
												psxchildupto3 psxchild4plus ///
												child1 child2 child3 child4 child5plus ///
												hsxpredcollege1 hsxpredcollege2 headstart ///
												psxpredcollege1 psxpredcollege2 preschool ///
												pred_college1 pred_college2 pred_college3 ///
												if sib==1 [aweight=weight2], fe i(mom_unique) cluster(mom_unique)

                    /* Table B.17 */
					esttab intfam intpredhs intfampredhs using "${tables}/tableB17_psid.tex", replace keep(headstart hsx*) ///
					se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
					stats(N , labels("Observations") fmt(0 )) mtitle("x Fam Size" "x Index" "Horse Race") ///
					label	postfoot("") ///
					varlabels( , blist(headstart "\midrule \it{\underline{Index = Predicted Head Start}} \\ "))

					esttab intfam intpredcol intfampredcol using "${tables}/tableB17_psid.tex", append keep(headstart hsx*) ///
					se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
					stats(N , labels("Observations") fmt(0 )) nomtitle ///
					label	prehead("") posthead("")  ///
					varlabels( , blist(headstart "\midrule \it{\underline{Index = Predicted Finish College}} \\ "))

	restore

	** FIGURE 1

    eststo clear

	// Titles for graph
	local title "Completed Some College"

	keep if sib==1 & black==0

	// Generate residuals of fixed effects regression of head start students with no other siblings in head start
	qui xi: reg `var' i.mom_unique
	predict `var'_devmean, residual

	// Round to 0 if very close
	replace `var'_devmean = 0 if `var'_devmean <.000001 & `var'_devmean>= -.000001

	// Create OLS and Logit samples
	collapse (count) cellsize=mom_unique, by (`var'_devmean	hs_devmean)
	gen ols_sample = (hs_devmean !=0)

	/* Figure 1 */
	gr tw scatter `var'_devmean	hs_devmean [aweight=cellsize], msymbol(Oh) mcolor(gs5) xtitle("Head Start Deviation") ytitle("`title' Deviation") || ///
	scatter `var'_devmean	hs_devmean  [aweight = cellsize], ms(i) mlabel(cellsize) mlabgap(2.5) mlabsize(vsmall) mlabcolor(black) || ///
	lfit `var'_devmean	hs_devmean [aweight=cellsize] if ols_sample==1, lcolor(blue) lpattern(dash)  ///
	legend(order(3 4) label(3 "OLS Fitted")) scheme(s1mono)

	gr export $figures/figure1_psid.eps , replace
	cd $figures
	!ps2pdf -dEPSCrop $figures/figure1_psid.eps
	!rm -f $figures/figure1_psid.eps
	cd $logs
	}
}

/********************************************************************
					TABLES B.3 - B.8
*****************************************************************/

// Create locals with main controls, index outcomes, and inputs to index
local controls_treat headstart preschool black female lbw mom_hoh momhs2 dadhs2  eldestchild  ///
 age momed2 daded2  cinc hhsize_age4

local outcomes hsgrad college nocrime earncpi_2325  ///
zsc_econ30 zsc_econ40 zsc_health30 zsc_health40

local auxoutcomes evafdctanf30 mfoodstamp30 lnmearn30_cpi_nozero  memp30 munemp30 mirp30 fin_college ///
					evafdctanf40 mfoodstamp40  lnmearn40_cpi_nozero memp40 munemp40 mirp40  mownhome40 ///
					neg_mcigsperday30 neg_msrhealthbad30   mbmi30   ///
					neg_mcigsperday40 neg_msrhealthbad40   mbmi40

set more off
estimates clear

foreach v in controls_treat outcomes auxoutcomes {
eststo clear
use "$data/psid_clean", clear

// All sample
estpost tabstat ``v'' [aweight=weight2] , stat(mean sd semean n count) col(stat)
est store ALL_all
// Headstart sample
estpost tabstat ``v''  [aweight=weight2] if headstart==1, stat(mean sd semean n count) col(stat)
est store HDST_all
// Not in Headstart sample
estpost tabstat ``v''  [aweight=weight2] if headstart==0, stat(mean sd semean n count) col(stat)
est store noHDST_all
// Siblings sample
estpost tabstat ``v''  [aweight=weight2] if sib==1, stat(mean sd semean n count) col(stat)
est store SIBL_all

// Tables for summary statistics
if "`v'" != "auxoutcomes" {

    /* Make Tables B.3 and B.4 */
    if "`v'" == "controls_treat" {
        local name "tableB3"
        local name2 "tableB6"
    }
    if "`v'" == "outcomes" {
        local name "tableB4"
        local name2 "tableB7"
    }
    esttab  ALL_all HDST_all noHDST_all SIBL_all using "$tables/`name'_psid.tex", main(mean %10.3f)  aux(sd %10.2f) nostar ///
			nodepvar label unstack nonumber nonote compress  nogaps replace nonotes ///
			mtitles("All" "Head Start" "No Head Start" "Sibling Sample")

    /* Make Tables B.6 and B.7 */
	esttab  ALL_all HDST_all noHDST_all SIBL_all using "$tables/`name2'_psid.tex", main(count) nostar ///
			nodepvar label unstack nonumber nonote compress  nogaps replace nonotes ///
			mtitles("All" "Head Start" "No Head Start" "Sibling Sample")
}
if "`v'" == "auxoutcomes" {

	/* Make Table B.5 */
	esttab  ALL_all HDST_all noHDST_all SIBL_all using "$tables/tableB5_psid.tex", main(mean %10.3f)  aux(sd %10.2f) nostar ///
			nodepvar label unstack nonumber nonote compress  nogaps replace nonotes ///
			mtitles("All" "Head Start" "No Head Start" "Sibling Sample") ///
			varlabels( ,  blist(evafdctanf30 " \it{\underline{Inputs to Economic Sufficiency Index, 30}} \\ " ///
			evafdctanf40 "\midrule \it{\underline{Inputs to Economic Sufficiency Index, 40}} \\ " ///
			neg_mcigsperday30 "\midrule \it{\underline{Inputs to Good Health Index, 30}} \\ " ///
			neg_mcigsperday40 "\midrule \it{\underline{Inputs to Good Health Index, 40}} \\ " ))

    /* Make Table B.8 */
	esttab  ALL_all HDST_all noHDST_all SIBL_all using "$tables/tableB8_psid.tex", main(count) nostar ///
			nodepvar label unstack nonumber nonote compress  nogaps replace nonotes ///
			mtitles("All" "Head Start" "No Head Start" "Sibling Sample")	///
			varlabels( ,  blist(evafdctanf30 " \it{\underline{Inputs to Economic Sufficiency Index, 30}} \\ " ///
			evafdctanf40 "\midrule \it{\underline{Inputs to Economic Sufficiency Index, 40}} \\ " ///
			neg_mcigsperday30 "\midrule \it{\underline{Inputs to Good Health Index, 30}} \\ " ///
			neg_mcigsperday40 "\midrule \it{\underline{Inputs to Good Health Index, 40}} \\ " ))
}
}
}

/********************************************************************
                    TABLE B.10
*****************************************************************/
clear all

foreach var in college zsc_econ30 {

	eststo clear
	use "`withweights_`var'wht'", replace
	gen target = .

	// Test conditional independence assumption
	replace target = hsel
	eststo testidwhthsel: reg beta target [aw=lscwt_hsel_grpwht]
	replace target = headstart
	eststo testidwhths: reg beta target [aw=lscwt_headstart_grpwht]

	if "`var'" == "college" local name "panela"

	if "`var'" == "zsc_econ30" local name "panelb"

    /* Table B.10 */
	esttab testid* using "${tables}/tableB10_`name'_psid.tex", replace ///
			se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
			stats(N , labels("Observations") fmt(0 )) mtitle("Eligible" "Participants")

}

/********************************************************************
		FIGURES 2, B.2
*****************************************************************/

foreach race in all black white {

	use $data/psid_clean.dta, clear
	gen all = 1
	keep if headstart !=. & college !=. & `race'==1

	gen mom_loed = momed2<=12
	gen mom_hied = 1-mom_loed

	forvalues sib = 2/5 {
		sum headstart if numsibs==`sib'
	local phs_`race'`sib' = r(mean)
		sum hs_switcher if numsibs==`sib'
		local psw_`race'`sib' = r(mean)
	}

	foreach momed in loed hied {
	forvalues sib = 2/5 {
		sum headstart if numsibs==`sib' & mom_`momed'==1
	local phs_`momed'`race'`sib' = r(mean)
		sum hs_switcher if numsibs==`sib'  & mom_`momed'==1
		local psw_`momed'`race'`sib' = r(mean)
	}
	}

}

clear

set obs 10000
gen p = runiform()

forvalues child = 1/5 {
	gen switch`child' = 1-(1-p)^`child' - p^`child'
	local text`child' = 1-(1-.5)^`child' - .5^`child' +.03
}

// Generate PSID markers
forvalues sib = 2/5 {
	gen marker`sib' = `psw_all`sib'' if p >= `phs_all`sib''-.0005 & p<= `phs_all`sib''+.0005
	gen markerbl`sib' = `psw_black`sib'' if p >= `phs_black`sib''-.0005 & p<= `phs_black`sib''+.0005
	gen markerwh`sib' = `psw_white`sib'' if p >= `phs_white`sib''-.0005 & p<= `phs_white`sib''+.0005
}
foreach momed in loed hied {
forvalues sib = 2/5 {
	gen marker`momed'`sib' = `psw_`momed'all`sib'' if p >= `phs_`momed'all`sib''-.0005 & p<= `phs_`momed'all`sib''+.0005
	gen marker`momed'bl`sib' = `psw_`momed'black`sib'' if p >= `phs_`momed'black`sib''-.0005 & p<= `phs_`momed'black`sib''+.0005
	gen marker`momed'wh`sib' = `psw_`momed'white`sib'' if p >= `phs_`momed'white`sib''-.0005 & p<= `phs_`momed'white`sib''+.0005
}
}
forvalues sib = 2/5 {
	gen marker_sim`sib' = switch`sib' if p >= (.5-.0002) & p<= (.5+.0002)

}
collapse switch* marker*, by(p)

// Figure B.2
gr tw line switch1 p, lcolor(blue) scheme(s1mono) text(`text1' .5 "1-child", size(small))|| ///
line switch2 p, lcolor(green) text(`text2' .5 "2-child", size(small)) || ///
line switch3 p, lcolor (red) text(`text3' .5 "3-child" , size(small)) || ///
line switch4 p, lcolor (purple) text(`text4' .5 "4-child", size(small)) || ///
line switch5 p, lcolor (orange) text(`text5' .5 "5-child", size(small)) ///
xtitle("Probability of Head Start ({&pi})") ytitle("P(Switching Family)") legend(off)

gr export $figures/figureB2_psid.eps, replace
cd $figures
!ps2pdf -dEPSCrop $figures/figureB2_psid.eps
!rm -f $figures/figureB2_psid.eps

// Figure 2
gr tw ///
scatter markerwh2 p, mcolor(green) msym(o) mcolor(green) text(`psw_white2' `phs_white2' "  2 kids", size(small) placement(e) color(black)) || ///
scatter markerbl2 p, mcolor(green) msym(o) mcolor(green)   text(`psw_black2' `phs_black2' "  2 kids", size(small) placement(e) color(black))  || ///
scatter markerhied2 p, mcolor(green) msym(o) mcolor(green)  text(`psw_hiedall2' `phs_hiedall2' "  2 kids", size(small) placement(e) color(black))  || ///
scatter markerloed2 p, mcolor(green) msym(o) mcolor(green)  text(`psw_loedall2' `phs_loedall2' "  2 kids", size(small) placement(e) color(black))  || ///
scatter markerwh3 p, mcolor(red) msym(d) mcolor(red) text(`psw_white3' `phs_white3' "  3 kids", size(small) placement(e) color(black))  || ///
scatter markerbl3 p, mcolor(red) msym(d) mcolor(red) text(`psw_black3' `phs_black3' "  3 kids", size(small) placement(e) color(black))  || ///
scatter markerhied3 p, mcolor(red) msym(d) mcolor(red)  text(`psw_hiedall3' `phs_hiedall3' "  3 kids", size(small) placement(e) color(black))  || ///
scatter markerloed3 p, mcolor(red) msym(d) mcolor(red)  text(`psw_loedall3' `phs_loedall3' "  3 kids", size(small) placement(e) color(black))  || ///
scatter markerwh4 p, mcolor(purple) msym(s) mcolor(purple) text(`psw_white4' `phs_white4' "  4 kids", size(small) placement(e) color(black))  || ///
scatter markerbl4 p, mcolor(purple) msym(s) mcolor(purple)  text(`psw_black4' `phs_black4' "  4 kids", size(small) placement(e) color(black))  || ///
scatter markerhied4 p, mcolor(purple) msym(s) mcolor(purple) text(`psw_hiedall4' `phs_hiedall4' "  4 kids", size(small) placement(e) color(black))  || ///
scatter markerloed4 p, mcolor(purple) msym(s) mcolor(purple)  text(`psw_loedall4' `phs_loedall4' "  4 kids", size(small) placement(e) color(black))  || ///
scatter markerwh5 p, mcolor(orange) msym(t) mcolor(orange) text(`psw_white5' `phs_white5' "  5 kids", size(small) placement(e) color(black))  || ///
scatter markerbl5 p, mcolor(orange) msym(t) mcolor(orange)  text(`psw_black5' `phs_black5' "  5 kids", size(small) placement(e) color(black))  || ///
scatter markerhied5 p, mcolor(orange) msym(t) mcolor(orange)  text(`psw_hiedall5' `phs_hiedall5' "  (>HS) 5 kids", size(vsmall) placement(e) color(black))  || ///
scatter markerloed5 p, mcolor(orange) msym(t) mcolor(orange) text(`psw_loedall5' `phs_loedall5' "  5 kids", size(small) placement(e) color(black))  ///
legend(off) scheme(s1mono) ylabel(0(0.2)1)  ///
xtitle("P(Head Start)") ytitle("P(Switching Family)") text(.45 .07 "white, or", place(n) size(small)) ///
|| pcarrowi .78 .38 .62 .38 (12) "black" .6 .22 .44 .22 (12) "mom{&le}HS" .39 .07 .21 .07 (12) "mom>HS"  , mcolor(black) lcolor(black)

gr export $figures/figure2a_psid.eps, replace
cd $figures
!ps2pdf -dEPSCrop $figures/figure2a_psid.eps
!rm -f $figures/figure2a_psid.eps

/********************************************************************
		FIGURES 3, B.4, B.5
*****************************************************************/
use "`weights_whtcollege'", clear

// Generate betas
gen betaf2 = beta if tfam==2
gen betaf3 = beta if tfam==3
gen betaf4 = beta if tfam==4
gen betaf5 = beta if tfam>=5 & tfam !=.

sum beta, detail
local median = r(p50)
gen abovemedbeta = beta if beta>=`median'
gen belowmedbeta = beta if beta<`median'

gen smallfam = tfam<=3

gen equalwt_headstartwht = .04-(.04/_N)*_n

// Figure 3
gr tw scatter   ffewt norm_wt_headstart_grpwht if (betaf2 !=. | betaf3 !=.)  & beta>`median', mcolor(orange) msym(Oh) msize(large) || ///
scatter  ffewt norm_wt_headstart_grpwht if (betaf2 !=. | betaf3 !=.) & beta<`median' , mcolor(orange) msym(o) 		 ///
ytitle("FFE Weight") xtitle("Head-Start-Participant-representative weight") scheme(s1mono) ///
legend(order(1 2 ) label(1 "2 or 3 kids, high beta") label(2 "2 or 3 kids, low beta") ) || ///
line equalwt_headstartwht  equalwt_headstartwht , lcolor(black) lp(shortdash) text(.032 .032 "45 degree", placement(se) size(small))

gr export $figures/figure3_a_psid.eps , replace
cd $figures
!ps2pdf -dEPSCrop $figures/figure3_a_psid.eps
!rm -f $figures/figure3_a_psid.eps
cd $logs

gr tw scatter   ffewt norm_wt_headstart_grpwht if (betaf4 !=. | betaf5 !=.)  & beta>`median', mcolor(green) msym(Oh) msize(large) || ///
scatter  ffewt norm_wt_headstart_grpwht if (betaf4 !=. | betaf5 !=.) & beta<`median' , mcolor(green) msym(o) 		 ///
ytitle("FFE Weight") xtitle("Head-Start-Participant-representative weight") scheme(s1mono) ///
legend(order(1 2 ) label(1 "4 or 5 kids, high beta") label(2 "4 or 5 kids, low beta") ) || ///
line equalwt_headstartwht  equalwt_headstartwht , lcolor(black) lp(shortdash) text(.032 .032 "45 degree", placement(se) size(small))

gr export $figures/figure3_b_psid.eps , replace
cd $figures
!ps2pdf -dEPSCrop $figures/figure3_b_psid.eps
!rm -f $figures/figure3_b_psid.eps
cd $logs

// Move some other code to separate do file
cd $dofiles
do convexhull.do

foreach sample in headstart hsel {

	use "`lscwt'", clear

	if "`sample'" == "headstart" local name "figureB4"
	if "`sample'" == "hsel" local name "figureB5"

	drop if switch == . // to make code run, limit to nonmissing px and qx and nonmissing switch
	keep if white == 1 & switch + `sample' > 0

	overlap , switcher(switch) target(`sample') px(pr_sw_`sample'_grpwht) qx(pr_`sample'_grpwht)

	qui cvxhull pr_`sample'_grpwht pr_sw_`sample'_grpwht if switch == 1 , hulls(1) nograph

	/* Figures B.4 and B.5 */
	graph twoway (rarea _cvxh1l _cvxh1r pr_sw_`sample'_grpwht if switch == 1 ///
			, color(orange%20) sort)  ///
		(scatter pr_`sample'_grpwht pr_sw_`sample'_grpwht if `sample' == 1 , color(blue) msize(vsmall)) ///
		, legend(label(1 "Convex Hull for switchers") label(2 "Target observations")) ///
		xtitle("Px = Pr(switcher|x)") ytitle("Qx = Pr(target|x)") scheme(s1mono)

	gr export "$figures/`name'_psid.eps" , replace
	cd $figures
	!ps2pdf -dEPSCrop "$figures/`name'_psid.eps"
	!rm -f "$figures/`name'_psid.eps"
}

/********************************************************************
		FIGURE B.3
*****************************************************************/
use "`lscwt'", clear

keep if pr_headstart_grpwht !=.

preserve

    // Create bins of predicted probability of being in Head Start
	sort numsibs_group pr_headstart_grpwht
	bys numsibs_group: gen pct = _n/_N

	gen grp = 1 if pct <=.2
	replace grp = 2 if pct>0.2 & pct<=.4
	replace grp = 3 if pct>0.4 & pct<=.6
	replace grp = 4 if pct>0.6 & pct<=.8
	replace grp = 5 if pct>0.8 & pct<=.1

	collapse pr_headstart_grpwht switch, by(grp numsibs_group)

	// Figure B.3
	gr tw scatter switch pr_headstart_grpwht if numsibs ==2, mcolor(green) || ///
	scatter switch pr_headstart_grpwht if numsibs ==3, mcolor(red) || ///
	scatter switch pr_headstart_grpwht if numsibs ==4, mcolor(purple) || ///
	scatter switch pr_headstart_grpwht if numsibs ==5, mcolor(orange) ///
	xtitle("Pr(Head Start) from Logit (Quintile Bins)") ytitle("Share Switchers") scheme(s1mono) ///
	legend( label(1 "2 sibs") label(2 "3 sibs") label(3 "4 sibs") label(4 "5+ sibs"))

	gr export $figures/figureB3_psid.eps , replace
	cd $figures
	!ps2pdf -dEPSCrop $figures/figureB3_psid.eps
	!rm -f $figures/figureB3_psid.eps
	cd $logs
restore


/*****************************************************************
FINISH AND CLOSE LOGS
*****************************************************************/

cd $logs

cap log close
