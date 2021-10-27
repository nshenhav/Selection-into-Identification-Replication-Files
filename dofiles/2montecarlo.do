#delimit ;

/* Set globals with file paths */
global logs "";
global data "";
cap log close;
log using $logs/2montecarlo.log , text replace ;

clear all;
set more off ;
set seed 20190622 ;
set matsize 1000 ;

prog def runme ;

local t1 = subinstr("${S_DATE}"," ","",.) ;
local savefile "$data/mc_results" ;

/* set up monte carlo parameters */
local nummcreps = 1500 ;
global var_factor = 0.01 ;
cap erase "`savefile'.dta"  ;

/* panel A */
run_one_mc `nummcreps' 0.08 dgp1 ;		
cap append using "`savefile.dta'" ;		
qui save "`savefile'.dta" , replace ;

/* panel B */
run_one_mc `nummcreps' 0.08 dgp2 ;		
cap append using "`savefile.dta'" ;		
qui save "`savefile'.dta" , replace ;

/* panel C */
run_one_mc `nummcreps' 0.08 dgp3 ;		
cap append using "`savefile.dta'" ;		
qui save "`savefile'.dta" , replace ;

/* panel D  */
run_one_mc `nummcreps' 0.08 dgp4 ;		
cap append using "`savefile.dta'" ;		
qui save "`savefile'.dta" , replace ;

/* panel B variant - adding x_f as covariate introduces 
	small bias for reweighted estimates. */
run_one_mc `nummcreps' 0.08 dgp5 ;		
cap append using "`savefile.dta'" ;		
qui save "`savefile'.dta" , replace ;

end ;

prog def run_one_mc ;
	args nummcreps beta_true dgp;

tempfile derivatives ;

/* set up options for the different DGPs */
/* start with baseline options for DGP 1 - linear*/
/* modify options one dgp at a time */
local opt_trt_hetero = "constant" ;		/* alternative = "large_fams"; "xf" */
local opt_fvars = "x_f" ;				/* alternative = " " */

if "`dgp'" == "dgp2" { ;
	local opt_trt_hetero = "large_fams" ;
	local opt_fvars = "large_family" ;
} ;

if "`dgp'" == "dgp3" { ;
	local opt_trt_hetero = "xf" ;
} ;

if "`dgp'" == "dgp4" { ;
	local opt_trt_hetero = "xf" ;
	local opt_fvars = "spl_*" ;
} ;

if "`dgp'" == "dgp5" { ;
	local opt_trt_hetero = "large_fams" ;
	local opt_fvars = "x_f large_family" ;
} ;

/* set up "shell" for data */
qui use $data/psid_for_mcs , replace ;
rename x_f_lpm x_f ;

gen one = 1 ;

_pctile x_f , p(5  20  50  80  95) ;
local p05 = r(r1) ;
local p20 = r(r2) ;
local p50 = r(r3) ;
local p80 = r(r4) ;
local p95 = r(r5) ;
mkspline spl_0 `p05' spl_1 `p20' spl_2 `p50' spl_3 `p80' spl_4 `p95' spl_5 = x_f ; 

reg college x_f ;
predict temporary_py0 ;
drop if temporary_py0 < 0 | temporary_py0 > 1 ;

cap drop famsize ;
egen famsize = sum(one) , by(hhid) ;
cap drop large_family ;
gen large_family = famsize >= 4 ;
cap drop siblings ;
gen siblings = famsize > 1 ;

cap drop sdrhs ;
egen sdrhs = sd(headstart) , by(hhid) ;

gen RHSswitcher = sdrhs > 0 ;
qui replace RHSswitcher = 0 if famsize == 1 ;

foreach vv in temp /*u_fam*/ x x_0 x_1 py py_0 py_1 dpdx dpdx_cont uniform_draw y 
	/*LHSswitcher*/ /*beta_logit_f*/ beta_lpm_f temporary_py1
	{ ;
		qui gen `vv' = . ;
	} ;

qui xtset hhid ;

/* set up MC output file to store results to */
tempfile mcoutput ;
cap postclose myout ;
postfile myout 
	mcrep
	b0t1 b0t2 b0t3 b0t4 
	b1a se1a
	bnew1t1 senew1t1 bnew1t2 senew1t2 bnew1t3 senew1t3 bnew1t4 senew1t4 
	bnewbt1 senewbt1 bnewbt2 senewbt2 bnewbt3 senewbt3 bnewbt4 senewbt4 
	using `mcoutput' ;

/* Construct the within-family variation in treatment - after partialling out covariates and FEs, for Broken-Fixed-Effects fix */
/* we don't want to "blow up" the weights for non switchers ... so they get weight 1. we normalize the weights among switchers to average 1 */
qui { ;
	xtreg headstart , fe ;
	predict resid_headstart , e ;
	gen r2 = resid_headstart * resid_headstart ;
	egen meanr2 = mean(r2) , by(hhid) ;
	replace meanr2 = . if famsize == 1 ;
	gen sqrt_meanr2_inv = 1 / sqrt(meanr2) ;
	gen meanr2_inv = 1 / meanr2 ;
	gen BFEweight = 1 ;
	replace BFEweight = meanr2_inv if RHSswitcher == 1 ;
} ;

/* generate weights for IPW and post-regression reweighting */
/* 6 target populations:  Switchers, Headstart == 1, All, Siblings, family HS == 1, siblings & family HS == 1 */
qui gen target = . ;
qui gen groupof4 = . ;
gen phat1 = 0 ;
gen phat2 = 0 ;
gen phat3 = 0 ;
gen phat4 = 0 ;

local target1 "RHSswitcher == 1" ;
local target2 "siblings == 1" ;
local target3 "headstart ~= ." ; // this is meant to capture "everybody"
local target4 "headstart == 1" ;

qui forvalues tt = 1/4 { ;
	replace target = `target`tt'' ;
	replace groupof4 = 1 + RHSswitcher + 2 * (target) ;
	/* 1 = not switcher, not target; 2= switcher, not target; 3 = target, not switcher; 4 = both switcher and target */
	cap { ;
	mlogit groupof4 `opt_fvars' , iterate(100) ;
	forvalues gg = 1/4 { ;
		replace phat`gg' = 0 ;
		cap drop tmp ;
		cap { ;
			predict tmp , pr outcome(`gg') ;
			replace phat`gg' = tmp ;
		} ;
	} ;
	} ; // end cap for mlogit loop
	
	qui summ groupof4 ;
	local mymean = r(mean) ;
	local mystdev = r(sd) ;
	if `mystdev' == 0 { ;
			forvalues gg = 1/4 { ;
				replace phat`gg' = (`mymean' == `gg') ;
			} ;
	} ;

	gen weight_target_`tt' = . ;
	replace weight_target_`tt' = (phat3 + phat4) / (phat1 + phat3) if groupof4 == 1 ;
	replace weight_target_`tt' = (phat3 + phat4) / (phat2 + phat4) if groupof4 == 2 ;
	replace weight_target_`tt' = (phat3 + phat4) / (phat1 + phat3) if groupof4 == 3 ;
	replace weight_target_`tt' = (phat3 + phat4) / (phat2 + phat4) if groupof4 == 4 ;

	gen IPW_weight_target_`tt' = weight_target_`tt' * BFEweight ;

	drop tmp ;
	egen tmp = mean(weight_target_`tt') , by(hhid) ;
	replace weight_target_`tt' = tmp ;

	drop tmp ;
	egen tmp = mean(IPW_weight_target_`tt') , by(hhid) ;
	replace IPW_weight_target_`tt' = tmp ;

} ;

/* generate for RHS switchers dummies to be used to estimate family-specific betas */
preserve ;
qui keep if RHSswitcher == 1 ;
keep hhid headstart weight_target_* ;
gen one = 1 ;
egen N_fam = sum(one) , by(hhid) ;
egen N_switcher = sum(one) ;
egen switcherid = group(hhid) ;
bysort switcherid: keep if _n == 1 ;

/* weights to be used for averaging up family-specific betas, "post regression weights" */
forvalues tt = 1/4 { ;
	gen fam_avgALT_weight`tt' = (N_fam / N_switcher) * weight_target_`tt' ;  /* alternative weight uses the pscore ratio */
} ;

qui summ switcherid ;
local maxn = r(max) ;
sort hhid headstart ;
tempfile RHSswitchervars ;
qui save `RHSswitchervars' ;
restore ;

sort hhid;
qui merge m:1 hhid using `RHSswitchervars' ;

forvalues fam = 1/`maxn' { ;
	gen headstart_fam_`fam' = (switcherid == `fam') * headstart ;
} ;

qui summ headstart ;
local bigN = r(N) ;
qui summ r2 ;
local meanr2_overall = r(mean) ;
bysort hhid: gen first_in_family = _n == 1 ;

/* run the MC */
noi di "DGP = `dgp':  " _continue ;
qui forvalues i = 1 / `nummcreps' { ;
	if (`i' < 5) | (mod(`i',50) == 0) { ;
		noi di "MC rep `i', ${S_DATE} ${S_TIME};  " _continue ;
	} ;
	
	sort hhid birthyear ;
	
	replace beta_lpm_f = 0 ;
		
	if "`opt_trt_hetero'" == "constant"  { ;		/* constant TE in LPM units */
		replace beta_lpm_f = `beta_true' ;
	} ;
	
	if "`opt_trt_hetero'" == "large_fams"  { ;		/* variable (large family) TE in LPM units */
		replace beta_lpm_f = (`beta_true' * 2.4 ) if large_family == 1 ;
	} ;
	if "`opt_trt_hetero'" == "xf"  { ;		/* variable (fn of x_f) TE in LPM units */
		qui sum x_f ;
		local xfmean = r(mean) ;
		local xfsd = r(sd) ;
		replace beta_lpm_f = `beta_true' * (1 - ((x_f - `xfmean') / `xfsd') / 3) ;
	} ;


/* set up underlying py0  */

	qui summ beta_lpm_f ;
	local max_lpm_beta = r(max) ;
	local min_lpm_beta = r(min) ;

	/* temporary_py0 comes from linear LPM model of Y on X's */
	qui replace temporary_py1 = temporary_py0 + beta_lpm_f ;
	qui summ temporary_py1 ;
	local temp_min_1 = r(min) - 0.0001 ;
	local temp_max_1 = r(max) + 0.0001 ;
	local excess_range = (max(`temp_max_1',1)-1) - min(`temp_min_1',0) ;
	qui replace py_0 = (temporary_py0 * (1-`excess_range')) - min(0,`min_lpm_beta') ;
	qui replace py_1 = (temporary_py0 * (1-`excess_range')) - min(0,`min_lpm_beta')  + beta_lpm_f ;

	qui replace py = py_0 * (1-headstart) + py_1 * headstart ;
	qui replace dpdx = py_1 - py_0 ;
	qui replace dpdx_cont = . ;   

	qui { ;
		replace uniform_draw = uniform() ;
        replace y = uniform_draw < py ;

		forvalues tt = 1/4 { ;
			summ dpdx if `target`tt'' == 1 ;
			local mean_dpdx_t`tt' = r(mean) ;
		} ;

	} ;

	/* compute estimation results - both analytic and actual-estimation */
	// Estimation models:
	// Model 1: OLS/LPM on full sample - estimation
	xtreg y headstart , fe cluster(hhid) ;
	local b_1a = _b[headstart] ;
	local se_1a = _se[headstart] ;

	foreach stub in b_new1t se_new1t { ;
		forvalues tt = 1/4 { ;
			local `stub'`tt' = . ;
		} ;
	} ;

	global targetlist = "1 2 3 4 " ;
	// OLS/LPM on full sample - regression re-weighted to target different populations 
	foreach tt in $targetlist { ;

		xtreg y headstart [pw =  IPW_weight_target_`tt'] , fe cluster(hhid) ;
		local b_new1t`tt' = _b[headstart] ;
		local se_new1t`tt' = _se[headstart] ;

	} ;  // end of foreach tt in targetlist	
	
	/* OLS/LPM, estimate family-specific betas, the average these betas up for an overall estimate */
	xtreg y headstart_fam* , fe ;
	matrix mybeta = e(b) ;
	preserve ;
	use `RHSswitchervars' , replace ;
	qui gen beta_family = . ;
	forvalues fam = 1/`maxn' { ;
		matrix subbeta = mybeta[1,"headstart_fam_`fam'".."headstart_fam_`fam'"] ;
		local myb_f = subbeta[1,1] ;
		qui replace beta_family = `myb_f' if switcherid == `fam' ;

	} ;

	forvalues tt = 1/4 { ;
		local b_newb_t`tt' = . ;
		local se_newb_t`tt' = . ;
	} ;

	foreach tt in $targetlist { ;
		
		mean beta_family [pw = fam_avgALT_weight`tt'] ;
		matrix myb = e(b) ;
		matrix myv = e(V) ;
		local beta = myb[1,1] ;
		local var = myv[1,1] ;
		local b_newb_t`tt' = `beta' ;
		local se_newb_t`tt' = sqrt(`var') ;

} ;
	
	restore ;
	post myout 
		(`i')
		(`mean_dpdx_t1') (`mean_dpdx_t2') (`mean_dpdx_t3') (`mean_dpdx_t4')
		(`b_1a') (`se_1a')
		(`b_new1t1') (`se_new1t1') (`b_new1t2') (`se_new1t2') (`b_new1t3') (`se_new1t3') (`b_new1t4') (`se_new1t4')
		(`b_newb_t1') (`se_newb_t1') (`b_newb_t2') (`se_newb_t2') (`b_newb_t3') (`se_newb_t3') (`b_newb_t4') (`se_newb_t4') 
		;
} ;		/* end forvalues i = 1 to nummcreps */

postclose myout ;

use `mcoutput' , replace ;
gen dgp = "`dgp'" ;

end ;

runme ;

prog def runme2 ;

use "$data/mc_results.dta" , replace ;

keep if dgp == "`1'" ;
local dgp = "`1'" ;

local dgp_describe_dgp1 = "Panel A: Fixed LPM beta; no RE; all famsizes; DGP linear in controls" ;
local dgp_describe_dgp2 = "Panel B: Variable (large family) LPM beta; large_fam as control; x_f as control; all famsizes; DGP linear in controls" ;
local dgp_describe_dgp3 = "Panel C: Variable (x_f) LPM beta; x_f as control; all famsizes; DGP linear in controls" ;
local dgp_describe_dgp4 = "Panel D: Variable (x_f) LPM beta; x_f spline as control; all famsizes; DGP linear in controls" ;
local dgp_describe_dgp5 = "Footnote: Variable (large family) LPM beta; large_fam and x_f as control; all famsizes; DGP linear in controls" ;

local collab1 = "True treatment effect" ;
local collab2 = "Basic LPM-FE" ;
local collab3 = "LPM w/ post-reg. wgts" ;

local colvar1 = "b0" ;
local colvar2 = "b1a" ;
local colvar3 = "bnewb" ;

local sevar2 = "se1a" ;
local sevar3 = "senewb" ;


forvalues rr = 1/4 { ;
	gen b1at`rr' = b1a ;
	gen se1at`rr' = se1a ;

	qui summ b0t`rr' ;
	local mean_truth_t`rr' = r(mean) ;
} ;

qui summ b1a ;
local bigN = r(N) ;

qui { ;

forvalues cc = 1/3 { ;
	forvalues rr = 1/4 { ;
		qui summ `colvar`cc''t`rr' ;
		local mean_r`rr'_c`cc' = r(mean) ;
	} ;
} ;

} ; /* end qui */

qui { ;

foreach cc in 2 3 { ;
	forvalues rr = 1/4 { ;
		local newvar = "`colvar`cc''t`rr'" ;
		qui gen error_r`rr'_c`cc' = `newvar' - `mean_truth_t`rr'' ;
		qui summ error_r`rr'_c`cc' ;
		local meanerr_r`rr'_c`cc' = r(mean) * 1000 ;
	} ;
} ;

foreach cc in 2 3 { ;
	forvalues rr = 1/4 { ;

		local p_r`rr'_c`cc' = . ;
		capture { ;
			mean error_r`rr'_c`cc' ;
			qui test error_r`rr'_c`cc' = 0 ;
			local p_r`rr'_c`cc' = r(p) ;
		} ;

		local star_r`rr'_c`cc' = " " ;

		if `p_r`rr'_c`cc'' <= .01 { ;
			local star_r`rr'_c`cc' = "*" ;
		} ;
	} ;
} ;

} ; /* end quietly */

qui { ;

foreach cc in 2 3 { ;
	forvalues rr = 1/4 { ;
		qui gen e2_r`rr'_c`cc' = (error_r`rr'_c`cc')^2 ;
		qui summ e2_r`rr'_c`cc' ;
		local rmse_r`rr'_c`cc' = sqrt(r(mean)) * 1000 ;
	} ;
} ;

} ; // end quietly


/* make calculations for information to present below */

foreach cc in 2 3 { ;
	forvalues rr = 1/4 { ;
		local star_r`rr'_c1 = " " ;
		/* for true value (column 1), use real mean (no error), but relabel the macro so the code will spit it out */
		local meanerr_r`rr'_c1 = `mean_r`rr'_c1' * 1000 ;

		local rmse_r`rr'_c1 =  0 ;    /* "true" MSE, - just so it won't crash out in the loop */
		local mse_r`rr'_c2 = (`rmse_r`rr'_c2' / 1000 )^2 ;    /* OLS MSE, for comparison */
		local mse_r`rr'_c`cc' = (`rmse_r`rr'_c`cc'' / 1000 )^2 ;
		local relative_mse_r`rr'_c`cc' = `mse_r`rr'_c`cc'' / `mse_r`rr'_c2' ;
	} ;
} ;



/* draft info to present */
local c1 = "_column(20)" ;
local c2 = "_column(30)" ;
local c3 = "_column(42)" ;
local c_mse3 = "_column(65)" ;


di ;
di ;
di "DGP = `dgp': `dgp_describe_`1''; Number of MC reps = `bigN'" ;
di ;
di _column(19) "True Beta " _column(31) "BIAS ( * 1000)"  _column(64) "MSE comp. to LPM"  ;
di _column(31) "LPM     Reweighted"  _column(64) "Reweighted" ;


di "Target Population" ;

local rowlab1 = "Switchers" ;
local rowlab2 = "Siblings" ;
local rowlab3 = "All" ;
local rowlab4 = "HS = 1" ;

foreach rr in 1 2 3 4 { ;
	di "`rowlab`rr''" _continue ;
	foreach cc in 1 2 3 { ;
	di `c`cc'' %5.1f `meanerr_r`rr'_c`cc'' "`star_r`rr'_c`cc''    " _continue ;
	} ;

	foreach cc in 3 { ;
		di  `c_mse`cc'' %5.2f `relative_mse_r`rr'_c`cc''  "     " _continue  ;
	} ;

	di ;
} ;
di "For bias: (*) p < 0.01" ;

end ;

* MAIN RESULT, PANEL A ;
runme2 dgp1 ;
* MAIN RESULT, PANEL B ;
runme2 dgp2 ;
* MAIN RESULT, PANEL C ;
runme2 dgp3 ;
* MAIN RESULT, PANEL D  ;
runme2 dgp4 ;
* footnote, variation on PANEL B  ;
runme2 dgp5 ;

log close ;
