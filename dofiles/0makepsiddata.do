
/**********************************************************************************

Notes:
Codes for relation to head:
10 = head
20 = legal wife, 22= female cohabitator lived with head 12 mos or more
33 = stepson or stepdaughter of head (children of wife), 35 = son or daughter of "wife", 30 = son or daughter of head
48 = brother or sister of head's cohabitator, 47= brotherinlaw or sisterinlaw of head (brother/sister of wife)
10 = head
40 = brother or sister of head
60 = grandson or granddaughter of head
65 = great grandson/great-granddaughter of head
74 = cousin of head
75 = cousin of legal wife
70 = nephew/niece of head
71 = nephew/niece of wife
72 = uncle/aunt of head
73 = uncle or aunt of wife
88 = first yr cohab
50 = father or mother of head
57 = mother/fatherinlaw of head
58 = father/mother of heads cohabitator
96/96 = other relation of wife/cohab

** Need to install renvars prior to running

**********************************************************************************/

global logs "C:\Users\naama\Desktop\jhr_replication\dofiles"
global data "C:\Users\naama\Desktop\jhr_replication\data"

set more off
clear all

cap log close
log using $logs/0makepsiddata.log, replace

use $data/psid19682011, clear

drop caseid*
// General flags
gen latino = (id1968>7000)

// Possible ways to define birth cohort for GTC replication
gen birth1 = (birthyr1995<=1977 & birthyr1995>=1966) // GTC say they look at birth cohorts 65-77 (footnote 4), and then say they exclude 64 & 65 cohorts (footnote 7)
gen birth2 = (age1995<=30 & age1995>=18) // GTC say that they are interested in age 18 and older (pg 2002)
gen birth3 = (age1995<=29 & age1995>=18) // Born b/w 66 & 77 implies one cannot be older than 29. 18 + from pg 2002.
gen birth4 = (birthyr1995<=1977 & birthyr1995>=1966 & age1995<=30 & age1995>=18) // combining
gen birth5 = (birthyr1995<=1977 & birthyr1995>=1966 & age1995<=29 & age1995>=18) //combining
gen birth6 = (birthyr1995<=1977 & birthyr1995>=1966 & age1995>=18) // combining 

// Merge on CPI for updating income to 1999$
gen i =1
merge m:1 i using $data/cpi99
drop i
drop _merge

// Make relation coding consistent
forvalues x = 68/82{
qui replace relation19`x'=10 if relation19`x'== 1
qui replace relation19`x'=20 if relation19`x'== 2
qui replace relation19`x'=30 if relation19`x'== 3
qui replace relation19`x'=40 if relation19`x'== 4
qui replace relation19`x'=50 if relation19`x'== 5
qui replace relation19`x'=60 if relation19`x'== 6
}

// Current heads/wives have sequence numbers between 1-20, while mover out wives/heads have sequence number values above that
gen sequence1968=sequence1969
foreach y of numlist 1968/1997 1999 2001 2003 2005 2007 2009 2011{
    qui replace relation`y'=0 if relation`y'== 10 & (sequence`y'>20|sequence`y'<1)
    qui replace relation`y'=0 if relation`y'== 20 & (sequence`y'>20|sequence`y'<1)
}

// For easier looping, create wife education 69-71
forvalues x = 69/71 {
gen w_edu19`x'= w_edu1968
}

// Translate buckets of education to years of education
foreach x of numlist 1968/1995 1996 1997 1999 2001 2003 2005 2007 2009 2011 {
	foreach y in h w {

	// Note: no wife education between 1969 & 1971
	// Prior to 1992, education was declared in buckets
	if `x'<1991 qui gen `y'_edu`x'2=.
	if `x'<1991 qui replace `y'_edu`x'2 = 0 if `y'_edu`x' ==1 /*0-5 grades*/
	if `x'<1991 qui replace `y'_edu`x'2 = 6 if `y'_edu`x' ==2 /*6-8 grades*/
	if `x'<1991 qui replace `y'_edu`x'2 = 9 if `y'_edu`x' ==3 /*9-11 grades*/
	if `x'<1991 qui replace `y'_edu`x'2 = 12 if `y'_edu`x' ==4 /*12 grades*/
	if `x'<1991 qui replace `y'_edu`x'2 = 12 if `y'_edu`x' ==5 /*12 grades + nonac training*/
	if `x'<1991 qui replace `y'_edu`x'2 = 13 if `y'_edu`x' ==6 /*some college*/
	if `x'<1991 qui replace `y'_edu`x'2 = 16 if `y'_edu`x' ==7 /*BA*/
	if `x'<1991 qui replace `y'_edu`x'2 = 17 if `y'_edu`x' ==8 /*Adv degree*/
	if `x'<1991 qui replace `y'_edu`x'2 = . if `y'_edu`x' ==9 /*Adv degree*/
	if `x'<1991 drop `y'_edu`x'
	if `x'<1991 rename `y'_edu`x'2 `y'_edu`x'

		// Education of mother/father of head/wife is reported in buckets
		foreach p in moth fath{
			if `x'>=1974 qui gen `y'_`p'_edu`x'2=.
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 0 if `y'_`p'_edu`x' ==1 /*0-5 grades*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 6 if `y'_`p'_edu`x' ==2 /*6-8 grades*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 9 if `y'_`p'_edu`x' ==3 /*9-11 grades*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 12 if `y'_`p'_edu`x' ==4 /*12 grades*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 12 if `y'_`p'_edu`x' ==5 /*12 grades + nonac training*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 13 if `y'_`p'_edu`x' ==6 /*some college*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 16 if `y'_`p'_edu`x' ==7 /*BA*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = 17 if `y'_`p'_edu`x' ==8 /*Adv degree*/
			if `x'>=1974 qui replace `y'_`p'_edu`x'2 = . if `y'_`p'_edu`x' ==9 /*Adv degree*/
			if `x'>=1974 drop `y'_`p'_edu`x'
			if `x'>=1974 rename `y'_`p'_edu`x'2 `y'_`p'_edu`x'
		}

	}
}

replace mom_uniqueid = . if mom_uniqueid ==0
replace dad_uniqueid = . if dad_uniqueid ==0

/***************************************************************************************
Merge on information about head/wife from other years to get parental characteristics
*************************************************************************************/

foreach i in mom dad {

	* prepare data for merge - rename parent's id to pid so it will match with pid in the merge
	rename pid pid_own
	ren hs1995 hs1995_own
	renvars h_race*, postfix(_own)
	renvars w_race*, postfix(_own)
	renvars relation*, postfix(_own)
	renvars h_marital*, postfix(_own)
	renvars edu*, postfix(_own)
	rename `i'_uniqueid pid

	merge m:1 pid using $data/psid19682011, keepusing(pid edu* relation* h_marital* h_race* w_race* hs1995)

	keep if _merge==3| _merge==1
	drop _merge

	// rename back to own variables
	forvalues j=1968/1968{
		rename edu`j' `i'edu`j'
		rename relation`j' `i'_relation`j'
		rename h_marital`j' `i'_h_marital`j'
		rename h_marital`j'_own h_marital`j'
	}

	forvalues j=1969/1969{
		rename relation`j' `i'_relation`j'
		rename h_marital`j' `i'_h_marital`j'
		rename h_marital`j'_own h_marital`j'
	}

	forvalues j=1970/1997{
		rename edu`j' `i'edu`j'
		rename relation`j' `i'_relation`j'
		rename h_marital`j' `i'_h_marital`j'
		rename h_marital`j'_own h_marital`j'
	}

	foreach j in 1999 2001 2003 2005 2007 2009 2011{
		rename edu`j' `i'edu`j'
		rename relation`j' `i'_relation`j'
		rename h_marital`j' `i'_h_marital`j'
		rename h_marital`j'_own h_marital`j'
	}

	forvalues j=1977/1993{
		rename h_marital_strict`j' `i'_h_marital_strict`j'
		rename h_marital_strict`j'_own h_marital_strict`j'
	}

	forvalues n=1968/1984{
			rename h_race`n' `i'_h_race`n'
	}
	forvalues n=1985/1997{
			rename (h_race1`n' h_race2`n' w_race1`n' w_race2`n') ///
				(`i'_h_race1`n' `i'_h_race2`n' `i'_w_race1`n' `i'_w_race2`n')
	}
	foreach n in 1999 2001 2003 2005 2007 2009 2011{						/* 1999-2001 renaming of mom/dad variables */
		rename (h_race1`n' h_race2`n' w_race1`n' w_race2`n') ///
			(`i'_h_race1`n' `i'_h_race2`n' `i'_w_race1`n' `i'_w_race2`n')
	}

	ren hs1995  `i'hs1995
	rename pid `i'_uniqueid
	renvars h_race*, postdrop(4)
	renvars edu*, postdrop(4)
	renvars w_race*, postdrop(4)
	renvars relation*, postdrop(4)
	rename pid_own pid
	ren hs1995_own hs1995

	// Mother's/Fathers years of education

	// Years completed
	qui gen `i'edu1969=`i'edu1968

	/* Two ways to get mother/father education:
	1. From declaration by mother/father identified by momid/dadid & merged on
	2. From declaration of head combined with relation to head
	*/

	// Method 1 - education declared by parent & merged on
	foreach x in 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 99 01 /*03 05 07 09*/ {
		if `x' != 01 	qui gen `i'ed`x'=`i'edu19`x'
		if `x' != 01 	qui replace `i'ed`x' =. if `i'edu19`x'==99 | `i'edu19`x'==98 | `i'edu19`x'==0
		if `x' == 01 	qui gen `i'ed`x'=`i'edu20`x'
		if `x' == 01 	qui replace `i'ed`x'=. if `i'edu20`x'==99 | `i'edu20`x'==98 | `i'edu20`x'==0
		}

	//h mother education only starts in '74
	foreach x in 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 {

	// Method 2 - education declared by head combined with relation to head
	qui gen `i'ed2`x'=.

	if "`i'" == "mom" & `x'>=74 qui replace `i'ed2`x'=h_moth_edu19`x' if (relation19`x'==10 | relation19`x'==40) & `i'ed2`x'==. //mom of head if hh head or sibling of head
	if "`i'" == "mom" & `x'>=74  qui replace `i'ed2`x'=w_moth_edu19`x' if (relation19`x'==20 | relation19`x'==22) & `i'ed2`x'==. //mom of wife if wife, female cohabitator
	if "`i'" == "mom" & `x'>=74 qui replace `i'ed2`x'=w_moth_edu19`x' if (relation19`x'==47 | relation19`x'==48) & `i'ed2`x'==. //mom of wife if sibling of wife

	if "`i'" == "dad" & `x'>=74  qui replace `i'ed2`x'=h_fath_edu19`x' if (relation19`x'==10 | relation19`x'==40) & `i'ed2`x'==. //dad of head if hh head or sibling of head
	if "`i'" == "dad" & `x'>=74  qui replace `i'ed2`x'=w_fath_edu19`x' if (relation19`x'==20 | relation19`x'==22) & `i'ed2`x'==. //dad of wife if wife, female cohabitator
	if "`i'" == "dad" & `x'>=74  qui replace `i'ed2`x'=w_fath_edu19`x' if (relation19`x'==47 | relation19`x'==48) & `i'ed2`x'==. //dad of wife if sibling of wife

	qui replace `i'ed2`x'=. if `i'ed2`x'==0| `i'ed2`x'>=98

	if "`i'" == "mom"  qui replace `i'ed2`x'=h_edu19`x' if (relation19`x'==30) & h_sex19`x' ==2 & `i'ed2`x'==. //son of head if hh head
	if `x' <94 & "`i'" == "mom"  qui replace `i'ed2`x'=w_edu19`x' if relation19`x'==30 & h_sex19`x' !=2 & h_marital19`x' ==1 & `i'ed2`x'==.
	if `x' <94 & "`i'" == "mom"  qui replace `i'ed2`x'=w_edu19`x' if (relation19`x'==33 | relation19`x'==35) & `i'ed2`x'==. //son of wife if wife

	if "`i'" == "dad"  qui replace `i'ed2`x'=h_edu19`x' if (relation19`x'==30) & h_sex19`x' !=2 & `i'ed2`x'==. //son of head if hh head
	if `x' <94 & "`i'" == "dad"  qui replace `i'ed2`x'=w_edu19`x' if (relation19`x'==30) & h_sex19`x' ==2 & h_marital19`x' ==1 & `i'ed2`x'==.
	if `x' <94 & "`i'" == "dad"  qui replace `i'ed2`x'=w_edu19`x' if (relation19`x'==33 | relation19`x'==35) & h_sex19`x' ==2 & `i'ed2`x'==. //son of wife if wife, female cohabitator
	if "`i'" == "dad"  qui replace `i'ed2`x'=h_edu19`x' if (relation19`x'==33 | relation19`x'==35) & h_sex19`x' !=2 & h_marital19`x' ==1 & `i'ed2`x'==.

	qui replace `i'ed2`x'=. if `i'ed2`x'==0| `i'ed2`x'>=98

	}

	qui egen `i'ed = rowmin(`i'ed68-`i'ed95) // lowest reported education in the period - probably closest to education when child was growing up
	qui egen `i'ed2 = rowmin(`i'ed268-`i'ed295)

	/*hs completed*/
	qui gen `i'hs = 1 if `i'ed>=12 & `i'ed != .
	qui replace `i'hs = 0 if `i'ed<12 & `i'ed != .
	qui gen `i'hs2 = 1 if `i'ed2>=12 & `i'ed2 != .
	qui replace `i'hs2 = 0 if `i'ed2<12 & `i'ed2 != .

	//use the information in the family survey if available and the merged on individual data otherwise

	replace `i'hs2=`i'hs if `i'hs2==.
	replace `i'hs2=0 if `i'hs==0
	replace `i'hs2 =0 if `i'hs1995==3
	replace `i'hs2 =1 if `i'hs1995==1|`i'hs1995==2
	replace `i'ed2=`i'ed if `i'ed2==.
	replace `i'ed2=`i'ed if `i'ed2>`i'ed & `i'ed2 !=.

}

/************************************************************************
Clean Individual variables
************************************************************************/

// Birthyear
rename birthyr1995 birthyear

// Create consistent age variable
foreach x of numlist 1968/1997 1999 2001 2003 2005 2007 2009 2011 {
replace age`x' = age1995 + (`x'-1995)
replace age`x' = . if age`x' <0
}

// Birthweight
gen lbw = 1 if birthweight<224 & (birthweight/16) <5.5
replace lbw = 1 if birthweight == 991
replace lbw = 0 if birthweight<224 & birthweight/16 >=5.5
replace lbw = 0 if birthweight==995
replace birthweight = . if birthweight == 999|birthweight == 998
replace birthweight = . if birthweight == 991 | birthweight == 995

/*******************************************************************************************************************
Identify race -- several possible ways to define this. We'll loop over these to see which one best replicates GTC
**********************************************************************************************************************/

// Definition 1 of race - basic definition
// Identify race from race of head/wife you are the head, wife or son/daughter of one of them
gen black1 = 1 if w_race11995==2 & relation1995==20 | w_race11995==2 & relation1995==22 /// *legal wife or cohabitator of head
| w_race11995==2 & relation1995==33 | w_race11995==2 & relation1995==35 /// children of wife
| h_race11995==2 & relation1995==10  /// head
| h_race11995==2 & relation1995==30  // children of head

gen white1 = 1 if w_race11995==1 & relation1995==20 | w_race11995==1 & relation1995==22 /// *legal wife or cohabitator of head
| w_race11995==1 & relation1995==33 | w_race11995==1 & relation1995==35  /// *children of wife
| h_race11995==1 & relation1995==10  /// head
| h_race11995==1 & relation1995==30  // children of head

// Definition 2 of race - basic definition looped over several years
gen black2 = black1
gen white2 = white1

foreach i in 1993 1994 1996 {
replace black2=1 if black2==. & (w_race1`i'==2 & relation`i'==20 | w_race1`i'==2 & relation`i'==22 /// *legal wife or cohabitator of head
| w_race1`i'==2 & relation`i'==33 | w_race1`i'==2 & relation`i'==35  /// son/daughter of wife
| h_race1`i'==2 & relation`i'==10  /// head
| h_race1`i'==2 & relation`i'==30) // children of head
replace white2=1 if white2==. & (w_race1`i'==1 & relation`i'==20 | w_race1`i'==1 & relation`i'==22 /// *wife of head
| w_race1`i'==1 & relation`i'==33 | w_race1`i'==1 & relation`i'==35 /// *children of wife
| h_race1`i'==1 & relation`i'==10  /// head
| h_race1`i'==1 & relation`i'==30)  // children of head
}

// Definition 3 of race - expansive definition - sibling or parent of head/wife
// Definition 1 plus identify race from race of head/wife you are a sibling one of them
gen black3 = 1 if w_race11995==2 & relation1995==20 | w_race11995==2 & relation1995==22 /// *legal wife or cohabitator of head
| w_race11995==2 & relation1995==33 | w_race11995==2 & relation1995==35 /// children of wife
| w_race11995==2 & relation1995==47| w_race11995==2 & relation1995==48 /// siblings of wife
| w_race11995==2 & relation1995==57 | w_race11995==2 & relation1995==58 /// parents of wife
| h_race11995==2 & relation1995==10  /// head
| h_race11995==2 & relation1995==30  /// children of head
| h_race11995==2 & relation1995==40 /// sibling of head
| h_race11995==2 & relation1995==50 // parent of head

gen white3 = 1 if w_race11995==1 & relation1995==20 | w_race11995==1 & relation1995==22 /// legal wife or cohabitator of head
| w_race11995==1 & relation1995==33 | w_race11995==1 & relation1995==35  /// children of wife
| w_race11995==1 & relation1995==47 | w_race11995==1 & relation1995==48 /// siblings of wife
| w_race11995==1 & relation1995==57 | w_race11995==1 & relation1995==58 /// parents of wife
| h_race11995==1 & relation1995==10  /// head
| h_race11995==1 & relation1995==30  /// children of head
| h_race11995==1 & relation1995==40 /// sibling of head
| h_race11995==1 & relation1995==50 // parents of head

// Definition 4 - loop over definition 3 for several years
gen black4 = black3
gen white4 = white3

// Wife race available 1985 on
forvalues i = 1985/1996{
qui replace black4 = 1 if black4 ==. & white4==. & (w_race1`i'==2 & relation`i'==20 | w_race1`i'==2 & relation`i'==22 ///legal wife or cohabitator of head
| w_race1`i'==2 & relation`i'==33 | w_race1`i'==2 & relation`i'==35 /// children of wife
| w_race1`i'==2 & relation`i'==47 | w_race1`i'==2 & relation`i'==48 /// siblings of wife
| w_race1`i'==2 & relation`i'==57 | w_race1`i'==2 & relation`i'==58) /// parents of wife

qui replace white4 = 1 if black4 ==. & white4==. & (w_race1`i'==1 & relation`i'==20 | w_race1`i'==1 & relation`i'==22 /// legal wife or cohabitator of head
| w_race1`i'==1 & relation`i'==33 | w_race1`i'==1 & relation`i'==35  /// children of wife
| w_race1`i'==1 & relation`i'==47 | w_race1`i'==1 & relation`i'==48 /// siblings of wife
| w_race1`i'==1 & relation`i'==57 | w_race1`i'==1 & relation`i'==58) //* parents of wife
}

forvalues x=1968/1984{
rename h_race`x' h_race1`x'
}

forvalues i = 1968/1996{
qui replace black4 = 1 if black4 ==. & white4==. & (h_race1`i'==2 & relation`i'==10  /// head
| h_race1`i'==2 & relation`i'==30  /// children of head
| h_race1`i'==2 & relation`i'==40 /// sibling of head
| h_race1`i'==2 & relation`i'==50) // parent of head
qui replace white4 = 1 if black4 ==. & white4==. & (h_race1`i'==1 & relation`i'==10  /// head
| h_race1`i'==1 & relation`i'==30  /// children of head
| h_race1`i'==1 & relation`i'==40 /// sibling of head
| h_race1`i'==1 & relation`i'==50) // parents of head
}

replace white1 = 0 if white1!=1
replace black1 = 0 if black1!=1
replace white2 = 0 if white2!=1
replace black2 = 0 if black2!=1
replace white3 = 0 if white3!=1
replace black3 = 0 if black3!=1
replace white4 = 0 if white4!=1
replace black4 = 0 if black4!=1

//Definition 5 - use sibling race if own race is not black or white and all of siblings are black or all siblings are white. Main definition. 

gen black5=black4
gen white5=white4

bysort mom_unique: egen meanwhite=mean(white4)
bysort mom_unique: egen meanblack=mean(black4)
gen norace = (black4==0 & white4==0)

bysort mom_unique: gen numsibs = _N
bysort mom_unique: egen numblacksibs = sum(black4)
bysort mom_unique: egen numwhitesibs = sum(white4)
bysort mom_unique: egen numnoracesibs = sum(norace)

replace black5=1 if norace==1 & numblacksibs==numsibs-numnoracesibs & numblacksibs>0 & mom_unique !=.
replace white5=1 if norace==1 & numwhitesibs==numsibs-numnoracesibs & numwhitesibs>0 & mom_unique !=.

drop numsibs numblacksibs numwhitesibs numnoracesibs

**Sex
gen female= (sex==2)

*** Head Start/preschool - identified only in 1995

// Head Start
gen headstart=1 if headstart1995==1
replace headstart=0 if headstart1995==5
replace headstart=. if headstart1995==8 | headstart1995==9 | headstart1995==0

// Preschool other than headstart
gen preschool=1 if preschool1995==1
replace preschool=0 if preschool1995==5
replace preschool=. if preschool1995==8 | preschool1995==9 | preschool1995==0
rename headstart orig_headstart
rename preschool orig_preschool

// Redefine Head Start and preschool to be mutually exclusive
gen headstart = orig_headstart // any head start
gen preschool = orig_preschool // ony preschool
replace preschool = 0 if headstart ==1

// Crime - identified in 1995 only
gen crime=1 if crime1995==1
replace crime=0 if crime1995==5
replace crime=. if crime1995==9 | crime1995==0
gen nocrime = 1-crime

// Eldest child in family if first child born to both mother and father
gen eldestchild= (parityofmom==1) // & ER32020==1
replace eldestchild=. if parityofmom>=98
replace eldestchild=0 if eldestchild!=1 & eldestchild !=.

// Another way to get at this
replace birthyear=. if birthyear==0
bysort mom_uniqueid: egen earlybirth= min(birthyear)
gen oldest = 1 if earlybirth==birthyear & mom_uniqueid !=0 & birthyear!=.
replace eldestchild=1 if oldest==1 & eldestchild==.
replace eldestchild=0 if eldestchild==.

/*****************************************************
Earnings outcomes
*****************************************************/

// Earnings for ages 23-25, 25-30.. ETC
// 2 earnings generated: 1) Earnings 23-25 in uninflated dollars 2) Earnings 23-25 in 1999$
// 1) Earnings 23-25 in uninflated dollars

rename (w_wages1994 w_wages1995 w_wages1995 w_wages1996 w_wages1997 w_wages1999 w_wages2001 w_wages2003 w_wages2005 w_wages2007 w_wages2009 w_wages2011) ///
(w_wagesold1994 w_wagesold1995 w_wagesold1995 w_wagesold1996 w_wagesold1997 w_wagesold1999 w_wagesold2001 w_wagesold2003 w_wagesold2005 w_wagesold2007 w_wagesold2009 w_wagesold2011)
rename (w_wages21994 w_wages21995 w_wages21995 w_wages21996 w_wages21997 w_wages21999 w_wages22001 w_wages22003 w_wages22005 w_wages22007 w_wages22009 w_wages22011) ///
(w_wages1994 w_wages1995 w_wages1995 w_wages1996 w_wages1997 w_wages1999 w_wages2001 w_wages2003 w_wages2005 w_wages2007 w_wages2009 w_wages2011)
rename (h_wages1991	h_wages1992 h_wages1993 h_wages1994 h_wages1995 h_wages1996 h_wages1997 h_wages1999 h_wages2001 h_wages2003 ///
h_wages2005 h_wages2007 h_wages2009 h_wages2011 ) ///
(h_wagesold1991 h_wagesold1992 h_wagesold1993 h_wagesold1994 h_wagesold1995 h_wagesold1996 h_wagesold1997 h_wagesold1999 h_wagesold2001 ///
h_wagesold2003 h_wagesold2005 h_wagesold2007 h_wagesold2009 h_wagesold2011)
rename (h_totlabor1991	h_totlabor1992 h_totlabor1993 h_totlabor1994 h_totlabor1995 h_totlabor1996 h_totlabor1997 h_totlabor1999 h_totlabor2001 ///
h_totlabor2003 h_totlabor2005 h_totlabor2007 h_totlabor2009 h_totlabor2011) ///
(h_wages1991 h_wages1992 h_wages1993 h_wages1994 h_wages1995 h_wages1996 h_wages1997 h_wages1999 h_wages2001 h_wages2003 h_wages2005 ///
h_wages2007 h_wages2009 h_wages2011)

forvalues x = 1998(2)2010 {
	qui gen w_wages`x' = .
	qui gen h_wages`x' = .
	qui gen relation`x' = .
}

forvalues x = 1968/2011 {
	qui gen wages`x' = h_wages`x' if relation`x' == 10
	qui replace wages`x' = w_wages`x' if relation`x' ==20 | relation`x'==22
	qui replace wages`x' = . if wages`x'<0 //may want to revisit
	qui replace wages`x' = . if wages`x'==9999999 & (`x'==1995|`x'==1994) // Latino families
	if `x'>= 1975 & `x'<=1993 qui replace wages`x' = indinc`x' if wages`x'==. & indinc`x' >0 //0 = latino
	gen wagescpi`x' = wages`x'*cpi99`x'
	gen lnwagescpi`x' = ln(wagescpi`x')
}

// Earnings 23-25 like in GTC
forvalues r = 23/25 {
    qui gen earn_`r'_cpi = .
}

forvalues i = 8/40 {
	local j = 1995 + (24-`i')
	local k = 1995 + (25-`i')
	local l =  1995 + (26-`i')
	cap qui replace earn_23_cpi = wagescpi`j' if earn_23_cpi==. & age1995==`i'
	cap qui replace earn_24_cpi = wagescpi`k'  if  earn_24_cpi==. & age1995==`i'
	cap qui replace earn_25_cpi = wagescpi`l' if earn_25_cpi==. & age1995==`i'
}
qui egen earncpi_2325 = rowmean(earn_23_cpi earn_24_cpi earn_25_cpi)
la var earncpi_2325 "Average earnings between age 23-25 - CPI adjusted"

// Extended Earnings
foreach t in 30 35 40 45 {
	qui gen mearn`t'_cpi=.
	qui gen lnmearn`t'_cpi=.
	
	foreach i of numlist 1973/2011 { // only have 5 years before beginning 1973
		local i1 = `i'-1
		local i2 = `i'-2
		local i3 = `i'-3
		local i4 = `i'-4
		qui egen temp=rowmean(wagescpi`i' wagescpi`i1' wagescpi`i2' wagescpi`i3' wagescpi`i4') if birthyear ==`i'-`t'+1
		qui replace mearn`t'_cpi = temp if birthyear ==`i'-`t'+1
		drop temp
		replace lnmearn`t'_cpi = ln(mearn`t'_cpi) if birthyear ==`i'-`t'+1
	}
}

// Now mean nonzero earnings
foreach i of numlist 1973/2011 {
	replace wagescpi`i' = . if wagescpi`i' ==0
}

foreach t in 30 35 40 45{
	qui gen mearn`t'_cpi_nozero=.
	qui gen lnmearn`t'_cpi_nozero=.

	// Wages for 25-30, 30-35... etc.
	foreach i of numlist 1973/2011 { // only have 5 years before beginning 1973
		local i1 = `i'-1
		local i2 = `i'-2
		local i3 = `i'-3
		local i4 = `i'-4
		qui egen temp=rowmean(wagescpi`i' wagescpi`i1' wagescpi`i2' wagescpi`i3' wagescpi`i4') if birthyear ==`i'-`t'+1
		qui replace mearn`t'_cpi_nozero = temp if birthyear ==`i'-`t'+1
		drop temp
		replace lnmearn`t'_cpi_nozero = ln(mearn`t'_cpi) if birthyear ==`i'-`t'+1
	}
}

// Employed/Unemployed
// Employed during 25-30, 30-35... etc.

foreach i of numlist 1968/2011 {
	gen emp`i' = 1 if wages`i' >0 & wages`i'!=.
	replace emp`i' = 0 if wages`i' ==0
	gen miss_emp`i'=(emp`i'!=1 & emp`i'!=0)
}

foreach t in 30 35 40 45{
	qui gen emp`t'=.
	qui gen memp`t' = .
	qui gen miss_emp`t'=.
	foreach i of numlist 1973/2011 { // only have 5 years before beginning 1973
		local i1 = `i'-1
		local i2 = `i'-2
		local i3 = `i'-3
		local i4 = `i'-4
		qui egen temp=rowtotal(emp`i' emp`i1' emp`i2' emp`i3' emp`i4') if birthyear ==`i'-`t'+1
		qui replace emp`t' = 1 if temp>=1 & temp!=. & birthyear ==`i'-`t'+1
		qui replace emp`t' = 0 if temp==0 & birthyear ==`i'-`t'+1
		drop temp
		qui egen temp=rowmean(emp`i' emp`i1' emp`i2' emp`i3' emp`i4') if birthyear ==`i'-`t'+1
		qui replace memp`t' = temp if birthyear ==`i'-`t'+1
		drop temp
		qui egen temp=rowtotal(miss_emp`i' miss_emp`i1' miss_emp`i2' miss_emp`i3' miss_emp`i4') if birthyear ==`i'-`t'+1
		qui replace miss_emp`t' = temp if birthyear ==`i'-`t'+1
		drop temp
	}
}

forvalues i = 1998(2)2010 {
	gen h_wksunemp`i'=.
	gen w_wksunemp`i'=.
}

foreach i of numlist 1969/2011{
	qui gen unemp`i' = 1 if h_wksunemp`i' >0 & h_wksunemp`i' <=52 & h_wksunemp`i'!=. & relation`i'==10
	qui replace unemp`i' = 0 if h_wksunemp`i' ==0  & relation`i'==10
	if `i'>=1976  qui replace unemp`i' = 1 if w_wksunemp`i' >0 & w_wksunemp`i' <=52 & w_wksunemp`i'!=. & (relation`i'==20|relation`i'==22)
	if `i'>=1976 replace unemp`i' = 0 if w_wksunemp`i' ==0 & (relation`i'==20|relation`i'==22)
	gen miss_unemp`i'=(unemp`i'!=1 & unemp`i'!=0)
}

foreach t in 30 35 40 45{
	qui gen unemp`t'=.
	qui gen munemp`t' = .
	qui gen miss_unemp`t'=.

	foreach i of numlist 1973/2011 { // only have 5 years before beginning 1973
		local i1 = `i'-1
		local i2 = `i'-2
		local i3 = `i'-3
		local i4 = `i'-4
		qui egen temp=rowtotal(unemp`i' unemp`i1' unemp`i2' unemp`i3' unemp`i4') if birthyear ==`i'-`t'+1
		qui replace unemp`t' = 1 if temp>=1 & temp!=. & birthyear ==`i'-`t'+1
		qui replace unemp`t' = 0 if temp==0 & birthyear ==`i'-`t'+1
		drop temp
		qui egen temp=rowmean(unemp`i' unemp`i1' unemp`i2' unemp`i3' unemp`i4') if birthyear ==`i'-`t'+1
		qui replace munemp`t' = temp if birthyear ==`i'-`t'+1
		drop temp
		qui egen temp=rowtotal(miss_unemp`i' miss_unemp`i1' miss_unemp`i2' miss_unemp`i3' miss_unemp`i4') if birthyear ==`i'-`t'+1
		qui replace miss_unemp`t' = temp if birthyear ==`i'-`t'+1
		drop temp
	}
}

forvalues x = 1998(2)2010 {
	qui drop w_wages`x' h_wages`x' relation`x'
}

/**************************************************
Childhood income measures
***************************************************/

// Childhood Family Income
qui gen cinc= .
qui gen cincage0= .
qui gen cincage1= .
qui gen cincage2= .

forvalues x = 8/35 {
	local a = 1995-`x' + 4
	local b = 1995-`x' + 5
	local c = 1995-`x' + 6
	local d = 1995-`x' + 7

	local e = 1995-`x' + 1 // 0 yrs old
	local f = 1995-`x' + 2 // 1 yr old
	local g = 1995-`x' + 3 // 2 yr old

	if `x'<=31	qui replace cinc=(cpi99`a'*f_moneyinc`a'+cpi99`b'*f_moneyinc`b'+cpi99`c'*f_moneyinc`c'+cpi99`d'*f_moneyinc`d')/4	if birthyear==1995-`x'
	if `x'<=28	qui replace cincage0=(cpi99`e'*f_moneyinc`e')	if birthyear==1995-`x'
	if `x'<=29	qui replace cincage1=(cpi99`f'*f_moneyinc`f')	if birthyear==1995-`x'
	if `x'<=30	qui replace cincage2=(cpi99`g'*f_moneyinc`g')	if birthyear==1995-`x'

}

// Childhood mom employment
qui gen momempage0= .
qui gen momempage1= .
qui gen momempage2= .

forvalues x = 8/35 {
	local e = 1995-`x' + 1 // 0 yrs old
	local f = 1995-`x' + 2 // 1 yr old
	local g = 1995-`x' + 3 // 2 yr old

	if `x'<=28	qui replace momempage0=1 if w_wages`e'>0 & w_wages`e'!=. & h_sex`e'==1 & birthyear==1995-`x'
	if `x'<=28	qui replace momempage0=0 if w_wages`e'==0 & h_sex`e'==1 & birthyear==1995-`x'
	if `x'<=28	qui replace momempage0=1 if h_wages`e'>0 & h_wages`e'!=. & h_sex`e'==2 & birthyear==1995-`x'
	if `x'<=28	qui replace momempage0=0 if h_wages`e'==0 &  h_sex`e'==2 & birthyear==1995-`x'

	if `x'<=29	qui replace momempage1=1 if w_wages`f'>0 & w_wages`f'!=. & h_sex`f'==1 & birthyear==1995-`x'
	if `x'<=29	qui replace momempage1=0 if w_wages`f'==0 & h_sex`f'==1 & birthyear==1995-`x'
	if `x'<=29	qui replace momempage1=1 if h_wages`f'>0 & h_wages`f'!=. & h_sex`f'==2 & birthyear==1995-`x'
	if `x'<=29	qui replace momempage1=0 if h_wages`f'==0 &  h_sex`f'==2 & birthyear==1995-`x'

	if `x'<=30	qui replace momempage2=1 if w_wages`g'>0 & w_wages`g'!=. & h_sex`g'==1 & birthyear==1995-`x'
	if `x'<=30	qui replace momempage2=0 if w_wages`g'==0 & h_sex`g'==1 & birthyear==1995-`x'
	if `x'<=30	qui replace momempage2=1 if h_wages`g'>0 & h_wages`g'!=. & h_sex`g'==2 & birthyear==1995-`x'
	if `x'<=30	qui replace momempage2=0 if h_wages`g'==0 &  h_sex`g'==2 & birthyear==1995-`x'

}
	
/************************************************************************
Education outcomes
************************************************************************/

// Education completed by 1995
gen edu1969 = edu1968

foreach x of numlist 1968/1997 1999 2001 2003 2005 2007 2009 2011 {
replace edu`x'=. if edu`x' ==98|edu`x' ==99|edu`x' ==0
}

// Dummy for completed high school
foreach x in 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 99 01 03 05 07 09 11 {
	if `x' != 01 & `x' != 03 &`x' != 05 & `x' != 07 &`x' != 09 & `x' != 11   	qui gen hsgrad`x'=cond(edu19`x'>=12, 1, 0)
	if `x' != 01 & `x' != 03 &`x' != 05 & `x' != 07 &`x' != 09 & `x' != 11 		qui replace hsgrad`x' = . if edu19`x'==.
	if `x' != 01 & `x' != 03 &`x' != 05 & `x' != 07 &`x' != 09 & `x' != 11 		qui replace hsgrad`x'=. if edu19`x'==99 | edu19`x'==98| edu19`x'==0
	if `x' == 01|`x' == 03|`x' == 05|`x' == 07|`x' == 09|`x' == 11 				qui gen hsgrad`x'=cond(edu20`x'>=12, 1, 0)
	if `x' == 01|`x' == 03|`x' == 05|`x' == 07|`x' == 09|`x' == 11  			qui replace hsgrad`x'=. if edu20`x'==.
	if `x' == 01|`x' == 03|`x' == 05|`x' == 07|`x' == 09|`x' == 11  			qui replace hsgrad`x'=. if edu20`x'==99 | edu20`x'==98| edu20`x'==0
}

// HS grad
gen hsgrad1 = 1 if (hsgrad91==1|hsgrad92==1|hsgrad93==1|hsgrad94==1|hsgrad95==1)
replace hsgrad1 = 0 if edu1996<12 & edu1996 !=0 & edu1996 !=. & hsgrad1!=1
replace hsgrad1 = 0 if edu1995<12 & edu1995 !=0 & edu1995 !=. & hsgrad1!=1
replace hsgrad1 = 1 if edu1996>12 & edu1996!=. & edu1996<98
replace hsgrad1 = 1 if hs1995==1|hs1995==2
replace hsgrad1 = 0 if hs1995==3

// HS grad considering birth year
gen hsgrad2 = 1 if (hsgrad86==1|hsgrad87==1|hsgrad88==1|hsgrad89==1|hsgrad90==1|hsgrad91==1|hsgrad92==1|hsgrad93==1|hsgrad94==1|hsgrad95==1|hsgrad96==1|hsgrad97==1|hsgrad99==1|hsgrad01==1 ///
|hsgrad03==1|hsgrad05==1|hsgrad07==1|hsgrad09==1|hsgrad11==1)
replace hsgrad2 = 0 if edu2011<12 & edu2011 !=0 & hsgrad2!=1 // code everyone to hs = 0 if by 2011 they haven't reported hs
replace hsgrad2 = 0 if edu2009<12 & edu2009 !=0 & birthyear<=1989 & hsgrad2!=1
replace hsgrad2 = 0 if edu2007<12 & edu2007 !=0 & birthyear<=1987 & hsgrad2!=1
replace hsgrad2 = 0 if edu2005<12 & edu2005 !=0 & birthyear<=1985 & hsgrad2!=1
replace hsgrad2 = 0 if edu2003<12 & edu2003 !=0 & birthyear<=1983 & hsgrad2!=1
replace hsgrad2 = 0 if edu2001<12 & edu2001 !=0 & birthyear<=1981 & hsgrad2!=1
replace hsgrad2 = 0 if edu1999<12 & edu1999 !=0 & birthyear<=1979 & hsgrad2!=1
replace hsgrad2 = 0 if edu1997<12 & edu1997 !=0 & birthyear<=1977 & hsgrad2!=1

foreach y of numlist 1986/1996{
	replace hsgrad2 = 0 if edu`y'<12 & edu`y' !=0  & (`y'-birthyear)>=20  & hsgrad2!=1
}

// Dummy for attended some college/college
foreach var in college fin_college {
	foreach x of numlist 1968/1997 1999 2001 2003 2005 2007 2009 2011{

		if "`var'" == "college" local years 12
		if "`var'" == "fin_college" local years 15
		if "`var'" == "college" local age 22
		if "`var'" == "fin_college" local age 26
		if `x'<2000 qui gen `var'`x'=cond(edu`x'>`years', 1, 0)
		if `x'<2000 qui replace `var'`x' = . if edu`x'==.
		if `x'<2000 qui replace `var'`x'=. if edu`x'==99 | edu`x'==98 | edu`x'==0
		if `x'>=2000 qui gen `var'`x'=cond(edu`x'>`years', 1, 0)
		if `x'>=2000 qui replace `var'`x'=. if edu`x'==.
		if `x'>=2000 qui replace `var'`x'=. if edu`x'==99 | edu`x'==98 | edu`x'==0

	}

    // Some college for old results (clean PSID final)
    egen `var'1 = rowmax(`var'1991-`var'1995)
    replace `var'1 = 0 if hsgrad1==0
    replace `var'1 = 0 if edu1996<=`years' & edu1996 !=. & `var'1 != 1
    replace `var'1 = 1 if edu1996>`years' & edu1996!=. & edu1996<98
	// Some college for new sample
	egen `var'2 = rowmax(`var'1991-`var'2011)
	replace `var'2 = 0 if hsgrad2==0
	replace `var'2 = 0 if edu2011<=`years' & `var'2!=1 // code everyone to college = 0 if by 2011 they haven't reported college
	replace `var'2 = 0 if edu2009<=`years' & edu2009 !=0  & birthyear<= (2009-`age') & `var'2!=1
	replace `var'2 = 0 if edu2007<=`years' & edu2007 !=0  & birthyear<= (2007-`age') & `var'2!=1
	replace `var'2 = 0 if edu2005<=`years' & edu2005 !=0  & birthyear<= (2005-`age') & `var'2!=1
	replace `var'2 = 0 if edu2003<=`years' & edu2003 !=0  & birthyear<= (2003-`age') & `var'2!=1
	replace `var'2 = 0 if edu2001<=`years' & edu2001 !=0  & birthyear<= (2001-`age') & `var'2!=1
	replace `var'2 = 0 if edu1999<=`years' & edu1999 !=0  & birthyear<= (1999-`age') & `var'2!=1
	replace `var'2 = 0 if edu1997<=`years' & edu1997 !=0  & birthyear<= (1997-`age') & `var'2!=1

}

foreach y of numlist 1986/1996{
	replace college2 = 0 if edu`y'<=12 & edu`y' !=0 & (`y'-birthyear)>=22 & college2!=1
}

foreach y of numlist 1986/1996{
	replace fin_college2 = 0 if edu`y'<=15 & edu`y' !=0 & (`y'-birthyear)>=26 & fin_college2!=1
}

// Create final HS grad variable that combines information from hsgrad1 and hsgrad2
gen hsgrad = hsgrad1 if birthyear>=1966 & birthyear<=1977 & age1995>=18
replace hsgrad = hsgrad2 if birthyear<1966 | birthyear>1977| (birthyear==1977 & age1995<18)
gen college = college1 if birthyear>=1966 & birthyear<=1977 & age1995>=18
replace college = college2 if birthyear<1966 | birthyear>1977| (birthyear==1977 & age1995<18)
egen college_test = rowmax(college1 college2)
gen fin_college = fin_college1 if birthyear>=1966 & birthyear<=1977 & age1995>=18
replace fin_college = fin_college2 if birthyear<1966 | birthyear>1977| (birthyear==1977 & age1995<18)


/*****************************************************************
Additional measures of childhood circumstances
******************************************************************/

// Single mom at age 4: two definitions: 1) mom head of household 2) female head of household
gen mom_hoh =.
gen singlemom_age4=.

forvalues k = 1965/1990 {
	local n = `k'+4

	// 1. mom head of household
	qui replace mom_hoh = 1 if mom_relation`n' == 1 & birthyear  ==  `k'

	// 2. head of household female
	qui replace singlemom_age4=1 if h_sex`n'==2 & birthyear==`k'
	qui replace singlemom_age4 =0 if h_sex`n'==1 & birthyear==`k'

	qui replace mom_hoh = singlemom_age4 if mom_hoh==. // combine definitions
	qui replace mom_hoh = 0 if mom_uniqueid !=0 & mom_hoh ==. & birthyear  ==  `k'

	qui replace singlemom_age4 = mom_hoh if singlemom_age4==. // combine definitions
}

drop mom_hoh
rename singlemom_age4 mom_hoh
la var mom_hoh "Had a single mother at age 4"

// Family Size at age 4
drop num_in_fam1970 num_in_fam1971 num_in_fam1973 num_in_fam1975
rename (num_in_fam21970 num_in_fam21971 num_in_fam21973 num_in_fam21975) (num_in_fam1970 num_in_fam1971 num_in_fam1973 num_in_fam1975)

gen hhsize_age4 = .
forvalues k = 1965/1990 {
	local n = `k'+4
	qui replace hhsize_age4 = num_in_fam`n' if age1995==1995-`k'
}
la var hhsize_age4 "Household size at age 4"

// Income relative to poverty
foreach y of numlist 1998(2)2010{
	gen irp`y'=.
}

foreach y of numlist 1968/1997 1999 2001 2003 2005 2007 2009 2011{
	gen irp`y'= (f_moneyinc`y'/povthresh`y')*100
	replace irp`y' = 0 if irp`y'<0
}

foreach t in 30 40{
	qui gen mirp`t'=.
	forvalues z=1950/1990{
		local z1=`z'+`t'+1 // because of lag, need to add 1
		local z2=`z1'-1
		local z3=`z1'-2
		local z4=`z1'-3
		local z5=`z1'-4

		if `z5'>=1968 &	 `z5'<=2007 qui egen temp=rowmean(irp`z1' irp`z2' irp`z3' irp`z4' irp`z5') if birthyear==`z'
			if `z5'>=1968 & `z5'<=2007 qui replace mirp`t'=temp if temp!=.
			cap drop temp
		if `z5'>=1968 & `z5'==2008 qui egen temp=rowmean(irp`z2' irp`z3' irp`z4' irp`z5') if birthyear==`z'
			if `z5'>=1968& `z5'==2008 qui replace mirp`t'=temp if temp!=.
			cap drop temp
		if `z5'>=1968 & `z5'==2009 qui egen temp=rowmean(irp`z3' irp`z4' irp`z5') if birthyear==`z'
			if `z5'>=1968 & `z5'==2009 qui replace mirp`t'=temp if temp!=.
			cap drop temp
		if `z5'>=1968 & `z5'==2010 qui egen temp=rowmean(irp`z4' irp`z5') if birthyear==`z'
			if `z5'>=1968 & `z5'==2010 qui replace mirp`t'=temp if temp!=.
			cap drop temp
		if `z5'>=1968 & `z5'==2011 qui egen temp=rowmean(irp`z5') if birthyear==`z'
			if `z5'>=1968 & `z5'==2011 qui replace mirp`t'=temp if temp!=.
			cap drop temp
	}
}

// Disabled? Only available 1969-1972, 1976-1978
// Ever disabled?
gen disabled = 1 if disab1969==1|disab1969==2 // disabled or requires extra care
replace disabled = 1 if disab1970==1|disab1970==2 // disabled or requires extra care
replace disabled = 1 if disab1971==1|disab1971==2 // disabled or requires extra care
replace disabled = 1 if disab1972==1|disab1972==2 // disabled or requires extra care
replace disabled = 1 if disab1976==1 // disabled or requires extra care
replace disabled = 1 if disab1977==1 // disabled or requires extra care
replace disabled = 1 if disab1978==1 // physical or nervous condition

// Make 0 if ever answered no and never answered yes
replace disabled = 0 if disab1969==5 & disabled!=1
replace disabled = 0 if disab1970==5 & disabled!=1
replace disabled = 0 if disab1971==5 & disabled!=1
replace disabled = 0 if disab1972==5 & disabled!=1
replace disabled = 0 if disab1976==5 & disabled!=1
replace disabled = 0 if disab1977==5 & disabled!=1
replace disabled = 0 if disab1978==5 & disabled!=1

/***********************************************************
ADDITIONAL OUTCOMES
***********************************************************/

***COLLEGE DEGREE/GED;
local range1 "1975/1997"
local range2 "1999(2)2011"
local range3 "1985/1997"
foreach r in range1 range2{
forvalues y=``r''{
qui gen collegedeg`y' = 1 if h_coldeg`y'==1 & relation`y'==10
	qui replace collegedeg`y'=0 if h_coldeg`y'==5 & relation`y'==10
	}
	}
foreach r in range3 range2{
forvalues y=``r''{	
qui replace collegedeg`y' = 1 if w_coldeg`y'==1 & relation`y'==20 | relation`y'==22 
	qui replace collegedeg`y'=0 if w_coldeg`y'==5 & relation`y'==20 | relation`y'==22
	}
	}

	
***HS GED ATTAINMENT
local range1 "1985/1991"
local range2 "1993/1997"
local range3 "1999(2)2011"
foreach r in range1 range2 range3{	
	forvalues y=``r'' {
	qui gen gotged`y' = 1 if h_hsged`y'==1 & relation`y'==10
		qui replace gotged`y'=0 if (h_hsged`y'==2 | h_hsged`y'==3) & relation`y'==10
	qui gen gotgedorhs`y' = 1 if (h_hsged`y'==1 | h_hsged`y'==2) & relation`y'==10
		qui replace gotgedorhs`y'=0 if h_hsged`y'==3 & relation`y'==10
	
	qui replace gotged`y' = 1 if w_hsged`y'==1 & relation`y'==20 | relation`y'==22
		qui replace gotged`y'=0 if (w_hsged`y'==2 | w_hsged`y'==3) & relation`y'==20 | relation`y'==22
	qui replace gotgedorhs`y' = 1 if (w_hsged`y'==1 | w_hsged`y'==2) & relation`y'==20 | relation`y'==22
		qui replace gotgedorhs`y'=0 if w_hsged`y'==3 & relation`y'==20 | relation`y'==22
	}
	}
		
		
***OCCUPATION CODE FOR MAIN OCCUPATION		
local range1 "2003(2)2011"
foreach r in range1{
	forvalues y=``r''{
	qui gen 	mainocc`y'=h_mainocc`y' if h_mainocc`y'~=999 & h_mainocc`y'~=0 & relation`y'==10
	qui replace mainocc`y'=w_mainocc`y' if w_mainocc`y'~=999 & w_mainocc`y'~=0 & relation`y'==20 | relation`y'==22
		}
		}
***INDUSTRY CODE FOR MAIN OCCUPATION
local range1 "2003(2)2011"
foreach r in range1{
	forvalues y=``r''{
	qui gen 	mainind`y'=h_mainind`y' if h_mainind`y'~=999 & h_mainind`y'~=0 & h_mainind`y'~=848 & relation`y'==10
	qui replace mainind`y'=w_mainind`y' if w_mainind`y'~=999 & w_mainind`y'~=0 & w_mainind`y'~=848 & relation`y'==20 | relation`y'==22
		}
		}		
		
***RENT OR OWN?
local range1 "1968/1997"
local range2 "1999(2)2011"
foreach r in range1 range2{
		forvalues y=``r''{
		qui gen ownhome`y'=f_rentown`y'==1
		qui gen renthome`y'=f_rentown`y'==5
		qui gen miss_ownhome`y'=f_rentown`y'~=1 & f_rentown`y'~=5
		
		drop renthome*
		}
		}
		
***FOOD STAMPS? This variable is based on t and t-1 recollection, but code as if total recall.
local range1 "1994/1997"
local range2 "1999(2)2011"
	foreach r in range1 range2 {
	forvalues y=``r''{
		local i=`y'-1
		gen foodstamps`i'=(f_fs`y'==1 & (f_fs`y'==1 | f_fs`y'==5))
		gen miss_foodstamps`i'=(f_fs`y'!=1 & f_fs`y'!=5)
		}
		}
	foreach r in range2{
	forvalues y=``r''{
		local i=`y'-2
		gen foodstamps`i'=(f_fs`y'==1 & (f_fs`y'==1 | f_fs`y'==5))
		gen miss_foodstamps`i'=(f_fs`y'!=1 & f_fs`y'!=5)		
		}
		}
		

***AFDC/TANF (question is framed as "last year")
local range1 "1994/1997"
local range2 "1999(2)2011"
	foreach r in range1 range2 {
	forvalues y=``r''{
		local i=`y'-1
		gen afdctanf`i'=(f_tanf`y'==1 & (f_tanf`y'==1 | f_tanf`y'==5))
		gen miss_afdctanf`i'=(f_tanf`y'!=1 & f_tanf`y'!=5)	
		}
		}		
		
***CIGARETTES
local range1 "1986 1999 2001 2003 2005 2007 2009 2011"
		foreach r in range1{
		foreach y in ``r''{
		qui gen cigsperday`y'=(h_cigs`y'>0) if h_cigs`y'<998 & relation`y'==10
		qui replace cigsperday`y'=(w_cigs`y'>0) if w_cigs`y'<998 & relation`y'==20 | relation`y'==22
		gen miss_cigsperday`y'=(h_cigs`y'>=998 & relation`y'==10)|	(w_cigs`y'>=998 & relation`y'==20 | relation`y'==22)

		}
		}


***SELF REPORTED HEALTH (Rated as good or better vs fair/poor)
local range1 "1984/1997"
local range2 "1999(2)2011"
		foreach r in range1 range2{
		forvalues y=``r''{
		qui gen srhealthbad`y'=(h_srhealth`y'>=4)  if h_srhealth`y'>0 & h_srhealth`y'<8 & relation`y'==10
		qui replace srhealthbad`y'=(w_srhealth`y'>=4)  if w_srhealth`y'>0 & w_srhealth`y'<8 & relation`y'==20 | relation`y'==22
		
		qui gen miss_srhealthbad`y'=h_srhealth`y'>=8 & relation`y'==10
		qui replace miss_srhealthbad`y'=h_srhealth`y'>=8 & relation`y'==20 | relation`y'==22		
		}
		}


***HEIGHT (in inches and feet, sum it up and also get it in centimeters)
local range1 "1999(2)2011"
foreach r in range1{
	forvalues y=``r''{
	qui gen 	htinches`y'=(h_htfeet`y'*12)+h_htinch`y' if h_htfeet`y'>3 & h_htfeet`y'<8 & h_htinch`y'>=0 & h_htinch`y'<12 & relation`y'==10
	qui replace htinches`y'=(w_htfeet`y'*12)+w_htinch`y' if w_htfeet`y'>3 & w_htfeet`y'<8 & w_htinch`y'>=0 & w_htinch`y'<12  & relation`y'==20 | relation`y'==22
	
	qui gen htcentimeters`y'=htinches`y'*2.54
	
	qui drop htinches*
	}
	}



***WEIGHT (in pounds, but also get in kilos) and BMI
local range1 "1999(2)2011"
foreach r in range1{
	forvalues y=``r''{
	qui gen 	wtlbs`y'=h_wlbs`y' if h_wlbs`y'<990 & h_wlbs`y'>0 & relation`y'==10
	qui replace wtlbs`y'=w_wlbs`y' if w_wlbs`y'<990 & w_wlbs`y'>0 & relation`y'==20 | relation`y'==22
	
	qui gen wtkilos`y'=wtlbs`y'*0.453592
	
	qui gen bmi`y'=wtkilos`y'/((htcentimeters`y'/100)^2)
	qui gen miss_bmi`y'=bmi`y'==.
	}
	}	

		
		****Create age variables to cover the missing years of data
		foreach t in 1998 2000 2002 2004 2006 2008 2010{
		local s=`t'-1
		qui gen age`t'=age`s'+1
		}
		
		*Foodstamps: ever within a 5 year range;
		foreach t in 20 25 30 35 40 45{
				qui gen foodstamp`t'=.
				qui gen mfoodstamp`t'=.
				qui gen miss_foodstamp`t'=.

			forvalues y=1997/2010{
				local i1=`y'
				local i2=`y'-1
				local i3=`y'-2
				local i4=`y'-3
				local i5=`y'-4
				qui egen temp=rowtotal(foodstamps`i1' foodstamps`i2' foodstamps`i3' foodstamps`i4' foodstamps`i5') if age`y'==`t'		
				qui replace foodstamp`t'= temp>=1 if age`y'==`t'
				qui egen temp2=rowmean(foodstamps`i1' foodstamps`i2' foodstamps`i3' foodstamps`i4' foodstamps`i5') if age`y'==`t'	
				qui replace mfoodstamp`t'= temp2 if age`y'==`t'
				drop temp temp2 
				qui egen temp=rowtotal(miss_foodstamps`i1' miss_foodstamps`i2' miss_foodstamps`i3' miss_foodstamps`i4' miss_foodstamps`i5') if age`y'==`t'		
				qui replace miss_foodstamp`t'= temp if age`y'==`t'
				drop temp
				}
				}
				*drop excess variables
				drop foodstamps*
				
				
		*AFDC TANF: Ever by a certain age, the years possible are 1993-1996 and 1998(2)2010;
		foreach t in 20 25 30 35 40 45{

		
				qui gen evafdctanf`t'=0
				qui replace evafdctanf`t'=1 if afdctanf1993==1 & age1993<=`t'
			foreach y in 1994 1995 1996 1998 2000 2002 2004 2006 2008 2010{
				qui replace evafdctanf`t'=1 if evafdctanf`t'==0  & afdctanf`y'==1 & age`y'<=`t'
				}
				}		
				replace evafdctanf45=1 if evafdctanf20==1 |evafdctanf25==1 | evafdctanf30==1 | evafdctanf35==1 | evafdctanf40==1 
				replace evafdctanf40=1 if evafdctanf20==1 |evafdctanf25==1 | evafdctanf30==1 | evafdctanf35==1
				replace evafdctanf35=1 if evafdctanf20==1 |evafdctanf25==1 | evafdctanf30==1 
				replace evafdctanf30=1 if evafdctanf20==1 |evafdctanf25==1 
				replace evafdctanf25=1 if evafdctanf20==1 
				*drop excess variables
				drop afdcta*
				

		
		*CIGARETTES AND ALCOHOL: ever report smoke/drink more than one pack/drink per day.
			foreach out in cigsperday  {
				foreach t in 20 25 30 35 40 45{
					qui gen ev`out'`t'=0
					qui replace ev`out'`t'=1 if `out'1999>=1 &  `out'1999!=. & age1999<=`t'
				
				
					
				foreach y in 2001 2003 2005 2007 2009 2011{
					qui replace ev`out'`t'=1 if   `out'`y'>=1 &  `out'`y'!=. & age`y'<=`t'
					}
					}
				qui replace ev`out'45=1 if ev`out'20==1 |ev`out'25==1 | ev`out'30==1 | ev`out'35==1 | ev`out'40==1 
				qui replace ev`out'40=1 if ev`out'20==1 |ev`out'25==1 | ev`out'30==1 | ev`out'35==1
				qui replace ev`out'35=1 if ev`out'20==1 |ev`out'25==1 | ev`out'30==1 
				qui replace ev`out'30=1 if ev`out'20==1 |ev`out'25==1 
				qui replace ev`out'25=1 if ev`out'20==1 				
				}
				
		foreach out in cigsperday  {
			forvalues p=2000(2)2010{
				qui gen `out'`p'=.
				qui gen miss_`out'`p'=.
				}
				
				
			forvalues p=1996/1998{
				qui gen `out'`p'=.
				qui gen miss_`out'`p'=.
				}
				
				
		foreach t in 20 25 30 35 40 45{
				qui gen `out'`t'=.
				qui gen m`out'`t'=.
				qui gen miss_`out'`t'=.
			forvalues y=2000/2011{
				local i1=`y'
				local i2=`y'-1
				local i3=`y'-2
				local i4=`y'-3
				local i5=`y'-4
				qui egen temp=rowtotal(`out'`i1' `out'`i2' `out'`i3' `out'`i4' `out'`i5') if age`y'==`t'		
				qui replace `out'`t'= temp>=1 if age`y'==`t'
				qui egen temp2=rowmean(`out'`i1' `out'`i2' `out'`i3' `out'`i4' `out'`i5') if age`y'==`t'	
				qui replace m`out'`t'= temp2 if age`y'==`t'
				
				drop temp temp2
				
				qui egen temp=rowtotal(miss_`out'`i1' miss_`out'`i2' miss_`out'`i3' miss_`out'`i4' miss_`out'`i5') if age`y'==`t'		
				qui replace miss_`out'`t'= temp if age`y'==`t'
				drop temp					
				
				}
				}		
				}			
								
		*SelfReportedHealth ever NOT GOOD.
				local range1 "1985/1997"
				local range2 "1999(2)2011"		
				foreach t in 20 25 30 35 40 45{
				

				
					qui gen evsrhealthbad`t'=.
					qui replace evsrhealthbad`t'=1 if srhealthbad1984==1 &  srhealthbad1984!=. & age1984<=`t'
				forvalues y =`range1'{
					qui replace evsrhealthbad`t'=1 if   srhealthbad`y'==1 &  srhealthbad`y'!=. & age`y'<=`t'
					}
				forvalues y =`range2'{
					qui replace evsrhealthbad`t'=1 if   srhealthbad`y'==1 &  srhealthbad`y'!=. & age`y'<=`t'
						}
						}	
				qui replace evsrhealthbad45=1 if evsrhealthbad20==1 |evsrhealthbad25==1 | evsrhealthbad30==1 | evsrhealthbad35==1 | evsrhealthbad40==1 
				qui replace evsrhealthbad40=1 if evsrhealthbad20==1 |evsrhealthbad25==1 | evsrhealthbad30==1 | evsrhealthbad35==1
				qui replace evsrhealthbad35=1 if evsrhealthbad20==1 |evsrhealthbad25==1 | evsrhealthbad30==1 
				qui replace evsrhealthbad30=1 if evsrhealthbad20==1 |evsrhealthbad25==1 
				qui replace evsrhealthbad25=1 if evsrhealthbad20==1 
				*drop excess variables
			
		forvalues p=1998(2)2010{
				qui gen srhealthbad`p'=.
				qui gen miss_srhealthbad`p'=.
				}
		foreach t in 20 25 30 35 40 45{
				qui gen srhealthbad`t'=.
				qui gen msrhealthbad`t'=.
				qui gen miss_srhealthbad`t'=.
			forvalues y=1989/2011{
				local i1=`y'
				local i2=`y'-1
				local i3=`y'-2
				local i4=`y'-3
				local i5=`y'-4
				qui egen temp=rowtotal(srhealthbad`i1' srhealthbad`i2' srhealthbad`i3' srhealthbad`i4' srhealthbad`i5') if age`y'==`t'		
				qui replace srhealthbad`t'= temp>=1 if age`y'==`t'
				qui egen temp2=rowmean(srhealthbad`i1' srhealthbad`i2' srhealthbad`i3' srhealthbad`i4' srhealthbad`i5') if age`y'==`t'	
				qui replace msrhealthbad`t'= temp2 if age`y'==`t'
				
				drop temp temp2
				
				qui egen temp=rowtotal(miss_srhealthbad`i1' miss_srhealthbad`i2' miss_srhealthbad`i3' miss_srhealthbad`i4' miss_srhealthbad`i5') if age`y'==`t'		
				qui replace miss_srhealthbad`t'= temp if age`y'==`t'
				drop temp				
				
				}
				}					
			
			
		*EVER OWN A HOME
				local range1 "1969/1997"
				local range2 "1999(2)2011"	
				foreach t in 20 25 30 35 40 45{
					qui gen evownshome`t'=.
					qui replace evownshome`t'=(ownhome1968==0 &  ownhome1968!=.) if   age1968<=`t' & age1968>`t'-5
				forvalues y =`range1'{
					qui replace evownshome`t'=(ownhome`y'==0 &  ownhome`y'!=.) if  age`y'<=`t' & age`y'>`t'-5
					}
				forvalues y =`range2'{
					qui replace evownshome`t'=(ownhome`y'==0 &  ownhome`y'!=.) if age`y'<=`t' & age`y'>`t'-5
					}					
					}		
		forvalues p=1998(2)2010{
				qui gen ownhome`p'=.
				qui gen miss_ownhome`p'=.
				}
		foreach t in 20 25 30 35 40 45{
				qui gen ownhome`t'=.
				qui gen mownhome`t'=.
				qui gen miss_ownhome`t'=.
			forvalues y=1980/2011{
				local i1=`y'
				local i2=`y'-1
				local i3=`y'-2
				local i4=`y'-3
				local i5=`y'-4
				qui egen temp=rowtotal(ownhome`i1' ownhome`i2' ownhome`i3' ownhome`i4' ownhome`i5') if age`y'==`t'		
				qui replace ownhome`t'= temp>=1 if age`y'==`t'
				qui egen temp2=rowmean(ownhome`i1' ownhome`i2' ownhome`i3' ownhome`i4' ownhome`i5') if age`y'==`t'	
				qui replace mownhome`t'= temp2 if age`y'==`t'
				qui egen temp3=rowtotal(miss_ownhome`i1' miss_ownhome`i2' miss_ownhome`i3' miss_ownhome`i4' miss_ownhome`i5') if age`y'==`t'		
				qui replace miss_ownhome`t'= temp3 if age`y'==`t'				
				drop temp temp2 temp3
				}
				}		
									
					*drop excess variables

			*AVERAGE BMI IN YEAR RANGE
				*first create missing bmis for ease of coding
				forvalues y=2000(2)2010{
						gen bmi`y'=.
						}
				forvalues y=1986/1998{
						gen bmi`y'=.
						}						
				foreach t in 20 25 30 35 40 45{
					qui gen mbmi`t'=.
					forvalues z= 1950/1990{ 
						local z1=`z'+`t'
						local z2=`z1'-1
						local z3=`z1'-2
						local z4=`z1'-3
						local z5=`z1'-4
						if `z5'>=1994 &	 `z5'<=2007 	qui egen temp=rowmean(bmi`z1' bmi`z2' bmi`z3' bmi`z4' bmi`z5') if birthyear==`z' 
							if `z5'>=1994 & `z5'<=2007 	qui replace mbmi`t'=temp if temp!=.
							cap drop temp
						if `z5'>=1994 & `z5'==2008  	qui egen temp=rowmean(		 bmi`z2' bmi`z3' bmi`z4' bmi`z5') if birthyear==`z' 
							if `z5'>=1994 & `z5'==2008 	qui replace mbmi`t'=temp if temp!=.
							cap drop temp		
						if `z5'>=1994 & `z5'==2009  	qui egen temp=rowmean(				 bmi`z3' bmi`z4' bmi`z5') if birthyear==`z' 
							if `z5'>=1994 & `z5'==2009  qui replace mbmi`t'=temp if temp!=.
							cap drop temp
						if `z5'>=1994 & `z5'==2010  	qui egen temp=rowmean(						 bmi`z4' bmi`z5') if birthyear==`z' 
							if `z5'>=1994 & `z5'==2010 	qui replace mbmi`t'=temp if temp!=.							
							cap drop temp
						if `z5'>=1994 & `z5'==2011  	qui egen temp=rowmean(								 bmi`z5') if birthyear==`z' 
							if `z5'>=1994 & `z5'==2011 	qui replace mbmi`t'=temp if temp!=.
							cap drop temp
						}
						}	

				*drop yearly variables		
						drop bmi*
				
	
	***************************************************************
	**** AFDC/TANF AT AGE 3/4/5
	**************************************************************
	
	gen anyafdc1968 = .
		
	forvalues year = 1969/1985 {
		sum f_tanfamt`year'
		gen anyafdc`year' = f_tanfamt`year' >0
		replace anyafdc`year' = . if  f_tanfamt`year' ==.
		tab anyafdc`year', m
	}

	forvalues year = 1986/1993 {
		egen f_tanfamt`year' = rowtotal(h_tanfamt`year' w_tanfamt`year')
		sum f_tanfamt`year'
		gen anyafdc`year' = f_tanfamt`year' >0
		replace anyafdc`year' = . if  f_tanfamt`year' ==.
		tab anyafdc`year', m
	}

	foreach year of numlist 1994 1995 1996 1997 1999 2001 2003 2005 2007 2009 2011 {
		sum f_tanf`year'
		gen anyafdc`year' = f_tanf`year' ==1
		replace anyafdc`year' = . if  f_tanf`year' == .
		replace anyafdc`year' = . if  f_tanf`year' == 0|f_tanf`year' == 9 |f_tanf`year' == 8
		tab anyafdc`year', m		
	}
	
	qui gen afdcage2= . 		
	qui gen afdcage3= . 		
	qui gen afdcage4= . 		
	qui gen afdcage5= . 		
		
	forvalues x = 8/35 {
	local e = 1995-`x' + 3 // 2 yrs old
	local f = 1995-`x' + 4 // 3 yr old
	local g = 1995-`x' + 5 // 4 yr old
	local h = 1995-`x' + 6 // 5 yr old

	if `x'<=30	qui replace afdcage2=1 if anyafdc`e'==1 & birthyear==1995-`x' 
	if `x'<=30	qui replace afdcage2=0 if anyafdc`e'==0 & birthyear==1995-`x' 

	if `x'<=31	qui replace afdcage3=1 if anyafdc`f'==1 & birthyear==1995-`x' 
	if `x'<=31	qui replace afdcage3=0 if anyafdc`f'==0 & birthyear==1995-`x' 

	if `x'<=32	qui replace afdcage4=1 if anyafdc`g'==1 & birthyear==1995-`x' 
	if `x'<=32	qui replace afdcage4=0 if anyafdc`g'==0 & birthyear==1995-`x' 
	
	if `x'<=33	qui replace afdcage5=1 if anyafdc`h'==1 & birthyear==1995-`x' 
	if `x'<=33	qui replace afdcage5=0 if anyafdc`h'==0 & birthyear==1995-`x' 
	
		}	
	

replace evafdctanf40 = . if birthyear >1976 // not 35 by 2011 so can not be included in 35-40 index

// Use crime only if age 16+ in 1995
replace crime = . if birthyear >1979
replace nocrime = . if birthyear >1979

// Identify sample of interest
// Basic demographics
gen black = black5
gen white = white5
gen age=age1995
la var age "Age in 1995"
la var black "Fraction African-American"
la var earncpi_2325 "Avg. Earnings age 23-25 (CPI adjusted)"

gen basesample = (preschool!=. & headstart!=. & (black==1 | white ==1) & hsgrad!=. & college!=. & latino !=1) // black or white and not missing key variables

// Identify siblings of those born b/w 1966 & 1977
replace mom_unique = . if mom_unique ==0

// Identify siblings of those born between 1966 & 1987
gen ageflag = 1 if birthyear<=1987 & birthyear>=1966 
bysort mom_unique: egen agesibs = mean(ageflag) //all siblings of individuals in new age sample
replace agesibs =. if birthyear>1987 & birthyear !=. // don't keep siblings that are too young to be 23 by 2011 (no interesting outcomes)
replace agesibs =. if mom_unique ==. & (birthyear>1987 | birthyear<1966) // only keep siblings if they are actually siblings 

// Main sample - non-missing key variables + sibling of GTC sample
gen main_sample = (preschool!=. & headstart!=. & (black==1 | white ==1) ///
					& hsgrad!=. & college!=. & latino !=1 & agesibs !=.)

// Flag to zero out Head Start for those that we are sure are born before 1961
// Table with counts of head start by birth year
label define hslabel 0 "No Head Start" 1 "Head Start" , replace
label values headstart hslabel

// Look at variance in birthyear with "Alternative Birth Years"
gen birthyr1995 = birthyear
qui gen birthyearalt1 =.
qui gen birthyearalt2 =.
qui gen birthyearalt3 =.
foreach x of numlist 1983/1997 1999 2001 2003 2005 2007 2009 2011 {
	qui replace birthyearalt1 = birthyr`x' if birthyr`x'!= birthyear & birthyr`x'!=0 & birthyr`x'!=9999 & birthyearalt1==.
	qui replace birthyearalt1 = . if abs(`x'-age`x'-birthyr`x')>2
	qui replace birthyearalt2 = birthyr`x' if birthyr`x'!= birthyear & birthyr`x'!= birthyearalt1 & birthyr`x'>0 & birthyr`x'<9999 & birthyearalt2==.
	qui replace birthyearalt2 = . if abs(`x'-age`x'-birthyr`x')>2
	qui replace birthyearalt3 = birthyr`x' if birthyr`x'!= birthyear & birthyr`x'!= birthyearalt1 & birthyr`x'!= birthyearalt2 ///
	& birthyr`x'>0 & birthyr`x'<9999 & birthyearalt3
	qui replace birthyearalt3 = . if abs(`x'-age`x'-birthyr`x')>2
}

// Define drop question = 1 if we should zero out head start based on being born in 1961 or prior
gen dropquestion = 1 if birthyear<=1961 & (birthyearalt1>1961 | birthyearalt2>1961| birthyearalt3>1961)
replace dropquestion = 0 if birthyear<=1961 & (birthyearalt1<=1961|birthyearalt1 ==.) & (birthyearalt2<=1961|birthyearalt2 ==.) & (birthyearalt3<=1961|birthyearalt3 ==.)
gen drop = 1 if birthyear<=1961
replace drop = 0 if birthyear>1961 & birthyear !=.
drop birthyr*

/**************************************************************************
CREATE INDICES OF OUTCOMES -
1. Health outcomes 25-30 yrs old
//cigarettes, self-reported bad health, BMI
2. Health outcomes 35-40 yrs old
3. Economic outcomes 25-30 yrs old
foodstamps, afdc/tanf,  earnings, employed, education
4. Economic outcomes for 35-40 yrs old
see above + owns home

--> higher health index = bad
--> higher economic index = good
evafdctanf30 mearn30_cpi memp30 hsgrad college
evafdctanf40  mearn40_cpi   memp40 mownhome40 hsgrad college
 mbmi30 mcigsperday30 msrhealthbad30
 mbmi40 mcigsperday40 msrhealthbad40
***************************************************************************/

// Take means of outcomes for those that are in our sample have pre-school, headstart ==0 & are black or white, born after 1966
gen flag_valid = 0
gen flag_validhlth30 = 0
gen flag_validhlth40 = 0
gen flag_validecn30 = 0
gen flag_validecn40 = 0

local outcomes evafdctanf30 lnmearn30_cpi_nozero memp30 hsgrad college fin_college  evafdctanf40  lnmearn40_cpi_nozero  memp40 mownhome40 mbmi30 mcigsperday30 msrhealthbad30 mbmi40 mcigsperday40 msrhealthbad40
local hlth30 mbmi30 mcigsperday30 msrhealthbad30
local hlth40 mbmi40 mcigsperday40 msrhealthbad40
local ecn30 evafdctanf30 lnmearn30_cpi_nozero memp30 hsgrad college	fin_college
local ecn40 evafdctanf40  lnmearn40_cpi_nozero  memp40 mownhome40 hsgrad college fin_college

// Flag individuals with at least one valid outcome
foreach x of varlist `outcomes' {
	replace flag_valid =1 if `x' !=.
}

// Flag valid outcome for each index
foreach x in hlth30 hlth40 ecn30 ecn40 {
	foreach y of varlist ``x'' {
		replace flag_valid`x' =1 if `y' !=.
		if "`x'" == "hlth40"|"`x'" == "ecn40" replace flag_valid`x' = 0 if birthyear >1976 // not 35 by 2011 so can not be included in 35-40 index
	}
}

// Label control variables/outcomes
la var momed2 "Mother's yrs education"
la var momhs2 "Fraction whose mother completed hs"
la var daded2 "Father's yrs education"
la var dadhs2 "Fraction whose father completed hs"
la var cinc "Family income (age 3-6) (CPI adjusted)"
la var cincage0 "Family income, age 0 (CPI adjusted)"
la var cincage1 "Family income, age 1 (CPI adjusted)"
la var cincage2 "Family income, age 2 (CPI adjusted)"
la var momempage0 "Mother employed, age 0"
la var momempage1 "Mother employed, age 1"
la var momempage2 "Mother employed, age 2"
la var lbw "Fraction low birth weight"
la var eldest "Fraction eldest child in family"
la var hsgrad "Fraction completed hs"
la var college "Fraction attended some college"
la var fin_college "Fraction completed college"
la var female "Fraction female"
la var crime "Fraction booked/charged with crime"
la var nocrime "Fraction not booked/charged with crime"
la var headstart "Head Start"
la var preschool "Other preschool"

// New outcome variables
foreach a in 30 40 {
	la var mfoodstamp`a' "Fraction of last 5 yrs on Food Stamps/SNAP, age `a'"
	la var evafdctanf`a' "Ever on AFDC/TANF by age `a'"
	la var mcigsperday`a' "Fraction of last 5 yrs smoked at least 1 cigarette/day, age `a'"
	la var msrhealthbad`a' "Fraction of last 5 yrs reported fair/poor health, age `a'"
	la var mbmi`a' "Mean BMI in last 5 years, age `a'"
	la var mearn`a'_cpi "mean earnings in last 5 years, age `a'"
	la var mearn`a'_cpi_nozero "mean earnings in last 5 years, age `a'"
	la var lnmearn`a'_cpi_nozero "ln(mean earnings in last 5 years), age `a'"
	la var memp`a' "Fraction of last 5 yrs with positive earnings, age `a'"
	la var munemp`a' "Fraction of last 5 yrs ever unemployed, age `a'"
	la var mirp`a' "Mean Inc. Rel. Pov. in last 5 years, age `a'"
}

la var mownhome40 "Fraction of last 5 yrs owned home, age 40"

drop hsgrad*7* hsgrad*8* hsgrad*9* college*7* college*8* college*9* h_marital* //need to drop variables because of limitations on stata on local computer
keep if basesample ==1

// Define samples

replace headstart =0 if birthyear<=1961 & dropquestion==0
cap replace preschool =1 if birthyear<=1961 & dropquestion==0 & orig_headstart ==1

replace hsgrad = hsgrad2 if hsgrad2 !=. // use all years of data to determine if graduated from hs/college
replace college = college2 if college2 !=. // use all years of data to determine if graduated from hs/college

// Determine birthorder before drop
sort mom_unique birthyear
bysort mom_unique: gen birthorder= _n
replace birthorder= . if mom_unique ==.
keep if agesibs ==1 // more expansive age group + more expansive siblings group

// Identify siblings for sibling sample
bysort mom_unique: gen numsibs =_N
bysort mom_unique: gen sib =(_N>1)
replace numsibs=. if mom_unique ==.
replace sib=0 if mom_unique==.

// Identify age spacing between siblings
sort mom_unique birthyear
bysort mom_unique: gen birthorder_drop = _n
replace birthorder_drop= . if mom_unique ==.
gen yrs_to_next_sib = birthyear[_n] - birthyear[_n+1] // difference between your birthyear and the birthyear of the sibling closest, but younger to you
replace yrs_to_next_sib = . if mom_unique[_n] != mom_unique[_n+1]
gen yrs_to_prev_sib = birthyear[_n] - birthyear[_n-1] // difference between your birthyear and the birthyear of the sibling closest, but older than you
replace yrs_to_prev_sib = . if mom_unique[_n] != mom_unique[_n-1]
gen min_yrs_to_sib = min(yrs_to_prev_sib,yrs_to_next_sib )

forvalues space = 1/3 {
	gen birth_space_less_`space' = abs(min_yrs_to_sib)<=3
	replace birth_space_less_`space' = . if sib==0
}

// Merge on CPS weights
merge m:1 black female age using $data/cps_1995weight, keepusing (black female age cpswgt_pop2)
keep if _merge==3
by black female age, sort: gen total=_N
gen weight2 = cpswgt_pop2/total 

/**************************************************************************
CREATE INDICES OF OUTCOMES -
1. Health outcomes 25-30 yrs old
//cigarettes, self-reported bad health, BMI
2. Health outcomes 35-40 yrs old
3. Economic outcomes 25-30 yrs old
foodstamps, afdc/tanf,  earnings, employed, unemployed education
4. Economic outcomes for 35-40 yrs old
see above + owns home

--> higher health index = bad
--> higher economic index = good
evafdctanf30 mfoodstamp30 mearn30_cpi memp30 hsgrad college munemp30
evafdctanf40  mfoodstamp40 mearn40_cpi memp40 mownhome40 hsgrad college munemp40
 mbmi30 mcigsperday30 msrhealthbad30
 mbmi40 mcigsperday40 msrhealthbad40
***************************************************************************/

// Transform unemployed, afdc/tanf, foodstamp to negative since it decreases economic wellbeing
foreach a in 30 40 {
	foreach q in munemp`a' mfoodstamp`a' evafdctanf`a' mcigsperday`a' msrhealthbad`a'{
        gen neg_`q' = (1-`q')
	}
	foreach q in mbmi`a'{
        gen neg_`q' = -1*`q'
	}
	la var neg_munemp`a' "Fraction of last 5 yrs no unemployment, age `a'"
	la var neg_mfoodstamp`a' "Fraction of last 5 yrs not on Food Stamps/SNAP, age `a'"
	la var neg_evafdctanf`a' "Never on AFDC/TANF by age `a'"
	la var neg_mbmi`a' "Negative BMI, age `a'"
	la var neg_mcigsperday`a' "Fraction of last 5 yrs smoked less than 1 cigarette/day, age `a'"
	la var neg_msrhealthbad`a' "Fraction of last 5 yrs reported good or better health, age `a'"
}

local outcomes neg_evafdctanf30 neg_mfoodstamp30 lnmearn30_cpi_nozero memp30 neg_munemp30 hsgrad college fin_college neg_evafdctanf40 neg_mfoodstamp40  lnmearn40_cpi_nozero memp40 neg_munemp40 mownhome40 neg_mbmi30 neg_mcigsperday30 neg_msrhealthbad30 neg_mbmi40 neg_mcigsperday40 neg_msrhealthbad40 mirp30 mirp40

foreach v of varlist `outcomes' {

	// Control mean for z-score
	qui sum `v'	if main_sample ==1 & headstart ==0 & preschool == 0 [aweight=weight2] // use mean of those with no headstart, no preschool as proxy of "control" mean
	local zscm`v' = r(mean)
	qui sum `v'  if main_sample ==1 & headstart ==0 & preschool == 0 [aweight=weight2] // use sd of those with no headstart, no preschool as proxy of "control" sd
	local zscsd`v' = r(sd)

	// Create temporary version of variable that will be the same as variable but have imputed values for those with one valid outcome
	gen tmp`v' = `v'

	// Impute values for those with at least one valid outcome
		if "`v'" == "neg_evafdctanf30"| "`v'" == "neg_mfoodstamp30" |"`v'" == "lnmearn30_cpi_nozero" |"`v'" == "memp30" |"`v'" == "neg_munemp30" |"`v'" == "hsgrad" |"`v'" == "college" |"`v'" == "fin_college" |"`v'" == "mirp30"  ///
			local flag_valid = "flag_validecn30"
		if "`v'" == "neg_evafdctanf40" | "`v'" == "neg_mfoodstamp40"  | "`v'" == "lnmearn40_cpi_nozero" | "`v'" == "memp40" | "`v'" == "neg_munemp40" | "`v'" == "mownhome40" | "`v'" == "mirp40" ///
			local flag_valid = "flag_validecn40"
		if "`v'" == "neg_mbmi30" | "`v'" == "neg_mcigsperday30" | "`v'" == "neg_msrhealthbad30" ///
			local flag_valid = "flag_validhlth30"
		if "`v'" == "neg_mbmi40" | "`v'" == "neg_mcigsperday40" | "`v'" == "neg_msrhealthbad40" ///
			local flag_valid = "flag_validhlth40"

	qui sum `v' if preschool ==1 [aweight=weight2]
	replace tmp`v' = r(mean) if preschool ==1 & tmp`v'==. & `flag_valid' ==1
	qui sum `v' if headstart ==1 [aweight=weight2]
	replace tmp`v' = r(mean) if headstart ==1 & tmp`v'==. & `flag_valid' ==1
	qui sum `v' if headstart !=1 & preschool !=1 [aweight=weight2]
	replace tmp`v' = r(mean) if headstart !=1 & preschool !=1 & tmp`v'==. & `flag_valid' ==1

	// Create z-score for all that have at least one valid outcome
	gen zsc`v' = (tmp`v' - `zscm`v'')/`zscsd`v'' if `flag_valid' ==1

}

// Final zscore = average of all zscores - then recenter
egen zsc_econ30 = rowmean(zscneg_evafdctanf30 zscneg_mfoodstamp30 zsclnmearn30_cpi_nozero zscmemp30 zscneg_munemp30 zschsgrad zsccollege zscfin_college zscmirp30) if flag_validecn30 ==1
la var zsc_econ30 "Economic Sufficiency Index at 30"
egen zsc_econ40 = rowmean(zscneg_evafdctanf40 zscneg_mfoodstamp40 zsclnmearn40_cpi_nozero zscmemp40  zscneg_munemp40 zscmownhome40 zschsgrad zsccollege zscfin_college zscmirp40) if flag_validecn40 ==1
la var zsc_econ40 "Economic Sufficiency Index at 40"
egen zsc_health30 = rowmean(zscneg_mbmi30 zscneg_mcigsperday30 zscneg_msrhealthbad30) if flag_validhlth30 ==1
la var zsc_health30 "Good Health Index at 30"
egen zsc_health40 = rowmean(zscneg_mbmi40 zscneg_mcigsperday40 zscneg_msrhealthbad40) if flag_validhlth40 ==1
la var zsc_health40 "Good Health Index at 40"

// Get factor loadings
drop tmp*

foreach p in zsc_econ30 zsc_econ40 zsc_health30 zsc_health40 {
	// "Uncentered z-score no headstart, no preschool"
	sum `p' if preschool !=1 & headstart !=1 [aweight = weight2]
	local zscm`p' = r(mean)
	local zscsd`p' = r(sd)
	 // Recenter using controls
	replace `p' = (`p'-`zscm`p'')/`zscsd`p'' if `p' !=.
}

keep pernum id1968 headstart birthyear afdcage* black white birthyear female mom_hoh momempage* hhsize_age4 eldestchild lbw momed2 daded2 sib mom_unique fin_college weight2 preschool numsibs headstart earncpi_2325 cinc momhs2 dadhs2 age pid disabled cincage* hsgrad college nocrime evafdctanf30 mfoodstamp30 lnmearn30_cpi_nozero memp30 munemp30 mirp30 evafdctanf40 mfoodstamp40 lnmearn40_cpi_nozero memp40 munemp40 mirp40 mownhome40 neg_mcigsperday30 neg_msrhealthbad30 mbmi30 neg_mcigsperday40 neg_msrhealthbad40 mbmi40 zsc_econ30 zsc_econ40 zsc_health30 zsc_health40 birthorder_drop


// Define Head Start switching families
// Statistics on within family variation
qui xi: reg headstart i.mom_unique
	qui predict hs_devmean, residual
qui replace hs_devmean = 0 if hs_devmean <.000001 & hs_devmean>= -.000001
gen hs_switcher = (hs_devmean !=0)
replace hs_switcher = . if mom_unique ==.

drop _*

// SPLINE in income
mkspline income 4 = cinc, pctile		/* Generate income spline and save covariates */
mkspline incomeage0 4 = cincage0, pctile		/* Generate income spline and save covariates */
mkspline incomeage1 4 = cincage1, pctile		/* Generate income spline and save covariates */
mkspline incomeage2 4 = cincage2, pctile		/* Generate income spline and save covariates */

save "$data/psid_clean", replace


log close
