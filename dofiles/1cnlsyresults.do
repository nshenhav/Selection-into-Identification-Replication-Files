/*******************************************************************************							
Produce Tables 4,6 and Figure 2 (main text); Tables B10, B15, B16 from appendix.
This program defines sample eligibility variables and generates the covariates and 
outcomes used in the paper. To match the Deming (2009) results as closely as possible,
the code for the key outcomes and covariates is identical to the Deming (2009) replication file.
********************************************************************************/

** Set globals with file paths
global logs ""
global data ""
global figures ""
global tables ""

clear
capture log close
log using "$logs/1cnlsyresults.log" , replace

set mem 400m
set more off

**Install necessary packages if missing**
	ssc install povguide, replace
	ssc install estout, replace
	ssc install ivreg2, replace
	ssc install ranktest, replace
	ssc install xtivreg2, replace

use "$data/data_Deming_2008_0217.dta", clear

# delimit ;

*NLSY codes missing values as negatives - so drop all non-answers;

foreach var of varlist ChildID- hhID {;
	replace `var'=. if `var'<0;
};


**************Data construction: Sample Eligibility (From Deming, 2009) *****************************

*Rule is 5 years old by 1990, so that they will be 19 by 2004;
*Next restrict the sample to families with at least 2 age-eligible children;
*Finally restrict to families where at least one (but not all) children were in Head Start;

*I create 3 sets of variables that define preschool participation. The first simply uses   
*the age variable and excludes those with any missing data. The second substitutes the PPVT
*age (ie age at test) when the original variable is unavailable, and also substitutes past 
*or future age variables from other survey years (plus or minus 24 months). The third is   
*the most restrictive definition. It codes inconsistent or missing responses across years  
*as zeros, as well as for children for whom mothers report "3 months or less" participation
*in the program. The variables are coded HS1, HS2, HS3 and Pre1, Pre2, Pre3. In general    
*different rules have very little impact on the estimates. I use #2 in the paper.            
********************************************************************************************

*Replace missing age values with PPVT Age or age reported from the nearest survey year instead;

local years "86 88 90 92 94 96 98 100 102 104";
foreach y of local years {;
	gen Age2_Mo`y'=Age_Mo`y';
	replace Age2_Mo`y'=PPVTAge`y' if Age2_Mo`y'==.;
};

replace Age2_Mo86= Age2_Mo88-24 if  Age2_Mo86==. &  Age2_Mo88>=25 & Age2_Mo88!=.;
replace Age2_Mo88= Age2_Mo86+24 if  Age2_Mo88==. & Age2_Mo86!=.;
replace Age2_Mo88= Age2_Mo90-24 if  Age2_Mo88==. &  Age2_Mo90>=25 & Age2_Mo90!=.;
replace Age2_Mo90= Age2_Mo88+24 if  Age2_Mo90==. & Age2_Mo88!=.;
replace Age2_Mo90= Age2_Mo92-24 if  Age2_Mo90==. &  Age2_Mo92>=25 & Age2_Mo92!=.;
replace Age2_Mo92= Age2_Mo90+24 if  Age2_Mo92==. & Age2_Mo90!=.;
replace Age2_Mo92= Age2_Mo94-24 if  Age2_Mo92==. &  Age2_Mo94>=25 & Age2_Mo94!=.;
replace Age2_Mo94= Age2_Mo92+24 if  Age2_Mo94==. & Age2_Mo92!=.;
replace Age2_Mo94= Age2_Mo96-24 if  Age2_Mo94==. &  Age2_Mo96>=25 & Age2_Mo96!=.;
replace Age2_Mo96= Age2_Mo94+24 if  Age2_Mo96==. & Age2_Mo94!=.;
replace Age2_Mo96= Age2_Mo98-24 if  Age2_Mo96==. &  Age2_Mo98>=25 & Age2_Mo98!=.;
replace Age2_Mo98= Age2_Mo96+24 if  Age2_Mo98==. & Age2_Mo96!=.;
replace Age2_Mo98= Age2_Mo100-24 if  Age2_Mo98==. &  Age2_Mo100>=25 & Age2_Mo100!=.;
replace Age2_Mo100= Age2_Mo98+24 if  Age2_Mo100==. & Age2_Mo98!=.;
replace Age2_Mo100= Age2_Mo102-24 if  Age2_Mo100==. &  Age2_Mo102>=25 & Age2_Mo102!=.;
replace Age2_Mo102= Age2_Mo100+24 if  Age2_Mo102==. & Age2_Mo100!=.;
replace Age2_Mo102= Age2_Mo104-24 if  Age2_Mo102==. &  Age2_Mo104>=25 & Age2_Mo104!=.;
replace Age2_Mo104= Age2_Mo102+24 if  Age2_Mo104==. & Age2_Mo102!=.;

*Create age in years (rather than months) variable*;

foreach x of local years {;
	gen Age_Yr`x'=0 if Age_Mo`x'<12;
	gen Age2_Yr`x'=0 if Age2_Mo`x'<12;
};
foreach x of local years {;
	forvalues y = 1(1)37 {;
		replace Age_Yr`x'=`y' if Age_Mo`x'>=12*`y' & Age_Mo`x'<(12*`y')+12;
		replace Age2_Yr`x'=`y' if Age2_Mo`x'>=12*`y' & Age2_Mo`x'<(12*`y')+12;
	};
};

*Create dummy for sample eligibility, by year*;
*Minimum of 2 siblings age 54 months or more, in each sample year*;
*Want to account in some way for the fact that people are surveyed at different times of the year*;
*But also don't want to include siblings who subsequently enroll*;
*So I chose 4 years, 6 months - these kids are almost certainly too old to enroll if they are not already in the program*;
*Estimates in the paper are not sensitive to this rule*;

foreach y in 86 88 90 {;
	bysort MotherID: egen Elig1_`y'=count(Age_Mo`y') if Age_Mo`y'>47 & Age_Mo`y'!=.;
	bysort MotherID: egen Elig2_`y'=count(Age2_Mo`y') if Age2_Mo`y'>47 & Age2_Mo`y'!=.;
	replace Elig1_`y'=0 if Elig1_`y'==1;
	replace Elig1_`y'=1 if Elig1_`y'>1 & Elig1_`y'!=.;
	replace Elig1_`y'=0 if Elig1_`y'!=1;
	replace Elig2_`y'=0 if Elig2_`y'==1;
	replace Elig2_`y'=1 if Elig2_`y'>1 & Elig2_`y'!=.;
	replace Elig2_`y'=0 if Elig2_`y'!=1;
};

*Exclude small number of kids who died prior to eligibility*;

foreach x in 86 88 90 {;
	gen byte Dead`x'=Res`x'==8;
	replace Elig1_`x'=. if Dead`x'==1;
	replace Elig2_`x'=. if Dead`x'==1;
};

drop Dead*;

*Create Head Start, Other Preschool, and No Preschool categories*;
*Create variables equal to one if respondent ever indicated enrollment in HS or other preschools in 1988-1992* ;
*Question was not asked in 1986*;

foreach y in 1 2 {;
	egen HS`y'_90=rowmax(Ever_HS88 Ever_HS90) if Elig`y'_90==1;
	egen Pre`y'_90=rowmax(Ever_Preschool88 Ever_Preschool90) if Elig`y'_90==1;

	** create head start variable that does not exclude singletons;
	egen HS`y'_all =rowmax(Ever_HS88 Ever_HS90);

	replace HS`y'_90=0 if Elig`y'_90==1 & HS`y'_90!=1;
	replace Pre`y'_90=0 if Elig`y'_90==1 & Pre`y'_90!=1;

	*I code "yes" responses to both HS and preschool as Head Start (Rule 3 does it the other way)*;

	replace Pre`y'_90=0 if HS`y'_90==1;
	gen byte None`y'_90=Elig`y'_90==1 ;
	replace None`y'_90=. if Elig`y'_90==0;
	replace None`y'_90=0 if (HS`y'_90==1 | Pre`y'_90==1);
};


*Alternative participation definition - code inconsistent responses across years as missing or zeros*;
*Also code as zero those who report being in Head Start for less than 3 months*;

foreach x in 90 {;
	foreach y in HS Pre None {;
		gen `y'3_`x'=`y'2_`x';
	};
};
replace HS3_90=0 if (Ever_HS88==1 & Ever_HS90==0) | (HowLong_HS88==1 | HowLong_HS90==1) & HS3_90!=.;
replace Pre3_90=0 if (Ever_Preschool88==1 & Ever_Preschool90==0) & Pre3_90!=.;
replace None3_90=1 if HS3_90==0 & Pre3_90==0;


*Create dummy for "fixed effects" sample - families where HS participation varies across siblings*;

foreach y in 86 88 90 {;
	bysort MotherID: egen Num1Elig`y'=count(Age_Mo`y') if Age_Mo`y'>47 & Age_Mo`y'!=.;
	bysort MotherID: egen Num2Elig`y'=count(Age2_Mo`y') if Age2_Mo`y'>47 & Age2_Mo`y'!=.;
	bysort MotherID: egen Num3Elig`y'=count(Age2_Mo`y') if Age2_Mo`y'>47 & Age2_Mo`y'!=.;

	*This creates a count of eligible siblings per household*;
};

*For each family, count the number of kids in each category (HS, Pre, None)*;
*Then set equal to missing if all kids in the family are in the same category (and thus ineligible)*;

bysort MotherID: egen temp= count(ChildID) if HS2_90==1;
replace temp=. if Num2Elig90==temp;
bysort MotherID: egen temp2= count(ChildID) if Pre2_90==1;
replace temp2=. if Num2Elig90==temp2;
bysort MotherID: egen temp3= count(ChildID) if None2_90==1;
replace temp3=. if Num2Elig90==temp3;

*Now create dummy vars with counts*;

gen byte HS2_FE90=1 if temp!=.;
gen byte Pre2_FE90=1 if temp2!=.;
gen byte None2_FE90=1 if temp3!=.;
replace HS2_FE90=0 if (Pre2_FE90==1 | None2_FE90==1);
replace Pre2_FE90=0 if (HS2_FE90==1 | None2_FE90==1);
replace None2_FE90=0 if (HS2_FE90==1 | Pre2_FE90==1);
drop temp temp2 temp3;


gen byte PreK_FE=1 if HS2_FE90==1;
replace PreK_FE=2 if Pre2_FE90==1;
replace PreK_FE=3 if None2_FE90==1;

la var HS2_FE90 "Head Start";
la var Pre2_FE90 "Other Preschool";

*END SAMPLE ELIGIBILITY SECTION*;

********************************************************************************************
*Now I create the covariates in the regressions in the rest of the paper.
********************************************************************************************
;
gen byte Hispanic=Race_Child==1;
gen byte Black=Race_Child==2;
gen byte White=Race_Child==3;
gen byte NonBlack=Race_Child!=2 & Race_Child!=.;

*Generate basic demographic variables*
*Permanent Income, Mother<HS, Mother some college, Maternal AFQT, Grandmother's Education*

**Poverty in hh;

**define number of kids in hh using birthorder plus kids born before age 5 plus 1 if no dad at home or plus 2 if dad at home;

* add on kids born by age 5;
gen birthyr = 2004 - Age2_Yr104;
sort MotherID birthyr;
bysort MotherID: gen space1 = birthyr -birthyr[_n+1];
gen space1_lt_5 = abs(space1)<5;
bysort MotherID: gen space2 = birthyr -birthyr[_n+2];
gen space2_lt_5 = abs(space2)<5;

gen hhsizewdadgma_03 = BirthOrder + space1_lt_5 + space2_lt_5 + 3;
gen hhsizewdad_03 = BirthOrder + space1_lt_5 + space2_lt_5  + 2;
gen hhsizewodad_03 = BirthOrder + space1_lt_5 + space2_lt_5 + 1;

forvalue yr = 1978/1990 {;
	povguide, gen(povdadgma`yr') famsize(hhsizewdadgma_03) year(`yr');
	povguide, gen(povdad`yr') famsize(hhsizewdad_03) year(`yr');
	povguide, gen(povnodad`yr') famsize(hhsizewodad_03) year(`yr');

};

foreach dad in dad dadgma nodad {;
	gen increlpov3_`dad'=NetFamInc78/pov`dad'1978 if Age2_Yr104==29;
	replace increlpov3_`dad'=NetFamInc79/pov`dad'1979 if Age2_Yr104==28;
	replace increlpov3_`dad'=NetFamInc80/pov`dad'1980 if Age2_Yr104==27;
	replace increlpov3_`dad'=NetFamInc81/pov`dad'1981 if Age2_Yr104==26;
	replace increlpov3_`dad'=NetFamInc82/pov`dad'1982 if Age2_Yr104==25;
	replace increlpov3_`dad'=NetFamInc83/pov`dad'1983 if Age2_Yr104==24;
	replace increlpov3_`dad'=NetFamInc84/pov`dad'1984 if Age2_Yr104==23;
	replace increlpov3_`dad'=NetFamInc85/pov`dad'1985 if Age2_Yr104==22;
	replace increlpov3_`dad'=NetFamInc86/pov`dad'1986 if Age2_Yr104==21;
	replace increlpov3_`dad'=NetFamInc87/pov`dad'1987 if Age2_Yr104==20;
	replace increlpov3_`dad'=NetFamInc88/pov`dad'1988 if Age2_Yr104==19;
	replace increlpov3_`dad'=NetFamInc89/pov`dad'1989 if Age2_Yr104==18;
	replace increlpov3_`dad'=NetFamInc90/pov`dad'1990 if Age2_Yr104==17;
};

foreach dad in dad dadgma nodad {;
	gen increlpov_0to3_`dad'=NetFamInc90/pov`dad'1990 if Age2_Yr104==14;
	egen temp1=rowmean(NetFamInc89-NetFamInc90) if Age2_Yr104==15;
	replace temp1 = temp1/pov`dad'1990;
	egen temp2=rowmean(NetFamInc88-NetFamInc90) if Age2_Yr104==16;
	replace temp2 = temp2/pov`dad'1990;
	egen temp3=rowmean(NetFamInc87-NetFamInc90) if Age2_Yr104==17;
	replace temp3 = temp3/pov`dad'1990;	
	egen temp4=rowmean(NetFamInc86-NetFamInc89) if Age2_Yr104==18;
	replace temp4 = temp4/pov`dad'1989;	
	egen temp5=rowmean(NetFamInc85-NetFamInc88) if Age2_Yr104==19;
	replace temp5 = temp3/pov`dad'1988;	
	egen temp6=rowmean(NetFamInc84-NetFamInc87) if Age2_Yr104==20;
	replace temp6 = temp3/pov`dad'1987;	
	egen temp7=rowmean(NetFamInc83-NetFamInc86) if Age2_Yr104==21;
	replace temp7 = temp3/pov`dad'1986;	
	egen temp8=rowmean(NetFamInc82-NetFamInc85) if Age2_Yr104==22;
	replace temp8 = temp3/pov`dad'1985;	
	egen temp9=rowmean(NetFamInc81-NetFamInc84) if Age2_Yr104==23;
	replace temp9 = temp3/pov`dad'1984;	
	egen temp10=rowmean(NetFamInc80-NetFamInc83) if Age2_Yr104==24;
	replace temp10 = temp3/pov`dad'1983;	
	egen temp11=rowmean(NetFamInc79-NetFamInc82) if Age2_Yr104==25;
	replace temp11 = temp3/pov`dad'1982;	
	egen temp12=rowmean(NetFamInc79-NetFamInc81) if Age2_Yr104==26;
	replace temp12 = temp3/pov`dad'1981;	
	egen temp13=rowmean(NetFamInc78-NetFamInc80) if Age2_Yr104==27;
	replace temp13 = temp3/pov`dad'1980;	
	egen temp14=rowmean(NetFamInc78-NetFamInc79) if Age2_Yr104==28;
	replace temp14 = temp3/pov`dad'1979;	
	forvalues x = 1(1)14 {;
		replace increlpov_0to3_`dad'=temp`x' if Age2_Yr104==`x'+14;
	};
	replace increlpov_0to3_`dad'=NetFamInc78 if Age2_Yr104==29;
	drop temp*;

};


*Create yearly income in constant 2004 dollars, as well as "permanent income" measure*;

replace NetFamInc78=NetFamInc78*2.82;
replace NetFamInc79=NetFamInc79*2.54;
replace NetFamInc80=NetFamInc80*2.24;
replace NetFamInc81=NetFamInc81*2.03;
replace NetFamInc82=NetFamInc82*1.90;
replace NetFamInc83=NetFamInc83*1.85;
replace NetFamInc84=NetFamInc84*1.78;
replace NetFamInc85=NetFamInc85*1.71;
replace NetFamInc86=NetFamInc86*1.68;
replace NetFamInc87=NetFamInc87*1.62;
replace NetFamInc88=NetFamInc88*1.55;
replace NetFamInc89=NetFamInc89*1.48;
replace NetFamInc90=NetFamInc90*1.41;
replace NetFamInc91=NetFamInc91*1.35;
replace NetFamInc92=NetFamInc92*1.31;
replace NetFamInc93=NetFamInc93*1.27;
replace NetFamInc95=NetFamInc95*1.21;
replace NetFamInc97=NetFamInc97*1.15;
replace NetFamInc99=NetFamInc99*1.10;
replace NetFamInc101=NetFamInc101*1.04;
egen PermInc=rowmean( NetFamInc78 NetFamInc79 NetFamInc80 NetFamInc81 NetFamInc82 NetFamInc83 NetFamInc84 
NetFamInc85 NetFamInc86 NetFamInc87 NetFamInc88 NetFamInc89 NetFamInc90 NetFamInc91 
NetFamInc92 NetFamInc93 NetFamInc95 NetFamInc97 NetFamInc99 NetFamInc101 NetFamInc103);
gen lnPermInc=ln(PermInc);
egen PermInc_std=std(PermInc), mean(0) std(1);

foreach var of varlist HighGrade_Moth* {;
	replace `var'=. if `var'==95;
};
egen MothED=rowmax( HighGrade_Moth*);
gen byte MomDropout=MothED<12;
replace MomDropout=. if MothED==.;
gen byte MomHS=MothED==12;
replace MomHS=. if MothED==.;
gen byte MomSomeColl=MothED>=13 & MothED!=.;
replace MomSomeColl=. if MothED==.;

*Create age-adjusted maternal AFQT score*;

gen AgeAFQT=AFQT_Pct81_REV;
replace AgeAFQT=AgeAFQT*(35.60881/28.79544) if Age_Mom79==14;
replace AgeAFQT=AgeAFQT*(35.60881/32.86273) if Age_Mom79==15;
replace AgeAFQT=AgeAFQT*(35.60881/32.86273) if Age_Mom79==16;
replace AgeAFQT=AgeAFQT*(35.60881/36.3544) if Age_Mom79==17;
replace AgeAFQT=AgeAFQT*(35.60881/33.45777) if Age_Mom79==18;
replace AgeAFQT=AgeAFQT*(35.60881/36.84) if Age_Mom79==19;
replace AgeAFQT=AgeAFQT*(35.60881/41.84536) if Age_Mom79==20;
replace AgeAFQT=AgeAFQT*(35.60881/40.95177) if Age_Mom79==21;
replace AgeAFQT=AgeAFQT*(35.60881/42.82069) if Age_Mom79==22;
egen AgeAFQT_std=std(AgeAFQT), mean(0) std(1);

*For a small number of mothers, AFQT score is missing - impute missings for use as a covariate later*;

impute AgeAFQT_std Black Hispanic Age_Moth_Birth, gen(impAFQT_std);

*Generate within-sibling difference covariates;

*Reside in same household, preexisting health limitation, low birthweight, Attrited from LT sample*;

*Reside in same household as mother - code zero if they did not reside with mom in any eligible year*;


forvalues x = 79(1)90 {;
	gen byte MomHH`x'=1 if Res`x'==1;
	replace MomHH`x'=0 if Res`x'!=1 & Res`x'!=.;
};
gen Res_0to3=MomHH90 if Age2_Yr104==14;
egen temp1=rowmin(MomHH89-MomHH90) if Age2_Yr104==15;
egen temp2=rowmin(MomHH88-MomHH90) if Age2_Yr104==16;
egen temp3=rowmin(MomHH87-MomHH90) if Age2_Yr104==17;
egen temp4=rowmin(MomHH86-MomHH89) if Age2_Yr104==18;
egen temp5=rowmin(MomHH85-MomHH88) if Age2_Yr104==19;
egen temp6=rowmin(MomHH84-MomHH87) if Age2_Yr104==20;
egen temp7=rowmin(MomHH83-MomHH86) if Age2_Yr104==21;
egen temp8=rowmin(MomHH82-MomHH85) if Age2_Yr104==22;
egen temp9=rowmin(MomHH81-MomHH84) if Age2_Yr104==23;
egen temp10=rowmin(MomHH80-MomHH83) if Age2_Yr104==24;
egen temp11=rowmin(MomHH79-MomHH82) if Age2_Yr104==25;
egen temp12=rowmin(MomHH79-MomHH81) if Age2_Yr104==26;
egen temp13=rowmin(MomHH79-MomHH80) if Age2_Yr104==27;
forvalues x = 1(1)13 {;
	replace Res_0to3=temp`x' if Age2_Yr104==`x'+14;
};
replace Res_0to3=MomHH79 if Age2_Yr104==28;
drop temp* MomHH*;

*Health Limitations that are reported prior to age 5*;

gen Ear86=.;
gen Blood86=.;
gen Epilepsy86=.;
local Health     "Brain 
Hyper 
Asthma 
Resp 
Speech 
Deaf 
Blind 
Disturb 
Allergy 
Crippled 
Retard 
Heart 
Nerve 
Ear 
Blood 
Epilepsy 
OtherLim";
foreach x of local Health {;
	forvalues y = 86(2)90 {;
		gen temp`x'`y'=.;
		replace temp`x'`y'=1 if `x'`y'>0 & `x'`y'!=.;
	};
	egen `x'=rowmax(temp`x'*);
	gen `x'_before=.;
	forvalues y =86(2)90  {;
		replace `x'_before=1 if temp`x'`y'==1 & Age2_Mo`y'<60;
	};
	drop temp*;
};
egen HealthCond_before=rowmax(*before);
replace HealthCond_before=0 if HealthCond_before!=1;

*Some children are too old and so pre-Head Start info is unavailable for them*;

replace HealthCond_before=. if Res_0to3==.;

*Low Birthweight*;

gen int Low_BW=BirthWeight<88;
replace Low_BW=. if BirthWeight==.;
gen int VLow_BW=BirthWeight<53;
replace VLow_BW=. if BirthWeight==.;

*Attrition - If respondent disappears from sample before age 19*;

gen Attrit=0 if YA_LastInterview==2004;
replace Attrit=1 if YA_LastInterview!=2004 & YA_LastInterview!=.;

*Count those who attrit before 2004 but after they turn 19 as being in the sample*;

replace Attrit=0 if Attrit==1 & YA_LastInterview==2002 & Age2_Yr102>=19;
replace Attrit=0 if Attrit==1 & YA_LastInterview==2000 & Age2_Yr100>=19;
foreach x in 94 96 98 {;
	replace Attrit=0 if Attrit==1 & YA_LastInterview==19`x' & Age2_Yr`x'>=19;
};

*Create dummies for estimation sample*;

gen Sample90_2=1 if HS2_FE90!=. & SampleID!=12 & Attrit==0 & Age2_Yr104>=19;

*Also include those who are 18 but have their 19th birthday in the first 7 months of 2004, for the school year*;

replace Sample90_2=1 if HS2_FE90!=. & Attrit==0 & Sample90_2!=1 & DOB_Yr_Child==1985 & DOB_Mo_Child<8;

*******************************************************************************************************
*****************  COUNT SWITCHERS AFTER DUMMY FOR ESTIMATION SAMPLE CREATED *****************************
*******************************************************************************************************

*** USE SAME SAMPLE CRITERIA EXCEPT DON'T RESTRICT TO SWITCHERS ONLY;

gen msg_Sample90_2=1 if HS2_90 !=. & SampleID!=12 & Attrit==0 & Age2_Yr104>=19;



*Focus on msg_Sample90_2 b/c Deming focuses on Sample90_2
*Also include those who are 18 but have their 19th birthday in the first 7 months of 2004, for the school year*;


replace msg_Sample90_2=1 if HS2_90 !=. & Attrit==0 & msg_Sample90_2!=1 & DOB_Yr_Child==1985 & DOB_Mo_Child<8;


*Separate kids that are in Head Start at age 3 from later*;

gen byte Three=(Age_1stHS88<=3 | Age_1stHS90<=3);
gen byte NotThree=(Age_1stHS88>3 & Age_1stHS88!=.) | (Age_1stHS90>3 & Age_1stHS90!=.);
gen HS_3=1 if Three==1;
replace HS_3=2 if NotThree==1;
replace HS_3=0 if HS_3==.;
gen PreK_FE_3=PreK_FE;
replace PreK_FE_3=0 if PreK_FE_3==1 & HS_3==1;
tabstat PermInc MomDropout MomSomeColl AgeAFQT_std Hispanic Black White, by(PreK_FE_3) s(mean semean);

*Log Income Ages 0-3, Log Income at Age 3*;

forvalues x = 78(1)90 {;
	gen Income`x'=NetFamInc`x';
};
gen Income_0to3=Income90 if Age2_Yr104==14;
egen temp1=rowmean(Income89-Income90) if Age2_Yr104==15;
egen temp2=rowmean(Income88-Income90) if Age2_Yr104==16;
egen temp3=rowmean(Income87-Income90) if Age2_Yr104==17;
egen temp4=rowmean(Income86-Income89) if Age2_Yr104==18;
egen temp5=rowmean(Income85-Income88) if Age2_Yr104==19;
egen temp6=rowmean(Income84-Income87) if Age2_Yr104==20;
egen temp7=rowmean(Income83-Income86) if Age2_Yr104==21;
egen temp8=rowmean(Income82-Income85) if Age2_Yr104==22;
egen temp9=rowmean(Income81-Income84) if Age2_Yr104==23;
egen temp10=rowmean(Income80-Income83) if Age2_Yr104==24;
egen temp11=rowmean(Income79-Income82) if Age2_Yr104==25;
egen temp12=rowmean(Income79-Income81) if Age2_Yr104==26;
egen temp13=rowmean(Income78-Income80) if Age2_Yr104==27;
egen temp14=rowmean(Income78-Income79) if Age2_Yr104==28;
forvalues x = 1(1)14 {;
	replace Income_0to3=temp`x' if Age2_Yr104==`x'+14;
};
replace Income_0to3=Income78 if Age2_Yr104==29;
drop temp* Income78-Income90;
gen LogInc_0to3=ln(Income_0to3);

gen IncAt3=NetFamInc78 if Age2_Yr104==29;
replace IncAt3=NetFamInc79 if Age2_Yr104==28;
replace IncAt3=NetFamInc80 if Age2_Yr104==27;
replace IncAt3=NetFamInc81 if Age2_Yr104==26;
replace IncAt3=NetFamInc82 if Age2_Yr104==25;
replace IncAt3=NetFamInc83 if Age2_Yr104==24;
replace IncAt3=NetFamInc84 if Age2_Yr104==23;
replace IncAt3=NetFamInc85 if Age2_Yr104==22;
replace IncAt3=NetFamInc86 if Age2_Yr104==21;
replace IncAt3=NetFamInc87 if Age2_Yr104==20;
replace IncAt3=NetFamInc88 if Age2_Yr104==19;
replace IncAt3=NetFamInc89 if Age2_Yr104==18;
replace IncAt3=NetFamInc90 if Age2_Yr104==17;

gen LogIncAt3=ln(IncAt3);


*First Born and Gender*;

gen byte FirstBorn=BirthOrder==1;
replace FirstBorn=. if BirthOrder==.;
gen byte Male=Sex_Child==1;

*PPVT score at age 3*;

gen PPVTat3=PPVT_Raw86 if (PPVTAge86>=36 & PPVTAge86<47);
replace PPVTat3=PPVT_Raw88 if (PPVTAge88>=36 & PPVTAge88<47) & PPVTat3==.;
replace PPVTat3=PPVT_Raw90 if (PPVTAge90>=36 & PPVTAge90<47) & PPVTat3==.;

*HOME score*;

egen HOME_Pct_0to3= rowmean(HOME_Pct86 HOME_Pct88) if Age2_Yr104<=19 & Age2_Yr104>=16;
egen temp1 = rowmean(HOME_Pct88 HOME_Pct90) if Age2_Yr104<=15 & Age2_Yr104>=14;
replace HOME_Pct_0to3=temp1 if Age2_Yr104<=15 & Age2_Yr104>=14;
replace HOME_Pct_0to3=HOME_Pct86 if Age2_Yr104>=20 & Age2_Yr104<=21;
drop temp1;

*Mom worked 0-3, Avg. wks. stopped work before birth (cond. on having a job), Mom hrs worked/wk 0-1*;

egen temp=rowmean(Moth_HrsWorked*Before);
gen Moth_HrsWorked_BefBirth=temp/13;
drop temp;
egen Moth_HrsWorked_0to3=rowmean(Moth_HrsWorked*Qtr);
egen Moth_HrsWorked_Avg_0to3=rowmean(Moth_HrsWorked*Avg);
egen Moth_HrsWorked_0to1=rowmean(Moth_HrsWorked_1_Avg Moth_HrsWorked_2_Avg Moth_HrsWorked_3_Avg Moth_HrsWorked_4_Avg);

*Father and/or Grandmother present, ages 0-3*;

egen Father_HH_0to3= rowmean(Father_HH90 Father_HH92 Father_HH93) if Age2_Yr104==14;
egen temp1 =rowmean(Father_HH89 Father_HH90) if Age2_Yr104==15;
egen temp2 =rowmean(Father_HH88 Father_HH89 Father_HH90) if Age2_Yr104==16;
egen temp3= rowmean(Father_HH87 Father_HH88 Father_HH89 Father_HH90) if Age2_Yr104==17;
egen temp4= rowmean(Father_HH86 Father_HH87 Father_HH88 Father_HH89) if Age2_Yr104==18;
egen temp5= rowmean(Father_HH85 Father_HH86 Father_HH87 Father_HH88) if Age2_Yr104==19;
egen temp6= rowmean(Father_HH84 Father_HH85 Father_HH86 Father_HH87) if Age2_Yr104==20;
egen temp7= rowmean(Father_HH84 Father_HH85 Father_HH86) if Age2_Yr104==21;
egen temp8= rowmean(Father_HH84 Father_HH85) if Age2_Yr104==22;
replace Father_HH_0to3=temp1 if Age2_Yr104==15 ;
replace Father_HH_0to3=temp2 if Age2_Yr104==16 ;
replace Father_HH_0to3=temp3 if Age2_Yr104==17 ;
replace Father_HH_0to3=temp4 if Age2_Yr104==18;
replace Father_HH_0to3=temp5 if Age2_Yr104==19;
replace Father_HH_0to3=temp6 if Age2_Yr104==20;
replace Father_HH_0to3=temp7 if Age2_Yr104==21;
replace Father_HH_0to3=temp8 if Age2_Yr104==22;
replace Father_HH_0to3=Father_HH84 if Age2_Yr104==23;
drop temp*;

forvalues x = 79(1)90 {;
	gen byte GMom`x'=1 if Grandmother`x'==1;
	replace GMom`x'=0 if Grandmother`x'!=1 & Grandmother`x'!=.;
};
gen GMom_0to3=GMom90 if Age2_Yr104==14;
egen temp1=rowmean(GMom89-GMom90) if Age2_Yr104==15;
egen temp2=rowmean(GMom88-GMom90) if Age2_Yr104==16;
egen temp3=rowmean(GMom87-GMom90) if Age2_Yr104==17;
egen temp4=rowmean(GMom86-GMom89) if Age2_Yr104==18;
egen temp5=rowmean(GMom85-GMom88) if Age2_Yr104==19;
egen temp6=rowmean(GMom84-GMom87) if Age2_Yr104==20;
egen temp7=rowmean(GMom83-GMom86) if Age2_Yr104==21;
egen temp8=rowmean(GMom82-GMom85) if Age2_Yr104==22;
egen temp9=rowmean(GMom81-GMom84) if Age2_Yr104==23;
egen temp10=rowmean(GMom80-GMom83) if Age2_Yr104==24;
egen temp11=rowmean(GMom79-GMom82) if Age2_Yr104==25;
egen temp12=rowmean(GMom79-GMom81) if Age2_Yr104==26;
egen temp13=rowmean(GMom79-GMom80) if Age2_Yr104==27;
forvalues x = 1(1)13 {;
	replace GMom_0to3=temp`x' if Age2_Yr104==`x'+14;
};
replace GMom_0to3=GMom79 if Age2_Yr104==28;
drop temp* GMom79-GMom90;


gen increlpov_0to3 = increlpov_0to3_nodad;
gen increlpov3 = increlpov3_nodad;

** dad only;
replace increlpov_0to3 = increlpov_0to3_dad if Father_HH_0to3 >0 & Father_HH_0to3 !=. & GMom_0to3==0;
replace increlpov3 = increlpov3_dad if Father_HH_0to3 >0  & Father_HH_0to3 !=. & GMom_0to3==0;

** gma only;
replace increlpov_0to3 = increlpov_0to3_dad if  GMom_0to3 >0 &  GMom_0to3 !=. & Father_HH_0to3==0;
replace increlpov3 = increlpov3_dad if  GMom_0to3 >0  &  GMom_0to3 !=. & Father_HH_0to3==0;

** gma and dad;
replace increlpov_0to3 = increlpov_0to3_dadgma if Father_HH_0to3 >0 & GMom_0to3>0 & Father_HH_0to3 !=. & GMom_0to3 !=.;
replace increlpov3 = increlpov3_dadgma if Father_HH_0to3 >0 & GMom_0to3>0 & Father_HH_0to3 !=. & GMom_0to3 !=.;

**HS eligible - use 200% or less of poverty b/c using averages unlike PSID (150%);
gen eligible = (increlpov_0to3 <=2) | (increlpov3 <=2) ;
replace eligible = . if increlpov_0to3==. & increlpov3==.;


*Mom Care 0-3, Relative Care 0-3, Nonrelative care 0-3*;

rename ChildCare_1stYr ChildCare_1_Yr;
rename ChildCare_Type_1stYr ChildCare_Type_1_Yr;
rename ChildCare_2ndYr ChildCare_2_Yr;
rename ChildCare_Type_2ndYr ChildCare_Type_2_Yr;
rename ChildCare_3rdYr ChildCare_3_Yr;
rename ChildCare_Type_3rdYr ChildCare_Type_3_Yr;
foreach y in 1 2 3 {;
	gen RelCare_`y'_Yr=1 if  ChildCare_Type_`y'_Yr!=. &  ChildCare_Type_`y'_Yr<=10;
	replace RelCare_`y'_Yr=0 if  ChildCare_`y'_Yr!=. & RelCare_`y'_Yr!=1;
	gen NonRelCare_`y'_Yr=1 if  ChildCare_Type_`y'_Yr!=. &  ChildCare_Type_`y'_Yr>10;
	replace NonRelCare_`y'_Yr=0 if  ChildCare_`y'_Yr!=. & NonRelCare_`y'_Yr!=1;
	gen MomCare_`y'_Yr=1 if  (RelCare_`y'_Yr==0 &  NonRelCare_`y'_Yr==0);
	replace MomCare_`y'_Yr=0 if  MomCare_`y'_Yr!=1 & ( RelCare_`y'_Yr!=. &  NonRelCare_`y'_Yr!=.);
};
egen RelCare=rowmean( RelCare*);
egen NonRelCare=rowmean( NonRelCare*);
egen MomCare=rowmean(MomCare*);

*Mom smoked, Mom drank, Breastfed, Doctor's visit in last 3 months, Dentist ever, Weight Change during preg*;

gen byte Alc_BefBirth= Freq_Alc_BefBirth>=3 & Freq_Alc_BefBirth!=.;
foreach x in Doctor Dentist {;
	egen `x'_temp= rowmean(Last_`x'86 Last_`x'88) if Age2_Yr104<=19 & Age2_Yr104>=16;
	egen temp1=rowmean(Last_`x'88 Last_`x'90) if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_temp=temp1 if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_temp=Last_`x'86 if Age2_Yr104>=20 & Age2_Yr104<=21;
	drop temp1;
};
gen Doctor_0to3=1 if Doctor_temp<=2;
replace Doctor_0to3=0 if Doctor_temp>2 & Doctor_temp!=.;
gen Dentist_0to3=1 if Dentist_temp<7;
replace Dentist_0to3=0 if Dentist_temp==7;
drop Doctor_temp Dentist_temp;

*UNCHANGED - Moth_Smoke_BefBirth, Breastfed, Moth_WeightChange*;

*Illness in 1st year, Premature birth, Birthweight, Priv Health Insurance 0-3, Medicaid 0-3*;

gen byte Premature=BornEarlyorLate==1;
replace Premature=. if BornOnTime==.;

foreach x in Insurance Medicaid {;
	egen `x'_0to3= rowmean(`x'86 `x'88) if Age2_Yr104<=19 & Age2_Yr104>=16;
	egen temp1=rowmean(`x'88 `x'90) if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_0to3=temp1 if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_0to3=`x'86 if Age2_Yr104>=20 & Age2_Yr104<=21;
	drop temp1;
};

gen logBW=ln(BirthWeight);

*Generate imputed values for missing covariate data, based on race and gender only*;

unab Covariates: Res_0to3 HealthCond_before VLow_BW logBW LogInc_0to3 LogIncAt3 FirstBorn PPVTat3 
HOME_Pct_0to3 Moth_HrsWorked_BefBirth Moth_HrsWorked_Avg_0to3 Moth_HrsWorked_0to1 
Father_HH_0to3 GMom_0to3 MomCare RelCare NonRelCare Moth_Smoke_BefBirth Alc_BefBirth 
Breastfed Doctor_0to3 Dentist_0to3 Moth_WeightChange Illness_1stYr Premature Insurance_0to3 
Medicaid_0to3;

foreach x of local Covariates {;
	impute `x' Male Black Hispanic if Sample90_2==1, gen(`x'_imp);
	gen `x'_miss=1 if `x'==. & `x'_imp!=.;
	replace `x'_miss=0 if `x'_miss!=1 & `x'_imp!=.;
};

*END COVARIATE SECTION*;

**More data construction**
;
gen headstart = HS2_90 ;
gen preschool = Pre2_90 ; 


*Count number of kids in Sample90_2;
gen temp = ChildID if Sample90_2 ==1;
bysort MotherID: egen numkids = count(temp);
tab numkids, g(child);
forvalues n = 1/7 {;
	local m = `n'-1;
	rename child`n' child`m';
};
drop temp;

//Define switchers within Sample90_2

gen temp = HS2_FE90 if Sample90_2 ==1 ;
bysort MotherID: egen mhs = mean(temp);
gen switchFE = 1 if HS2_FE90 !=mhs & Sample90_2==1;
drop temp mhs;

//Count number of kids total

bysort MotherID: gen realkids = _N;
replace realkids = . if MotherID ==.;
tab realkids, g(rchild);

** Target populations - use realkids;

gen sibfe = numkids>=2;
gen sib = realkids>=2;
gen blk = Black;
gen wht = NonBlack;
gen female = 1-Male;

gen all = 1;

gen numsibs_simp = realkids;
replace numsibs_simp = 5 if numsibs_simp >=5 & numsibs_simp !=.;


*quadratic for number of kids
;
gen numsibs_sq = numsibs_simp^2;

gen hsswitcher = . ;
gen hs_sample = HS2_90;
bysort MotherID: egen temp = mean(hs_sample);
replace hsswitcher = (temp != HS2_90);
replace hsswitcher = . if MotherID==. | temp==.;


*THIS SECTION CREATES OUTCOME VARIABLES*;

*need to create child interactions again;

**fraction that attend HS;
bys MotherID: egen numhs = total(HS2_FE90);
gen frachs = numhs/numkids;
replace frachs=1 if frachs>1;


*kids in Sample90;

gen child5plus = 1-(child0 + child1+child2+child3+child4);

foreach child in 0 1 2 3 4 5plus  {;
	gen hsfexchild`child' = HS2_FE90*child`child';
	la var hsfexchild`child' "Head Start x `child' child family";

	gen psfexchild`child' = Pre2_FE90*child`child';
	la var psfexchild`child' "Preschool x `child' child family";

	gen hsxchild`child' = headstart*child`child';
	la var hsxchild`child' "Head Start x `child' child family";

	gen psxchild`child' = preschool*child`child';
	la var psxchild`child' "Preschool x `child' child family";		

	gen hsfexchild`child'xfrac = hsfexchild`child'*frachs;
	la var hsfexchild`child'xfrac "Head Start x `child' child family x Fraction";

	gen psfexchild`child'xfrac = psfexchild`child'*frachs;
	la var psfexchild`child'xfrac "Preschool x `child' child family";	

};

gen hsfexfrac = HS2_FE90*frachs;
la var hsfexfrac "Head Start x Fraction";

*Long-Term Outcomes - High School Graduation,  Idle, Learning Disability, Poor health*;

*Code educational attainment outcomes using the recoded and cleaned variable YA_Educ, rather than other measures*;
*Other measures are sometimes only reported if the respondent answers yes or no certain screener questions*;

*HS Graduation;

gen HSGrad=YA_Educ104>=12 & YA_Educ104!=.;
replace HSGrad=. if YA_Educ104==.;

*Labor Force Participation*;
*Define "Idle" as Not in school AND zero wages in 2004 or most recent interview year*;

gen Wages=Wages104;
replace Wages=Wages102 if Wages==. & YA_LastInterview==2002;
replace Wages=Wages100 if Wages==. & YA_LastInterview==2000;
replace Wages=Wages98 if Wages==. & YA_LastInterview==1998;
replace Wages=Wages96 if Wages==. & YA_LastInterview==1996;
replace Wages=Wages94 if Wages==. & YA_LastInterview==1994;
foreach x in 104 102 100 98 96 94 {;
	gen PosWages`x'=Wages`x'>0 & Wages`x'!=.;
	replace PosWages`x'=. if Wages`x'==.;
};

*If respondent did not know wages, but did report an estimate, code as 0 for lowest estimated category and 1 for all others*;

gen PosWages=PosWages104;
replace PosWages=0 if PosWages104==. & Wages_Est104==1;
replace PosWages=1 if PosWages104==. & Wages_Est104!=. & Wages_Est104>1;	
replace PosWages=PosWages102 if PosWages==. & YA_LastInterview==2002;
replace PosWages=0 if PosWages==. & PosWages102==. & Wages_Est102==1 & YA_LastInterview==2002;
replace PosWages=1 if PosWages==. & PosWages102==. & Wages_Est102!=. & Wages_Est102>1;
replace PosWages=PosWages100 if PosWages==. & YA_LastInterview==2000;
replace PosWages=0 if PosWages==. & PosWages100==. & Wages_Est100==1 & YA_LastInterview==2000;
replace PosWages=1 if PosWages==. & PosWages100==. & Wages_Est100!=. & Wages_Est100>1;

*There is no "estimate" code for earlier years*;

replace PosWages=PosWages98 if PosWages==. & YA_LastInterview==1998;
replace PosWages=PosWages96 if PosWages==. & YA_LastInterview==1996;
replace PosWages=PosWages94 if PosWages==. & YA_LastInterview==1994;

gen InSchool=InSchool104;
replace InSchool=InSchool102 if InSchool==. & YA_LastInterview==2002;
replace InSchool=InSchool100 if InSchool==. & YA_LastInterview==2000;
replace InSchool=InSchool98 if InSchool==. & YA_LastInterview==1998;
replace InSchool=InSchool96 if InSchool==. & YA_LastInterview==1996;
replace InSchool=InSchool94 if InSchool==. & YA_LastInterview==1994;

gen Idle=1 if InSchool==0 & PosWages==0;
replace Idle=0 if Idle!=1 & InSchool!=.;

*Learning disabilities*;

*Create yearly indicators, then take the max across years to get"ever LD" measures*;

forvalues x = 86(2)100 {;
	gen tempLD`x'=LD`x';
	replace tempLD`x'=0 if HealthCond`x'!=. & tempLD`x'!=1;
};
egen LD=rowmax(tempLD*);

*Want to exclude small number of kids that were diagnosed with an LD before age 5*;

gen LD_before=.;
forvalues x =86(2)100  {;
	replace LD_before=1 if tempLD`x'==1 & Age2_Yr`x'<5;
};
replace LD=. if LD_before==1;
drop temp*;


*Poor Health - Self-reported status*;

egen HealthReport=rowmean(Health_Report*);
gen PoorHealth=1 if HealthReport<3 & HealthReport!=.;
replace PoorHealth=0 if HealthReport!=. & PoorHealth!=1;


*Coefficients on each subgroup should be multiplied by the coefficients in the wage regressions*;

gen hsel_nos = eligible if numsibs_simp !=1;
gen hs_nos =  headstart if numsibs_simp !=1;


** Group together 1 and 2 child families for p-score;

gen numsibs_group = numsibs_simp;
replace numsibs_group = 2 if numsibs_group ==1;

**********************************************
*Calculate effective observations for Table 4;
**********************************************

** For comparison;
gen temp = hsswitcher if Sample90_2 ==1;
egen totswitcher = total(temp);

** Generate family means of covariates;
foreach fcov of varlist `Covariates' {;
	bys MotherID: egen fm`fcov' = mean(`fcov') if Sample90_2==1;
};

** Get variance of Head Start residualized of family means;
reg HS2_FE90 fm*  if Sample90_2==1;
predict res if Sample90_2, r;
qui sum res if Sample90_2==1;
**note: need to adjust variance by df/N because stata gives sample variance;
gen vce_all = (r(sd))^2*(r(N)-1)/r(N) if Sample90_2==1;

** Get N_g -1;
bys MotherID: egen numfam = total(Sample90_2);
gen df = numfam -1 if numfam>0 & Sample90_2==1;

** Get variance of Head Start within families;
qui reg HS2_FE90 i.MotherID  if Sample90_2==1;
predict r1 if Sample90_2==1, r;
bys MotherID: egen sd1 = sd(r1) if Sample90_2==1;
**note: need to adjust variance by df/Nbecause stata gives sample variance;
gen vce1 = sd1^2*(df/numfam);

** Effective obs using vce of residuals;
gen vratio1 = (vce1/vce_all);		
gen temp_eff_obs1 = vratio1*df;
** only one per family;
bysort MotherID: gen eff_obs1 = temp_eff_obs1 if _n==1; 
egen toteff_obs1= total(eff_obs1);
**for comparison - weight df by variance;
bysort MotherID: gen temp_vcedf1 = vce1*df if _n==1; 
egen totvcedf1= total(temp_vcedf1);
drop temp*;

** Effective obs using vce of 2-kid families;
gen vratio1_2fam = (vce1/0.125);		
gen temp_eff_obs1 = vratio1_2fam*df;
** only one per family;
bysort MotherID: gen eff_obs1_2fam = temp_eff_obs1 if _n==1; 
egen toteff_obs1_2fam= total(eff_obs1_2fam);
drop temp*;

#delimit cr

/*********************************************************************
   End main data construction section. Now generate Fig2, Table4
 **********************************************************************/

* Figure 2: Likelihood of Being a Switcher Family Increases with Family Size and P(treatment)

preserve

drop numkids

keep if msg_Sample90_2== 1 & Age2_Mo90 >47 & Age2_Mo90 !=.

bysort MotherID: egen numkids = count(ChildID)
replace numkids = . if MotherID ==.  

// indicator for switching

gen hsswitcher2 = . 

**restrict to msg_Sample
gen tempHS = HS2_90 if msg_Sample90_2== 1
bysort MotherID: egen temp2 = mean(tempHS)

replace hsswitcher2 = (temp2 != HS2_90)
replace hsswitcher2 = . if MotherID==. | msg_Sample90_2 !=1
drop tempHS


collapse hsswitcher2 HS2_90 if msg_Sample90_2==1, by(numkids) // no weights used in estimation

tostring numkids, g(numkids_text)
replace numkids_text = numkids_text + " kids"

gr tw scatter  hsswitcher2 HS2_90 if numkids ==2, mcolor(green) msize(medlarge) mlabel(numkids_text) mlabsize(medium) || ///
	scatter hsswitcher2 HS2_90 if numkids ==3, mcolor(red) msize(medium) mlabel(numkids_text) mlabsize(medium) || ///
	scatter hsswitcher2 HS2_90 if numkids ==4, mcolor(purple) msize(medium) mlabel(numkids_text) mlabsize(medium)|| ///
	scatter hsswitcher2 HS2_90 if numkids ==5, mcolor(orange) msize(medium) mlabel(numkids_text) mlabsize(medium) ///
	xtitle("P(Head Start)") ytitle("P(Switch)") xlabel(0(0.2)1) ylabel(0(0.2)1)  scheme(s1mono) legend(off) 

gr export "$figures/figure2b_cnlsy.pdf",replace

restore


/*************************************
				TABLE 4
 **************************************/


*Overall*
eststo clear
unab Covariates: *_imp *_miss
xtset MotherID

foreach o in LD HSGrad Idle  {

	eststo int`o': xi: xtreg `o' hsfexchild2 hsfexchild3 hsfexchild4 hsfexchild5plus ///
		psfexchild* Male i.Age2_Yr104 `Covariates' if Sample90_2==1, fe vce(cluster MotherID)


	*Add effective observations using denominator = variance of HS net of group variables
	qui sum toteff_obs1
	estadd scalar effobshs = r(mean)

	*Add effective observations using denominator = variance of 2 person families
	qui sum toteff_obs1_2fam
	estadd scalar effobshs_2fam = r(mean)

	*Add Number of HS switchers
	qui sum totswitcher
	estadd scalar numswitch = r(mean)

	*Interaction with fraction that attend Head Start

	foreach child in 1 2 3 4 5plus  {

		*weights
		qui sum child`child' 
		local child`child'wt = r(mean)
		qui sum child`child' if sibfe==1
		local child`child'sibwt = r(mean)

		qui sum child`child' if switchFE==1
		local child`child'switchwt = r(mean)

		*probability of head start for each family size
		qui sum  HS2_all if child`child'==1 & sibfe==1
		local pr_hs_child`child' = r(mean)

		qui sum HS2_FE90 if child`child'==1 & switchFE==1
		local pr_hs_child`child'_sw = r(mean)
	}

	foreach weight in sib switch {
		if "`weight'" == "sib" local end ""
		if "`weight'" == "switch" local end "_sw"				

		// Weighted average of coefficients.  Weights = distribution among sibs or switchers
		cap lincom 	hsfexchild2*`pr_hs_child2`end''*(1-`pr_hs_child2`end'')*`child2`weight'wt' + ///
			hsfexchild3*`pr_hs_child3`end''*(1-`pr_hs_child3`end'')*`child3`weight'wt' + ///
			hsfexchild4*`pr_hs_child4`end''*(1-`pr_hs_child4`end'')*`child4`weight'wt' + ///
			hsfexchild5plus*`pr_hs_child5plus`end''*(1-`pr_hs_child5plus`end'')*`child5plus`weight'wt'

		// Create regression weights for 2+ families (Angrist and Pischke formula in MHE page 75)
		cap local numerator = r(estimate)
		cap local denominator = `pr_hs_child2`end''*(1-`pr_hs_child2`end'')*`child2`weight'wt' + ///
			`pr_hs_child3`end''*(1-`pr_hs_child3`end'')*`child3`weight'wt' + ///
			`pr_hs_child4`end''*(1-`pr_hs_child4`end'')*`child4`weight'wt' + ///
			`pr_hs_child5plus`end''*(1-`pr_hs_child5plus`end'')*`child5plus`weight'wt'


		cap local ap_wtd_coeff`weight' = `numerator'/`denominator'

	}

	estadd scalar ap_wtd_coeffsib = `ap_wtd_coeffsib'
	estadd scalar ap_wtd_coeffswitch = `ap_wtd_coeffswitch'

}


esttab intHSGrad intIdle intLD using "${tables}/table4_cnlsy.tex", replace keep(hsfe*) ///
	se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
	stats(N  ap_wtd_coeffsib ap_wtd_coeffswitch effobshs effobshs_2fam numswitch, /// 
	labels("Observations"  "Siblings" "Switchers" "Effective Obs. (CX Indivs.)" "Effective Obs. (Indivs. 2-Person Fams)"  "Head Start Switchers") fmt(0 3 3 1 1 0 3 3 3 1 ))  ///
	mtitle("HS Grad" "Idle" "Learn. Disab.") ///
	label  varlabels( , blist(hsfexchild2 "\midrule \it{\underline{Interaction}} \\ "))


tempfile demingFinal
save `demingFinal', replace



/*****************************************************************************
		CREATE PSCORE FOR REWEIGHTING BETAS
****************************************************************************/


use `demingFinal', clear

unab Covariates2: Res_0to3 HealthCond_before logBW LogInc_0to3 LogIncAt3 ///
	FirstBorn Male Age2_Yr104 HOME_Pct_0to3 Moth_HrsWorked_BefBirth ///
	Moth_HrsWorked_0to1 Father_HH_0to3 GMom_0to3 MomCare RelCare NonRelCare /// 
	Moth_Smoke_BefBirth Alc_BefBirth Breastfed Doctor_0to3 Dentist_0to3 Moth_WeightChange /// 
	Illness_1stYr Premature Insurance_0to3 Medicaid_0to3 /// 
	Moth_HrsWorked_Avg_0to3 



**imputation that includes non-sample90
foreach x of local Covariates2 {
	impute `x' Male Black Hispanic , gen(`x'_impmsg)

}


local icov "Moth_HrsWorked_BefBirth_impmsg Moth_HrsWorked_0to1_impmsg Moth_HrsWorked_Avg_0to3_impmsg LogInc_0to3_impmsg LogIncAt3_impmsg MothED MomHS MomSomeColl Male"


foreach sample in sib headstart eligible hsel_nos hs_nos{ 
	
	** weight for post-regression weighting (no BFE adjustment) - divideby pr(switch)instead of pr(owncat)
	gen lscwt_`sample'_grpall = .

	**group of four - target or switcher or both
	** Sample90_2 already limits to switchers basically...
	cap drop t_sw t_sw*
	gen t_sw = 1 if `sample' ==1 & hsswitcher ==1   
	replace t_sw = 2 if `sample' ==1 & hsswitcher !=1  
	replace t_sw = 3 if `sample' !=1 & hsswitcher ==1  
	replace t_sw = 4 if `sample' !=1 & hsswitcher !=1 

	** indicator for which of the "in the target" categories we have
	local cat1and2 0 
	local cat1only 0
	local cat2only 0
	local cat1and3 0 
	local cat3only 0
	
** 1. Make list of predicted variables -- code is dynamic in case there aren't four categories, but if you know how many categories there are you can simplify this by setting predlistgrp with the names of the variables you want to predict from mlogit

	qui tab t_sw if all==1 & (t_sw ==1 | t_sw ==2)
	local rows12 = r(r) 
	if `rows12'==2 local cat1and2 = 1	
	qui tab t_sw if t_sw ==1  & all==1
	local rows1 = r(r)	
	if `rows1' ==1 & `rows12'==1 local cat1only =1
	if `rows1' <1 & `rows12'==1 local cat2only =1

	** Figure out which types of switchers we have
	qui tab t_sw if all==1 & (t_sw ==1 | t_sw ==3)
	local rows13 = r(r) 
	if `rows13'==2 local cat1and3 = 1
	if `rows1' <1 & `rows13'==1 local cat3only =1

	** Figure out which types of switchers we have
	qui tab t_sw if t_sw <=3 & all==1
	local rows123 = r(r) 

	**dummies for group of 4 category
	tab t_sw if all==1, g(t_sw)
	local maxcat = r(r)
	
	** Create a list of 1-4 variables to be predicted from mlogit (# of variables depends on the number of categories in t_sw)
	
	* Local with list of variables to be predicted
	local predlistgrp ""

	forvalues cat = 1/`maxcat' {		
		local predlistgrp `predlistgrp' lsc_`sample'_grpall`cat'			
		}
	
	qui tab t_sw if t_sw<=2 // number of target groups
	local numtarget = r(r)


** 2. Run mlogit to get predicted probabilities
	mlogit t_sw i.numsibs_group  `icov' if  all==1, iterate(50)
	
	predict `predlistgrp' if all ==1  & e(sample)==1 // don't predict if dropped from estimation

	// set probability = own status if dropped from estimation
	forvalues cat = 1/`maxcat' {
		replace lsc_`sample'_grpall`cat' = 1 if e(sample) == 0 & all==1 & t_sw`cat'==1
		replace lsc_`sample'_grpall`cat' = 0 if e(sample) == 0 & all==1 & t_sw`cat'!=1

	}
	
** 3. To create weights, set numerator as pr(target) & denominator as pr(switch). If you know which of the four categories you have in your data, you can shorten this to two lines that define the numerator as the sum of pr(switch + target) and pr(no switch + target) and the denominator as the sum of pr(switch + target) and pr(switch + not target)
	gen numerator = .
	gen denominator = .
	if `cat1and2' ==1 replace numerator = lsc_`sample'_grpall1 + lsc_`sample'_grpall2  // numerator = pr(target+switch) + pr(target + no switch) if both categories exist
	if `cat1only' ==1 | `cat2only'==1 replace numerator = lsc_`sample'_grpall1  // numerator = pr(target+switch) or pr(target + no switch) if only one category exists
	if `cat1and3'==1 & `rows123' ==3 replace  denominator = lsc_`sample'_grpall1 + lsc_`sample'_grpall3 // denominator = pr(target + switch) + pr(not target + switch) if both categories exist
	if `cat1and3'==1 & `rows123' <3 replace  denominator = lsc_`sample'_grpall1 + lsc_`sample'_grpall2 // denominator = pr(target + switch) + pr(not target + switch) if both categories exist
	if `cat3only'==1 & `cat2only' ==1 replace denominator =  lsc_`sample'_grpall2  // denominator = pr(not target + switch) if only one category exists
	if `cat1and3'==0 & `cat3only'==0 & `rows1' <2 replace  denominator =  lsc_`sample'_grpall1 // denominator = pr(target + switch) if only one category exists

	replace lscwt_`sample'_grpall = numerator/denominator if all==1   //weight2 becomes one

	drop numerator denominator		
}

//save dataset
tempfile plweights
save `plweights', replace

	
/************************************************************
    MAKE TABLES 6, B15, B16)
 *************************************************************/

foreach var in  HSGrad Idle  LD PoorHealth {

	eststo clear
	
	use `plweights', clear
	
	unab Covariates: *_imp *_miss


	gen allcontrols = 1 if Res_0to3 !=. & HealthCond_before !=. & logBW !=. & LogInc_0to3 !=. & LogIncAt3 !=. & ///
		FirstBorn !=. & Male !=. & Age2_Yr104 !=. & HOME_Pct_0to3 !=. & Moth_HrsWorked_BefBirth !=. & ///
		Moth_HrsWorked_0to1 !=. & Father_HH_0to3 !=. & GMom_0to3 !=. & MomCare !=. & RelCare !=. & NonRelCare !=. & /// 
		Moth_Smoke_BefBirth !=. & Alc_BefBirth !=. & Breastfed !=. & Doctor_0to3 !=. & Dentist_0to3 !=. & Moth_WeightChange !=. & /// 
		Illness_1stYr !=. & Premature !=. & Insurance_0to3 !=. & Medicaid_0to3 !=.	

	keep LD HSGrad Idle PoorHealth  `Covariates' numkids  numsibs* ///
		MotherID hsswitch wht blk all Sample90_2 HS2_FE90 Pre2_FE90 Male Age2_Yr104 sib headstart allcontrols *imp *miss lsc* realkids child1 ///
		eligible numsibs_simp numsibs_sq Moth_HrsWorked_BefBirth_impmsg Moth_HrsWorked_0to1_impmsg Moth_HrsWorked_Avg_0to3_impmsg LogInc_0to3_impmsg LogIncAt3_impmsg MothED MomHS MomSomeColl Male hs*

	rename MotherID mom_unique
	rename  hsswitcher hs_switcher

	qui tab mom_unique if Sample90_2==1 & all==1 , g(mm) 
	local maxid = r(r)

	** Generate interaction between momid and head start for estimating family-level betas
	forvalues id = 1/`maxid' {
		qui gen hsxm`id' = HS2_FE90*mm`id'  if Sample90_2==1 & all==1  
	}

	** Store base FE regression coefficients for comparison to weighted betas
	xtset mom_unique
	xi: xtivreg2 `var' HS2_FE90 Pre2_FE90 Male i.Age2_Yr104 `Covariates' if Sample90_2==1 &all==1 /// 
		, fe i(mom_unique) cluster(mom_unique) 	

	sum `var' if e(sample) ==1 & all==1 & Sample90_2==1 
	local targetmean = r(mean)		
	gen wtdb =_b[HS2_FE90]
	gen wtdse = _se[HS2_FE90]
	local obs = e(N)
	
	* Create target means for each sample
	foreach sample in sib headstart eligible hsel_nos hs_nos {
		sum `var' if `sample' ==1 & all==1 & Sample90_2==1
		local targetmean`sample' = r(mean)
	}		

	estpost tabstat wtdb wtdse if all==1 , stat(mean sd semean n count sum) col(stat)
	estimates store sibfe_all  
	estadd scalar targetmean = `targetmean'

	drop wtdb wtdse

	** Estimate family-level betas with interacted regressions
	xi: eststo all: xtivreg2 `var' hsxm* Pre2_FE90 i.Age2_Yr104 `Covariates' if Sample90_2==1 &all==1  ///
		, fe i(mom_unique) cluster(mom_unique)
	estadd ysumm 			

	**Regression sample indicator
	gen sample = e(sample) if hs_switcher ==1 & all==1 & Sample90_2==1

	* Create variable with each family's share of switcher sample
	bysort mom_unique: egen tfam = total(sample)
	replace tfam = . if hs_switcher==0
	egen totsw = total(sample)
	gen share = tfam/totsw if hs_switcher ==1 & all==1 & Sample90_2==1

	* Save variable with family-specific beta
	gen beta = .
	forvalues id = 1/`maxid' {
		cap qui replace beta = _b[hsxm`id'] if mm`id'==1
	}	

	drop hsxm* mm* sample

	gen target = .
	
	**Test Conditional Independence Assumption for Table B10: are betas the same across target and non-target?

	replace target = eligible
	eststo testid`var'allhsel2: reg beta target if Sample90_2==1 & HS2_FE90 !=. &all==1 & hs_switcher==1 [aw=lscwt_eligible_grpall]
	replace target = headstart
	eststo testid`var'allhs2: reg beta target if Sample90_2==1 & HS2_FE90 !=.  & all==1 & hs_switcher==1 [aw=lscwt_headstart_grpall]

	drop target


	/***************************************************************
		REWEIGHTING FAMILY-LEVEL BETAS
	****************************************************************/
		
	preserve
	
	* Placeholder for weighted beta
	gen wtdb = .
	gen wtdse = .

	keep if Sample90_2==1

	** Collapse weights to family-level.
	collapse `var' beta share tfam totsw  wtdb wtdse (mean) lsc*wt_* ///
		numsibs_simp numsibs_sq Moth_HrsWorked_BefBirth_impmsg Moth_HrsWorked_0to1_impmsg Moth_HrsWorked_Avg_0to3_impmsg LogInc_0to3_impmsg LogIncAt3_impmsg MothED MomHS MomSomeColl Male, ///
		by(mom_unique hs_switcher all)

	foreach wt in wt nowt { 

		* Aggregate family-level betas for each target using weights
		foreach sample in sib eligible headstart hsel_nos hs_nos {

			if "`wt'" == "nowt" {
				gen wt_temp = share if all==1 // family-level weight = share of sample -- no pscore wieght (only fix cond'l variance weighting)
			}
			else{
				gen wt_temp = share*lsc`wt'_`sample'_grpall if all==1 // family-level weight = share of sample*pscore weight
			}
			
			egen totwt = total(wt_temp) // need to divide by total weight so weights sum to one
			gen norm_`wt'_temp = wt_temp/totwt
			gen norm_`wt'_`sample'_grpall = wt_temp/totwt

            // Final beta = weighted average of betas
			gen wtdb_temp = beta*(wt_temp/totwt)		
			egen `wt'db_`sample'_grpall = total(wtdb_temp)

			mean beta  [pw = norm_`wt'_temp]  // can use this to get conventional s.e.'s, but in the paper we bootstrap s.e.'s
			matrix myv = e(V) 
			matrix myb = e(b) 
			local beta = myb[1,1] 
			local vce = myv[1,1] 
			local se = sqrt(`vce')
			gen `wt'dse_`sample'_grp =`se' // placeholder s.e. for table, which is manually replaced with bootstrapped s.e.'s

			drop *temp totwt

			la var `wt'db_`sample'_grp "Wtd. B: Target = `sample', controls = grp"
			
			** Make tables with weighted betas and placeholder s.e.'s
			replace wtdb = `wt'db_`sample'_grp
			replace wtdse = `wt'dse_`sample'_grp

			la var wtdb "Beta"
			la var wtdse "S.E."

			sum wtdb if all==1 
			local `wt'db_`sample'_grpall = r(mean)

			sum wtdse if all==1 
			local `wt'dse_`sample'_grpall = r(mean)

			estpost tabstat wtdb wtdse if all==1, stat(mean sd semean n count sum) col(stat)
			estimates store `wt'db_`sample'_grpall  		
			estadd scalar targetmean = `targetmean`sample''

		}

	}
	
	tempfile varweights
	save `varweights', replace
	
	drop tfam totsw

	restore


	/*******************************************************
	 Extrapolating to singletons (comment out for efficiency)
	 *******************************************************/

	*for extrapolation, use same covariates as in pscore regression
	local icov "numsibs_simp numsibs_sq Moth_HrsWorked_BefBirth_impmsg Moth_HrsWorked_0to1_impmsg Moth_HrsWorked_Avg_0to3_impmsg LogInc_0to3_impmsg LogIncAt3_impmsg MothED MomHS MomSomeColl Male"

	use `plweights', clear
	
	foreach sample in hsel_nos hs_nos {

		if "`sample'" == "hsel_nos" local fullsample  "eligible"
		if "`sample'" == "hs_nos" local fullsample "headstart"

		preserve
		use `varweights', clear
		qui sum beta
		local numfam = r(N)
		reg beta `icov' [aw = norm_wt_`sample'_grpall]

		qui sum wtdb_`sample'_grpall	
		local  beta_pxgr0_`sample' = r(mean)
		restore

		predict beta_hat_`sample', xb
		qui sum beta_hat_`sample'  if `fullsample'==1 & child1 ==1 & msg_Sample90_2==1
		local numsing_`sample' =r(N)

		qui sum child1 if `fullsample'==1 
		local single_`sample' = r(mean)

		*** Head Start coeff = pr(1-child)*betahat + pr(2+child)*wtdb
		gen wtdb_adj_`sample' = `single_`sample''*beta_hat_`sample' + (1-`single_`sample'')*`beta_pxgr0_`sample''	
		qui sum wtdb_adj_`sample'
		local wtdb_adj_act_`sample' = r(mean)

	}

	** Bootstrap for extrapolation to singletons
	
	tempfile adj
		
	forvalues mc = 1/400 {

		use `plweights', clear
		
		foreach sample in hsel_nos hs_nos {

			if "`sample'" == "hsel_nos" local fullsample "eligible"
			if "`sample'" == "hs_nos" local fullsample "headstart"

			preserve
			use `varweights', clear
			bsample `numfam'

			**renormalize weights
			gen wt_temp = share*lscwt_`sample'_grpall if all==1
			egen totwt = total(wt_temp)
			gen norm_wt= wt_temp/totwt	

			gen beta_temp = beta*norm_wt
			egen beta_pxgr0 = total(beta_temp)
			sum beta_pxgr0
			local  beta_pxgr0_`sample' = r(mean)

			**extrapolation regression
			reg beta `icov' [aw = norm_wt]

			restore


			preserve

			predict beta_hat_`sample', xb

			keep if child1 ==1 & `fullsample'==1 & msg_Sample90_2==1
			bsample `numsing_`sample''
			sum beta_hat_`sample'
			local beta_hat_`sample' = r(mean)

			*** Head Start coeff = pr(1-child)*betahat + pr(2+child)*wtdb
			local wtdb_adj_`sample' = `single_`sample''*`beta_hat_`sample'' + (1-`single_`sample'')*`beta_pxgr0_`sample''
			restore

		}

		*** Save estimates with extrapolation
		preserve
		clear
		set obs 1 
		gen mc = `mc'
		gen wtdb_adj_headstart_mc = `wtdb_adj_hs_nos'
		gen wtdb_adj_eligible_mc = `wtdb_adj_hsel_nos'

		gen wtdb_adj_headstart_act = `wtdb_adj_act_hs_nos'
		gen wtdb_adj_eligible_act = `wtdb_adj_act_hsel_nos'

		gen sharesing_headstart = `single_hs_nos'
		gen sharesing_eligible = `single_hsel_nos'
		
		
		if `mc' > 1 append using `adj'
		
		save `adj', replace
		
		restore
	} 


	  /*************************************************
	             End Extrapolation
	   *************************************************/


	*** OUTPUT ESTIMATES WITH EXTRAPOLATION

	preserve

	foreach sample in eligible headstart {
		
		use `adj',clear
		gen wtdb = wtdb_adj_`sample'_act 
		egen wtdse = sd(wtdb_adj_`sample'_mc)
		estpost tabstat wtdb wtdse, stat(mean sd semean n count sum) col(stat)
		estimates store wtdb_adj_`sample' 		
		estadd scalar targetmean = `targetmean`sample''

	}

	restore



	***Table 6 (Main text)***
	** OUTPUT ESTIMATES WITHOUT EXTRAPOLATION TO SINGLETONS
	** Stack outcomes - 2 decimals

	if "`var'" == "HSGrad" ///
		esttab sibfe_all wtdb_eligible_grpall wtdb_headstart_grpall wtdb_sib_grpall using /// 
		"${tables}/table6_cnlsy.tex", replace  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		stats(targetmean, labels( "Y Mean in Target") fmt( 3)) mtitle(	"Baseline" "Eligible"  "Participants" "Siblings") ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ ")) postfoot("")

	if "`var'" != "HSGrad" & "`var'" != "PoorHealth" ///
		esttab sibfe_all wtdb_eligible_grpall wtdb_headstart_grpall wtdb_sib_grpall using  /// 
		"${tables}/table6_cnlsy.tex", append  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		stats(targetmean, labels( "Y Mean in Target") fmt( 3)) nomtitle ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ "))	 prehead("") posthead("") postfoot("")			  

	if "`var'" == "PoorHealth" ///
		esttab sibfe_all wtdb_eligible_grpall wtdb_headstart_grpall wtdb_sib_grpall using  /// 
		"${tables}/table6_cnlsy.tex", append  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		stats(targetmean, labels( "Y Mean in Target") fmt( 3)) nomtitle ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ ")) prehead("") posthead("")


	****Table B10 (Appendix)***
	** OUTPUT TEST OF CIA

	if "`var'" == "HSGrad" ///
		esttab testid`var'allhsel2 testid`var'allhs2 using "${tables}/tableb10_cnlsy.tex", replace ///
		se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
		stats(N , labels("Observations") fmt(0 )) mtitle("Eligible" "Participants" ) postfoot ("") ///
		varlabels(target "In target" , blist(target "\midrule \it{\underline{`var'}} \\ "))	


	if "`var'" == "LD" | "`var'" == "Idle"   ///
		esttab testid`var'allhsel2 testid`var'allhs2 using "${tables}/tableb10_cnlsy.tex", append ///
		se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
		stats(N , labels("Observations") fmt(0 )) nomtitle  postfoot ("") prehead("") posthead("") ///
		varlabels(target "In target" , blist(target "\midrule \it{\underline{`var'}} \\ "))	


	if "`var'" == "PoorHealth"   ///
		esttab testid`var'allhsel2 testid`var'allhs2 using "${tables}/tableb10_cnlsy.tex", append ///
		se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant  ///
		stats(N , labels("Observations") fmt(0 )) nomtitle prehead("") posthead("") /// 
		varlabels(target "In target" , blist(target "\midrule \it{\underline{`var'}} \\ "))	

	***Table B15 (appendix)***
	** OUTPUT ESTIMATES WITH EXTRAPOLATION TO SINGLETONS
	** Stack outcomes - 2 decimals

	if "`var'" == "HSGrad" ///
		esttab wtdb_adj_eligible wtdb_adj_headstart using /// 
		"${tables}/tableb15_cnlsy.tex", replace  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		stats(targetmean, labels( "Y Mean in Target") fmt( 3)) mtitle(	"Eligible"  "Participants" /// 
		"Eligible" "Participants" ) ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ ")) postfoot("")

	if "`var'" != "HSGrad" & "`var'" != "PoorHealth" ///
		esttab wtdb_adj_eligible wtdb_adj_headstart using /// 
		"${tables}/tableb15_cnlsy.tex", append  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		stats(targetmean, labels( "Y Mean in Target") fmt( 3)) nomtitle ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ "))	 prehead("") posthead("") postfoot("")			  

	if "`var'" == "PoorHealth" ///
		esttab wtdb_adj_eligible wtdb_adj_headstart using /// 
		"${tables}/tableb15_cnlsy.tex", append  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		stats(targetmean, labels( "Y Mean in Target") fmt( 3)) nomtitle ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ ")) prehead("") posthead("")	

	***Table B16 (appendix)***
	**OUTPUT REWEIGHTED RESULTS FOR SIBS AND HEAD START
	** Stack outcomes - 2 decimals

	if "`var'" == "HSGrad" ///
		esttab sibfe_all nowtdb_eligible_grpall using ///
		"${tables}/tableb16_cnlsy.tex", replace  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		mtitle(	"Baseline" "Eligible"  "Participants" "Siblings" /// 
		"Baseline" "Eligible" "Participants" "Siblings"  "Baseline" "Eligible"  "Participants" "Siblings") ///
		mgroups("All" "White" "Black", pattern(1 0 0 0 1 0 0 0 1 0 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ ")) postfoot("")

	if "`var'" != "HSGrad" & "`var'" != "PoorHealth" ///
		esttab sibfe_all nowtdb_eligible_grpall using ///
		"${tables}/tableb16_cnlsy.tex", append  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		nomtitle ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ "))	 prehead("") posthead("") postfoot("")			  

	if "`var'" == "PoorHealth" ///
		esttab sibfe_all nowtdb_eligible_grpall using ///
		"${tables}/tableb16_cnlsy.tex", append  ///
		main(mean %10.3f) collabels(none) nodepvar nostar label unstack nonum nonotes  compress nogaps noobs ///
		nomtitle ///
		varlabels(wtdb "Beta" wtdse "SE" , blist(wtdb "\midrule \it{\underline{`var'}} \\ ")) prehead("") posthead("")	


}

log close

	