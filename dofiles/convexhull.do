// Install necessary command
ssc install cvxhull

#delimit ;

cap prog drop overlap ;
prog def overlap ;
syntax  , [switcher(varname) target(varname) px(varname) qx(varname)];

qui cvxhull `qx' `px' if `switcher' == 1 , hulls(1) nograph ;
sort `px' `qx' ;
mkmat `qx' `px' if _cvxh1l ~= . , matrix(upper) ;
mkmat `qx' `px' if _cvxh1r ~= . , matrix(lower) ;
local segments_upper = rowsof(upper) - 1 ;
local segments_lower = rowsof(lower) - 1 ;

graph twoway (rarea _cvxh1l _cvxh1r `px' if `switcher' == 1
		, color(orange*.2) sort)
	(scatter `qx' `px' if `target' == 1 , color(blue) msize(vsmall))
	, scheme(s1mono) legend(label(1 "Convex Hull for Switchers") label(2 "Target Observations"))
	xtitle("Px = Pr(switcher|x)") ytitle("Qx = Pr(target|x)") ;

qui summ `px' if _cvxhhull == 1 ;
local leftlimit = r(mean) ;
qui summ `px' if _cvxhhull == -1 ;
local rightlimit = r(mean) ;

count ;
local bigN = r(N) ;

qui { ;
	gen left = `px' < `leftlimit' if `target' == 1 ;
	gen right = `px' > `rightlimit' if `target' == 1 ;
	gen above = . ;
	gen below = . ;
	gen inside = . ;

	gen fraction = . ;
	gen yabove = . ;
	gen ybelow = . ;
} ;

// First count who is inside and who is outside
qui { ;
	replace inside = 1 if `switcher' == 1 ;
	replace inside = 0 if left == 1 ;
	replace inside = 0 if right == 1 ;
} ;

forvalues i = 1/`bigN' { ;
	local myinside = inside[`i'] ;
	if `myinside' == . { ;

	local myx = `px'[`i'] ;
	local myy = `qx'[`i'] ;

	// Find the relevant "upper" segment and check if currently I above it
	local mysegment = 0 ;
	local stop = 0 ;
	while `stop' == 0 { ;
		local mysegment = `mysegment' + 1 ;
		local segplus1 = `mysegment' + 1 ;

		local x_beg_of_segment = upper[`mysegment',2] ;
		local x_end_of_segment = upper[`segplus1',2] ;
		local y_beg_of_segment = upper[`mysegment',1] ;
		local y_end_of_segment = upper[`segplus1',1] ;

		if (`myx' >= `x_beg_of_segment' ) &
				(`myx' <= `x_end_of_segment' ) { ;

			local stop = 1 ;

			// Am I above or below this segment ?
			local fraction = (`myx' - `x_beg_of_segment') /
				(`x_end_of_segment' - `x_beg_of_segment') ;
			qui replace fraction = `fraction' in `i' ;
			local y_on_segment = `y_beg_of_segment' * (1-`fraction') +
				`y_end_of_segment' * (`fraction') ;
			qui replace yabove = `y_on_segment' in `i' ;
			local above_upper = `myy' > `y_on_segment' ;
		} ;

	} ;			/* end while stop == 0 */

	// Find the relevant "lower" segment and check if currently below it
	local mysegment = 0 ;
	local stop = 0 ;
	while `stop' == 0 { ;
		local mysegment = `mysegment' + 1 ;
		local segplus1 = `mysegment' + 1 ;

		local x_beg_of_segment = lower[`mysegment',2] ;
		local x_end_of_segment = lower[`segplus1',2] ;
		local y_beg_of_segment = lower[`mysegment',1] ;
		local y_end_of_segment = lower[`segplus1',1] ;

		if (`myx' >= `x_beg_of_segment' ) &
				(`myx' <= `x_end_of_segment' ) { ;

			local stop = 1 ;

			// Am I above or below this segment ?
			local fraction = (`myx' - `x_beg_of_segment') /
				(`x_end_of_segment' - `x_beg_of_segment') ;
			local y_on_segment = `y_beg_of_segment' * (1-`fraction') +
				`y_end_of_segment' * (`fraction') ;
			qui replace ybelow = `y_on_segment' in `i' ;
			local above_lower = `myy' > `y_on_segment' ;
		} ;

	} ;			/* end while stop == 0 */

	qui replace above = (`above_upper' == 1) & (`above_lower' == 1) in `i' ;
	qui replace below = (`above_upper' == 0) & (`above_lower' == 0) in `i' ;
	qui replace inside = (`above_upper' == 0) & (`above_lower' == 1) in `i' ;


	} ;			/* end if inside == . */
} ;				/* end forvalues i = 1/`bigN' */
summ inside if `target' == 1 ;

end ;
