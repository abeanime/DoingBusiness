
* base beta convergence reg 

cd "/Users/null/Desktop/Doing Business/prettydoc"

use final_index,clear
encode province,gen(pro)
/* redecode province,method(encode) */
xtset pro year 
gsort pro year 
bysort pro :gen y = log(f.index/index)
gen x = log(index)
gen tag = (year <= 2014)

forval j = 0/1 {
	preserve
	keep if tag == `j'
	qui xtreg y x i.pro i.year,r
	est store m1
	forval i = 2/4{
		qui xtreg y x i.pro i.year if loca == `i',r
		est store m`i'
	}
	  reg2docx m1 m2 m3 m4 using "~/Desktop/beta_`j'.docx", replace scalars(N r2_o(%9.3f) ) b(%9.3f) t(%7.2f)  mtitles("全国" "东部地区" "中部地区" "西部地区")

	restore
}


* marginal vertical beta convergence reg 
cd "~/Desktop"
use "/Users/null/Desktop/Doing Business/prettydoc/final_index.dta",clear
encode province,gen(pro)
xtset pro year 
gsort pro year 
bysort pro:gen y = log(f.index/index)
gen x = log(index)
gen tag = (year <= 2014)

forval j = 0/1 {
	preserve
	keep if tag == `j'
	qui xtreg y x i.pro i.year ,r 
	sca pro_`j' = e(b)[1,1]

	qui levelsof pro ,clean local(prolist)
	foreach r of loca prolist { // 除去第i个地区后的general convergence beta 
		qui xtreg y x i.pro i.year if pro != `r',r
		sca pro_`j'_`r' = e(b)[1,1]
	}	

	qui tab pro 
	clear 
	set obs  `r(r)'
	qui gen pro = _n
	qui gen general_alpha_`j' = pro_`j'
	qui gen exclude_alpha_`j' = pro_`j'_1
	forval i = 2/`r(r)' {
		qui replace exclude_alpha_`j' = pro_`j'_`i' in `i'
	}
	
	save country_`j',replace 
	restore
}

duplicates drop province,force 
keep province pro 
merge 1:1 pro using "country_0.dta",nogen
merge 1:1 pro using "country_1.dta",nogen
drop pro
save 整体.dta,replace 

/*---- break line (Made for macOS)--- */ 

use "/Users/null/Desktop/Doing Business/prettydoc/整体.dta",clear  
foreach v of var general_alpha_0-exclude_alpha_1{
	replace `v' = -log(`v'+1)
}
gen marginal_1 = general_alpha_0-exclude_alpha_0
gen marginal_2 = general_alpha_1-exclude_alpha_1

/* keep pro m*
gsort marginal_1  */
save marginal_vertical_beta.dta,replace 



* Moran’s I指数

cd "/Users/null/Desktop/Doing Business/prettydoc"
use final_index.dta,clear 
merge m:1  province using "geo.dta",keep(match)

forval i = 2010/2019 {
	preserve
		keep if year == `i'
		moransi index ,lon(维度) lat(经度) swm(pow 2) dist(.) dunit(km)
		egen std_index = std(index)
		spgen std_index,lon(维度) lat(经度) swm(pow 2) dist(.) dunit(km)
		ren splag1_std_index_p w_std_index


	gr tw ///
		sc w_std_index std_index if location == 2, m(O) mfc("black") || ///
		sc w_std_index std_index if location == 3, m(O) mfc("red") || ///
		sc w_std_index std_index if location == 4,m(O) mfc("blue") || ///
		lfit w_std_index std_index,estopts(nocons) || ///
		,xline(0,lp(dash)) yline(0,lp(dash)) ///
		leg(on pos(6) ring(1) ///
			order(1 "东部地区" 2 "中部地区" 3 "西部地区" 4 "线性拟合" ) rows(1)) ///
		ylab(#20) ysc(r(-1(0.5)1)) xsc(r(-3(1)3)) xlab(#9) ///
		ti("`i'年Moran’s I指数")
		 graph export "/Users/null/Desktop/Doing Business/prettydoc/moranI/`i'年Moran’s I指数.jpg",replace width(2666) height(2000) /*4:3输出图形*/ 
	restore
}



cd "/Users/null/Desktop/Doing Business/prettydoc"
use final_index.dta,clear 
merge m:1  province using "geo.dta",keep(match)

forval i = 2010/2019 {
	preserve
		keep if year == `i'
		moransi index ,lon(维度) lat(经度) swm(pow 2) dist(.) dunit(km)
		
		sca I_`i' = `r(I)'
		sca p_`i' = `r(pI)'

	restore
}

clear 
set obs 10
gen I = I_2010 in 1
gen p = p_2010 in 1

local y 2011

forval i = 2/10{
	replace I = I_`y' in `i'
	replace p = p_`y' in `i'
	local ++y
}

save moranSC.dta








