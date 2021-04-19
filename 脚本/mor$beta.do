
* beta收敛

cd "/Users/null/Desktop/Doing Business/prettydoc"

use final_index,clear
redecode province,method(encode)
xtset pro year 
gsort pro year 

bysort province :gen y = log(f.index/index)
gen x = log(index)


xtreg y x i.pro i.year,r
est store m1

forval i = 2/4{
	xtreg y x i.pro i.year if loca == `i',r
	est store m`i'
}

  reg2docx m1 m2 m3 m4 using "~/Desktop/beta.docx", replace scalars(N r2_o(%9.3f) ) b(%9.3f) t(%7.2f)  mtitles("全国" "东部地区" "中部地区" "西部地区")






















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
		egen std_index = std(index)
		spgen std_index,lon(维度) lat(经度) swm(pow 2) dist(.) dunit(km)
		ren splag1_std_index_p w_std_index

		save "/Users/null/Desktop/Doing Business/prettydoc/moranI/`i'.dta",replace 

	/* gr tw ///
		sc w_std_index std_index if location == 2, m(O) mfc("black") || ///
		sc w_std_index std_index if location == 3, m(O) mfc("red") || ///
		sc w_std_index std_index if location == 4,m(O) mfc("blue") || ///
		lfit w_std_index std_index,estopts(nocons) || ///
		,xline(0,lp(dash)) yline(0,lp(dash)) ///
		leg(on pos(6) ring(1) ///
			order(1 "东部地区" 2 "中部地区" 3 "西部地区" 4 "线性拟合" ) rows(1)) ///
		ylab(#20) ysc(r(-1(0.5)1)) xsc(r(-3(1)3)) xlab(#9) ///
		ti("`i'年Moran’s I指数")
		 graph export "/Users/null/Desktop/Doing Business/prettydoc/moranI/`i'年Moran’s I指数.jpg",replace width(2666) height(2000) /*4:3输出图形*/  */
	restore
}
















