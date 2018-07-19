/*---------------------------*/
* Main Analysis - X-sectional
/*---------------------------*/
clear
*iMac
use "/Users/daiglechris/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
*BigLaptop
*use "/Users/2011home/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
set more off

forvalues i = 0(1)6 {
	display 2013 -`i'
	gen interest`i' = 0
	replace interest`i' = 1 if Grade == 9 - `i' & Yr == 2013 - `i'
}

* Creating a district average of passing score
gen numAllP = 1
gen avgAllP = 0
sort Yr Dist Grade
forvalues j = 2007(1)2013 {
	bys Yr Dist: replace avgAllP = sum(allp) / sum(numAllP) if Dist == Dist & Yr == `j' & allp != .
}
bys Yr Dist: replace avgAllP = avgAllP[_N]

replace FRLProp = FRLProp *100

sum avgAllP Mem InfStatePPE FRLProp Day
bys Yr: sum avgAllP, detail
bys Yr: sum Mem, detail
bys Yr: sum InfStatePPE, detail
bys Yr: sum FRLProp
bys Yr: sum Day

* Base Model: Regressing district x-sections starting in 2007 going through 2013
forvalues i = 2007(1)2013 {
	dis "year `i' "
	reg avgAllP InfStatePPE FRLProp Day if Yr == `i'
}

* Base Model: including robust se
forvalues i = 2007(1)2013 {
	dis "year `i' "
	reg avgAllP InfStatePPE FRLProp Day if Yr == `i' , r
}

* Chow Test:
*First regress on pooled - 
reg avgAllP InfStatePPE FRLProp Day
*Next, perform the F-test for the chow
gen sumResOth = 87247.9645 + 147654.003 + 110011.85 + 235850.349 + 212775.523 + 194540.266 + 242011.896
gen sumDFOth = 831 + 923 + 929 + 1044 + 1038 + 1040 + 1015
display (2130765.99 - (sumResOth))/(6844 - (sumDFOth)) / ((sumResOth) / 6844)
* F = 208.79923
display Ftail(6844,6844 - (sumDFOth),208.79923)
* P = 2.620e-24
* Very close to zero, reject the hypothesis 

* Grade Model: including robust se
forvalues i = 3(1)9 {
	dis "Grade `i' in year " 2004 + `i'
	reg allp InfStatePPE FRLProp Day if Grade == `i' & Yr == 2004 + `i' , r
}

xtset cohort Yr
xtsum avgAllP Mem InfStatePPE FRLProp Day

/*---------------------------*/
*         COMPLETE
/*---------------------------*/

clear
*iMac
use "/Users/daiglechris/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
*BigLaptop
*use "/Users/2011home/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"

set more off

sort Grade
by Grade: sum Yr
sort Yr
by Yr: sum Grade
* Grade 6 appears most frequently, so I drop all others to so that the proportion passing, the independent to be measured, is appropriate at the district level.
*By only observing one grade per district, we only observe each district once instead of 10 times, once for each grade, where the proportion passing is given at the school district's grade level and the membership is given at the district level (the bases of measurement are different). I select year to be 2013 as that is the base for all other measurements.
keep if Grade == 6
keep if Yr == 2013

*IQR
sort Yr
xtile MemIQR = Mem, n(4)
tab MemIQR, gen(iq)
* Creates 4 variables - iq1, iq2, iq3, iq4 - by innerquartile ranges of about 25%.

* Observe first quartile

keep if iq1 == 1 & Yr == 2013
egen AvgMem=mean(Mem)
gen AbsMem=abs(AvgMem-Mem)
sort AbsMem
keep in 1
browse Dist
* Dist 11 is closest to the mean of the 1'st quartile when selecting only on the 6th Grade in 2013. When desiring to check only IQR1, load data and "keep if Dist == 11"

* Observe second quartile
clear
*iMac
use "/Users/daiglechris/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
*BigLaptop
*use "/Users/2011home/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
set more off
keep if Grade == 6
keep if Yr == 2013

*IQR
sort Yr
xtile MemIQR = Mem, n(4)
tab MemIQR, gen(iq)
sum Mem

keep if iq2 == 1 & Yr == 2013
egen AvgMem=mean(Mem)
gen AbsMem=abs(AvgMem-Mem)
sort AbsMem
keep in 1
browse Dist 
* Dist 135 is closest to the mean of the 2'nd quartile when selecting only on the 6th Grade in 2013. When desiring to check only IQR3, load data and "keep if Dist == 135"


* Observe third quartile
clear
*iMac
use "/Users/daiglechris/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
*BigLaptop
*use "/Users/2011home/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
set more off
keep if Grade == 6
keep if Yr == 2013

*IQR
sort Yr
xtile MemIQR = Mem, n(4)
tab MemIQR, gen(iq)
sum Mem

keep if iq3 == 1 & Yr == 2013
egen AvgMem=mean(Mem)
gen AbsMem=abs(AvgMem-Mem)
sort AbsMem
keep in 1
browse Dist
* Dist 372 is closest to the mean of the 3'rd quartile when selecting only on the 6th Grade in 2013. When desiring to check only IQR3, load data and "keep if Dist == 372"

* Observe fourth quartile
clear
*iMac
use "/Users/daiglechris/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
*BigLaptop
*use "/Users/2011home/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
set more off
keep if Grade == 6
keep if Yr == 2013

*IQR
sort Yr
xtile MemIQR = Mem, n(4)
tab MemIQR, gen(iq)
sum Mem

keep if iq4 == 1 & Yr == 2013
egen AvgMem=mean(Mem)
gen AbsMem=abs(AvgMem-Mem)
sort AbsMem
keep in 1
browse Dist
* Dist 132 is closest to the mean of the 4th quartile when selecting only on the 6th Grade in 2013. When desiring to check only IQR3, load data and "keep if Dist == 132"

/*---------------------------*/
*        Time Series
/*---------------------------*/
clear
*iMac
use "/Users/daiglechris/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
*BigLaptop
*use "/Users/2011home/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
set more off
keep if Grade == 6

egen AvgMem=mean(Mem)
gen AbsMem=abs(AvgMem-Mem)
sort AbsMem
keep in 1
browse Dist
* District 60 is the one nearest the mean.
clear
*iMac
use "/Users/daiglechris/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
*BigLaptop
*use "/Users/2011home/Library/Mobile Documents/com~apple~CloudDocs/Education/UConn/Spring 2018/App Metrics/Project/ISATMetrics2.dta"
set more off
keep if Grade == 6 & Dist == 60
browse

sum Yr allp InfTotPPE InfStatePPE FRLProp Day
