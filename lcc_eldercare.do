/*--------------------------------------------------------------------*
University of Minnesota Life Course Center Aging and Health Launchpad
What ATUS Data Tells Us About Gender and Family Eldercare
*---------------------------------------------------------------------*/

*install weighted mean package if not already installed
ssc install _gwtmean

*set cd (change to your cd)
cd ""

*read in ATUS data by running the do file that comes with the fixed-width ATUS extract (change to whatever you saved the Stata syntax as)
do ".do"

*create combined weight variable
gen weight = wt06
replace weight = wt20 if year==2020

*recode sex
replace sex = 0 if sex==1
replace sex = 1 if sex==2
la def Lsex 0 "male" 1 "female", modify
la val sex Lsex

*identify the caregiver on the person record of the ATUS respondent, family caregiving only

*person record
gen eldercaregiver = (ecnum<97)

*eldercare recipient record
gen famcarerec = (inrange(relater, 200,280))

*all records
egen famcarerec_p = max(famcarerec), by(year serial)
*all records
egen eldercaregiver_p = max(eldercaregiver), by(year serial)

*person record
gen cg = (eldercaregiver_p==1 & famcarerec_p==1 & rectype=="2")
*person record of ATUS respondent only (missing for all other record types and for non respondent person records)
replace cg = . if rectype!="2" | relate!=10

*look at the number of eldercare recipient records per person record

*binary variable on eldercare recipient record indicating it's an eldercare recipient record
gen ecrec = (rectype=="5")

*sum these up for each household (all record types)
egen ecnumrec = sum(ecrec), by(year serial)

tab ecnumrec if cg==1
*73% of ATUS respondents who provided family eldercare have one EC recipient record. The rest have more than one

*care provider level analysis: sex composition, probability of being care provider by sex
*care recipient level analysis: sex composition
*care recipient level analysis: sex composition of caregiver by sex of recipient. Put the caregiver sex on the eldercare recipient record. necessary since one caregiver can have more than one eldercare recipient record.

*sex of eldercare provider on the eldercare recipient record

*sex of the ATUS respondent
gen respsex = sex
replace respsex = . if relate!=10
la def Lrespsex 1 "Female" 0 "Male", modify
la val respsex Lrespsex

*sex of the ATUS respondent (the caregiver) attached to every type of record within the household
egen cgsex_hh = max(respsex), by(year serial)

*sex of the caregiver, on the eldercare recipient record
gen cgsex = cgsex_hh
replace cgsex = . if rectype!="5"
la def Lcgsex 1 "Female" 0 "Male", modify
la val cgsex Lcgsex

*eldercare recipient sex, eldercare recipient record
*use the relater variable when it provides sex
gen recsex = .
replace recsex = 1 if inlist(relater, 241, 243, 252, 261, 264)
replace recsex = 0 if inlist(relater, 242, 244, 251, 262, 265)

*use line number if they're in the household
foreach num of numlist 1/16 {
	gen sex_`num' = sex if lineno==`num'
	egen sex_`num'_hh = max(sex_`num'), by(year serial)
	replace recsex = sex_`num'_hh if linenor==`num' & recsex!=0 & recsex!=1 
}

*use opposite sex of spouse if it's a non-coresident spouse (150 cases)
replace recsex = abs(cgsex-1) if inlist(relater, 200, 210) & hh_ec==0

la val recsex Lsex

*how many recipients of family eldercare have missing sex
count if famcarerec==1 & recsex==. 
count if famcarerec==1
*6.6%

*who are these
tab relater if famcarerec==1 & recsex==.
*half "other related person", half "grandparent", 2 "parent"

*sex composition of family eldercare providers
sum sex if cg==1 [aweight=weight]

*probability of being an eldercare provider by sex
sum cg [aweight=weight] if relate==10 & sex==1
sum cg [aweight=weight] if relate==10 & sex==0

*variable on the ATUS respondent record to indicate whether they cared for a man/woman or both
gen recf = (recsex==1)
gen recm = (recsex==0)
bysort year serial: egen recf_hh = max(recf)
bysort year serial: egen recm_hh = max(recm)

gen ectype = .
replace ectype = 1 if recf_hh>0 & recf_hh!=. & recm_hh>0 & recm_hh!=. & relate==10 & cg==1 
replace ectype = 2 if recf_hh>0 & recf_hh!=. & recm_hh==0 & relate==10 & cg==1
replace ectype = 3 if recf_hh==0 & recm_hh>0 & recm_hh!=. & cg==1 
 
la def Lectype 1 "Cares for male and female" 2 "Cares for female" 3 "Cares for male", modify
la val ectype Lectype

tab ectype, gen(ectype_)
la var ectype_1 "Cares for male and female"
la var ectype_2 "Cares for female"
la var ectype_3 "Cares for male"

egen mean_ectype_1 = wtmean(ectype_1), weight(weight) by(sex)
egen mean_ectype_2 = wtmean(ectype_2), weight(weight) by(sex)
egen mean_ectype_3 = wtmean(ectype_3), weight(weight) by(sex)

*multiply by 100 for graph
replace mean_ectype_1 = mean_ectype_1*100
replace mean_ectype_2 = mean_ectype_2*100
replace mean_ectype_3 = mean_ectype_3*100

*dummy variables for each frequency of eldercare provision, for family eldercare providers (cg=1) only
replace ecfreq = . if ecfreq>7
*exclude refused, don't know, and NIU. Include "other" as a category since it's a good share of people

tab ecfreq, gen(ecfreq_)
*make the dummies only available for cg=1 
foreach var of varlist ecfreq_* {
	replace `var' = . if cg!=1
}

*weighted mean of each dummy by sex and multiply by 100 for graphs
foreach var of varlist ecfreq_* {
	egen mean_`var' = wtmean(`var'), weight(weight) by(sex)
	replace mean_`var' = mean_`var'*100
}


*mean eldercare provider status by sex and year
egen cg_mean = wtmean(cg), weight(weight) by(sex year)
*multiplly by 100 for graphs
replace cg_mean = cg_mean*100
 
preserve
collapse (mean) cg_mean, by(sex year)
*line graph plotting share of men vs women who are family eldercare providers by year 
twoway (connected cg_mean year if sex==1, sort lcolor("0 114 178") lwidth(medium) mcolor("0 85 133")) ///
       (connected cg_mean year if sex==0, sort lcolor("230 159 0") lwidth(medium) mcolor("173 119 0")), ///
       ytitle("Percent that provide family eldercare", margin(right=2)) ///
       xtitle("Year", margin(bottom=2)) ///
       legend(label(1 "Women") label(2 "Men") region(lstyle(none))) ///
       graphregion(color(white)) ///
	   note("Data: 2011-2024 ATUS samples from IPUMS ATUS." "Share of ATUS respondents with at least one eldercare recipient record who is" "a family member, by sex." "Sampling weights applied.", margin(t=2))
	   
graph export "caregiversex.pdf", replace
graph export "caregiversex.png", replace

restore

*stacked bar graph plotting share of providers who care for a woman, man, or both
graph bar mean_ectype*, over(sex, relabel(1 "Male caregivers" 2 "Female caregivers")) stack ///
graphregion(color(white)) plotregion(color(white) lcolor(white)) ///
ytitle("Percent of family eldercare providers") ///
bar(1, color("34 139 87")) ///
bar(2, color("230 126 140")) ///
bar(3, color("70 130 180")) ///
legend(label(1 "Cares for man & woman") label(2 "Cares for woman") label(3 "Cares for man") rows(3)) ///
blabel(bar, position(center) color(white) format(%9.1f)) ///
note("Data: 2011-2024 pooled ATUS samples from IPUMS ATUS." "Sex of family eldercare recipient(s) not identifiable for 5.7 percent of caregivers." "Non-household spouses/partners who are recipients of eldercare are assumed to be the opposite sex" "of the spouse/partner providing care." "Caregivers may care for one or more family members." "Sampling weights applied.", size(small)) 

graph export "recipientsex.pdf", replace
graph export "recipientsex.png", replace


*stacked bar graph plotting share of providers who provide care at different frequencies
graph bar mean_ecfreq_1 mean_ecfreq_2 mean_ecfreq_3 mean_ecfreq_4 mean_ecfreq_5 mean_ecfreq_7, over(sex, relabel(1 "Male caregivers" 2 "Female caregivers")) stack ///
graphregion(color(white)) plotregion(color(white) lcolor(white)) ///
ytitle("Percent of family eldercare providers") ///
bar(1, color("27 71 52")) ///
bar(2, color("0 105 92")) ///
bar(3, color("0 131 143")) ///
bar(4, color("0 151 178")) ///
bar(5, color("41 171 210")) ///
bar(6, color("179 229 252")) ///
legend(label(1 "Daily") label(2 "Several times a week") label(3 "About once a week") label(4 "Several times a month") label(5 "Once a month") label(6 "Other") rows(3)) ///
blabel(bar, position(center) color(white) format(%9.1f)) ///
note("Data: 2011-2024 pooled ATUS samples from IPUMS ATUS." "Includes ATUS respondents with at least one eldercare recipient record who is" "a family member." "Sampling weights applied.", size(small)) 

graph export "freqencysex.pdf", replace
graph export "frequencysex.png", replace
