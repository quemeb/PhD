*# import data from GitHub
import delimited "C:\Users\bryan\OneDrive - University of Southern California\Academic\Courses\Spring 2022\511B-Data-Analysis\Final Project\PM511B_Project7_data.csv", varn(1)

* taking a 10k feet view
codebook 
*it seems that the variables are have bad input: 
*cough: 252 - None
*fever: 252 - None
*sore_throat: 1 - None
*shortness_of_breath: 1- None 
*head_ache: 1 - None
*corona_result: 3,892 - "other"
*age_60_and_above: 127,320 - "None"
*gender: 19,563 - "None"


*dropping observations
drop if cough == "None"
drop if fever == "None"
drop if sore_throat == "None"
drop if shortness_of_breath == "None"
drop if head_ache == "None"
drop if corona_result == "other"
drop if age_60_and_above == "None"
drop if gender == "None"

replace corona_result = "0" if corona_result == "negative"
replace corona_result = "1" if corona_result == "positive"
replace age_60_and_above = "1" if age_60_and_above == "Yes"
replace age_60_and_above = "0" if age_60_and_above == "No"
replace gender = "0" if gender == "female" 
replace gender = "1" if gender == "male"
replace test_indication = "0" if test_indication == "Abroad"
replace test_indication = "1" if test_indication == "Contact with confirmed"
replace test_indication = "2" if test_indication == "Other"

*136,294 observations 
gen cough_r = real(cough)
gen fever_r = real(fever)
gen sore_throat_r = real(sore_throat)
gen shortness_of_breath_r = real(shortness_of_breath)
gen head_ache_r = real(head_ache)
gen corona_result_r = real(corona_result)
gen age_60_and_above_r = real(age_60_and_above)
gen gender_r = real(gender)
gen test_indication_r = real(test_indication)

**#univariate analysis

global cont "cough_r fever_r sore_throat_r shortness_of_breath_r head_ache_r gender_r age_60_and_above_r i.test_indication_r"
*foreach i in $cont {
*	logit corona_result_r `i'
*}

*all variables statistically significant
*logit corona_result_r $cont, nolog

**# collinearity 
*collin $cont
*no colinearity

*train set
set seed 1919
gen runi = uniform()
sort runi
gen train = 1
*20% training data
replace train = 0 in 1/20444 

**#stepwise 
stepwise, pr(0.01): logit corona_result_r $cont if train
*useless... includes everything
*stepwise, pe(0.01) forward: logit corona_result_r $cont if train

**#best subset
*gvselect <term> cough_r fever_r sore_throat_r shortness_of_breath_r head_ache_r gender_r test_indication_r age_60_and_above_r: logit corona_result_r <term> if train

logit corona_result_r cough_r fever_r sore_throat_r shortness_of_breath_r head_ache_r age_60_and_above_r i.test_indication_r gender_r if train
estat gof if train, group(10)

predict p
predict std_resid, rstand
predict dx2, dx2
predict ddev, ddev
predict dbeta, dbeta
predict lev, hat

sum lev

scatter std_resid lev [w = dbeta], xline(0.018) yline(-2 2) msymbol(Oh)

*sensitivity
lsens if train, genprob(probcut) genspec(spec) gensens(sens)
cutpt corona_result_r p if train

*all variables
logit corona_result_r cough_r fever_r sore_throat_r shortness_of_breath_r head_ache_r age_60_and_above_r i.test_indication_r gender_r if train
estat class if train, cut(0.03966)
estat class if !train, cut(0.03966)

**# 1 variable less
*minus gender
logit corona_result_r fever_r sore_throat_r shortness_of_breath_r head_ache_r age_60_and_above_r i.test_indication_r cough_r if train, nolog
lsens if train
predict p_1
cutpt corona_result_r p_1 if train
estat class if train, cut(0.03329)
estat class if !train, cut(0.03329)

*minus i.test_indication_r
logit corona_result_r fever_r sore_throat_r shortness_of_breath_r head_ache_r age_60_and_above_r gender_r cough_r if train, nolog
lsens if train
predict p_2
cutpt corona_result_r p_2 if train
estat class if train, cut(0.05137)
estat class if !train, cut(0.05137)

*M3 - minus gender & cough_r
logit corona_result_r fever_r sore_throat_r shortness_of_breath_r head_ache_r age_60_and_above_r i.test_indication_r if train, nolog
lsens if train
predict p_3
cutpt corona_result_r p_3 if train
estat class if train, cut(0.03728)
estat class if !train, cut(0.03728)

*M4 - minus gender & cough_r & age_60_and_above_r
logit corona_result_r fever_r sore_throat_r shortness_of_breath_r head_ache_r i.test_indication_r if train, nolog
lsens if train
predict p_4
cutpt corona_result_r p_4 if train
estat class if train, cut(0.07285)
estat class if !train, cut(0.07285)


*PCA analysis
*pca cough_r fever_r sore_throat_r shortness_of_breath_r head_ache_r age_60_and_above_r test_indication_r gender_r

**# Q2
gen symp = 0
replace symp = 1 if cough_r == 1 | fever_r == 1 | sore_throat_r ==1 | shortness_of_breath_r |head_ache_r == 1

stepwise, pe(0.05) pr(0.10): logit corona_result_r symp age_60_and_above_r i.test_indication_r gender_r if train 
logit corona_result_r symp age_60_and_above_r i.test_indication_r gender_r if train 
predict p_6
cutpt corona_result_r p_6 if train
logit corona_result_r symp age_60_and_above_r i.test_indication_r gender_r if train 
estat class if train, cut(0.09939)
estat class if !train, cut(0.09939)


gen test_date_r = date(test_date, "MDY")
format test_date_r %td
bysort test_date: egen Mean = mean(corona_result_r)
graph twoway scatter Mean test_date_r, ytitle(Positivity RateT) xtitle(Test Dates) title(Daily Covid_19 Positivity Rate)


egen datecat = cut(test_date_r), group(4) label

logit corona_result_r i.test_indication_r if datecat==0, or nolog
logit corona_result_r i.test_indication_r if datecat==1, or nolog
logit corona_result_r i.test_indication_r if datecat==2, or nolog
logit corona_result_r i.test_indication_r if datecat==3, or nolog

scatter corona_result_r datecat