##Outline
##Most physicians base decisions about cardiovascular treatment based on a calculation of overall cardiovascular disease risk.

##Nutrition is not currently accounted for in cardiovascular risk calculators, despite being a major contributor to cardiovascular risk. Nutrition is commonly measured through 24-hour dietary recalls, which are difficult to summarize and condense into standard metrics to improve cardiovascular risk estimation.

##We propose to use deep learning to study the large sparse matrix of nutrition data from the National Health and Nutrition Examination Survey (1999-2010) linked to the National Death Index (2011), to determine how nutrition data may advance cardiovascular mortality prediction beyond traditional risk factors (age, sex, race/ethnicity, systolic blood pressure, blood pressure treatment, tobacco smoking, diabetes status).

##We will compare our deep learning approach to four standard metrics of nutritional quality: the Healthy Eating Index (HEI-2015), Alternative Healthy Eating Index (AHEI-2010), the Mediterranean Diet Score (MDS), and DASH Diet Score.

##Load packages and functions
#library(readr)
source('/Users/jrigdon/Box sync/Rigdon/Useful Functions/Tables_v2.R')
source('/Users/jrigdon/Box sync/Rigdon/Useful Functions/Functions.R')
source('/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Code/Evaluate.R')
library(mice)
library(survival)
library(party)
library(gbm)
library(pROC)
library(multcomp)

##Load data
d = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Data_2019-01-14.csv", header=TRUE)

##Summarize key variables descriptively (work on function with officeR)
##Age 40-79 (years), Gender (Male/Female), Race (African American/Other), Total cholesterol (mg/dL), HDL cholesterol (mg/dL), Systolic blood pressure (mmHg), Diastolic blood pressure (mmHg), Treated for high blood pressure (No/Yes), Diabetes (No/Yes), Smoker (No/Yes)
##age, sex, black, totchol, hdl, sbp, bpmeds, dm, tob
tab1 = mktab(data=d, var.names=c("permth_int", "wave", "age", "sex", "black", "hispanic", "total_chol", "hdl", "sbp", "dbp", "bpmeds", "dm", "tob", "hei", "ahei", "mds", "dash"), ind.cat=c(0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0), group.name="cvdevent", cfn=describeMean, miss="always", pval=TRUE, tot=FALSE, digit=1)

word.tab(tab=tab1, dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Tables/Table1_2019-01-14.docx", help=TRUE)

##Make lists of variables
dems = c("age", "sex", "black", "hispanic")
acc = c("total_chol", "hdl", "sbp", "dbp", "bpmeds", "dm", "tob")
comp = c("hei", "ahei", "mds", "dash")
nutr1 = c("milk_g", "cream_g", "milk_dessert_g", "cheese_g", "meat_ns_g", "beef_g", "pork_g", "lamb_g", "poultry_g", "organ_meat_g", "fish_g", "meat_nonmeat_g", "protein_frozen_g", "eggs_g", "egg_mixture_g", "egg_sub_g", "egg_frozen_g", "legumes_g", "nuts_g", "seeds_g", "carob_g", "flour_mix_g", "bread_yeast_g", "bread_quick_g", "pastries_g", "crackers_g", "pancakes_g", "pastas_g", "cereals_g", "grain_mix_g", "meat_sub_g", "citrus_g", "fruit_dried_g", "fruit_other_g", "fruit_juice_g", "fruit_baby_g", "potatoes_g", "veg_darkgreen_g", "veg_deepyellow_g", "tomatoes_g", "veg_other_g", "veg_baby_g", "veg_meat_g", "veg_mixture_g", "fats_g", "oils_g", "salad_dressing_g", "sweets_g", "bev_nonalcohol_g", "bev_alcohol_g", "water_g", "bev_nutrition_g")
nutr2 = c("kcal", "protein_g", "carb_g", "fiber_g", "fat_g", "fat_sat_g", "fat_mono_g", "fat_poly_g", "cholesterol_mg",  "vite_mg", "vita_mg", "betacaro_mcg", "vitb1_mg", "vitb2_mg", "niacin_mg", "vitb6_mg", "folate_mcg", "vitb12_mcg", "vitc_mg", "calcium_mg", "phosphorus_mg", "magnesium_mg", "iron_mg", "zinc_mg", "copper_mg", "sodium_mg", "potassium_mg", "selenium_mcg", "caffeine_mg", "theobromine_mg", "alcohol_gm", "sfa_40_gm", "sfa_60_gm", "sfa_80_gm", "sfa_100_gm", "sfa_120_gm", "sfa_140_gm", "sfa_160_gm", "sfa_180_gm", "mfa_161h_gm", "mfa_161o_gm", "mfa_201_gm", "mfa_221_gm", "pfa_182_gm", "pfa_183_gm", "pfa_184_gm", "pfa_204_gm", "pfa_205_gm", "pfa_225_gm", "pfa_226_gm", "water_yesterday_gm")

##Take a 70/30 split within each wave
set.seed(41)
table(d$wave, exclude=NULL)

##99-00
s1 = sample(rownames(d[d$wave=="a.9900", ]), floor(0.7*length(rownames(d[d$wave=="a.9900", ]))))

##01-02
s2 = sample(rownames(d[d$wave=="b.0102", ]), floor(0.7*length(rownames(d[d$wave=="b.0102", ]))))

##03-04
s3 = sample(rownames(d[d$wave=="c.0304", ]), floor(0.7*length(rownames(d[d$wave=="c.0304", ]))))

##05-06
s4 = sample(rownames(d[d$wave=="d.0506", ]), floor(0.7*length(rownames(d[d$wave=="d.0506", ]))))

##07-08
s5 = sample(rownames(d[d$wave=="e.0708", ]), floor(0.7*length(rownames(d[d$wave=="e.0708", ]))))

##09-10
s6 = sample(rownames(d[d$wave=="f.0910", ]), floor(0.7*length(rownames(d[d$wave=="f.0910", ]))))

##Record train and test data sets
d$test = ifelse(rownames(d) %in% c(s1, s2, s3, s4, s5, s6), 0, 1)
train = d[rownames(d) %in% c(s1, s2, s3, s4, s5, s6), names(d) %in% c("cvdevent", "permth_int", "wave", dems, acc, comp, nutr1, nutr2)]
test = d[!rownames(d) %in% c(s1, s2, s3, s4, s5, s6),  names(d) %in% c("cvdevent", "permth_int", "wave", dems, acc, comp, nutr1, nutr2)]

##Supplementary Table 2: Descriptive summary of Table 1 variables by Train/Test status
supp2 = mktab(data=d, var.names=c("cvdevent", "permth_int", "wave", "age", "sex", "black", "hispanic", "total_chol", "hdl", "sbp", "dbp", "bpmeds", "dm", "tob", "hei", "ahei", "mds", "dash"), ind.cat=c(1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0), group.name="test", cfn=describeMean, miss="always", pval=TRUE, tot=FALSE, digit=1)

word.tab(tab=supp2, dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Tables/Supplementary_Table2_2019-01-14.docx", help=TRUE)

########################
##MULTIPLE IMPUTATAION##
########################

##Training set
dim(train)
ini = mice(train, maxit=0)

pred2 = ini$pred
pred2  # set predictor matrix to 0 for vars that shouldn't be in imp model
set.seed(11)
#imp1 = mice(data=train, m=10, predictorMatrix=-(diag(121)-1), maxit=1)
imp1 = mice(data=train, m=10, predictorMatrix=pred2, maxit=1)

##Save imputed train data sets
completed1 = mice::complete(imp1, 'long')
#write.csv(completed1, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Imputed_train_2019-01-15.csv", row.names=FALSE)

##Test set
dim(test)
ini2 = mice(test, maxit=0)

pred3 = ini2$pred
pred3  # set predictor matrix to 0 for vars that shouldn't be in imp model
set.seed(11)
imp2 = mice(data=test, m=10, predictorMatrix=pred3, maxit=1)

##Save imputed train data sets
completed2 = mice::complete(imp2, 'long')
#write.csv(completed2, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Imputed_test_2019-01-15.csv", row.names=FALSE)

##1. Apply all models to each training set
##2. Get predictions for all models for each training set
##3. Get discrimination/calibration for each model for each training set
##4. Externally validate



#######################
##TRAINING THE MODELS##
#######################
tr = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Imputed_train_2019-01-15.csv", header=TRUE)

tr1 = tr[tr$.imp==1, ]
tr2 = tr[tr$.imp==2, ]
tr3 = tr[tr$.imp==3, ]
tr4 = tr[tr$.imp==4, ]
tr5 = tr[tr$.imp==5, ]
tr6 = tr[tr$.imp==6, ]
tr7 = tr[tr$.imp==7, ]
tr8 = tr[tr$.imp==8, ]
tr9 = tr[tr$.imp==9, ]
tr10 = tr[tr$.imp==10, ]

##Fit models
##Model 1: Dems and acc only
##A: Cox
m1a1 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr1)
m1a2 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr2)
m1a3 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr3)
m1a4 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr4)
m1a5 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr5)
m1a6 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr6)
m1a7 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr7)
m1a8 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr7)
m1a9 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr8)
m1a10 = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr10)

##B: GBM
m1b1 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr1)
m1b2 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr2)
m1b3 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr3)
m1b4 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr4)
m1b5 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr5)
m1b6 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr6)
m1b7 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr7)
m1b8 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr8)
m1b9 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr9)
m1b10 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", data=tr10)


##C: cforest with with dems, acc
m1c1 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr1)
m1c2 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr2)
m1c3 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr3)
m1c4 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr4)
m1c5 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr5)
m1c6 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr6)
m1c7 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr7)
m1c8 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr8)
m1c9 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr8)
m1c10 = cforest(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), control = cforest_unbiased(ntree = 20), data=tr10)


##Model 2: dems, acc, hei
##A: Cox model
m2a1 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr1)
m2a2 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr2)
m2a3 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr3)
m2a4 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr4)
m2a5 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr5)
m2a6 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr6)
m2a7 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr7)
m2a8 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr8)
m2a9 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr9)
m2a10 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr10)

##B: GBM
m2b1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr1)
m2b2 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr2)
m2b3 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr3)
m2b4 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr4)
m2b5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr5)
m2b6 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr6)
m2b7 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr7)
m2b8 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr8)
m2b9 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr8)
m2b10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", data=tr10)

##C: cforest
m2c1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr1)
m2c2 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr2)
m2c3 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr3)
m2c4 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr4)
m2c5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr5)
m2c6 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr6)
m2c7 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr7)
m2c8 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr8)
m2c9 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr9)
m2c10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr10)


##Model 3: dems, acc, ahei
##A: Cox model
m3a1 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr1)
m3a2 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr2)
m3a3 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr3)
m3a4 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr4)
m3a5 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr5)
m3a6 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr6)
m3a7 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr7)
m3a8 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr8)
m3a9 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr9)
m3a10 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr10)

##B: GBM
m3b1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr1)
m3b2 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr2)
m3b3 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr3)
m3b4 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr4)
m3b5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr5)
m3b6 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr6)
m3b7 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr7)
m3b8 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr8)
m3b9 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr9)
m3b10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", data=tr10)

##C: cforest
m3c1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr1)
m3c2 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr2)
m3c3 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr3)
m3c4 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr4)
m3c5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr5)
m3c6 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr6)
m3c7 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr7)
m3c8 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr8)
m3c9 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr9)
m3c10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr10)


##Model 4: dems, acc, mds
##A: Cox model
m4a1 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr1)
m4a2 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr2)
m4a3 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr3)
m4a4 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr4)
m4a5 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr5)
m4a6 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr6)
m4a7 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr7)
m4a8 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr8)
m4a9 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr9)
m4a10 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr10)

##B: GBM
m4b1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr1)
m4b2 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr2)
m4b3 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr3)
m4b4 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr4)
m4b5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr5)
m4b6 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr6)
m4b7 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr7)
m4b8 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr8)
m4b9 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr8)
m4b10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", data=tr10)

##C: cforest
m4c1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr1)
m4c2 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr2)
m4c3 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr3)
m4c4 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr4)
m4c5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr5)
m4c6 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr6)
m4c7 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr7)
m4c8 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr8)
m4c9 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr9)
m4c10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr10)


##Model 5: dems, acc, dash
##A: Cox model
m5a1 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr1)
m5a2 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr2)
m5a3 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr3)
m5a4 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr4)
m5a5 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr5)
m5a6 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr6)
m5a7 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr7)
m5a8 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr8)
m5a9 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr9)
m5a10 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr10)

##B: GBM
m5b1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr1)
m5b2 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr2)
m5b3 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr3)
m5b4 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr4)
m5b5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr5)
m5b6 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr6)
m5b7 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr7)
m5b8 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr8)
m5b9 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr8)
m5b10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", data=tr10)

##C: cforest
m5c1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr1)
m5c2 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr2)
m5c3 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr3)
m5c4 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr4)
m5c5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr5)
m5c6 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr6)
m5c7 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr7)
m5c8 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr8)
m5c9 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr9)
m5c10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control = cforest_unbiased(ntree = 20), data=tr10)


##Model 6: dems, acc, all 24 hour recall data (minus "bev_nutrition_g" and "egg_frozen_g" - have NAs there)
nutr = c(nutr1, nutr2)
nutr0 = nutr[!nutr %in% c("bev_nutrition_g", "egg_frozen_g")]

##A: Cox model
m6a1 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr1)
m6a2 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr2)
m6a3 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr3)
m6a4 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr4)
m6a5 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr5)
m6a6 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr6)
m6a7 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr7)
m6a8 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr8)
m6a9 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr9)
m6a10 = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr10)

##B: GBM
m6b1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr1)
m6b2 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr2)
m6b3 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr3)
m6b4 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr4)
m6b5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr5)
m6b6 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr6)
m6b7 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr7)
m6b8 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr8)
m6b9 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr9)
m6b10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", data=tr10)

##C: cforest
m6c1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr1)
m6c2 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr2)
m6c3 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr3)
m6c4 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr4)
m6c5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr5)
m6c6 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr6)
m6c7 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr7)
m6c8 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr8)
m6c9 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr9)
m6c10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), control = cforest_unbiased(ntree = 20), data=tr10)


##Get predictions for all models
##Model 1: Dems and acc only
##A: Cox
pred1a1 = predCox(mod=m1a1, data=tr1, stime="permth_int", time=120)
pred1a2 = predCox(mod=m1a2, data=tr2, stime="permth_int", time=120)
pred1a3 = predCox(mod=m1a3, data=tr3, stime="permth_int", time=120)
pred1a4 = predCox(mod=m1a4, data=tr4, stime="permth_int", time=120)
pred1a5 = predCox(mod=m1a5, data=tr5, stime="permth_int", time=120)
pred1a6 = predCox(mod=m1a6, data=tr6, stime="permth_int", time=120)
pred1a7 = predCox(mod=m1a7, data=tr7, stime="permth_int", time=120)
pred1a8 = predCox(mod=m1a8, data=tr8, stime="permth_int", time=120)
pred1a9 = predCox(mod=m1a9, data=tr9, stime="permth_int", time=120)
pred1a10 = predCox(mod=m1a10, data=tr10, stime="permth_int", time=120)

##B: GBM
pred1b1 = predGBM(mod=m1b1, data=tr1, stime="permth_int", sevent="cvdevent", time=120)
pred1b2 = predGBM(mod=m1b2, data=tr2, stime="permth_int", sevent="cvdevent", time=120)
pred1b3 = predGBM(mod=m1b3, data=tr3, stime="permth_int", sevent="cvdevent", time=120)
pred1b4 = predGBM(mod=m1b4, data=tr4, stime="permth_int", sevent="cvdevent", time=120)
pred1b5 = predGBM(mod=m1b5, data=tr5, stime="permth_int", sevent="cvdevent", time=120)
pred1b6 = predGBM(mod=m1b6, data=tr6, stime="permth_int", sevent="cvdevent", time=120)
pred1b7 = predGBM(mod=m1b7, data=tr7, stime="permth_int", sevent="cvdevent", time=120)
pred1b8 = predGBM(mod=m1b8, data=tr8, stime="permth_int", sevent="cvdevent", time=120)
pred1b9 = predGBM(mod=m1b9, data=tr9, stime="permth_int", sevent="cvdevent", time=120)
pred1b10 = predGBM(mod=m1b10, data=tr10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
pred1c1 = predCF(mod=m1c1, data=tr1, time=120)
pred1c2 = predCF(mod=m1c2, data=tr2, time=120)
pred1c3 = predCF(mod=m1c3, data=tr3, time=120)
pred1c4 = predCF(mod=m1c4, data=tr4, time=120)
pred1c5 = predCF(mod=m1c5, data=tr5, time=120)
pred1c6 = predCF(mod=m1c6, data=tr6, time=120)
pred1c7 = predCF(mod=m1c7, data=tr7, time=120)
pred1c8 = predCF(mod=m1c8, data=tr8, time=120)
pred1c9 = predCF(mod=m1c9, data=tr9, time=120)
pred1c10 = predCF(mod=m1c10, data=tr10, time=120)

##Model 2: Dems, acc, hei
##A: Cox
pred2a1 = predCox(mod=m2a1, data=tr1, stime="permth_int", time=120)
pred2a2 = predCox(mod=m2a2, data=tr2, stime="permth_int", time=120)
pred2a3 = predCox(mod=m2a3, data=tr3, stime="permth_int", time=120)
pred2a4 = predCox(mod=m2a4, data=tr4, stime="permth_int", time=120)
pred2a5 = predCox(mod=m2a5, data=tr5, stime="permth_int", time=120)
pred2a6 = predCox(mod=m2a6, data=tr6, stime="permth_int", time=120)
pred2a7 = predCox(mod=m2a7, data=tr7, stime="permth_int", time=120)
pred2a8 = predCox(mod=m2a8, data=tr8, stime="permth_int", time=120)
pred2a9 = predCox(mod=m2a9, data=tr9, stime="permth_int", time=120)
pred2a10 = predCox(mod=m2a10, data=tr10, stime="permth_int", time=120)

##B: GBM
pred2b1 = predGBM(mod=m2b1, data=tr1, stime="permth_int", sevent="cvdevent", time=120)
pred2b2 = predGBM(mod=m2b2, data=tr2, stime="permth_int", sevent="cvdevent", time=120)
pred2b3 = predGBM(mod=m2b3, data=tr3, stime="permth_int", sevent="cvdevent", time=120)
pred2b4 = predGBM(mod=m2b4, data=tr4, stime="permth_int", sevent="cvdevent", time=120)
pred2b5 = predGBM(mod=m2b5, data=tr5, stime="permth_int", sevent="cvdevent", time=120)
pred2b6 = predGBM(mod=m2b6, data=tr6, stime="permth_int", sevent="cvdevent", time=120)
pred2b7 = predGBM(mod=m2b7, data=tr7, stime="permth_int", sevent="cvdevent", time=120)
pred2b8 = predGBM(mod=m2b8, data=tr8, stime="permth_int", sevent="cvdevent", time=120)
pred2b9 = predGBM(mod=m2b9, data=tr9, stime="permth_int", sevent="cvdevent", time=120)
pred2b10 = predGBM(mod=m2b10, data=tr10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
pred2c1 = predCF(mod=m2c1, data=tr1, time=120)
pred2c2 = predCF(mod=m2c2, data=tr2, time=120)
pred2c3 = predCF(mod=m2c3, data=tr3, time=120)
pred2c4 = predCF(mod=m2c4, data=tr4, time=120)
pred2c5 = predCF(mod=m2c5, data=tr5, time=120)
pred2c6 = predCF(mod=m2c6, data=tr6, time=120)
pred2c7 = predCF(mod=m2c7, data=tr7, time=120)
pred2c8 = predCF(mod=m2c8, data=tr8, time=120)
pred2c9 = predCF(mod=m2c9, data=tr9, time=120)
pred2c10 = predCF(mod=m2c10, data=tr10, time=120)

##Model 3: Dems, acc, ahei
##A: Cox
pred3a1 = predCox(mod=m3a1, data=tr1, stime="permth_int", time=120)
pred3a2 = predCox(mod=m3a2, data=tr2, stime="permth_int", time=120)
pred3a3 = predCox(mod=m3a3, data=tr3, stime="permth_int", time=120)
pred3a4 = predCox(mod=m3a4, data=tr4, stime="permth_int", time=120)
pred3a5 = predCox(mod=m3a5, data=tr5, stime="permth_int", time=120)
pred3a6 = predCox(mod=m3a6, data=tr6, stime="permth_int", time=120)
pred3a7 = predCox(mod=m3a7, data=tr7, stime="permth_int", time=120)
pred3a8 = predCox(mod=m3a8, data=tr8, stime="permth_int", time=120)
pred3a9 = predCox(mod=m3a9, data=tr9, stime="permth_int", time=120)
pred3a10 = predCox(mod=m3a10, data=tr10, stime="permth_int", time=120)

##B: GBM
pred3b1 = predGBM(mod=m3b1, data=tr1, stime="permth_int", sevent="cvdevent", time=120)
pred3b2 = predGBM(mod=m3b2, data=tr2, stime="permth_int", sevent="cvdevent", time=120)
pred3b3 = predGBM(mod=m3b3, data=tr3, stime="permth_int", sevent="cvdevent", time=120)
pred3b4 = predGBM(mod=m3b4, data=tr4, stime="permth_int", sevent="cvdevent", time=120)
pred3b5 = predGBM(mod=m3b5, data=tr5, stime="permth_int", sevent="cvdevent", time=120)
pred3b6 = predGBM(mod=m3b6, data=tr6, stime="permth_int", sevent="cvdevent", time=120)
pred3b7 = predGBM(mod=m3b7, data=tr7, stime="permth_int", sevent="cvdevent", time=120)
pred3b8 = predGBM(mod=m3b8, data=tr8, stime="permth_int", sevent="cvdevent", time=120)
pred3b9 = predGBM(mod=m3b9, data=tr9, stime="permth_int", sevent="cvdevent", time=120)
pred3b10 = predGBM(mod=m3b10, data=tr10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
pred3c1 = predCF(mod=m3c1, data=tr1, time=120)
pred3c2 = predCF(mod=m3c2, data=tr2, time=120)
pred3c3 = predCF(mod=m3c3, data=tr3, time=120)
pred3c4 = predCF(mod=m3c4, data=tr4, time=120)
pred3c5 = predCF(mod=m3c5, data=tr5, time=120)
pred3c6 = predCF(mod=m3c6, data=tr6, time=120)
pred3c7 = predCF(mod=m3c7, data=tr7, time=120)
pred3c8 = predCF(mod=m3c8, data=tr8, time=120)
pred3c9 = predCF(mod=m3c9, data=tr9, time=120)
pred3c10 = predCF(mod=m3c10, data=tr10, time=120)

##Model 4: Dems, acc, mds
##A: Cox
pred4a1 = predCox(mod=m4a1, data=tr1, stime="permth_int", time=120)
pred4a2 = predCox(mod=m4a2, data=tr2, stime="permth_int", time=120)
pred4a3 = predCox(mod=m4a3, data=tr3, stime="permth_int", time=120)
pred4a4 = predCox(mod=m4a4, data=tr4, stime="permth_int", time=120)
pred4a5 = predCox(mod=m4a5, data=tr5, stime="permth_int", time=120)
pred4a6 = predCox(mod=m4a6, data=tr6, stime="permth_int", time=120)
pred4a7 = predCox(mod=m4a7, data=tr7, stime="permth_int", time=120)
pred4a8 = predCox(mod=m4a8, data=tr8, stime="permth_int", time=120)
pred4a9 = predCox(mod=m4a9, data=tr9, stime="permth_int", time=120)
pred4a10 = predCox(mod=m4a10, data=tr10, stime="permth_int", time=120)

##B: GBM
pred4b1 = predGBM(mod=m4b1, data=tr1, stime="permth_int", sevent="cvdevent", time=120)
pred4b2 = predGBM(mod=m4b2, data=tr2, stime="permth_int", sevent="cvdevent", time=120)
pred4b3 = predGBM(mod=m4b3, data=tr3, stime="permth_int", sevent="cvdevent", time=120)
pred4b4 = predGBM(mod=m4b4, data=tr4, stime="permth_int", sevent="cvdevent", time=120)
pred4b5 = predGBM(mod=m4b5, data=tr5, stime="permth_int", sevent="cvdevent", time=120)
pred4b6 = predGBM(mod=m4b6, data=tr6, stime="permth_int", sevent="cvdevent", time=120)
pred4b7 = predGBM(mod=m4b7, data=tr7, stime="permth_int", sevent="cvdevent", time=120)
pred4b8 = predGBM(mod=m4b8, data=tr8, stime="permth_int", sevent="cvdevent", time=120)
pred4b9 = predGBM(mod=m4b9, data=tr9, stime="permth_int", sevent="cvdevent", time=120)
pred4b10 = predGBM(mod=m4b10, data=tr10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
pred4c1 = predCF(mod=m4c1, data=tr1, time=120)
pred4c2 = predCF(mod=m4c2, data=tr2, time=120)
pred4c3 = predCF(mod=m4c3, data=tr3, time=120)
pred4c4 = predCF(mod=m4c4, data=tr4, time=120)
pred4c5 = predCF(mod=m4c5, data=tr5, time=120)
pred4c6 = predCF(mod=m4c6, data=tr6, time=120)
pred4c7 = predCF(mod=m4c7, data=tr7, time=120)
pred4c8 = predCF(mod=m4c8, data=tr8, time=120)
pred4c9 = predCF(mod=m4c9, data=tr9, time=120)
pred4c10 = predCF(mod=m4c10, data=tr10, time=120)

##Model 5: Dems, acc, dash
##A: Cox
pred5a1 = predCox(mod=m5a1, data=tr1, stime="permth_int", time=120)
pred5a2 = predCox(mod=m5a2, data=tr2, stime="permth_int", time=120)
pred5a3 = predCox(mod=m5a3, data=tr3, stime="permth_int", time=120)
pred5a4 = predCox(mod=m5a4, data=tr4, stime="permth_int", time=120)
pred5a5 = predCox(mod=m5a5, data=tr5, stime="permth_int", time=120)
pred5a6 = predCox(mod=m5a6, data=tr6, stime="permth_int", time=120)
pred5a7 = predCox(mod=m5a7, data=tr7, stime="permth_int", time=120)
pred5a8 = predCox(mod=m5a8, data=tr8, stime="permth_int", time=120)
pred5a9 = predCox(mod=m5a9, data=tr9, stime="permth_int", time=120)
pred5a10 = predCox(mod=m5a10, data=tr10, stime="permth_int", time=120)

##B: GBM
pred5b1 = predGBM(mod=m5b1, data=tr1, stime="permth_int", sevent="cvdevent", time=120)
pred5b2 = predGBM(mod=m5b2, data=tr2, stime="permth_int", sevent="cvdevent", time=120)
pred5b3 = predGBM(mod=m5b3, data=tr3, stime="permth_int", sevent="cvdevent", time=120)
pred5b4 = predGBM(mod=m5b4, data=tr4, stime="permth_int", sevent="cvdevent", time=120)
pred5b5 = predGBM(mod=m5b5, data=tr5, stime="permth_int", sevent="cvdevent", time=120)
pred5b6 = predGBM(mod=m5b6, data=tr6, stime="permth_int", sevent="cvdevent", time=120)
pred5b7 = predGBM(mod=m5b7, data=tr7, stime="permth_int", sevent="cvdevent", time=120)
pred5b8 = predGBM(mod=m5b8, data=tr8, stime="permth_int", sevent="cvdevent", time=120)
pred5b9 = predGBM(mod=m5b9, data=tr9, stime="permth_int", sevent="cvdevent", time=120)
pred5b10 = predGBM(mod=m5b10, data=tr10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
pred5c1 = predCF(mod=m5c1, data=tr1, time=120)
pred5c2 = predCF(mod=m5c2, data=tr2, time=120)
pred5c3 = predCF(mod=m5c3, data=tr3, time=120)
pred5c4 = predCF(mod=m5c4, data=tr4, time=120)
pred5c5 = predCF(mod=m5c5, data=tr5, time=120)
pred5c6 = predCF(mod=m5c6, data=tr6, time=120)
pred5c7 = predCF(mod=m5c7, data=tr7, time=120)
pred5c8 = predCF(mod=m5c8, data=tr8, time=120)
pred5c9 = predCF(mod=m5c9, data=tr9, time=120)
pred5c10 = predCF(mod=m5c10, data=tr10, time=120)

##Model 6: Dems, acc, all nutrition variables
##A: Cox
pred6a1 = predCox(mod=m6a1, data=tr1, stime="permth_int", time=120)
pred6a2 = predCox(mod=m6a2, data=tr2, stime="permth_int", time=120)
pred6a3 = predCox(mod=m6a3, data=tr3, stime="permth_int", time=120)
pred6a4 = predCox(mod=m6a4, data=tr4, stime="permth_int", time=120)
pred6a5 = predCox(mod=m6a5, data=tr5, stime="permth_int", time=120)
pred6a6 = predCox(mod=m6a6, data=tr6, stime="permth_int", time=120)
pred6a7 = predCox(mod=m6a7, data=tr7, stime="permth_int", time=120)
pred6a8 = predCox(mod=m6a8, data=tr8, stime="permth_int", time=120)
pred6a9 = predCox(mod=m6a9, data=tr9, stime="permth_int", time=120)
pred6a10 = predCox(mod=m6a10, data=tr10, stime="permth_int", time=120)

##B: GBM
pred6b1 = predGBM(mod=m6b1, data=tr1, stime="permth_int", sevent="cvdevent", time=120)
pred6b2 = predGBM(mod=m6b2, data=tr2, stime="permth_int", sevent="cvdevent", time=120)
pred6b3 = predGBM(mod=m6b3, data=tr3, stime="permth_int", sevent="cvdevent", time=120)
pred6b4 = predGBM(mod=m6b4, data=tr4, stime="permth_int", sevent="cvdevent", time=120)
pred6b5 = predGBM(mod=m6b5, data=tr5, stime="permth_int", sevent="cvdevent", time=120)
pred6b6 = predGBM(mod=m6b6, data=tr6, stime="permth_int", sevent="cvdevent", time=120)
pred6b7 = predGBM(mod=m6b7, data=tr7, stime="permth_int", sevent="cvdevent", time=120)
pred6b8 = predGBM(mod=m6b8, data=tr8, stime="permth_int", sevent="cvdevent", time=120)
pred6b9 = predGBM(mod=m6b9, data=tr9, stime="permth_int", sevent="cvdevent", time=120)
pred6b10 = predGBM(mod=m6b10, data=tr10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
pred6c1 = predCF(mod=m6c1, data=tr1, time=120)
pred6c2 = predCF(mod=m6c2, data=tr2, time=120)
pred6c3 = predCF(mod=m6c3, data=tr3, time=120)
pred6c4 = predCF(mod=m6c4, data=tr4, time=120)
pred6c5 = predCF(mod=m6c5, data=tr5, time=120)
pred6c6 = predCF(mod=m6c6, data=tr6, time=120)
pred6c7 = predCF(mod=m6c7, data=tr7, time=120)
pred6c8 = predCF(mod=m6c8, data=tr8, time=120)
pred6c9 = predCF(mod=m6c9, data=tr9, time=120)
pred6c10 = predCF(mod=m6c10, data=tr10, time=120)


##Internal validation for all models
##Define true event for each data set
event1 = ifelse(tr1$permth_int<=120 & tr1$cvdevent==1, 1, 0)
event2 = ifelse(tr2$permth_int<=120 & tr2$cvdevent==1, 1, 0)
event3 = ifelse(tr3$permth_int<=120 & tr3$cvdevent==1, 1, 0)
event4 = ifelse(tr4$permth_int<=120 & tr4$cvdevent==1, 1, 0)
event5 = ifelse(tr5$permth_int<=120 & tr5$cvdevent==1, 1, 0)
event6 = ifelse(tr6$permth_int<=120 & tr6$cvdevent==1, 1, 0)
event7 = ifelse(tr7$permth_int<=120 & tr7$cvdevent==1, 1, 0)
event8 = ifelse(tr8$permth_int<=120 & tr8$cvdevent==1, 1, 0)
event9 = ifelse(tr9$permth_int<=120 & tr9$cvdevent==1, 1, 0)
event10 = ifelse(tr10$permth_int<=120 & tr10$cvdevent==1, 1, 0)

##Model 1: Dems and acc only
##A: Cox
s1a1 = getInfo(event1, pred1a1)
s1a2 = getInfo(event2, pred1a2)
s1a3 = getInfo(event3, pred1a3)
s1a4 = getInfo(event4, pred1a4)
s1a5 = getInfo(event5, pred1a5)
s1a6 = getInfo(event6, pred1a6)
s1a7 = getInfo(event7, pred1a7)
s1a8 = getInfo(event8, pred1a8)
s1a9 = getInfo(event9, pred1a9)
s1a10 = getInfo(event10, pred1a10)

##B: GBM
s1b1 = getInfo(event1, pred1b1)
s1b2 = getInfo(event2, pred1b2)
s1b3 = getInfo(event3, pred1b3)
s1b4 = getInfo(event4, pred1b4)
s1b5 = getInfo(event5, pred1b5)
s1b6 = getInfo(event6, pred1b6)
s1b7 = getInfo(event7, pred1b7)
s1b8 = getInfo(event8, pred1b8)
s1b9 = getInfo(event9, pred1b9)
s1b10 = getInfo(event10, pred1b10)

##C: Cforest
s1c1 = getInfo(event1, pred1c1)
s1c2 = getInfo(event2, pred1c2)
s1c3 = getInfo(event3, pred1c3)
s1c4 = getInfo(event4, pred1c4)
s1c5 = getInfo(event5, pred1c5)
s1c6 = getInfo(event6, pred1c6)
s1c7 = getInfo(event7, pred1c7)
s1c8 = getInfo(event8, pred1c8)
s1c9 = getInfo(event9, pred1c9)
s1c10 = getInfo(event10, pred1c10)

##Model 2: Dems, hei
##A: Cox
s2a1 = getInfo(event1, pred2a1)
s2a2 = getInfo(event2, pred2a2)
s2a3 = getInfo(event3, pred2a3)
s2a4 = getInfo(event4, pred2a4)
s2a5 = getInfo(event5, pred2a5)
s2a6 = getInfo(event6, pred2a6)
s2a7 = getInfo(event7, pred2a7)
s2a8 = getInfo(event8, pred2a8)
s2a9 = getInfo(event9, pred2a9)
s2a10 = getInfo(event10, pred2a10)

##B: GBM
s2b1 = getInfo(event1, pred2b1)
s2b2 = getInfo(event2, pred2b2)
s2b3 = getInfo(event3, pred2b3)
s2b4 = getInfo(event4, pred2b4)
s2b5 = getInfo(event5, pred2b5)
s2b6 = getInfo(event6, pred2b6)
s2b7 = getInfo(event7, pred2b7)
s2b8 = getInfo(event8, pred2b8)
s2b9 = getInfo(event9, pred2b9)
s2b10 = getInfo(event10, pred2b10)

##C: Cforest
s2c1 = getInfo(event1, pred2c1)
s2c2 = getInfo(event2, pred2c2)
s2c3 = getInfo(event3, pred2c3)
s2c4 = getInfo(event4, pred2c4)
s2c5 = getInfo(event5, pred2c5)
s2c6 = getInfo(event6, pred2c6)
s2c7 = getInfo(event7, pred2c7)
s2c8 = getInfo(event8, pred2c8)
s2c9 = getInfo(event9, pred2c9)
s2c10 = getInfo(event10, pred2c10)

##Model 3: Dems, acc, ahei
##A: Cox
s3a1 = getInfo(event1, pred3a1)
s3a2 = getInfo(event2, pred3a2)
s3a3 = getInfo(event3, pred3a3)
s3a4 = getInfo(event4, pred3a4)
s3a5 = getInfo(event5, pred3a5)
s3a6 = getInfo(event6, pred3a6)
s3a7 = getInfo(event7, pred3a7)
s3a8 = getInfo(event8, pred3a8)
s3a9 = getInfo(event9, pred3a9)
s3a10 = getInfo(event10, pred3a10)

##B: GBM
s3b1 = getInfo(event1, pred3b1)
s3b2 = getInfo(event2, pred3b2)
s3b3 = getInfo(event3, pred3b3)
s3b4 = getInfo(event4, pred3b4)
s3b5 = getInfo(event5, pred3b5)
s3b6 = getInfo(event6, pred3b6)
s3b7 = getInfo(event7, pred3b7)
s3b8 = getInfo(event8, pred3b8)
s3b9 = getInfo(event9, pred3b9)
s3b10 = getInfo(event10, pred3b10)

##C: Cforest
s3c1 = getInfo(event1, pred3c1)
s3c2 = getInfo(event2, pred3c2)
s3c3 = getInfo(event3, pred3c3)
s3c4 = getInfo(event4, pred3c4)
s3c5 = getInfo(event5, pred3c5)
s3c6 = getInfo(event6, pred3c6)
s3c7 = getInfo(event7, pred3c7)
s3c8 = getInfo(event8, pred3c8)
s3c9 = getInfo(event9, pred3c9)
s3c10 = getInfo(event10, pred3c10)

##Model 4: Dems, acc, mds
##A: Cox
s4a1 = getInfo(event1, pred4a1)
s4a2 = getInfo(event2, pred4a2)
s4a3 = getInfo(event3, pred4a3)
s4a4 = getInfo(event4, pred4a4)
s4a5 = getInfo(event5, pred4a5)
s4a6 = getInfo(event6, pred4a6)
s4a7 = getInfo(event7, pred4a7)
s4a8 = getInfo(event8, pred4a8)
s4a9 = getInfo(event9, pred4a9)
s4a10 = getInfo(event10, pred4a10)

##B: GBM
s4b1 = getInfo(event1, pred4b1)
s4b2 = getInfo(event2, pred4b2)
s4b3 = getInfo(event3, pred4b3)
s4b4 = getInfo(event4, pred4b4)
s4b5 = getInfo(event5, pred4b5)
s4b6 = getInfo(event6, pred4b6)
s4b7 = getInfo(event7, pred4b7)
s4b8 = getInfo(event8, pred4b8)
s4b9 = getInfo(event9, pred4b9)
s4b10 = getInfo(event10, pred4b10)

##C: Cforest
s4c1 = getInfo(event1, pred4c1)
s4c2 = getInfo(event2, pred4c2)
s4c3 = getInfo(event3, pred4c3)
s4c4 = getInfo(event4, pred4c4)
s4c5 = getInfo(event5, pred4c5)
s4c6 = getInfo(event6, pred4c6)
s4c7 = getInfo(event7, pred4c7)
s4c8 = getInfo(event8, pred4c8)
s4c9 = getInfo(event9, pred4c9)
s4c10 = getInfo(event10, pred4c10)

##Model 5: Dems, acc, dash
##A: Cox
s5a1 = getInfo(event1, pred5a1)
s5a2 = getInfo(event2, pred5a2)
s5a3 = getInfo(event3, pred5a3)
s5a4 = getInfo(event4, pred5a4)
s5a5 = getInfo(event5, pred5a5)
s5a6 = getInfo(event6, pred5a6)
s5a7 = getInfo(event7, pred5a7)
s5a8 = getInfo(event8, pred5a8)
s5a9 = getInfo(event9, pred5a9)
s5a10 = getInfo(event10, pred5a10)

##B: GBM
s5b1 = getInfo(event1, pred5b1)
s5b2 = getInfo(event2, pred5b2)
s5b3 = getInfo(event3, pred5b3)
s5b4 = getInfo(event4, pred5b4)
s5b5 = getInfo(event5, pred5b5)
s5b6 = getInfo(event6, pred5b6)
s5b7 = getInfo(event7, pred5b7)
s5b8 = getInfo(event8, pred5b8)
s5b9 = getInfo(event9, pred5b9)
s5b10 = getInfo(event10, pred5b10)

##C: Cforest
s5c1 = getInfo(event1, pred5c1)
s5c2 = getInfo(event2, pred5c2)
s5c3 = getInfo(event3, pred5c3)
s5c4 = getInfo(event4, pred5c4)
s5c5 = getInfo(event5, pred5c5)
s5c6 = getInfo(event6, pred5c6)
s5c7 = getInfo(event7, pred5c7)
s5c8 = getInfo(event8, pred5c8)
s5c9 = getInfo(event9, pred5c9)
s5c10 = getInfo(event10, pred5c10)

##Model 6: Dems, acc, all 24 hour recall data
##A: Cox
s6a1 = getInfo(event1, pred6a1)
s6a2 = getInfo(event2, pred6a2)
s6a3 = getInfo(event3, pred6a3)
s6a4 = getInfo(event4, pred6a4)
s6a5 = getInfo(event5, pred6a5)
s6a6 = getInfo(event6, pred6a6)
s6a7 = getInfo(event7, pred6a7)
s6a8 = getInfo(event8, pred6a8)
s6a9 = getInfo(event9, pred6a9)
s6a10 = getInfo(event10, pred6a10)

##B: GBM
s6b1 = getInfo(event1, pred6b1)
s6b2 = getInfo(event2, pred6b2)
s6b3 = getInfo(event3, pred6b3)
s6b4 = getInfo(event4, pred6b4)
s6b5 = getInfo(event5, pred6b5)
s6b6 = getInfo(event6, pred6b6)
s6b7 = getInfo(event7, pred6b7)
s6b8 = getInfo(event8, pred6b8)
s6b9 = getInfo(event9, pred6b9)
s6b10 = getInfo(event10, pred6b10)

##C: Cforest
s6c1 = getInfo(event1, pred6c1)
s6c2 = getInfo(event2, pred6c2)
s6c3 = getInfo(event3, pred6c3)
s6c4 = getInfo(event4, pred6c4)
s6c5 = getInfo(event5, pred6c5)
s6c6 = getInfo(event6, pred6c6)
s6c7 = getInfo(event7, pred6c7)
s6c8 = getInfo(event8, pred6c8)
s6c9 = getInfo(event9, pred6c9)
s6c10 = getInfo(event10, pred6c10)

##Put all results in one data set
##C-statistics
cstat = rbind(
    rbind(s1a1$C, s1a2$C, s1a3$C, s1a4$C, s1a5$C, s1a6$C, s1a7$C, s1a8$C, s1a9$C, s1a10$C),
    rbind(s1b1$C, s1b2$C, s1b3$C, s1b4$C, s1b5$C, s1b6$C, s1b7$C, s1b8$C, s1b9$C, s1b10$C),
    rbind(s1c1$C, s1c2$C, s1c3$C, s1c4$C, s1c5$C, s1c6$C, s1c7$C, s1c8$C, s1c9$C, s1c10$C),
    rbind(s2a1$C, s2a2$C, s2a3$C, s2a4$C, s2a5$C, s2a6$C, s2a7$C, s2a8$C, s2a9$C, s2a10$C),
    rbind(s2b1$C, s2b2$C, s2b3$C, s2b4$C, s2b5$C, s2b6$C, s2b7$C, s2b8$C, s2b9$C, s2b10$C),
    rbind(s2c1$C, s2c2$C, s2c3$C, s2c4$C, s2c5$C, s2c6$C, s2c7$C, s2c8$C, s2c9$C, s2c10$C),
    rbind(s3a1$C, s3a2$C, s3a3$C, s3a4$C, s3a5$C, s3a6$C, s3a7$C, s3a8$C, s3a9$C, s3a10$C),
    rbind(s3b1$C, s3b2$C, s3b3$C, s3b4$C, s3b5$C, s3b6$C, s3b7$C, s3b8$C, s3b9$C, s3b10$C),
    rbind(s3c1$C, s3c2$C, s3c3$C, s3c4$C, s3c5$C, s3c6$C, s3c7$C, s3c8$C, s3c9$C, s3c10$C),
    rbind(s4a1$C, s4a2$C, s4a3$C, s4a4$C, s4a5$C, s4a6$C, s4a7$C, s4a8$C, s4a9$C, s4a10$C),
    rbind(s4b1$C, s4b2$C, s4b3$C, s4b4$C, s4b5$C, s4b6$C, s4b7$C, s4b8$C, s4b9$C, s4b10$C),
    rbind(s4c1$C, s4c2$C, s4c3$C, s4c4$C, s4c5$C, s4c6$C, s4c7$C, s4c8$C, s4c9$C, s4c10$C),
    rbind(s5a1$C, s5a2$C, s5a3$C, s5a4$C, s5a5$C, s5a6$C, s5a7$C, s5a8$C, s5a9$C, s5a10$C),
    rbind(s5b1$C, s5b2$C, s5b3$C, s5b4$C, s5b5$C, s5b6$C, s5b7$C, s5b8$C, s5b9$C, s5b10$C),
    rbind(s5c1$C, s5c2$C, s5c3$C, s5c4$C, s5c5$C, s5c6$C, s5c7$C, s5c8$C, s5c9$C, s5c10$C),
    rbind(s6a1$C, s6a2$C, s6a3$C, s6a4$C, s6a5$C, s6a6$C, s6a7$C, s6a8$C, s6a9$C, s6a10$C),
    rbind(s6b1$C, s6b2$C, s6b3$C, s6b4$C, s6b5$C, s6b6$C, s6b7$C, s6b8$C, s6b9$C, s6b10$C),
    rbind(s6c1$C, s6c2$C, s6c3$C, s6c4$C, s6c5$C, s6c6$C, s6c7$C, s6c8$C, s6c9$C, s6c10$C))

colnames(cstat) = c("est", "l", "u")
cstat = as.data.frame(cstat)
cstat$model = rep(c(1:6), each=30)
cstat$type = rep(rep(c("a", "b", "c"), each=10), 6)
cstat$imp = rep(c(1:10), 18)

##Make table of confidence intervals using Rubin's rules
cstat$se = ((cstat$u-cstat$l)/2) / qnorm(0.975)
cstat$ID = paste(cstat$model, cstat$type, sep="")
cstat2 = cstat[, names(cstat) %in% c("ID", "est", "se")]

tabCi = rbind(
    c(MI.conf.int(cstat2$est[cstat2$ID=="1a"], cstat2$se[cstat2$ID=="1a"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="1b"], cstat2$se[cstat2$ID=="1b"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="1c"], cstat2$se[cstat2$ID=="1c"])$pres),
    c(MI.conf.int(cstat2$est[cstat2$ID=="2a"], cstat2$se[cstat2$ID=="2a"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="2b"], cstat2$se[cstat2$ID=="2b"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="2c"], cstat2$se[cstat2$ID=="2c"])$pres),
    c(MI.conf.int(cstat2$est[cstat2$ID=="3a"], cstat2$se[cstat2$ID=="3a"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="3b"], cstat2$se[cstat2$ID=="3b"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="3c"], cstat2$se[cstat2$ID=="3c"])$pres),
    c(MI.conf.int(cstat2$est[cstat2$ID=="4a"], cstat2$se[cstat2$ID=="4a"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="4b"], cstat2$se[cstat2$ID=="4b"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="4c"], cstat2$se[cstat2$ID=="4c"])$pres),
    c(MI.conf.int(cstat2$est[cstat2$ID=="5a"], cstat2$se[cstat2$ID=="5a"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="5b"], cstat2$se[cstat2$ID=="5b"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="5c"], cstat2$se[cstat2$ID=="5c"])$pres),
    c(MI.conf.int(cstat2$est[cstat2$ID=="6a"], cstat2$se[cstat2$ID=="6a"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="6b"], cstat2$se[cstat2$ID=="6b"])$pres, MI.conf.int(cstat2$est[cstat2$ID=="6c"], cstat2$se[cstat2$ID=="6c"])$pres)
)
rownames(tabCi) = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
colnames(tabCi) = c("a: Cox", "b: GBM", "c: cforest")

word.tab(tab=tabCi, dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Tables/Internal_C_2019-02-04.docx", help=TRUE)

##GND slope and CI
gnd = rbind(
    rbind(s1a1$GND, s1a2$GND, s1a3$GND, s1a4$GND, s1a5$GND, s1a6$GND, s1a7$GND, s1a8$GND, s1a9$GND, s1a10$GND),
    rbind(s1b1$GND, s1b2$GND, s1b3$GND, s1b4$GND, s1b5$GND, s1b6$GND, s1b7$GND, s1b8$GND, s1b9$GND, s1b10$GND),
    rbind(s1c1$GND, s1c2$GND, s1c3$GND, s1c4$GND, s1c5$GND, s1c6$GND, s1c7$GND, s1c8$GND, s1c9$GND, s1c10$GND),
    rbind(s2a1$GND, s2a2$GND, s2a3$GND, s2a4$GND, s2a5$GND, s2a6$GND, s2a7$GND, s2a8$GND, s2a9$GND, s2a10$GND),
    rbind(s2b1$GND, s2b2$GND, s2b3$GND, s2b4$GND, s2b5$GND, s2b6$GND, s2b7$GND, s2b8$GND, s2b9$GND, s2b10$GND),
    rbind(s2c1$GND, s2c2$GND, s2c3$GND, s2c4$GND, s2c5$GND, s2c6$GND, s2c7$GND, s2c8$GND, s2c9$GND, s2c10$GND),
    rbind(s3a1$GND, s3a2$GND, s3a3$GND, s3a4$GND, s3a5$GND, s3a6$GND, s3a7$GND, s3a8$GND, s3a9$GND, s3a10$GND),
    rbind(s3b1$GND, s3b2$GND, s3b3$GND, s3b4$GND, s3b5$GND, s3b6$GND, s3b7$GND, s3b8$GND, s3b9$GND, s3b10$GND),
    rbind(s3c1$GND, s3c2$GND, s3c3$GND, s3c4$GND, s3c5$GND, s3c6$GND, s3c7$GND, s3c8$GND, s3c9$GND, s3c10$GND),
    rbind(s4a1$GND, s4a2$GND, s4a3$GND, s4a4$GND, s4a5$GND, s4a6$GND, s4a7$GND, s4a8$GND, s4a9$GND, s4a10$GND),
    rbind(s4b1$GND, s4b2$GND, s4b3$GND, s4b4$GND, s4b5$GND, s4b6$GND, s4b7$GND, s4b8$GND, s4b9$GND, s4b10$GND),
    rbind(s4c1$GND, s4c2$GND, s4c3$GND, s4c4$GND, s4c5$GND, s4c6$GND, s4c7$GND, s4c8$GND, s4c9$GND, s4c10$GND),
    rbind(s5a1$GND, s5a2$GND, s5a3$GND, s5a4$GND, s5a5$GND, s5a6$GND, s5a7$GND, s5a8$GND, s5a9$GND, s5a10$GND),
    rbind(s5b1$GND, s5b2$GND, s5b3$GND, s5b4$GND, s5b5$GND, s5b6$GND, s5b7$GND, s5b8$GND, s5b9$GND, s5b10$GND),
    rbind(s5c1$GND, s5c2$GND, s5c3$GND, s5c4$GND, s5c5$GND, s5c6$GND, s5c7$GND, s5c8$GND, s5c9$GND, s5c10$GND),
    rbind(s6a1$GND, s6a2$GND, s6a3$GND, s6a4$GND, s6a5$GND, s6a6$GND, s6a7$GND, s6a8$GND, s6a9$GND, s6a10$GND),
    rbind(s6b1$GND, s6b2$GND, s6b3$GND, s6b4$GND, s6b5$GND, s6b6$GND, s6b7$GND, s6b8$GND, s6b9$GND, s6b10$GND),
    rbind(s6c1$GND, s6c2$GND, s6c3$GND, s6c4$GND, s6c5$GND, s6c6$GND, s6c7$GND, s6c8$GND, s6c9$GND, s6c10$GND))

colnames(gnd) = c("est", "l", "u")
gnd = as.data.frame(gnd)
gnd$model = rep(c(1:6), each=30)
gnd$type = rep(rep(c("a", "b", "c"), each=10), 6)
gnd$imp = rep(c(1:10), 18)

##Make table of confidence intervals using Rubin's rules
gnd$se = ((gnd$u-gnd$l)/2) / qnorm(0.975)
gnd$ID = paste(gnd$model, gnd$type, sep="")
gnd2 = gnd[, names(gnd) %in% c("ID", "est", "se")]

tabGi = rbind(
    c(MI.conf.int(gnd2$est[gnd2$ID=="1a"], gnd2$se[gnd2$ID=="1a"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="1b"], gnd2$se[gnd2$ID=="1b"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="1c"], gnd2$se[gnd2$ID=="1c"])$pres),
    c(MI.conf.int(gnd2$est[gnd2$ID=="2a"], gnd2$se[gnd2$ID=="2a"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="2b"], gnd2$se[gnd2$ID=="2b"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="2c"], gnd2$se[gnd2$ID=="2c"])$pres),
    c(MI.conf.int(gnd2$est[gnd2$ID=="3a"], gnd2$se[gnd2$ID=="3a"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="3b"], gnd2$se[gnd2$ID=="3b"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="3c"], gnd2$se[gnd2$ID=="3c"])$pres),
    c(MI.conf.int(gnd2$est[gnd2$ID=="4a"], gnd2$se[gnd2$ID=="4a"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="4b"], gnd2$se[gnd2$ID=="4b"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="4c"], gnd2$se[gnd2$ID=="4c"])$pres),
    c(MI.conf.int(gnd2$est[gnd2$ID=="5a"], gnd2$se[gnd2$ID=="5a"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="5b"], gnd2$se[gnd2$ID=="5b"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="5c"], gnd2$se[gnd2$ID=="5c"])$pres),
    c(MI.conf.int(gnd2$est[gnd2$ID=="6a"], gnd2$se[gnd2$ID=="6a"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="6b"], gnd2$se[gnd2$ID=="6b"])$pres, MI.conf.int(gnd2$est[gnd2$ID=="6c"], gnd2$se[gnd2$ID=="6c"])$pres)
)
rownames(tabGi) = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
colnames(tabGi) = c("a: Cox", "b: GBM", "c: cforest")

word.tab(tab=tabGi, dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Tables/Internal_GND_2019-02-04.docx", help=TRUE)



##Validate the models on test data
##Impute data for the test set
te = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Imputed_test_2019-01-15.csv", header=TRUE)

te1 = te[te$.imp==1, ]
te2 = te[te$.imp==2, ]
te3 = te[te$.imp==3, ]
te4 = te[te$.imp==4, ]
te5 = te[te$.imp==5, ]
te6 = te[te$.imp==6, ]
te7 = te[te$.imp==7, ]
te8 = te[te$.imp==8, ]
te9 = te[te$.imp==9, ]
te10 = te[te$.imp==10, ]

##Get predictions for all models
##Model 1: Dems and acc only
##A: Cox
Pred1a1 = predCox(mod=m1a1, data=te1, stime="permth_int", time=120)
Pred1a2 = predCox(mod=m1a2, data=te2, stime="permth_int", time=120)
Pred1a3 = predCox(mod=m1a3, data=te3, stime="permth_int", time=120)
Pred1a4 = predCox(mod=m1a4, data=te4, stime="permth_int", time=120)
Pred1a5 = predCox(mod=m1a5, data=te5, stime="permth_int", time=120)
Pred1a6 = predCox(mod=m1a6, data=te6, stime="permth_int", time=120)
Pred1a7 = predCox(mod=m1a7, data=te7, stime="permth_int", time=120)
Pred1a8 = predCox(mod=m1a8, data=te8, stime="permth_int", time=120)
Pred1a9 = predCox(mod=m1a9, data=te9, stime="permth_int", time=120)
Pred1a10 = predCox(mod=m1a10, data=te10, stime="permth_int", time=120)

##B: GBM
Pred1b1 = predGBM(mod=m1b1, data=te1, stime="permth_int", sevent="cvdevent", time=120)
Pred1b2 = predGBM(mod=m1b2, data=te2, stime="permth_int", sevent="cvdevent", time=120)
Pred1b3 = predGBM(mod=m1b3, data=te3, stime="permth_int", sevent="cvdevent", time=120)
Pred1b4 = predGBM(mod=m1b4, data=te4, stime="permth_int", sevent="cvdevent", time=120)
Pred1b5 = predGBM(mod=m1b5, data=te5, stime="permth_int", sevent="cvdevent", time=120)
Pred1b6 = predGBM(mod=m1b6, data=te6, stime="permth_int", sevent="cvdevent", time=120)
Pred1b7 = predGBM(mod=m1b7, data=te7, stime="permth_int", sevent="cvdevent", time=120)
Pred1b8 = predGBM(mod=m1b8, data=te8, stime="permth_int", sevent="cvdevent", time=120)
Pred1b9 = predGBM(mod=m1b9, data=te9, stime="permth_int", sevent="cvdevent", time=120)
Pred1b10 = predGBM(mod=m1b10, data=te10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
Pred1c1 = predCF(mod=m1c1, data=te1, time=120)
Pred1c2 = predCF(mod=m1c2, data=te2, time=120)
Pred1c3 = predCF(mod=m1c3, data=te3, time=120)
Pred1c4 = predCF(mod=m1c4, data=te4, time=120)
Pred1c5 = predCF(mod=m1c5, data=te5, time=120)
Pred1c6 = predCF(mod=m1c6, data=te6, time=120)
Pred1c7 = predCF(mod=m1c7, data=te7, time=120)
Pred1c8 = predCF(mod=m1c8, data=te8, time=120)
Pred1c9 = predCF(mod=m1c9, data=te9, time=120)
Pred1c10 = predCF(mod=m1c10, data=te10, time=120)

##Model 2: Dems, acc, hei
##A: Cox
Pred2a1 = predCox(mod=m2a1, data=te1, stime="permth_int", time=120)
Pred2a2 = predCox(mod=m2a2, data=te2, stime="permth_int", time=120)
Pred2a3 = predCox(mod=m2a3, data=te3, stime="permth_int", time=120)
Pred2a4 = predCox(mod=m2a4, data=te4, stime="permth_int", time=120)
Pred2a5 = predCox(mod=m2a5, data=te5, stime="permth_int", time=120)
Pred2a6 = predCox(mod=m2a6, data=te6, stime="permth_int", time=120)
Pred2a7 = predCox(mod=m2a7, data=te7, stime="permth_int", time=120)
Pred2a8 = predCox(mod=m2a8, data=te8, stime="permth_int", time=120)
Pred2a9 = predCox(mod=m2a9, data=te9, stime="permth_int", time=120)
Pred2a10 = predCox(mod=m2a10, data=te10, stime="permth_int", time=120)

##B: GBM
Pred2b1 = predGBM(mod=m2b1, data=te1, stime="permth_int", sevent="cvdevent", time=120)
Pred2b2 = predGBM(mod=m2b2, data=te2, stime="permth_int", sevent="cvdevent", time=120)
Pred2b3 = predGBM(mod=m2b3, data=te3, stime="permth_int", sevent="cvdevent", time=120)
Pred2b4 = predGBM(mod=m2b4, data=te4, stime="permth_int", sevent="cvdevent", time=120)
Pred2b5 = predGBM(mod=m2b5, data=te5, stime="permth_int", sevent="cvdevent", time=120)
Pred2b6 = predGBM(mod=m2b6, data=te6, stime="permth_int", sevent="cvdevent", time=120)
Pred2b7 = predGBM(mod=m2b7, data=te7, stime="permth_int", sevent="cvdevent", time=120)
Pred2b8 = predGBM(mod=m2b8, data=te8, stime="permth_int", sevent="cvdevent", time=120)
Pred2b9 = predGBM(mod=m2b9, data=te9, stime="permth_int", sevent="cvdevent", time=120)
Pred2b10 = predGBM(mod=m2b10, data=te10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
Pred2c1 = predCF(mod=m2c1, data=te1, time=120)
Pred2c2 = predCF(mod=m2c2, data=te2, time=120)
Pred2c3 = predCF(mod=m2c3, data=te3, time=120)
Pred2c4 = predCF(mod=m2c4, data=te4, time=120)
Pred2c5 = predCF(mod=m2c5, data=te5, time=120)
Pred2c6 = predCF(mod=m2c6, data=te6, time=120)
Pred2c7 = predCF(mod=m2c7, data=te7, time=120)
Pred2c8 = predCF(mod=m2c8, data=te8, time=120)
Pred2c9 = predCF(mod=m2c9, data=te9, time=120)
Pred2c10 = predCF(mod=m2c10, data=te10, time=120)

##Model 3: Dems, acc, ahei
##A: Cox
Pred3a1 = predCox(mod=m3a1, data=te1, stime="permth_int", time=120)
Pred3a2 = predCox(mod=m3a2, data=te2, stime="permth_int", time=120)
Pred3a3 = predCox(mod=m3a3, data=te3, stime="permth_int", time=120)
Pred3a4 = predCox(mod=m3a4, data=te4, stime="permth_int", time=120)
Pred3a5 = predCox(mod=m3a5, data=te5, stime="permth_int", time=120)
Pred3a6 = predCox(mod=m3a6, data=te6, stime="permth_int", time=120)
Pred3a7 = predCox(mod=m3a7, data=te7, stime="permth_int", time=120)
Pred3a8 = predCox(mod=m3a8, data=te8, stime="permth_int", time=120)
Pred3a9 = predCox(mod=m3a9, data=te9, stime="permth_int", time=120)
Pred3a10 = predCox(mod=m3a10, data=te10, stime="permth_int", time=120)

##B: GBM
Pred3b1 = predGBM(mod=m3b1, data=te1, stime="permth_int", sevent="cvdevent", time=120)
Pred3b2 = predGBM(mod=m3b2, data=te2, stime="permth_int", sevent="cvdevent", time=120)
Pred3b3 = predGBM(mod=m3b3, data=te3, stime="permth_int", sevent="cvdevent", time=120)
Pred3b4 = predGBM(mod=m3b4, data=te4, stime="permth_int", sevent="cvdevent", time=120)
Pred3b5 = predGBM(mod=m3b5, data=te5, stime="permth_int", sevent="cvdevent", time=120)
Pred3b6 = predGBM(mod=m3b6, data=te6, stime="permth_int", sevent="cvdevent", time=120)
Pred3b7 = predGBM(mod=m3b7, data=te7, stime="permth_int", sevent="cvdevent", time=120)
Pred3b8 = predGBM(mod=m3b8, data=te8, stime="permth_int", sevent="cvdevent", time=120)
Pred3b9 = predGBM(mod=m3b9, data=te9, stime="permth_int", sevent="cvdevent", time=120)
Pred3b10 = predGBM(mod=m3b10, data=te10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
Pred3c1 = predCF(mod=m3c1, data=te1, time=120)
Pred3c2 = predCF(mod=m3c2, data=te2, time=120)
Pred3c3 = predCF(mod=m3c3, data=te3, time=120)
Pred3c4 = predCF(mod=m3c4, data=te4, time=120)
Pred3c5 = predCF(mod=m3c5, data=te5, time=120)
Pred3c6 = predCF(mod=m3c6, data=te6, time=120)
Pred3c7 = predCF(mod=m3c7, data=te7, time=120)
Pred3c8 = predCF(mod=m3c8, data=te8, time=120)
Pred3c9 = predCF(mod=m3c9, data=te9, time=120)
Pred3c10 = predCF(mod=m3c10, data=te10, time=120)

##Model 4: Dems, acc, mds
##A: Cox
Pred4a1 = predCox(mod=m4a1, data=te1, stime="permth_int", time=120)
Pred4a2 = predCox(mod=m4a2, data=te2, stime="permth_int", time=120)
Pred4a3 = predCox(mod=m4a3, data=te3, stime="permth_int", time=120)
Pred4a4 = predCox(mod=m4a4, data=te4, stime="permth_int", time=120)
Pred4a5 = predCox(mod=m4a5, data=te5, stime="permth_int", time=120)
Pred4a6 = predCox(mod=m4a6, data=te6, stime="permth_int", time=120)
Pred4a7 = predCox(mod=m4a7, data=te7, stime="permth_int", time=120)
Pred4a8 = predCox(mod=m4a8, data=te8, stime="permth_int", time=120)
Pred4a9 = predCox(mod=m4a9, data=te9, stime="permth_int", time=120)
Pred4a10 = predCox(mod=m4a10, data=te10, stime="permth_int", time=120)

##B: GBM
Pred4b1 = predGBM(mod=m4b1, data=te1, stime="permth_int", sevent="cvdevent", time=120)
Pred4b2 = predGBM(mod=m4b2, data=te2, stime="permth_int", sevent="cvdevent", time=120)
Pred4b3 = predGBM(mod=m4b3, data=te3, stime="permth_int", sevent="cvdevent", time=120)
Pred4b4 = predGBM(mod=m4b4, data=te4, stime="permth_int", sevent="cvdevent", time=120)
Pred4b5 = predGBM(mod=m4b5, data=te5, stime="permth_int", sevent="cvdevent", time=120)
Pred4b6 = predGBM(mod=m4b6, data=te6, stime="permth_int", sevent="cvdevent", time=120)
Pred4b7 = predGBM(mod=m4b7, data=te7, stime="permth_int", sevent="cvdevent", time=120)
Pred4b8 = predGBM(mod=m4b8, data=te8, stime="permth_int", sevent="cvdevent", time=120)
Pred4b9 = predGBM(mod=m4b9, data=te9, stime="permth_int", sevent="cvdevent", time=120)
Pred4b10 = predGBM(mod=m4b10, data=te10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
Pred4c1 = predCF(mod=m4c1, data=te1, time=120)
Pred4c2 = predCF(mod=m4c2, data=te2, time=120)
Pred4c3 = predCF(mod=m4c3, data=te3, time=120)
Pred4c4 = predCF(mod=m4c4, data=te4, time=120)
Pred4c5 = predCF(mod=m4c5, data=te5, time=120)
Pred4c6 = predCF(mod=m4c6, data=te6, time=120)
Pred4c7 = predCF(mod=m4c7, data=te7, time=120)
Pred4c8 = predCF(mod=m4c8, data=te8, time=120)
Pred4c9 = predCF(mod=m4c9, data=te9, time=120)
Pred4c10 = predCF(mod=m4c10, data=te10, time=120)

##Model 5: Dems, acc, dash
##A: Cox
Pred5a1 = predCox(mod=m5a1, data=te1, stime="permth_int", time=120)
Pred5a2 = predCox(mod=m5a2, data=te2, stime="permth_int", time=120)
Pred5a3 = predCox(mod=m5a3, data=te3, stime="permth_int", time=120)
Pred5a4 = predCox(mod=m5a4, data=te4, stime="permth_int", time=120)
Pred5a5 = predCox(mod=m5a5, data=te5, stime="permth_int", time=120)
Pred5a6 = predCox(mod=m5a6, data=te6, stime="permth_int", time=120)
Pred5a7 = predCox(mod=m5a7, data=te7, stime="permth_int", time=120)
Pred5a8 = predCox(mod=m5a8, data=te8, stime="permth_int", time=120)
Pred5a9 = predCox(mod=m5a9, data=te9, stime="permth_int", time=120)
Pred5a10 = predCox(mod=m5a10, data=te10, stime="permth_int", time=120)

##B: GBM
Pred5b1 = predGBM(mod=m5b1, data=te1, stime="permth_int", sevent="cvdevent", time=120)
Pred5b2 = predGBM(mod=m5b2, data=te2, stime="permth_int", sevent="cvdevent", time=120)
Pred5b3 = predGBM(mod=m5b3, data=te3, stime="permth_int", sevent="cvdevent", time=120)
Pred5b4 = predGBM(mod=m5b4, data=te4, stime="permth_int", sevent="cvdevent", time=120)
Pred5b5 = predGBM(mod=m5b5, data=te5, stime="permth_int", sevent="cvdevent", time=120)
Pred5b6 = predGBM(mod=m5b6, data=te6, stime="permth_int", sevent="cvdevent", time=120)
Pred5b7 = predGBM(mod=m5b7, data=te7, stime="permth_int", sevent="cvdevent", time=120)
Pred5b8 = predGBM(mod=m5b8, data=te8, stime="permth_int", sevent="cvdevent", time=120)
Pred5b9 = predGBM(mod=m5b9, data=te9, stime="permth_int", sevent="cvdevent", time=120)
Pred5b10 = predGBM(mod=m5b10, data=te10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
Pred5c1 = predCF(mod=m5c1, data=te1, time=120)
Pred5c2 = predCF(mod=m5c2, data=te2, time=120)
Pred5c3 = predCF(mod=m5c3, data=te3, time=120)
Pred5c4 = predCF(mod=m5c4, data=te4, time=120)
Pred5c5 = predCF(mod=m5c5, data=te5, time=120)
Pred5c6 = predCF(mod=m5c6, data=te6, time=120)
Pred5c7 = predCF(mod=m5c7, data=te7, time=120)
Pred5c8 = predCF(mod=m5c8, data=te8, time=120)
Pred5c9 = predCF(mod=m5c9, data=te9, time=120)
Pred5c10 = predCF(mod=m5c10, data=te10, time=120)

##Model 6: Dems, acc, all nuteition variables
##A: Cox
Pred6a1 = predCox(mod=m6a1, data=te1, stime="permth_int", time=120)
Pred6a2 = predCox(mod=m6a2, data=te2, stime="permth_int", time=120)
Pred6a3 = predCox(mod=m6a3, data=te3, stime="permth_int", time=120)
Pred6a4 = predCox(mod=m6a4, data=te4, stime="permth_int", time=120)
Pred6a5 = predCox(mod=m6a5, data=te5, stime="permth_int", time=120)
Pred6a6 = predCox(mod=m6a6, data=te6, stime="permth_int", time=120)
Pred6a7 = predCox(mod=m6a7, data=te7, stime="permth_int", time=120)
Pred6a8 = predCox(mod=m6a8, data=te8, stime="permth_int", time=120)
Pred6a9 = predCox(mod=m6a9, data=te9, stime="permth_int", time=120)
Pred6a10 = predCox(mod=m6a10, data=te10, stime="permth_int", time=120)

##B: GBM
Pred6b1 = predGBM(mod=m6b1, data=te1, stime="permth_int", sevent="cvdevent", time=120)
Pred6b2 = predGBM(mod=m6b2, data=te2, stime="permth_int", sevent="cvdevent", time=120)
Pred6b3 = predGBM(mod=m6b3, data=te3, stime="permth_int", sevent="cvdevent", time=120)
Pred6b4 = predGBM(mod=m6b4, data=te4, stime="permth_int", sevent="cvdevent", time=120)
Pred6b5 = predGBM(mod=m6b5, data=te5, stime="permth_int", sevent="cvdevent", time=120)
Pred6b6 = predGBM(mod=m6b6, data=te6, stime="permth_int", sevent="cvdevent", time=120)
Pred6b7 = predGBM(mod=m6b7, data=te7, stime="permth_int", sevent="cvdevent", time=120)
Pred6b8 = predGBM(mod=m6b8, data=te8, stime="permth_int", sevent="cvdevent", time=120)
Pred6b9 = predGBM(mod=m6b9, data=te9, stime="permth_int", sevent="cvdevent", time=120)
Pred6b10 = predGBM(mod=m6b10, data=te10, stime="permth_int", sevent="cvdevent", time=120)

##C: Cforest
Pred6c1 = predCF(mod=m6c1, data=te1, time=120)
Pred6c2 = predCF(mod=m6c2, data=te2, time=120)
Pred6c3 = predCF(mod=m6c3, data=te3, time=120)
Pred6c4 = predCF(mod=m6c4, data=te4, time=120)
Pred6c5 = predCF(mod=m6c5, data=te5, time=120)
Pred6c6 = predCF(mod=m6c6, data=te6, time=120)
Pred6c7 = predCF(mod=m6c7, data=te7, time=120)
Pred6c8 = predCF(mod=m6c8, data=te8, time=120)
Pred6c9 = predCF(mod=m6c9, data=te9, time=120)
Pred6c10 = predCF(mod=m6c10, data=te10, time=120)


##External validation
##Define true event for each data set
Event1 = ifelse(te1$permth_int<=120 & te1$cvdevent==1, 1, 0)
Event2 = ifelse(te2$permth_int<=120 & te2$cvdevent==1, 1, 0)
Event3 = ifelse(te3$permth_int<=120 & te3$cvdevent==1, 1, 0)
Event4 = ifelse(te4$permth_int<=120 & te4$cvdevent==1, 1, 0)
Event5 = ifelse(te5$permth_int<=120 & te5$cvdevent==1, 1, 0)
Event6 = ifelse(te6$permth_int<=120 & te6$cvdevent==1, 1, 0)
Event7 = ifelse(te7$permth_int<=120 & te7$cvdevent==1, 1, 0)
Event8 = ifelse(te8$permth_int<=120 & te8$cvdevent==1, 1, 0)
Event9 = ifelse(te9$permth_int<=120 & te9$cvdevent==1, 1, 0)
Event10 = ifelse(te10$permth_int<=120 & te10$cvdevent==1, 1, 0)

##Model 1: Dems and acc only
##A: Cox
S1a1 = getInfo(Event1, Pred1a1)
S1a2 = getInfo(Event2, Pred1a2)
S1a3 = getInfo(Event3, Pred1a3)
S1a4 = getInfo(Event4, Pred1a4)
S1a5 = getInfo(Event5, Pred1a5)
S1a6 = getInfo(Event6, Pred1a6)
S1a7 = getInfo(Event7, Pred1a7)
S1a8 = getInfo(Event8, Pred1a8)
S1a9 = getInfo(Event9, Pred1a9)
S1a10 = getInfo(Event10, Pred1a10)

##B: GBM
S1b1 = getInfo(Event1, Pred1b1)
S1b2 = getInfo(Event2, Pred1b2)
S1b3 = getInfo(Event3, Pred1b3)
S1b4 = getInfo(Event4, Pred1b4)
S1b5 = getInfo(Event5, Pred1b5)
S1b6 = getInfo(Event6, Pred1b6)
S1b7 = getInfo(Event7, Pred1b7)
S1b8 = getInfo(Event8, Pred1b8)
S1b9 = getInfo(Event9, Pred1b9)
S1b10 = getInfo(Event10, Pred1b10)

##C: Cforest
S1c1 = getInfo(Event1, Pred1c1)
S1c2 = getInfo(Event2, Pred1c2)
S1c3 = getInfo(Event3, Pred1c3)
S1c4 = getInfo(Event4, Pred1c4)
S1c5 = getInfo(Event5, Pred1c5)
S1c6 = getInfo(Event6, Pred1c6)
S1c7 = getInfo(Event7, Pred1c7)
S1c8 = getInfo(Event8, Pred1c8)
S1c9 = getInfo(Event9, Pred1c9)
S1c10 = getInfo(Event10, Pred1c10)

##Model 2: Dems, hei
##A: Cox
S2a1 = getInfo(Event1, Pred2a1)
S2a2 = getInfo(Event2, Pred2a2)
S2a3 = getInfo(Event3, Pred2a3)
S2a4 = getInfo(Event4, Pred2a4)
S2a5 = getInfo(Event5, Pred2a5)
S2a6 = getInfo(Event6, Pred2a6)
S2a7 = getInfo(Event7, Pred2a7)
S2a8 = getInfo(Event8, Pred2a8)
S2a9 = getInfo(Event9, Pred2a9)
S2a10 = getInfo(Event10, Pred2a10)

##B: GBM
S2b1 = getInfo(Event1, Pred2b1)
S2b2 = getInfo(Event2, Pred2b2)
S2b3 = getInfo(Event3, Pred2b3)
S2b4 = getInfo(Event4, Pred2b4)
S2b5 = getInfo(Event5, Pred2b5)
S2b6 = getInfo(Event6, Pred2b6)
S2b7 = getInfo(Event7, Pred2b7)
S2b8 = getInfo(Event8, Pred2b8)
S2b9 = getInfo(Event9, Pred2b9)
S2b10 = getInfo(Event10, Pred2b10)

##C: Cforest
S2c1 = getInfo(Event1, Pred2c1)
S2c2 = getInfo(Event2, Pred2c2)
S2c3 = getInfo(Event3, Pred2c3)
S2c4 = getInfo(Event4, Pred2c4)
S2c5 = getInfo(Event5, Pred2c5)
S2c6 = getInfo(Event6, Pred2c6)
S2c7 = getInfo(Event7, Pred2c7)
S2c8 = getInfo(Event8, Pred2c8)
S2c9 = getInfo(Event9, Pred2c9)
S2c10 = getInfo(Event10, Pred2c10)

##Model 3: Dems, acc, ahei
##A: Cox
S3a1 = getInfo(Event1, Pred3a1)
S3a2 = getInfo(Event2, Pred3a2)
S3a3 = getInfo(Event3, Pred3a3)
S3a4 = getInfo(Event4, Pred3a4)
S3a5 = getInfo(Event5, Pred3a5)
S3a6 = getInfo(Event6, Pred3a6)
S3a7 = getInfo(Event7, Pred3a7)
S3a8 = getInfo(Event8, Pred3a8)
S3a9 = getInfo(Event9, Pred3a9)
S3a10 = getInfo(Event10, Pred3a10)

##B: GBM
S3b1 = getInfo(Event1, Pred3b1)
S3b2 = getInfo(Event2, Pred3b2)
S3b3 = getInfo(Event3, Pred3b3)
S3b4 = getInfo(Event4, Pred3b4)
S3b5 = getInfo(Event5, Pred3b5)
S3b6 = getInfo(Event6, Pred3b6)
S3b7 = getInfo(Event7, Pred3b7)
S3b8 = getInfo(Event8, Pred3b8)
S3b9 = getInfo(Event9, Pred3b9)
S3b10 = getInfo(Event10, Pred3b10)

##C: Cforest
S3c1 = getInfo(Event1, Pred3c1)
S3c2 = getInfo(Event2, Pred3c2)
S3c3 = getInfo(Event3, Pred3c3)
S3c4 = getInfo(Event4, Pred3c4)
S3c5 = getInfo(Event5, Pred3c5)
S3c6 = getInfo(Event6, Pred3c6)
S3c7 = getInfo(Event7, Pred3c7)
S3c8 = getInfo(Event8, Pred3c8)
S3c9 = getInfo(Event9, Pred3c9)
S3c10 = getInfo(Event10, Pred3c10)

##Model 4: Dems, acc, mds
##A: Cox
S4a1 = getInfo(Event1, Pred4a1)
S4a2 = getInfo(Event2, Pred4a2)
S4a3 = getInfo(Event3, Pred4a3)
S4a4 = getInfo(Event4, Pred4a4)
S4a5 = getInfo(Event5, Pred4a5)
S4a6 = getInfo(Event6, Pred4a6)
S4a7 = getInfo(Event7, Pred4a7)
S4a8 = getInfo(Event8, Pred4a8)
S4a9 = getInfo(Event9, Pred4a9)
S4a10 = getInfo(Event10, Pred4a10)

##B: GBM
S4b1 = getInfo(Event1, Pred4b1)
S4b2 = getInfo(Event2, Pred4b2)
S4b3 = getInfo(Event3, Pred4b3)
S4b4 = getInfo(Event4, Pred4b4)
S4b5 = getInfo(Event5, Pred4b5)
S4b6 = getInfo(Event6, Pred4b6)
S4b7 = getInfo(Event7, Pred4b7)
S4b8 = getInfo(Event8, Pred4b8)
S4b9 = getInfo(Event9, Pred4b9)
S4b10 = getInfo(Event10, Pred4b10)

##C: Cforest
S4c1 = getInfo(Event1, Pred4c1)
S4c2 = getInfo(Event2, Pred4c2)
S4c3 = getInfo(Event3, Pred4c3)
S4c4 = getInfo(Event4, Pred4c4)
S4c5 = getInfo(Event5, Pred4c5)
S4c6 = getInfo(Event6, Pred4c6)
S4c7 = getInfo(Event7, Pred4c7)
S4c8 = getInfo(Event8, Pred4c8)
S4c9 = getInfo(Event9, Pred4c9)
S4c10 = getInfo(Event10, Pred4c10)

##Model 5: Dems, acc, dash
##A: Cox
S5a1 = getInfo(Event1, Pred5a1)
S5a2 = getInfo(Event2, Pred5a2)
S5a3 = getInfo(Event3, Pred5a3)
S5a4 = getInfo(Event4, Pred5a4)
S5a5 = getInfo(Event5, Pred5a5)
S5a6 = getInfo(Event6, Pred5a6)
S5a7 = getInfo(Event7, Pred5a7)
S5a8 = getInfo(Event8, Pred5a8)
S5a9 = getInfo(Event9, Pred5a9)
S5a10 = getInfo(Event10, Pred5a10)

##B: GBM
S5b1 = getInfo(Event1, Pred5b1)
S5b2 = getInfo(Event2, Pred5b2)
S5b3 = getInfo(Event3, Pred5b3)
S5b4 = getInfo(Event4, Pred5b4)
S5b5 = getInfo(Event5, Pred5b5)
S5b6 = getInfo(Event6, Pred5b6)
S5b7 = getInfo(Event7, Pred5b7)
S5b8 = getInfo(Event8, Pred5b8)
S5b9 = getInfo(Event9, Pred5b9)
S5b10 = getInfo(Event10, Pred5b10)

##C: Cforest
S5c1 = getInfo(Event1, Pred5c1)
S5c2 = getInfo(Event2, Pred5c2)
S5c3 = getInfo(Event3, Pred5c3)
S5c4 = getInfo(Event4, Pred5c4)
S5c5 = getInfo(Event5, Pred5c5)
S5c6 = getInfo(Event6, Pred5c6)
S5c7 = getInfo(Event7, Pred5c7)
S5c8 = getInfo(Event8, Pred5c8)
S5c9 = getInfo(Event9, Pred5c9)
S5c10 = getInfo(Event10, Pred5c10)

##Model 6: Dems, acc, all 24 hour recall data
##A: Cox
S6a1 = getInfo(Event1, Pred6a1)
S6a2 = getInfo(Event2, Pred6a2)
S6a3 = getInfo(Event3, Pred6a3)
S6a4 = getInfo(Event4, Pred6a4)
S6a5 = getInfo(Event5, Pred6a5)
S6a6 = getInfo(Event6, Pred6a6)
S6a7 = getInfo(Event7, Pred6a7)
S6a8 = getInfo(Event8, Pred6a8)
S6a9 = getInfo(Event9, Pred6a9)
S6a10 = getInfo(Event10, Pred6a10)

##B: GBM
S6b1 = getInfo(Event1, Pred6b1)
S6b2 = getInfo(Event2, Pred6b2)
S6b3 = getInfo(Event3, Pred6b3)
S6b4 = getInfo(Event4, Pred6b4)
S6b5 = getInfo(Event5, Pred6b5)
S6b6 = getInfo(Event6, Pred6b6)
S6b7 = getInfo(Event7, Pred6b7)
S6b8 = getInfo(Event8, Pred6b8)
S6b9 = getInfo(Event9, Pred6b9)
S6b10 = getInfo(Event10, Pred6b10)

##C: Cforest
S6c1 = getInfo(Event1, Pred6c1)
S6c2 = getInfo(Event2, Pred6c2)
S6c3 = getInfo(Event3, Pred6c3)
S6c4 = getInfo(Event4, Pred6c4)
S6c5 = getInfo(Event5, Pred6c5)
S6c6 = getInfo(Event6, Pred6c6)
S6c7 = getInfo(Event7, Pred6c7)
S6c8 = getInfo(Event8, Pred6c8)
S6c9 = getInfo(Event9, Pred6c9)
S6c10 = getInfo(Event10, Pred6c10)

##Put all results in one data set
##C-statistics
CSTAT = rbind(
    rbind(S1a1$C, S1a2$C, S1a3$C, S1a4$C, S1a5$C, S1a6$C, S1a7$C, S1a8$C, S1a9$C, S1a10$C),
    rbind(S1b1$C, S1b2$C, S1b3$C, S1b4$C, S1b5$C, S1b6$C, S1b7$C, S1b8$C, S1b9$C, S1b10$C),
    rbind(S1c1$C, S1c2$C, S1c3$C, S1c4$C, S1c5$C, S1c6$C, S1c7$C, S1c8$C, S1c9$C, S1c10$C),
    rbind(S2a1$C, S2a2$C, S2a3$C, S2a4$C, S2a5$C, S2a6$C, S2a7$C, S2a8$C, S2a9$C, S2a10$C),
    rbind(S2b1$C, S2b2$C, S2b3$C, S2b4$C, S2b5$C, S2b6$C, S2b7$C, S2b8$C, S2b9$C, S2b10$C),
    rbind(S2c1$C, S2c2$C, S2c3$C, S2c4$C, S2c5$C, S2c6$C, S2c7$C, S2c8$C, S2c9$C, S2c10$C),
    rbind(S3a1$C, S3a2$C, S3a3$C, S3a4$C, S3a5$C, S3a6$C, S3a7$C, S3a8$C, S3a9$C, S3a10$C),
    rbind(S3b1$C, S3b2$C, S3b3$C, S3b4$C, S3b5$C, S3b6$C, S3b7$C, S3b8$C, S3b9$C, S3b10$C),
    rbind(S3c1$C, S3c2$C, S3c3$C, S3c4$C, S3c5$C, S3c6$C, S3c7$C, S3c8$C, S3c9$C, S3c10$C),
    rbind(S4a1$C, S4a2$C, S4a3$C, S4a4$C, S4a5$C, S4a6$C, S4a7$C, S4a8$C, S4a9$C, S4a10$C),
    rbind(S4b1$C, S4b2$C, S4b3$C, S4b4$C, S4b5$C, S4b6$C, S4b7$C, S4b8$C, S4b9$C, S4b10$C),
    rbind(S4c1$C, S4c2$C, S4c3$C, S4c4$C, S4c5$C, S4c6$C, S4c7$C, S4c8$C, S4c9$C, S4c10$C),
    rbind(S5a1$C, S5a2$C, S5a3$C, S5a4$C, S5a5$C, S5a6$C, S5a7$C, S5a8$C, S5a9$C, S5a10$C),
    rbind(S5b1$C, S5b2$C, S5b3$C, S5b4$C, S5b5$C, S5b6$C, S5b7$C, S5b8$C, S5b9$C, S5b10$C),
    rbind(S5c1$C, S5c2$C, S5c3$C, S5c4$C, S5c5$C, S5c6$C, S5c7$C, S5c8$C, S5c9$C, S5c10$C),
    rbind(S6a1$C, S6a2$C, S6a3$C, S6a4$C, S6a5$C, S6a6$C, S6a7$C, S6a8$C, S6a9$C, S6a10$C),
    rbind(S6b1$C, S6b2$C, S6b3$C, S6b4$C, S6b5$C, S6b6$C, S6b7$C, S6b8$C, S6b9$C, S6b10$C),
    rbind(S6c1$C, S6c2$C, S6c3$C, S6c4$C, S6c5$C, S6c6$C, S6c7$C, S6c8$C, S6c9$C, S6c10$C))

colnames(CSTAT) = c("est", "l", "u")
CSTAT = as.data.frame(CSTAT)
CSTAT$model = rep(c(1:6), each=30)
CSTAT$type = rep(rep(c("a", "b", "c"), each=10), 6)
CSTAT$imp = rep(c(1:10), 18)

##Make table of confidence intervals using Rubin's rules
CSTAT$se = ((CSTAT$u-CSTAT$l)/2) / qnorm(0.975)
CSTAT$ID = paste(CSTAT$model, CSTAT$type, sep="")
CSTAT2 = CSTAT[, names(CSTAT) %in% c("ID", "est", "se")]

tabCe = rbind(
    c(MI.conf.int(CSTAT2$est[CSTAT2$ID=="1a"], CSTAT2$se[CSTAT2$ID=="1a"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="1b"], CSTAT2$se[CSTAT2$ID=="1b"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="1c"], CSTAT2$se[CSTAT2$ID=="1c"])$pres),
    c(MI.conf.int(CSTAT2$est[CSTAT2$ID=="2a"], CSTAT2$se[CSTAT2$ID=="2a"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="2b"], CSTAT2$se[CSTAT2$ID=="2b"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="2c"], CSTAT2$se[CSTAT2$ID=="2c"])$pres),
    c(MI.conf.int(CSTAT2$est[CSTAT2$ID=="3a"], CSTAT2$se[CSTAT2$ID=="3a"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="3b"], CSTAT2$se[CSTAT2$ID=="3b"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="3c"], CSTAT2$se[CSTAT2$ID=="3c"])$pres),
    c(MI.conf.int(CSTAT2$est[CSTAT2$ID=="4a"], CSTAT2$se[CSTAT2$ID=="4a"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="4b"], CSTAT2$se[CSTAT2$ID=="4b"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="4c"], CSTAT2$se[CSTAT2$ID=="4c"])$pres),
    c(MI.conf.int(CSTAT2$est[CSTAT2$ID=="5a"], CSTAT2$se[CSTAT2$ID=="5a"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="5b"], CSTAT2$se[CSTAT2$ID=="5b"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="5c"], CSTAT2$se[CSTAT2$ID=="5c"])$pres),
    c(MI.conf.int(CSTAT2$est[CSTAT2$ID=="6a"], CSTAT2$se[CSTAT2$ID=="6a"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="6b"], CSTAT2$se[CSTAT2$ID=="6b"])$pres, MI.conf.int(CSTAT2$est[CSTAT2$ID=="6c"], CSTAT2$se[CSTAT2$ID=="6c"])$pres)
)
rownames(tabCe) = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
colnames(tabCe) = c("a: Cox", "b: GBM", "c: cforest")

word.tab(tab=tabCe, dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Tables/External_C_2019-02-04.docx", help=TRUE)

##GND slope and CI
GND = rbind(
    rbind(S1a1$GND, S1a2$GND, S1a3$GND, S1a4$GND, S1a5$GND, S1a6$GND, S1a7$GND, S1a8$GND, S1a9$GND, S1a10$GND),
    rbind(S1b1$GND, S1b2$GND, S1b3$GND, S1b4$GND, S1b5$GND, S1b6$GND, S1b7$GND, S1b8$GND, S1b9$GND, S1b10$GND),
    rbind(S1c1$GND, S1c2$GND, S1c3$GND, S1c4$GND, S1c5$GND, S1c6$GND, S1c7$GND, S1c8$GND, S1c9$GND, S1c10$GND),
    rbind(S2a1$GND, S2a2$GND, S2a3$GND, S2a4$GND, S2a5$GND, S2a6$GND, S2a7$GND, S2a8$GND, S2a9$GND, S2a10$GND),
    rbind(S2b1$GND, S2b2$GND, S2b3$GND, S2b4$GND, S2b5$GND, S2b6$GND, S2b7$GND, S2b8$GND, S2b9$GND, S2b10$GND),
    rbind(S2c1$GND, S2c2$GND, S2c3$GND, S2c4$GND, S2c5$GND, S2c6$GND, S2c7$GND, S2c8$GND, S2c9$GND, S2c10$GND),
    rbind(S3a1$GND, S3a2$GND, S3a3$GND, S3a4$GND, S3a5$GND, S3a6$GND, S3a7$GND, S3a8$GND, S3a9$GND, S3a10$GND),
    rbind(S3b1$GND, S3b2$GND, S3b3$GND, S3b4$GND, S3b5$GND, S3b6$GND, S3b7$GND, S3b8$GND, S3b9$GND, S3b10$GND),
    rbind(S3c1$GND, S3c2$GND, S3c3$GND, S3c4$GND, S3c5$GND, S3c6$GND, S3c7$GND, S3c8$GND, S3c9$GND, S3c10$GND),
    rbind(S4a1$GND, S4a2$GND, S4a3$GND, S4a4$GND, S4a5$GND, S4a6$GND, S4a7$GND, S4a8$GND, S4a9$GND, S4a10$GND),
    rbind(S4b1$GND, S4b2$GND, S4b3$GND, S4b4$GND, S4b5$GND, S4b6$GND, S4b7$GND, S4b8$GND, S4b9$GND, S4b10$GND),
    rbind(S4c1$GND, S4c2$GND, S4c3$GND, S4c4$GND, S4c5$GND, S4c6$GND, S4c7$GND, S4c8$GND, S4c9$GND, S4c10$GND),
    rbind(S5a1$GND, S5a2$GND, S5a3$GND, S5a4$GND, S5a5$GND, S5a6$GND, S5a7$GND, S5a8$GND, S5a9$GND, S5a10$GND),
    rbind(S5b1$GND, S5b2$GND, S5b3$GND, S5b4$GND, S5b5$GND, S5b6$GND, S5b7$GND, S5b8$GND, S5b9$GND, S5b10$GND),
    rbind(S5c1$GND, S5c2$GND, S5c3$GND, S5c4$GND, S5c5$GND, S5c6$GND, S5c7$GND, S5c8$GND, S5c9$GND, S5c10$GND),
    rbind(S6a1$GND, S6a2$GND, S6a3$GND, S6a4$GND, S6a5$GND, S6a6$GND, S6a7$GND, S6a8$GND, S6a9$GND, S6a10$GND),
    rbind(S6b1$GND, S6b2$GND, S6b3$GND, S6b4$GND, S6b5$GND, S6b6$GND, S6b7$GND, S6b8$GND, S6b9$GND, S6b10$GND),
    rbind(S6c1$GND, S6c2$GND, S6c3$GND, S6c4$GND, S6c5$GND, S6c6$GND, S6c7$GND, S6c8$GND, S6c9$GND, S6c10$GND))

colnames(GND) = c("est", "l", "u")
GND = as.data.frame(GND)
GND$model = rep(c(1:6), each=30)
GND$type = rep(rep(c("a", "b", "c"), each=10), 6)
GND$imp = rep(c(1:10), 18)

##Make table of confidence intervals using Rubin's rules
GND$se = ((GND$u-GND$l)/2) / qnorm(0.975)
GND$ID = paste(GND$model, GND$type, sep="")
GND2 = GND[, names(GND) %in% c("ID", "est", "se")]

tabGe = rbind(
    c(MI.conf.int(GND2$est[GND2$ID=="1a"], GND2$se[GND2$ID=="1a"])$pres, MI.conf.int(GND2$est[GND2$ID=="1b"], GND2$se[GND2$ID=="1b"])$pres, MI.conf.int(GND2$est[GND2$ID=="1c"], GND2$se[GND2$ID=="1c"])$pres),
    c(MI.conf.int(GND2$est[GND2$ID=="2a"], GND2$se[GND2$ID=="2a"])$pres, MI.conf.int(GND2$est[GND2$ID=="2b"], GND2$se[GND2$ID=="2b"])$pres, MI.conf.int(GND2$est[GND2$ID=="2c"], GND2$se[GND2$ID=="2c"])$pres),
    c(MI.conf.int(GND2$est[GND2$ID=="3a"], GND2$se[GND2$ID=="3a"])$pres, MI.conf.int(GND2$est[GND2$ID=="3b"], GND2$se[GND2$ID=="3b"])$pres, MI.conf.int(GND2$est[GND2$ID=="3c"], GND2$se[GND2$ID=="3c"])$pres),
    c(MI.conf.int(GND2$est[GND2$ID=="4a"], GND2$se[GND2$ID=="4a"])$pres, MI.conf.int(GND2$est[GND2$ID=="4b"], GND2$se[GND2$ID=="4b"])$pres, MI.conf.int(GND2$est[GND2$ID=="4c"], GND2$se[GND2$ID=="4c"])$pres),
    c(MI.conf.int(GND2$est[GND2$ID=="5a"], GND2$se[GND2$ID=="5a"])$pres, MI.conf.int(GND2$est[GND2$ID=="5b"], GND2$se[GND2$ID=="5b"])$pres, MI.conf.int(GND2$est[GND2$ID=="5c"], GND2$se[GND2$ID=="5c"])$pres),
    c(MI.conf.int(GND2$est[GND2$ID=="6a"], GND2$se[GND2$ID=="6a"])$pres, MI.conf.int(GND2$est[GND2$ID=="6b"], GND2$se[GND2$ID=="6b"])$pres, MI.conf.int(GND2$est[GND2$ID=="6c"], GND2$se[GND2$ID=="6c"])$pres)
)
rownames(tabGe) = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
colnames(tabGe) = c("a: Cox", "b: GBM", "c: cforest")

word.tab(tab=tabGe, dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Tables/External_GND_2019-02-04.docx", help=TRUE)


##Saving all objects in your R session (will never make this mistake again!):
#The save.image() function will save all objects currently in your R session:
#save.image(file="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Predictions.RData")
#These objects can then be loaded back into a new R session using the load() function:
#load(file="1.RData")
load(file="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Predictions.RData")

##Function to get data frames
reOrd = function(tabGi) {
taGi = matrix(unlist(lapply(tabGi, function(x) as.numeric(unlist(strsplit(gsub("[^[:digit:].]", " ", x), "  "))))), 18, 3, byrow=TRUE)
rownames(taGi) = c("1A", "2A", "3A", "4A", "5A", "6A", "1B", "2B", "3B", "4B", "5B", "6B", "1C", "2C", "3C", "4C", "5C", "6C")
colnames(taGi) = c("est", "l", "u")
ord = c(1, 7, 13, 2:6, 8, 14, 9, 15, 10, 16, 11, 17, 12, 18)
taGi[ord, ]
}

##Figure 1: Internal validation GND test slopes and confidence intervals (tabGi)
tabGi
taGi = reOrd(tabGi)

pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Figures/Fig1_2019-03-12.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0, 1.6), ylab="GND slope", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taGi[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taGi[i, 2], y1=taGi[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i])
}

legend("topleft", bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=0.8)
legend(0.4, 1.2, lty=c(1, 2, 3), c("Cox", "GBM", "RF"), cex=0.8, bty="n")
dev.off()



##Figure 2: Internal validation C-statistics (tabCi)
tabCi
taCi = reOrd(tabCi)

pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Figures/Fig2_2019-03-12.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0.85, 1), ylab="C-statistic", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taCi[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taCi[i, 2], y1=taCi[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i])
}

legend("topleft", bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=0.8)
legend(0.4, 0.965, lty=c(1, 2, 3), c("Cox", "GBM", "RF"), cex=0.8, bty="n")
dev.off()


##Figure 3: External validation GND test slopes and confidence intervals (tabGe)
tabGe
taGe = reOrd(tabGe)

pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Figures/Fig3_2019-03-12.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0, 1.6), ylab="GND slope", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taGe[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taGe[i, 2], y1=taGe[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i])
}

legend("topleft", bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=0.8)
legend(0.4, 1.2, lty=c(1, 2, 3), c("Cox", "GBM", "RF"), cex=0.8, bty="n")
dev.off()



##Figure 4: External validation C-statistics (tabCe)
tabCe
taCe = reOrd(tabCe)

pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Figures/Fig4_2019-03-12.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0.85, 1), ylab="C-statistic", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taCe[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taCe[i, 2], y1=taCe[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i])
}

legend("topleft", bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=0.8)
legend(0.4, 0.965, lty=c(1, 2, 3), c("Cox", "GBM", "RF"), cex=0.8, bty="n")
dev.off()































