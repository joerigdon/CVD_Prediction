##Load packages and code
source('/Users/joerigdon/Documents/Useful_Functions/Tables_v2.R')
source('/Users/joerigdon/Documents/Useful_Functions/Functions.R')
source('/Users/joerigdon/Documents/Sanjay/Code/Evaluate.R')
library(mice)
library(survival)
library(party)
library(gbm)
library(pROC)
library(multcomp)

##Make lists of variables
dems = c("age", "sex", "black", "hispanic")
acc = c("total_chol", "hdl", "sbp", "bpmeds", "dm", "tob")
comp = c("hei", "ahei", "mds", "dash")
nutr1 = c("milk_g", "cream_g", "milk_dessert_g", "cheese_g", "meat_ns_g", "beef_g", "pork_g", "lamb_g", "poultry_g", "organ_meat_g", "fish_g", "meat_nonmeat_g", "protein_frozen_g", "eggs_g", "egg_mixture_g", "egg_sub_g", "egg_frozen_g", "legumes_g", "nuts_g", "seeds_g", "carob_g", "flour_mix_g", "bread_yeast_g", "bread_quick_g", "pastries_g", "crackers_g", "pancakes_g", "pastas_g", "cereals_g", "grain_mix_g", "meat_sub_g", "citrus_g", "fruit_dried_g", "fruit_other_g", "fruit_juice_g", "fruit_baby_g", "potatoes_g", "veg_darkgreen_g", "veg_deepyellow_g", "tomatoes_g", "veg_other_g", "veg_baby_g", "veg_meat_g", "veg_mixture_g", "fats_g", "oils_g", "salad_dressing_g", "sweets_g", "bev_nonalcohol_g", "bev_alcohol_g", "water_g", "bev_nutrition_g")
nutr2 = c("kcal", "protein_g", "carb_g", "fiber_g", "fat_g", "fat_sat_g", "fat_mono_g", "fat_poly_g", "cholesterol_mg",  "vite_mg", "vita_mg", "betacaro_mcg", "vitb1_mg", "vitb2_mg", "niacin_mg", "vitb6_mg", "folate_mcg", "vitb12_mcg", "vitc_mg", "calcium_mg", "phosphorus_mg", "magnesium_mg", "iron_mg", "zinc_mg", "copper_mg", "sodium_mg", "potassium_mg", "selenium_mcg", "caffeine_mg", "theobromine_mg", "alcohol_gm", "sfa_40_gm", "sfa_60_gm", "sfa_80_gm", "sfa_100_gm", "sfa_120_gm", "sfa_140_gm", "sfa_160_gm", "sfa_180_gm", "mfa_161h_gm", "mfa_161o_gm", "mfa_201_gm", "mfa_221_gm", "pfa_182_gm", "pfa_183_gm", "pfa_184_gm", "pfa_204_gm", "pfa_205_gm", "pfa_225_gm", "pfa_226_gm", "water_yesterday_gm")
envr = c("educ2", "pov")
nutr = c(nutr1, nutr2)
nutr0 = nutr[!nutr %in% c("bev_nutrition_g", "egg_frozen_g")]

#######################
##TRAINING THE MODELS##
#######################
tr = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Imputed_train_2019-09-21.csv", header=TRUE)

##C: cforest
m5c100_1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=100, mtry=1), data=tr)
m5c100_5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=100, mtry=5), data=tr)
m5c100_10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=100, mtry=10), data=tr)
m5c300_1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=300, mtry=1), data=tr)
m5c300_5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=300, mtry=5), data=tr)
m5c300_10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=300, mtry=10), data=tr)
m5c500_1 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=500, mtry=1), data=tr)
m5c500_5 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=500, mtry=5), data=tr)
m5c500_10 = cforest(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), control=cforest_unbiased(ntree=500, mtry=10), data=tr)

##C: Cforest
pred5c_100_1 = predCF(mod=m5c100_1, data=tr, time=120)
pred5c_100_5 = predCF(mod=m5c100_5, data=tr, time=120)
pred5c_100_10 = predCF(mod=m5c100_10, data=tr, time=120)
pred5c_300_1 = predCF(mod=m5c300_1, data=tr, time=120)
pred5c_300_5 = predCF(mod=m5c300_5, data=tr, time=120)
pred5c_300_10 = predCF(mod=m5c300_10, data=tr, time=120)
pred5c_500_1 = predCF(mod=m5c500_1, data=tr, time=120)
pred5c_500_5 = predCF(mod=m5c500_5, data=tr, time=120)
pred5c_500_10 = predCF(mod=m5c500_10, data=tr, time=120)

##Internal validation for all models
##Define true event for each data set
event = ifelse(tr$permth_int<=120 & tr$cvdevent==1, 1, 0)

##C: Cforest
getInfo(event, pred5c_100_1)
getInfo(event, pred5c_100_5)
getInfo(event, pred5c_100_10)
getInfo(event, pred5c_300_1)
getInfo(event, pred5c_300_5)
getInfo(event, pred5c_300_10)
getInfo(event, pred5c_500_1)
getInfo(event, pred5c_500_5)
getInfo(event, pred5c_500_10)

##Validate the models on test data
##Impute data for the test set
te = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Imputed_test_2019-09-21.csv", header=TRUE)

##C: Cforest
Pred5c_100_1 = predCF(mod=m5c100_1, data=te, time=120)
Pred5c_100_5 = predCF(mod=m5c100_5, data=te, time=120)
Pred5c_100_10 = predCF(mod=m5c100_10, data=te, time=120)
Pred5c_300_1 = predCF(mod=m5c300_1, data=te, time=120)
Pred5c_300_5 = predCF(mod=m5c300_5, data=te, time=120)
Pred5c_300_10 = predCF(mod=m5c300_10, data=te, time=120)
Pred5c_500_1 = predCF(mod=m5c500_1, data=te, time=120)
Pred5c_500_5 = predCF(mod=m5c500_5, data=te, time=120)
Pred5c_500_10 = predCF(mod=m5c500_10, data=te, time=120)

##External validation for all models
##Define true event for each data set
Event = ifelse(te$permth_int<=120 & te$cvdevent==1, 1, 0)

##C: CforeSt
getInfo(Event, Pred5c_100_1)
getInfo(Event, Pred5c_100_5)
getInfo(Event, Pred5c_100_10)
getInfo(Event, Pred5c_300_1)
getInfo(Event, Pred5c_300_5)
getInfo(Event, Pred5c_300_10)
getInfo(Event, Pred5c_500_1)
getInfo(Event, Pred5c_500_5)
getInfo(Event, Pred5c_500_10)



