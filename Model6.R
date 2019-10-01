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

##Fit models
##Model 1: Dems and acc only
##A: Cox model
m6a = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr)

##B: GBM
m6b_100_1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=100, interaction.depth=1, data=tr)
m6b_100_5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=100, interaction.depth=5, data=tr)
m6b_100_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=100, interaction.depth=10, data=tr)
m6b_300_1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=300, interaction.depth=1, data=tr)
m6b_300_5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=300, interaction.depth=5, data=tr)
m6b_300_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=300, interaction.depth=10, data=tr)
m6b_500_1 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=500, interaction.depth=1, data=tr)
m6b_500_5 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=500, interaction.depth=5, data=tr)
m6b_500_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=500, interaction.depth=10, data=tr)


##Get predictions for all models
##Model 1: Dems and acc only
##A: Cox
pred6a = predCox(mod=m6a, data=tr, stime="permth_int", time=120)

##B: GBM
pred6b_100_1 = predGBM(mod=m6b_100_1, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_100_5 = predGBM(mod=m6b_100_5, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_100_10 = predGBM(mod=m6b_100_10, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_300_1 = predGBM(mod=m6b_300_1, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_300_5 = predGBM(mod=m6b_300_5, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_300_10 = predGBM(mod=m6b_300_10, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_500_1 = predGBM(mod=m6b_500_1, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_500_5 = predGBM(mod=m6b_500_5, data=tr, stime="permth_int", sevent="cvdevent", time=120)
pred6b_500_10 = predGBM(mod=m6b_500_10, data=tr, stime="permth_int", sevent="cvdevent", time=120)


##Internal validation for all models
##Define true event for each data set
event = ifelse(tr$permth_int<=120 & tr$cvdevent==1, 1, 0)

##Model 1: Dems and acc only
##A: Cox
getInfo(event, pred6a)

##B: GBM
getInfo(event, pred6b_100_1)
getInfo(event, pred6b_100_5)
getInfo(event, pred6b_100_10)
s1 = getInfo(event, pred6b_300_1)
s2 = getInfo(event, pred6b_300_5)
s3 = getInfo(event, pred6b_300_10)
s4 = getInfo(event, pred6b_500_1)
s5 = getInfo(event, pred6b_500_5)
s6 = getInfo(event, pred6b_500_10)

word.tab(tab=rbind(
cbind(s1$GND_int[1:3], s1$GND_slope[1:3], s1$C, getCr2(s1)),
cbind(s2$GND_int[1:3], s2$GND_slope[1:3], s2$C, getCr2(s2)),
cbind(s3$GND_int[1:3], s3$GND_slope[1:3], s3$C, getCr2(s3)),
cbind(s4$GND_int[1:3], s4$GND_slope[1:3], s4$C, getCr2(s4)),
cbind(s5$GND_int[1:3], s5$GND_slope[1:3], s5$C, getCr2(s5)),
cbind(s6$GND_int[1:3], s6$GND_slope[1:3], s6$C, getCr2(s6))
), dest="/Users/joerigdon/Documents/Sanjay/Tables/Help6a.docx", help=FALSE)


##Validate the models on test data
##Impute data for the test set
te = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Imputed_test_2019-09-21.csv", header=TRUE)

##Get predictions for all models
##Model 1: Dems and acc only
##A: Cox
Pred6a = predCox(mod=m6a, data=te, stime="permth_int", time=120)

##B: GBM
Pred6b_100_1 = predGBM(mod=m6b_100_1, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_100_5 = predGBM(mod=m6b_100_5, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_100_10 = predGBM(mod=m6b_100_10, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_300_1 = predGBM(mod=m6b_300_1, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_300_5 = predGBM(mod=m6b_300_5, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_300_10 = predGBM(mod=m6b_300_10, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_500_1 = predGBM(mod=m6b_500_1, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_500_5 = predGBM(mod=m6b_500_5, data=te, stime="permth_int", sevent="cvdevent", time=120)
Pred6b_500_10 = predGBM(mod=m6b_500_10, data=te, stime="permth_int", sevent="cvdevent", time=120)

##External validation for all models
##Define true event for each data set
Event = ifelse(te$permth_int<=120 & te$cvdevent==1, 1, 0)

##Model 1: DemS and acc only
##A: Cox
getInfo(Event, Pred6a)

##B: GBM
getInfo(Event, Pred6b_100_1)
getInfo(Event, Pred6b_100_5)
getInfo(Event, Pred6b_100_10)
s1 = getInfo(Event, Pred6b_300_1)
s2 = getInfo(Event, Pred6b_300_5)
s3 = getInfo(Event, Pred6b_300_10)
s4 = getInfo(Event, Pred6b_500_1)
s5 = getInfo(Event, Pred6b_500_5)
s6 = getInfo(Event, Pred6b_500_10)

word.tab(tab=rbind(
cbind(s1$GND_int[1:3], s1$GND_slope[1:3], s1$C, getCr2(s1)),
cbind(s2$GND_int[1:3], s2$GND_slope[1:3], s2$C, getCr2(s2)),
cbind(s3$GND_int[1:3], s3$GND_slope[1:3], s3$C, getCr2(s3)),
cbind(s4$GND_int[1:3], s4$GND_slope[1:3], s4$C, getCr2(s4)),
cbind(s5$GND_int[1:3], s5$GND_slope[1:3], s5$C, getCr2(s5)),
cbind(s6$GND_int[1:3], s6$GND_slope[1:3], s6$C, getCr2(s6))
), dest="/Users/joerigdon/Documents/Sanjay/Tables/Help6b.docx", help=FALSE)


