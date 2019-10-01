##Load packages and code
library(survival)
library(gbm)
source('/Users/joerigdon/Documents/Useful_Functions/Tables_v2.R')

##Load data
tr = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Imputed_train_2019-09-21.csv", header=TRUE)

##Make lists of variables
dems = c("age", "sex", "black", "hispanic")
acc = c("total_chol", "hdl", "sbp", "bpmeds", "dm", "tob")
comp = c("hei", "ahei", "mds", "dash")
nutr1 = c("milk_g", "cream_g", "milk_dessert_g", "cheese_g", "meat_ns_g", "beef_g", "pork_g", "lamb_g", "poultry_g", "organ_meat_g", "fish_g", "meat_nonmeat_g", "protein_frozen_g", "eggs_g", "egg_mixture_g", "egg_sub_g", "egg_frozen_g", "legumes_g", "nuts_g", "seeds_g", "carob_g", "flour_mix_g", "bread_yeast_g", "bread_quick_g", "pastries_g", "crackers_g", "pancakes_g", "pastas_g", "cereals_g", "grain_mix_g", "meat_sub_g", "citrus_g", "fruit_dried_g", "fruit_other_g", "fruit_juice_g", "fruit_baby_g", "potatoes_g", "veg_darkgreen_g", "veg_deepyellow_g", "tomatoes_g", "veg_other_g", "veg_baby_g", "veg_meat_g", "veg_mixture_g", "fats_g", "oils_g", "salad_dressing_g", "sweets_g", "bev_nonalcohol_g", "bev_alcohol_g", "water_g", "bev_nutrition_g")
nutr2 = c("kcal", "protein_g", "carb_g", "fiber_g", "fat_g", "fat_sat_g", "fat_mono_g", "fat_poly_g", "cholesterol_mg",  "vite_mg", "vita_mg", "betacaro_mcg", "vitb1_mg", "vitb2_mg", "niacin_mg", "vitb6_mg", "folate_mcg", "vitb12_mcg", "vitc_mg", "calcium_mg", "phosphorus_mg", "magnesium_mg", "iron_mg", "zinc_mg", "copper_mg", "sodium_mg", "potassium_mg", "selenium_mcg", "caffeine_mg", "theobromine_mg", "alcohol_gm", "sfa_40_gm", "sfa_60_gm", "sfa_80_gm", "sfa_100_gm", "sfa_120_gm", "sfa_140_gm", "sfa_160_gm", "sfa_180_gm", "mfa_161h_gm", "mfa_161o_gm", "mfa_201_gm", "mfa_221_gm", "pfa_182_gm", "pfa_183_gm", "pfa_184_gm", "pfa_204_gm", "pfa_205_gm", "pfa_225_gm", "pfa_226_gm", "water_yesterday_gm")
envr = c("educ2", "pov")
nutr = c(nutr1, nutr2)
nutr0 = nutr[!nutr %in% c("bev_nutrition_g", "egg_frozen_g", "veg_baby_g", "carob_g")]

##Cox models
summC = function(mod) {
ex = summary(mod)$conf.int
round(ex[, c(1, 3, 4)], 2)
}

m1a = coxph(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), data=tr)
m2a = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), data=tr)
m3a = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), data=tr)
m4a = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), data=tr)
m5a = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), data=tr)
m6a = coxph(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), data=tr)

summC(m1a)
summC(m2a)
summC(m3a)
summC(m4a)
summC(m5a)
summC(m6a)


##Best performing GBMs
m1b_500_10 = gbm(as.formula( paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~") ), distribution="coxph", n.trees=500, interaction.depth=10, data=tr)
m2b_100_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "hei", sep="+") ), distribution="coxph", n.trees=100, interaction.depth=10, data=tr)
m3b_100_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "ahei", sep="+") ), distribution="coxph", n.trees=100, interaction.depth=10, data=tr)
m4b_100_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "mds", sep="+") ), distribution="coxph", n.trees=100, interaction.depth=10, data=tr)
m5b_100_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), "dash", sep="+") ), distribution="coxph", n.trees=100, interaction.depth=10, data=tr)
m6b_300_10 = gbm(as.formula( paste(paste("Surv(permth_int, cvdevent)", paste(paste(dems, collapse="+"), paste(acc, collapse="+"), sep="+"), sep="~"), paste(nutr0, collapse="+"), sep="+") ), distribution="coxph", n.trees=300, interaction.depth=10, data=tr)

summary(m1b_500_10)
summary(m2b_100_10)
summary(m3b_100_10)
summary(m4b_100_10)
summary(m5b_100_10)
rr = summary(m6b_300_10)

df1 = data.frame(var=rr$var, relInfl=round(rr$rel.inf, 2))
df2 = df1[!df1$var %in% c(dems, acc), ]
write.csv(df2, "/Users/joerigdon/Documents/Sanjay/Data/Help_2019-09-29.csv", row.names=FALSE)
#word.tab(df2, dest="/Users/joerigdon/Documents/Sanjay//Help_2019-09-29.docx", help=FALSE)

