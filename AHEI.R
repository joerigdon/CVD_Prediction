##From Chiuve et al

##Interpolation function
interp = function(x, x1, y1, x2, y2) {
    m = (y2-y1) / (x2-x1)
    b = y2-m*x2
    y = NA
if (!is.na(x) & x<=x1) {
    y = y1
} else if (!is.na(x) & x>x1 & x<x2) {
    y = m*x + b
} else if (!is.na(x) & x>=x2) {
    y = y2
}
    y
}

ahei = function(all) {
#all = nhall2
##Define categories
total_veg = c("potatoes_g", "veg_darkgreen_g", "veg_deepyellow_g", "tomatoes_g", "veg_other_g", "veg_baby_g", "veg_meat_g", "veg_mixture_g")
total_fruits = c("citrus_g", "fruit_dried_g", "fruit_other_g", "fruit_juice_g", "fruit_baby_g")
whole_grains = c("pastas_g", "cereals_g", "grain_mix_g")
ssb = c("fruit_juice_g", "bev_nutrition_g")
nuts_legumes = c("legumes_g", "nuts_g")
red_processed_meat = c("meat_ns_g", "beef_g", "pork_g", "lamb_g", "organ_meat_g")
#trans_pct = fat_g-(fat_sat_g+fat_mono_g+fat_poly_g)
long_chain = c("fish_g")
pufa = c("pfa_182_gm", "pfa_183_gm", "pfa_184_gm", "pfa_204_gm", "pfa_205_gm", "pfa_225_gm", "pfa_226_gm")
#sodium_mg
#alcohol_gm (to drinks/day)

##Sum up within categories (cup eq/kcal)
##Total vegetables (servings/d): One serving is 0.5 cup of vegetables or 1 cup of green leafy vegetables (1 cup = 236.59 g).
all$total_veg = (2/236.59)*apply(all[, names(all) %in% total_veg], 1, function(x) sum(x, na.rm=TRUE))
summary(all$total_veg)

##Total fruit (servings/d): One serving is 1 medium piece of fruit or 0.5 cup of berries (1 cup = 236.59 g).
all$total_fruits = (2/236.59)*apply(all[, names(all) %in% total_fruits], 1, function(x) sum(x, na.rm=TRUE))
summary(all$total_fruits)

##Whole grains (g/d)
all$whole_grains_g = apply(all[, names(all) %in% whole_grains], 1, function(x) sum(x, na.rm=TRUE))
summary(all$whole_grains_g)

##SSB (servings/d): One serving is 8 oz (1 oz = 28.35 g)
all$ssb = (1/(8*28.35))*apply(all[, names(all) %in% ssb], 1, function(x) sum(x, na.rm=TRUE))
summary(all$ssb)

##Nuts/legumes (servings/d): One serving is 1 oz (1 oz = 28.35 g) of nuts or 1 tablespoon (15 mL) of peanut butter
all$nuts_legumes = (1/28.35)*apply(all[, names(all) %in% nuts_legumes], 1, function(x) sum(x, na.rm=TRUE))
summary(all$nuts_legumes)

##Red/processed meat (servings/d): One serving is 4 oz of unprocessed meat or 1.5 oz of processed meat (1 oz = 28.35 g).
all$red_processed_meat = (1/(3*28.35))*apply(all[, names(all) %in% red_processed_meat], 1, function(x) sum(x, na.rm=TRUE))
summary(all$red_processed_meat)

##Trans fat (% energy)
all$trans_fat = 100*((9*(all$fat_g-(all$fat_sat_g+all$fat_mono_g+all$fat_poly_g))) / all$kcal)
all$trans_fat[all$trans_fat<=0] = 0
summary(all$trans_fat)

##Long chain (mg/d): optimal intake (250 mg/d) is ;2 4-oz servings of fish /wk, which is consistent with current guidelines (1 oz = 28.35 g).
all$long_chain = all$fish_g*1000
summary(all$long_chain)

##PUFA (%energy)
all$pufa = 100*(9*apply(all[, names(all) %in% pufa], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$pufa)

##Sodium (mg/d)
#all$sodium_mg

##Alcohol (drinks/d): One drink is 4 oz of wine, 12 oz of beer, or 1.5 oz of liquor (1 oz = 28.35 g).
all$alcohol = (1/(6*28.35))*all$alcohol_gm
summary(all$alcohol)

##Add component scores##
##Need sex/gender (0 = male, 1 = female)

##Vegetables (servings/d): 0-10 for 0-5+
all$ScVegetables = sapply(all$total_veg, function(x) interp(x, 0, 0, 5, 10))
#plot(all$total_veg, all$ScVegetables)

##Fruits (servings/d): 0-10 for 0-4+
all$ScFruits = sapply(all$total_fruits, function(x) interp(x, 0, 0, 4, 10))
#plot(all$total_fruits, all$ScFruits)

##Whole Grains (g/d): women 0-10 for 0-75+; men 0-10 for 0-90+
all$ScWholeGrains[all$sex==1] = sapply(all$whole_grains_g[all$sex==1], function(x) interp(x, 0, 0, 75, 10))
all$ScWholeGrains[all$sex==0] = sapply(all$whole_grains_g[all$sex==0], function(x) interp(x, 0, 0, 90, 10))
#plot(all$whole_grains_g[all$sex==1], all$ScWholeGrains[all$sex==1])
#plot(all$whole_grains_g[all$sex==0], all$ScWholeGrains[all$sex==0])
all$ScWholeGrains = as.numeric(all$ScWholeGrains)

##Sugar Sweetened Beverages (servings/d): 10-0 for 0-1+
all$ScSSB = sapply(all$ssb, function(x) interp(x, 0, 10, 1, 0))
#plot(all$ssb, all$ScSSB)

##Nuts/legumes (servings/d): 0-10 for 0-1+
all$ScNutsLegumes = sapply(all$nuts_legumes, function(x) interp(x, 0, 0, 1, 10))
#plot(all$nuts_legumes, all$ScNutsLegumes)

##Red/processed meat (servings/d): 10-0 for 0-1.5+
all$ScRedProcessedMeat = sapply(all$red_processed_meat, function(x) interp(x, 0, 10, 1.5, 0))
#plot(all$red_processed_meat, all$ScRedProcessedMeat)

##Trans fat (%energy): 10-0 for <=0.5-4+
all$ScTransFat = sapply(all$trans_fat, function(x) interp(x, 0.5, 10, 4, 0))
#plot(all$trans_fat, all$ScTransFat)

##Long chain fats (mg/d): 0-10 for 0-250+
all$ScLongChainFats = sapply(all$long_chain, function(x) interp(x, 0, 0, 250, 10))
#plot(all$long_chain, all$ScLongChainFats)

##PUFA (%energy): 0-10 for <=2-10+
all$ScPUFA = sapply(all$pufa, function(x) interp(x, 2, 0, 10, 10))
#plot(all$pufa, all$ScPUFA)

##Sodium (mg/d): 10-0 for lowest-highest decile in men/women
all$ScSodium[all$sex==1] = sapply(all$sodium_mg[all$sex==1], function(x) interp(x, 1112, 10, 3337, 0))
all$ScSodium[all$sex==0] = sapply(all$sodium_mg[all$sex==0], function(x) interp(x, 1612, 10, 5271, 0))
#plot(all$sodium_mg, all$ScSodium)
all$ScSodium = as.numeric(all$ScSodium)

##Alcohol (drinks/d): 2.5 if <0.5, women: 10 if 0.5-1.5, 10-0 if 1.5-2.5, 0 if 2.5+; men: 10 if 0.5-2, 10-0 if 2-3.5, 0 if 3.5+
all$ScAlcohol[all$sex==1] = sapply(all$alcohol[all$sex==1], function(x) interp(x, 1.5, 10, 2.5, 0))
all$ScAlcohol[all$sex==1 & all$alcohol>=0.5 & all$alcohol<=1.5] = 10
all$ScAlcohol[all$sex==0] = sapply(all$alcohol[all$sex==0], function(x) interp(x, 2, 10, 3.5, 0))
all$ScAlcohol[all$sex==0 & all$alcohol>=0.5 & all$alcohol<=2] = 10

all$ScAlcohol[all$alcohol<0.5] = 2.5
#plot(all$alcohol, all$ScAlcohol)
all$ScAlcohol = as.numeric(all$ScAlcohol)

##Calculate AHEI (and radar plot later)
all$AHEI = apply(all[, names(all) %in% c("ScVegetables", "ScFruits", "ScWholeGrains", "ScSSB", "ScNutsLegumes", "ScRedProcessedMeat", "ScTransFat", "ScLongChainFats", "ScPUFA", "ScSodium", "ScAlcohol")], 1, function(x) sum(as.numeric(x), na.rm=TRUE))
#summary(all$ahei)

tmis = apply(all[, names(all) %in% c("ScVegetables", "ScFruits", "ScWholeGrains", "ScSSB", "ScNutsLegumes", "ScRedProcessedMeat", "ScTransFat", "ScLongChainFats", "ScPUFA", "ScSodium", "ScAlcohol")], 1, function(x) sum(is.na(x)))
all$AHEI[tmis!=0] = NA
all$AHEI = as.numeric(all$AHEI)
all$AHEI

#all3 = all[, names(all) %in% c("ID", "date", "ScVegetables", "ScFruits", "ScWholeGrains", "ScSSB", "ScNutsLegumes", "ScRedProcessedMeat", "ScTransFat", "ScLongChainFats", "ScPUFA", "ScSodium", "ScAlcohol", "AHEI")]
}


