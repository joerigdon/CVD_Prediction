##From Guenther et al

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


hei = function(all) {
#all = nhall2
##Define categories
total_fruits = c("citrus_g", "fruit_dried_g", "fruit_other_g", "fruit_juice_g", "fruit_baby_g")
whole_fruits = c("citrus_g", "fruit_other_g")
total_veg = c("potatoes_g", "veg_darkgreen_g", "veg_deepyellow_g", "tomatoes_g", "veg_other_g", "veg_baby_g", "veg_meat_g", "veg_mixture_g")
greens_beans = c("veg_darkgreen_g", "legumes_g")
whole_grains = c("pastas_g", "cereals_g", "grain_mix_g")
dairy = c("milk_g", "cream_g", "milk_dessert_g", "cheese_g")
total_protein = c("meat_ns_g", "beef_g", "pork_g", "lamb_g", "poultry_g", "organ_meat_g", "meat_nonmeat_g", "protein_frozen_g", "eggs_g", "egg_mixture_g", "egg_sub_g", "egg_frozen_g", "legumes_g", "nuts_g", "seeds_g", "meat_sub_g")
sea_plant_protein = c("fish_g", "carob_g")
#fatty_acids = (PUFAs + MUFAs)/SFAs
pufa = c("pfa_182_gm", "pfa_183_gm", "pfa_184_gm", "pfa_204_gm", "pfa_205_gm", "pfa_225_gm", "pfa_226_gm")
mufa = c("mfa_161h_gm", "mfa_161o_gm", "mfa_201_gm", "mfa_221_gm")
sfa = c("sfa_40_gm", "sfa_60_gm", "sfa_80_gm", "sfa_100_gm", "sfa_120_gm", "sfa_140_gm", "sfa_160_gm", "sfa_180_gm")
refined_grains = c("flour_mix_g",  "bread_yeast_g", "bread_quick_g", "pastries_g", "crackers_g", "pancakes_g", "pastas_g", "cereals_g", "grain_mix_g")
#sodium (convert from mg to g)

##Empty calories: Calories from solid fats, alcohol, and added  sugars; threshold for counting alcohol is 13 g/1,000 kcal.
empty = c("fats", "oils", "salad_dressing", "sweets", "bev_nonalcohol", "bev_alcohol")

##Sum up within categories (cup eq/kcal)
#236.588237500002 grams in 1 cup
all$total_fruits = (1000/236.588237500002)*(apply(all[, names(all) %in% total_fruits], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$total_fruits)

all$whole_fruits = (1000/236.588237500002)*(apply(all[, names(all) %in% whole_fruits], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$whole_fruits)

all$total_veg = (1000/236.588237500002)*(apply(all[, names(all) %in% total_veg], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$total_veg)

all$greens_beans = (1000/236.588237500002)*(apply(all[, names(all) %in% greens_beans], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$greens_beans)

all$whole_grains = (1000/236.588237500002)*(apply(all[, names(all) %in% whole_grains], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$whole_grains)

all$dairy = (1000/236.588237500002)*(apply(all[, names(all) %in% dairy], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$dairy)

all$total_protein = (1000/236.588237500002)*(apply(all[, names(all) %in% total_protein], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$total_protein)

all$sea_plant_protein = (1000/236.588237500002)*(apply(all[, names(all) %in% sea_plant_protein], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$sea_plant_protein)

all$fatty_acids = (apply(all[, names(all) %in% pufa], 1, function(x) sum(x, na.rm=TRUE))+apply(all[, names(all) %in% mufa], 1, function(x) sum(x, na.rm=TRUE))) / apply(all[, names(all) %in% sfa], 1, function(x) sum(x, na.rm=TRUE))
all$fatty_acids[all$fatty_acids==Inf | is.na(all$fatty_acids)] = NA
summary(all$fatty_acids)

all$refined_grains = (1000/236.588237500002)*(apply(all[, names(all) %in% refined_grains], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$refined_grains)

all$sodium = all$sodium_mg / 1000
summary(all$sodium)

all$empty = 100*(apply(all[, names(all) %in% empty], 1, function(x) sum(x, na.rm=TRUE)) / all$kcal)
summary(all$empty) #% of calories

##Add component scores
##Adequacy
##Total fruits: 0-5 for 0- ≥0.8 cup equiv. per 1,000 kcal
all$ScTotalFruits = sapply(all$total_fruits, function(x) interp(x, 0, 0, 0.8, 5))
#plot(all$total_fruits, all$ScTotalFruits)

##Whole fruits: 0-5 for 0- ≥0.4 cup equiv. per 1,000 kcal
all$ScWholeFruits = sapply(all$whole_fruits, function(x) interp(x, 0, 0, 0.4, 5))
#plot(all$whole_fruits, all$ScWholeFruits)

##Total veg: 0-5 for 0- ≥1.1 cup equiv. per 1,000 kcal
all$ScTotalVeg = sapply(all$total_veg, function(x) interp(x, 0, 0, 1.1, 5))
#plot(all$total_veg, all$ScTotalVeg)

##Greens beans: 0-5 for 0- ≥0.2 cup equiv. per 1,000 kcal
all$ScGreensBeans = sapply(all$greens_beans, function(x) interp(x, 0, 0, 0.2, 5))
#plot(all$greens_beans, all$ScGreensBeans)

##Whole grains: 0-10 for 0- ≥1.5 oz equiv. per 1,000 kcal
all$ScWholeGrains = sapply(all$whole_grains, function(x) interp(x, 0, 0, 1.5, 10))
#plot(all$whole_grains, all$ScWholeGrains)

##Dairy: 0-10 for 0- ≥1.3 cup equiv. per 1,000 kcal
all$ScDairy = sapply(all$dairy, function(x) interp(x, 0, 0, 1.3, 10))
#plot(all$dairy, all$ScDairy)

##Total protein: 0-5 for 0- ≥2.5 oz equiv. per 1,000 kcal
all$ScTotalProtein = sapply(all$total_protein, function(x) interp(x, 0, 0, 2.5, 5))
#plot(all$total_protein, all$ScTotalProtein)

##Sea food / plant protein: 0-5 for 0- ≥0.8 oz equiv. per 1,000 kcal
all$ScSeaPlantProtein = sapply(all$sea_plant_protein, function(x) interp(x, 0, 0, 0.8, 5))
#plot(all$sea_plant_protein, all$ScSeaPlantProtein)

##Fatty acids: 0-10 for ≤1.2-≥2.5 ratio
all$ScFattyAcids = sapply(all$fatty_acids, function(x) interp(x, 1.2, 0, 2.5, 10))
#plot(all$fatty_acids, all$ScFattyAcids)

##Moderation
##Refined grains: 10-0 for ≤1.8 - ≥4.3 oz equiv. per 1,000 kcal
all$ScRefinedGrains = sapply(all$refined_grains, function(x) interp(x, 1.8, 10, 4.3, 0))
#plot(all$refined_grains, all$ScRefinedGrains)

##Sodium: 10-0 for ≤1.1 - ≥2.0 gram per 1,000 kcal
all$ScSodium = sapply(all$sodium, function(x) interp(x, 1.1, 10, 2.0, 0))
#plot(all$sodium, all$ScSodium)

##Empty calories: 20-0 for ≤19% - ≥50% of energy
all$ScEmpty = sapply(all$empty, function(x) interp(x, 19, 20, 50, 0))


##Calculate HEI (and radar plot later)
all$HEI = apply(all[, names(all) %in% c("ScTotalFruits", "ScWholeFruits", "ScTotalVeg", "ScGreensBeans", "ScWholeGrains", "ScDairy", "ScTotalProtein", "ScSeaPlantProtein", "ScFattyAcids", "ScRefinedGrains", "ScSodium", "ScEmpty")], 1, function(x) sum(x, na.rm=TRUE))

##Set as missing the ones with missing components
tmis = apply(all[, names(all) %in% c("ScTotalFruits", "ScWholeFruits", "ScTotalVeg", "ScGreensBeans", "ScWholeGrains", "ScDairy", "ScTotalProtein", "ScSeaPlantProtein", "ScFattyAcids", "ScRefinedGrains", "ScSodium", "ScEmpty")], 1, function(x) sum(is.na(x)))
all$HEI[tmis!=0] = NA
all$HEI

#all2 = all[, names(all) %in% c("ID", "date", "ScTotalFruits", "ScWholeFruits", "ScTotalVeg", "ScGreensBeans", "ScWholeGrains", "ScDairy", "ScTotalProtein", "ScSeaPlantProtein", "ScFattyAcids", "ScRefinedGrains", "ScSodium", "ScEmpty", "HEI")]
}
