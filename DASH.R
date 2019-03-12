##Gunther et al
##Association Between the Dietary Approaches to Hypertension Diet and Hypertension in Youth With Diabetes Mellitus (save tomorrow)

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

dash = function(all) {

grains = c("flour_mix_g", "bread_yeast_g", "bread_quick_g", "pastries_g", "crackers_g", "pancakes_g", "pastas_g", "cereals_g", "grain_mix_g")
whole_grains = c("pastas_g", "cereals_g", "grain_mix_g") #for high fiber calculation
veg = c("potatoes_g", "veg_darkgreen_g", "veg_deepyellow_g", "tomatoes_g", "veg_other_g", "veg_baby_g", "veg_meat_g", "veg_mixture_g")
fruit = c("citrus_g", "fruit_dried_g", "fruit_other_g", "fruit_juice_g", "fruit_baby_g")
dairy = c("milk_g", "cream_g", "milk_dessert_g", "cheese_g") #use milk_g as only low-fat option
meat = c("meat_ns_g", "beef_g", "pork_g", "lamb_g", "poultry_g", "organ_meat_g", "meat_nonmeat_g", "protein_frozen_g", "eggs_g", "egg_mixture_g", "egg_sub_g", "egg_frozen_g", "legumes_g", "nuts_g", "seeds_g", "meat_sub_g")
nuts = c("legumes_g", "nuts_g", "seeds_g")
fats = c("fats_g", "oils_g")
#"sweets_g"

##Total up servings/day for grains, veg, fruit, dairy, meat, fats
##1 serving = 0.5 cup; 1 cup = 236.59 g
all$grains = (2/236.59)*apply(all[, names(all) %in% grains], 1, function(x) sum(x, na.rm=TRUE))
summary(all$grains)

all$whole_grains = (2/236.59)*apply(all[, names(all) %in% whole_grains], 1, function(x) sum(x, na.rm=TRUE))
summary(all$whole_grains)

all$veg = (2/236.59)*apply(all[, names(all) %in% veg], 1, function(x) sum(x, na.rm=TRUE))
summary(all$veg)

all$fruit = (2/236.59)*apply(all[, names(all) %in% fruit], 1, function(x) sum(x, na.rm=TRUE))
summary(all$fruit)

all$dairy = (2/236.59)*apply(all[, names(all) %in% dairy], 1, function(x) sum(x, na.rm=TRUE))
summary(all$dairy)

all$meat = (2/236.59)*apply(all[, names(all) %in% meat], 1, function(x) sum(x, na.rm=TRUE))
summary(all$meat)

all$fats = (2/236.59)*apply(all[, names(all) %in% fats], 1, function(x) sum(x, na.rm=TRUE))
summary(all$fats)

##Total up servings/week for nuts, sweets
all$nuts = 7*(2/236.59)*apply(all[, names(all) %in% nuts], 1, function(x) sum(x, na.rm=TRUE))
summary(all$nuts)

all$sweets = 7*(2/236.59)*all$sweets_g
summary(all$sweets)

##Calculate scores
##Grains: 0-5 for 0-6 servings/day,
all$ScGrains = sapply(all$grains, function(x) interp(x, 0, 0, 6, 5))

##Whole_grains/grains: 0-5 for 0-50% high fiber per day
all$fiber = all$whole_grains / all$grains
all$ScFiber = sapply(all$fiber, function(x) interp(x, 0, 0, 0.5, 5))

##Vegetables: 0-10 for 0-4 servings/day
all$ScVegetables = sapply(all$veg, function(x) interp(x, 0, 0, 4, 10))

##Fruit: 0-10 for 0-4 servings/day
all$ScFruits = sapply(all$fruit, function(x) interp(x, 0, 0, 4, 10))

##Dairy: 0-5 for 0-2 servings/day,
all$ScDairy = sapply(all$dairy, function(x) interp(x, 0, 0, 2, 5))

##Milk/dairy: 0-5 for 0-75% low-fat per day
all$lowfat = (2/236.59)*all$milk_g / all$dairy
all$ScLowFat = sapply(all$lowfat, function(x) interp(x, 0, 0, 0.75, 5))

##Meat/poultry/fish/eggs: 10-0 for 2-4 servings/day
all$ScMeat = sapply(all$meat, function(x) interp(x, 2, 10, 4, 0))

##Nuts/seeds/legumes: 0-10 for 0-4 servings/WEEK
all$ScNuts = sapply(all$nuts, function(x) interp(x, 0, 0, 4, 10))

##Fats/oils: 10-0 for 3-6 servings/day
all$ScFats = sapply(all$fats, function(x) interp(x, 3, 10, 6, 0))

##Sweets: 10-0 for 5-10 servings/WEEK
all$ScSweets = sapply(all$sweets, function(x) interp(x, 5, 10, 10, 0))

##Calculate DASH (and radar plot later)
all$DASH = apply(all[, names(all) %in% c("ScGrains", "ScFiber", "ScVegetables", "ScFruits", "ScDairy", "ScLowFat", "ScMeat", "ScNuts", "ScFats", "ScSweets")], 1, function(x) sum(as.numeric(x), na.rm=TRUE))
#summary(all$ahei)

tmis = apply(all[, names(all) %in% c("ScGrains", "ScFiber", "ScVegetables", "ScFruits", "ScDairy", "ScLowFat", "ScMeat", "ScNuts", "ScFats", "ScSweets")], 1, function(x) sum(is.na(x)))
all$DASH[tmis!=0] = NA
all$DASH
}

##Adherence to the DASH diet was assessed with an index variable (ie, a score) comprising 8 DASH food groups (grains, vegetables, fruits, dairy, meat, nuts/seeds/legumes, fats/oils, and sweets),23,24 as described in detail.13 For each food group, a maximum score of 10 could be achieved when the intake met the recommendation, and lower intakes were scored proportionately. Reverse scoring was applied for meat, fats/oils, and sweets, and a score of 0 was applied to intakes ≥200% of the upper recommended level. The resulting 8 component scores were summed to create the overall DASH adherence score, which ranged from 0 to 80. Following the DASH eating plan guidelines,24 we assigned each individual the energy level closest to his or her estimated energy requirement on the basis of age, sex, and physical activity.13
