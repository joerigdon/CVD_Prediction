##Trichopoulou NEJM

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

##0-9 score, sex specific median
##For beneficial components (1 if at or above; 0 below)
##(vegetables, legumes, fruits and nuts, cereal, and fish),
##persons whose consumption was below the median were assigned a value of 0, and persons whose consumption was at or above the median were assigned a value of 1.
##For components presumed to be detrimental (0 if at or below; 1 above)
##(meat, poultry, and dairy products, which are rarely nonfat or low-fat in Greece),
##persons whose consumption was below the median were assigned a value of 1, and persons whose consumption was at or above the median were assigned a value of 0.
##For ethanol, a value of 1 was assigned to men who consumed between 10 and 50 g per day and to women who consumed between 5 and 25 g per day.
##Finally, for fat intake, we used the ratio of monounsaturated lipids to saturated lipids, rather than the ratio of polyunsaturated to saturated lipids, because in Greece, monounsaturated lipids are used in much higher quantities than polyunsaturated lipids. Thus, the total Mediterranean-diet score ranged from 0 (minimal adherence to the traditional Mediterranean diet) to 9 (maximal adherence).

##Get Med Diet score

mds = function(all) {
##BENEFICIAL: Persons whose consumption was BELOW the sex-specific median were assigned a value of 0, ABOVE value = 1
veg = c("potatoes_g", "veg_darkgreen_g", "veg_deepyellow_g", "tomatoes_g", "veg_other_g", "veg_baby_g", "veg_meat_g", "veg_mixture_g")
##Legumes
all$leg = all$legumes_g
##Fruits and Nuts
fruits_nuts = c("nuts_g", "seeds_g", "citrus_g", "fruit_dried_g", "fruit_other_g", "fruit_juice_g", "fruit_baby_g")
##Whole Grains
wg = c("pastas_g", "cereals_g", "grain_mix_g")
##Fish
all$fish = all$fish_g
##Quality Oils / Fats
#qual = c("fats_g", "oils_g")
mufa = c("mfa_161h_gm", "mfa_161o_gm", "mfa_201_gm", "mfa_221_gm")
sfa = c("sfa_40_gm", "sfa_60_gm", "sfa_80_gm", "sfa_100_gm", "sfa_120_gm", "sfa_140_gm", "sfa_160_gm", "sfa_180_gm")

##ETHANOL: Persons within the following ranges were assigned a value of 1, outside of the ranges in either direction = 0
##Men: 10 - 50g / day
##Women: 5 - 25g / day
all$eth_g = all$bev_alcohol_g

##DETRIMENTAL: persons whose consumption was ABOVE the sex-specific median were assigned to a value of 0, BELOW = 1
##Meat
meat = c("meat_ns_g", "beef_g", "pork_g", "lamb_g", "poultry_g", "organ_meat_g", "fish_g", "protein_frozen_g")
##Dairy Products
dairy = c("milk_g", "cream_g", "milk_dessert_g", "cheese_g")

added_sugars = c("fruit_juice_g", "bev_nutrition_g", "bev_nonalcohol_g")

refined_grain = c("flour_mix_g",  "bread_yeast_g", "bread_quick_g", "pastries_g", "crackers_g", "pancakes_g", "pastas_g", "cereals_g", "grain_mix_g")

##Sum up categories
all$veg = apply(all[, names(all) %in% veg], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
#all$qual = apply(all[, names(all) %in% qual], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
all$ratio = apply(all[, names(all) %in% mufa], 1, function(x) sum(x, na.rm=TRUE)) / apply(all[, names(all) %in% sfa], 1, function(x) sum(x, na.rm=TRUE))
#all$mufa = apply(all[, names(all) %in% mufa], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
#all$sfa = apply(all[, names(all) %in% sfa], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
all$fruits_nuts= apply(all[, names(all) %in% fruits_nuts], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
all$wg = apply(all[, names(all) %in% wg], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
all$meat = apply(all[, names(all) %in% meat], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
all$dairy = apply(all[, names(all) %in% dairy], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
all$added_sugars = apply(all[, names(all) %in% added_sugars], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))
all$refined_grain = apply(all[, names(all) %in% refined_grain], 1, function(x) sum(as.numeric(as.character(x)), na.rm=TRUE))

##Get sex-specific medians for MDS
##Need sex/gender (0 = male, 1 = female)
j5w = all[all$sex==1, ]
j5m = all[all$sex==0, ]

##Apply sex-specific criteria for MDS
##Women
j5w$critV = ifelse(j5w$veg>=median(j5w$veg, na.rm=TRUE), 1, 0)
j5w$critL = ifelse(j5w$leg>=median(j5w$leg, na.rm=TRUE), 1, 0)
j5w$critFN = ifelse(j5w$fruits_nuts>=median(j5w$fruits_nuts, na.rm=TRUE), 1, 0)
j5w$critWG = ifelse(j5w$wg>=median(j5w$wg, na.rm=TRUE), 1, 0)
j5w$critF = ifelse(j5w$fish>=median(j5w$fish, na.rm=TRUE), 1, 0)
j5w$critQ = ifelse(j5w$ratio>=median(j5w$ratio, na.rm=TRUE), 1, 0)
j5w$critE = ifelse(j5w$eth_g>=5 & j5w$eth_g<=25, 1, 0)
j5w$critM = ifelse(j5w$meat>median(j5w$meat, na.rm=TRUE), 0, 1)
j5w$critD = ifelse(j5w$dairy>median(j5w$dairy, na.rm=TRUE), 0, 1)
j5w$critAS = ifelse(j5w$added_sugars>median(j5w$added_sugars, na.rm=TRUE), 0, 1)
j5w$critRG = ifelse(j5w$refined_grain>median(j5w$refined_grain, na.rm=TRUE), 0, 1)
j5w$MDS1 = apply(j5w[, names(j5w) %in% c('critV', 'critL', 'critFN', 'critWG', 'critF', 'critQ', 'critE', 'critM', 'critD')], 1, function(x) sum(x, na.rm=TRUE))
table(j5w$MDS1, exclude=NULL)
#j5w$MDS2 = apply(j5w[, names(j5w) %in% c('critV', 'critL', 'critFN', 'critWG', 'critF', 'critQ', 'critE', 'critM', 'critD', 'critAS', 'critRG')], 1, function(x) sum(x, na.rm=TRUE))
#table(j5w$MDS2, exclude=NULL)

##Men
j5m$critV = ifelse(j5m$veg>=median(j5m$veg, na.rm=TRUE), 1, 0)
j5m$critL = ifelse(j5m$leg>=median(j5m$leg, na.rm=TRUE), 1, 0)
j5m$critFN = ifelse(j5m$fruits_nuts>=median(j5m$fruits_nuts, na.rm=TRUE), 1, 0)
j5m$critWG = ifelse(j5m$wg>=median(j5m$wg, na.rm=TRUE), 1, 0)
j5m$critF = ifelse(j5m$fish>=median(j5m$fish, na.rm=TRUE), 1, 0)
j5m$critQ = ifelse(j5m$ratio>=median(j5m$ratio, na.rm=TRUE), 1, 0)
j5m$critE = ifelse(j5m$eth_g>=10 & j5m$eth_g<=50, 1, 0)
j5m$critM = ifelse(j5m$meat>median(j5m$meat, na.rm=TRUE), 0, 1)
j5m$critD = ifelse(j5m$dairy>median(j5m$dairy, na.rm=TRUE), 0, 1)
j5m$critAS = ifelse(j5m$added_sugars>median(j5m$added_sugars, na.rm=TRUE), 0, 1)
j5m$critRG = ifelse(j5m$refined_grain>median(j5m$refined_grain, na.rm=TRUE), 0, 1)
j5m$MDS1 = apply(j5m[, names(j5m) %in% c('critV', 'critL', 'critFN', 'critWG', 'critF', 'critQ', 'critE', 'critM', 'critD')], 1, function(x) sum(x, na.rm=TRUE))
table(j5m$MDS1, exclude=NULL)
#j5m$MDS2 = apply(j5m[, names(j5m) %in% c('critV', 'critL', 'critFN', 'critWG', 'critF', 'critQ', 'critE', 'critM', 'critD', 'critAS', 'critRG')], 1, function(x) sum(x, na.rm=TRUE))

##Merge back in with all
all$ID = rownames(all)
temp = rbind(j5m[, names(j5m) %in% c("sex", "MDS1")], j5w[, names(j5w) %in% c("sex", "MDS1")])
temp$sex2 = temp$sex
temp$ID = rownames(temp)
all2 = merge(all, temp[, names(temp) %in% c("ID", "MDS1", "sex2")], by="ID", all.x=TRUE)

##Set as missing the ones with missing components
tmis = apply(all2[, names(all2) %in% c("veg", "leg", "fruits_nuts", "wg", "fish", "ratio", "eth_g", "meat", "dairy")], 1, function(x) sum(is.na(x)))
all2$MDS1[tmis!=0] = NA
#table(all2$MDS1, exclude=NULL)

##Return med diet score
all2$MDS1
}

##therefore can be reasonably scored in terms of eight component characteristics: high monounsaturated:saturated fat ratio; moderate ethanol consumption (there were no men who drank more than seven glasses of wine a day and no women who drank more than two glasses of wine a day so that no study subject could be considered a heavy drinker); high consumption of legumes; high consumption of cereals (including bread and potatoes); high consumption of fruits; high consumption of vegetables; low consumption of meat and meat products; and low consumption of milk and dairy products. We used as a cut off point for all characteristics the corresponding median values specific for each sex. We a priori hypothesised that a diet with more of these components has beneficial health effects whereas a diet with fewer of these components would be less healthy. These considerations are based on the collective epidemiological and biological evidence as summarised in the report of the National Academy of Science1 and a recent critical overview
