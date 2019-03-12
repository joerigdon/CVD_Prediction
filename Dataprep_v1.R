##Load packages and functions
#library(readr)
source('/Users/jrigdon/Box sync/Rigdon/Useful Functions/Tables.R')
source('/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Code/HEI.R') #for calculating HEI
source('/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Code/AHEI.R') #for calculating AHEI
source('/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Code/MDS.R') #for calculating MDS
source('/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Code/DASH.R') #for calculating DASH

##Load NHANES data
nhanesnutr9900 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/nhanesnutr9900.csv")
nhanesnutr0102 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/nhanesnutr0102.csv")
nhanesnutr0304 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/nhanesnutr0304.csv")
nhanesnutr0506 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/nhanesnutr0506.csv")
nhanesnutr0708 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/nhanesnutr0708.csv")
nhanesnutr0910 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/nhanesnutr0910.csv")


##Variables we need
keep1 = c("age", "sex", "black", "hisp", "tob", "sbp", "dbp", "cvdhx", "bpmeds", "oraldmrx", "anticoag", "statin", "a1c", "totchol", "hdl", "screat", "ualbcr", "mortstat", "ucod_leading", "dodqtr", "dodyear", "permth_int", "permth_exm", "diq010", "diq070", "did070", "lbxglu", "drxtkcal", "drxtprot", "drxtcarb", "drxttfat", "drxtsfat", "drxtmfat", "drxtpfat", "drxtchol", "drxtfibe", "drxtvaiu", "drxtcaro", "drxtvb1", "drxtvb2", "drxtniac", "drxtvb6", "drxtfola", "drxtvb12", "drxtvc", "drxtve", "drxtcalc", "drxtphos", "drxtmagn", "drxtiron", "drxtzinc", "drxtcopp", "drdtsodi", "drxtpota", "drxtsele", "drxtcaff", "drxttheo", "drxtalco", "drxtwate", "drxts040", "drxts060", "drxts080", "drxts100", "drxts120", "drxts140", "drxts160", "drxts180", "drxtm161", "drxtm181", "drxtm201", "drxtm221", "drxtp182", "drxtp183", "drxtp184", "drxtp204", "drxtp205", "drxtp225", "drxtp226", "drxtvara", "drxtbcar", "drxtatoc", "drd320gw", "kcal11", "kcal12", "kcal13", "kcal14")

keep2 = c("kcal20", "kcal21", "kcal22", "kcal23", "kcal24", "kcal25", "kcal26", "kcal27", "kcal28", "kcal31", "kcal32", "kcal33", "kcal35", "kcal41", "kcal42", "kcal43", "kcal44", "kcal50", "kcal51", "kcal52", "kcal53", "kcal54", "kcal55", "kcal56", "kcal57", "kcal58", "kcal59", "kcal61", "kcal62", "kcal63", "kcal64", "kcal67", "kcal71", "kcal72", "kcal73", "kcal74", "kcal75", "kcal76", "kcal77", "kcal78", "kcal81", "kcal82", "kcal83", "kcal91", "kcal92", "kcal93", "kcal94", "kcal95", "gram11", "gram12", "gram13", "gram14", "gram20", "gram21", "gram22", "gram23", "gram24", "gram25", "gram26", "gram27", "gram28", "gram31", "gram32", "gram33", "gram35", "gram41", "gram42", "gram43", "gram44", "gram50", "gram51", "gram52", "gram53", "gram54", "gram55", "gram56", "gram57", "gram58", "gram59", "gram61", "gram62", "gram63", "gram64", "gram67", "gram71", "gram72", "gram73", "gram74", "gram75", "gram76", "gram77", "gram78", "gram81", "gram82", "gram83", "gram91", "gram92", "gram93", "gram94", "gram95")

keep3 = c("dr1tkcal", "dr1tprot", "dr1tcarb", "dr1ttfat", "dr1tsfat", "dr1tmfat", "dr1tpfat", "dr1tchol", "dr1tfibe", "dr1tvara", "dr1tbcar", "dr1tvb1", "dr1tvb2", "dr1tniac", "dr1tvb6", "dr1tfola", "dr1tvb12", "dr1tvc", "dr1tatoc", "dr1tcalc", "dr1tphos", "dr1tmagn", "dr1tiron", "dr1tzinc", "dr1tcopp", "dr1tsodi", "dr1tpota", "dr1tsele", "dr1tcaff", "dr1ttheo", "dr1talco", "dr1_320", "dr1_320z", "dr1ts040", "dr1ts060", "dr1ts080", "dr1ts100", "dr1ts120", "dr1ts140", "dr1ts160", "dr1ts180", "dr1tm161", "dr1tm181", "dr1tm201", "dr1tm221", "dr1tp182", "dr1tp183", "dr1tp184", "dr1tp204", "dr1tp205", "dr1tp225", "dr1tp226")

nh9900 = nhanesnutr9900[, names(nhanesnutr9900) %in% c(keep1, keep2)]
nh9900 = nh9900[, names(nh9900) != "drd320gw"]

nh0102 = nhanesnutr0102[, names(nhanesnutr0102) %in% c(keep1, keep2)]

nh0304 = nhanesnutr0304[, names(nhanesnutr0304) %in% c(keep1, keep2, keep3)]

nh0506 = nhanesnutr0506[, names(nhanesnutr0506) %in% c(keep1, keep2, keep3)]
names(nh0506)[names(nh0506)=="did070"] = "diq070"

nh0708 = nhanesnutr0708[, names(nhanesnutr0708) %in% c(keep1, keep2, keep3)]
names(nh0708)[names(nh0708)=="did070"] = "diq070"

nh0910 = nhanesnutr0910[, names(nhanesnutr0910) %in% c(keep1, keep2, keep3)]


##Harmonize names of data sets
dim(nh9900)
dim(nh0102)
dim(nh0304)
dim(nh0506)
dim(nh0708)
dim(nh0910)

##Reorder nh9900
reord = c("drxtfibe", "drxttfat", "drxtsfat", "drxtmfat", "drxtpfat", "drxtchol", "drxtve", "drxtvaiu", "drxtcaro", "drxtvb1", "drxtvb2", "drxtniac", "drxtvb6", "drxtfola", "drxtvb12", "drxtvc")

new = match(reord, names(nh9900))

nh9900r = nh9900[, c(1:117, new, 134:181)]
names(nh9900r)[118:133]

##More reordering for 9900
names(nh9900)[146:165]
nh9900s = nh9900r[, c(1:145, 147:165, 146, 166:181)]

##Tag each data set with a wave
nh9900s$wave = "a.9900"
nh0102$wave = "b.0102"
nh0304$wave = "c.0304"
nh0506$wave = "d.0506"
nh0708$wave = "e.0708"
nh0910$wave = "f.0910"

dta = cbind(names(nh9900s), names(nh0102), names(nh0304), names(nh0506), names(nh0708), names(nh0910))
#write.csv(dta, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/data_names.csv", row.names=FALSE)

##Merge data together all into one
namesF = names(nh0910)
names(nh9900s) = namesF
names(nh0102) = namesF
names(nh0304) = namesF
names(nh0506) = namesF
names(nh0708) = namesF
names(nh0910) = namesF

nhall = rbind(nh9900s, nh0102, nh0304, nh0506, nh0708, nh0910)

##Look at summaries of a few things
str(nhall)
summary(nhall)
table(nhall$ucod, exclude=NULL)
table(nhall$mortstat, exclude=NULL)
summary(nhall$a1c)
summary(nhall$lbxglu)
table(nhall$diq010, exclude=NULL)

##Rename nhall2 data set
names = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/NHANES/names.csv")
names(nhall) = names$new_name

##Define a few derived variables
##Diabetes
nhall$dm = (nhall$a1c>=6.5) | (nhall$glucose>=126) | (nhall$diab_md==1)
table(nhall$dm, exclude=NULL)

##CVD death
nhall$cvddeath = (nhall$mortstat=="Assumed Deceased") & ((nhall$ucod==1) | (nhall$ucod==5))
table(nhall$cvddeath, exclude=NULL)

##Redefine mortality status
table(nhall$mortstat, exclude=NULL)
nhall$mortstat2 = NA
nhall$mortstat2[nhall$mortstat=="Assumed Deceased"] = "a.Dead"
nhall$mortstat2[nhall$mortstat=="Assumed alive"] = "b.Alive"
nhall$mortstat2[nhall$mortstat==""] = "c.NoInfo"

table(nhall$mortstat2, nhall$mortstat, exclude=NULL)
table(nhall$mortstat2, nhall$ucod, exclude=NULL)

##Create time to event outcome
##For decedents (MORTSTAT = 1), the variable PERMTH_INT is calculated as the time from the interview date and to the date of death and the variable PERMTH_EXM is calculated from the exam date and to the date of death.
summary(nhall$permth_int[nhall$mortstat2=="a.Dead"])
summary(nhall$permth_exm[nhall$mortstat2=="a.Dead"])

##For survey participants who are assumed alive (MORSTAT = 0), PERMTH_INT is calculated from the interview date to the end of the linkage period (December 31, 2011) and PERMTH_EXM is calculated from the exam date and to the end of the linkage period.
summary(nhall$permth_int[nhall$mortstat2=="b.Alive"])
summary(nhall$permth_exm[nhall$mortstat2=="b.Alive"])

##What about for those with no info on mortality status
summary(nhall$permth_int[nhall$mortstat2=="c.NoInfo"])
summary(nhall$permth_exm[nhall$mortstat2=="c.NoInfo"])

##Outcome definition
##Note on outcome from NHANES: The variables DODYEAR and DODQTR are only available the National Health Interview Survey and  Second Longitudinal Study on Aging.  For NHANES, PERMTH_INT (Person Months of Follow-up from Interview Date) and PERMTH_EXM (Person Months of Follow-up from MEC/Exam Date) are provided.
nhall2 = nhall[nhall$mortstat2!="c.NoInfo", ]
summary(nhall2$permth_int[nhall2$mortstat2=="a.Dead"])
summary(nhall2$permth_int[nhall2$mortstat2=="b.Alive"])

table(nhall2$mortstat2, nhall2$cvddeath, exclude=NULL)
table(nhall2$mortstat2, exclude=NULL)

##29 deaths we can't classify; let's say they are not CVD deaths in the defition of cvdevent variable
nhall2$cvdevent = ifelse(nhall2$cvddeath==0 | is.na(nhall2$cvddeath), 0, 1)
table(nhall2$cvdevent, nhall2$cvddeath, exclude=NULL)

##Look at follow-up time by cvddeath
#nhall2 = nhall2[!is.na(nhall2$cvddeath), ]
summary(nhall2$permth_int[nhall2$cvdevent==0])
summary(nhall2$permth_int[nhall2$cvdevent==1])

##Look at missingness in all variables
m = 100*apply(nhall2, 2, function(x) round(mean(is.na(x)), 4))
mis = data.frame(variable=names(m), pmis=as.numeric(m))
tt = complete.cases(nhall2)
table(tt) #no complete cases

##Save and send to Sanjay
#word.doc(obj.list=list(mis), obj.title=c("Percent missingness of selected variables in NHANES cohort"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Tables/Missing_data_2018-11-13.docx", ftype="Arial", col.odd="white")

##Quick K-M plot of time to cvdevent
library(survival)
source("/Users/jrigdon/Box sync/TG-HDL Chart Review/Statistical_Analysis/Survival/Code/kmplot.R") #fancier code
summary(nhall2$permth_int[nhall2$cvdevent==0])
summary(nhall2$permth_int[nhall2$cvdevent==1])

fit = survfit(Surv(permth_int, cvdevent) ~ 1, data = nhall2)
par(mfrow=c(1, 1))
plot(fit)

pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Figures/KM_plot_2018-11-01.pdf")
kmplot(fit, ylab='Probability of no CVD death', xlab='Time since interview (months)', grid=TRUE, draw.lines=FALSE)
dev.off()

##Yeah agree about appendix. I think kcal make more sense

##Calculate HEI (0-100)
##Need total fruits (cup equiv/1000 kcal), whole fruits (cup equiv/1000 kcal), total veg (cup equiv/1000 kcal), greens and beens (cup equiv/1000 kcal), whole grains (oz equiv/1000 kcal), dairy (cuq equiv/1000 kcal), total protein (oz equiv/1000 kcal), sea food / plant protein (oz equiv/1000 kcal), fatty acids (ratio of (mufa + pufa) / sfa), refined grains (oz equiv/1000 kcal), sodium (g/1000 kcal), empty calories (%energy)
nhall2$hei = hei(nhall2)

##Calculate AHEI (0-110) #START HERE: 2018-12-27 (plus Vinicio)
##Need sex, vegetables (servings/d), fruits (servings/d), whole grains (g/d), sugar sweetened beverages (servings/d), nuts/legumes (servings/d), red/processed meat (servings/d), trans fat (%energy), long chain fats (mg/d), PUFA (%energy), sodium (mg/d by sex), alcohol (drinks/d by sex),
nhall2$ahei = ahei(nhall2)

##Calculate MDS (0-9)
##For benefit, need vegetables, legumes, fruits and nuts, cereal, fish; for detriment, need meat, poultry, dairy; then ethanol (g) is complicated
nhall2$mds = mds(nhall2)

##Calculate DASH (0-80)
##0-80; 8 10-point domains of grains, vegetables, fruits, dairy, meat, nuts/seeds/legumes, fats/oils, and sweets
nhall2$dash = dash(nhall2)

##Refit ASCVD risk score for CVD mort only (choose covariates)
##Age 40-79 (years), Gender (Male/Female), Race (African American/Other), Total cholesterol (mg/dL), HDL cholesterol (mg/dL), Systolic blood pressure (mmHg), Diastolic blood pressure (mmHg), Treated for high blood pressure (No/Yes), Diabetes (No/Yes), Smoker (No/Yes)
##age, sex, black, totchol, hdl, sbp, bpmeds, dm, tob

##Save data set with only necessary variables
#vv = c("permth_int", "cvdevent", "hei", "ahei", "mds", "dash")
write.csv(nhall2, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CVD_Prediction/Data/Data_2019-01-14.csv", row.names=FALSE)
