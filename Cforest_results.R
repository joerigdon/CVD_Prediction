##Load packages and code
source('/Users/joerigdon/Documents/Useful_Functions/Tables_v2.R')
source('/Users/joerigdon/Documents/Useful_Functions/Functions.R')
source('/Users/joerigdon/Documents/Sanjay/Code/Evaluate.R')
library(pROC)

##Internal validation results
tr = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Imputed_train_2019-09-21.csv", header=TRUE)
event = ifelse(tr$permth_int<=120 & tr$cvdevent==1, 1, 0)

##Model 1: Dems and acc only
pred1c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN1_100_1.csv", header=TRUE)
getInfo(event, pred1c_100_1$x)
pred1c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN1_100_5.csv", header=TRUE)
getInfo(event, pred1c_100_5$x)
pred1c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN1_100_10.csv", header=TRUE)
getInfo(event, pred1c_100_10$x)

pred1c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN1_300_1.csv", header=TRUE)
getInfo(event, pred1c_300_1$x)
pred1c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN1_300_5.csv", header=TRUE)
getInfo(event, pred1c_300_5$x)
pred1c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN1_300_10.csv", header=TRUE)
getInfo(event, pred1c_300_10$x)

pred1c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_100_1.csv", header=TRUE)
getInfo(event, pred1c_500_1$x)
pred1c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_100_5.csv", header=TRUE)
getInfo(event, pred1c_500_5$x)
pred1c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_100_10.csv", header=TRUE)
getInfo(event, pred1c_500_10$x)


##Model 2: Dems, acc, hei
pred2c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_100_1.csv", header=TRUE)
getInfo(event, pred2c_100_1$x)
pred2c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_100_5.csv", header=TRUE)
getInfo(event, pred2c_100_5$x)
pred2c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_100_10.csv", header=TRUE)
getInfo(event, pred2c_100_10$x)

pred2c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_300_1.csv", header=TRUE)
getInfo(event, pred2c_300_1$x)
pred2c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_300_5.csv", header=TRUE)
getInfo(event, pred2c_300_5$x)
pred2c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_300_10.csv", header=TRUE)
getInfo(event, pred2c_300_10$x)

pred2c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_500_1.csv", header=TRUE)
getInfo(event, pred2c_500_1$x)
pred2c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_500_5.csv", header=TRUE)
getInfo(event, pred2c_500_5$x)
pred2c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN2_500_10.csv", header=TRUE)
getInfo(event, pred2c_500_10$x)


##Model 3: Dems, acc, ahei
pred3c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_100_1.csv", header=TRUE)
getInfo(event, pred3c_100_1$x)
pred3c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_100_5.csv", header=TRUE)
getInfo(event, pred3c_100_5$x)
pred3c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_100_10.csv", header=TRUE)
getInfo(event, pred3c_100_10$x)

pred3c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_300_1.csv", header=TRUE)
getInfo(event, pred3c_300_1$x)
pred3c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_300_5.csv", header=TRUE)
getInfo(event, pred3c_300_5$x)
pred3c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_300_10.csv", header=TRUE)
getInfo(event, pred3c_300_10$x)

pred3c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_500_1.csv", header=TRUE)
getInfo(event, pred3c_500_1$x)
pred3c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_500_5.csv", header=TRUE)
getInfo(event, pred3c_500_5$x)
pred3c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN3_500_10.csv", header=TRUE)
getInfo(event, pred3c_500_10$x)


##Model 4: Dems, acc, mds
pred4c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_100_1.csv", header=TRUE)
getInfo(event, pred4c_100_1$x)
pred4c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_100_5.csv", header=TRUE)
getInfo(event, pred4c_100_5$x)
pred4c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_100_10.csv", header=TRUE)
getInfo(event, pred4c_100_10$x)

pred4c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_300_1.csv", header=TRUE)
getInfo(event, pred4c_300_1$x)
pred4c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_300_5.csv", header=TRUE)
getInfo(event, pred4c_300_5$x)
pred4c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_300_10.csv", header=TRUE)
getInfo(event, pred4c_300_10$x)

pred4c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_500_1.csv", header=TRUE)
getInfo(event, pred4c_500_1$x)
pred4c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_500_5.csv", header=TRUE)
getInfo(event, pred4c_500_5$x)
pred4c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN4_500_10.csv", header=TRUE)
getInfo(event, pred4c_500_10$x)


##Model 5: Dems, acc, dash
pred5c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_100_1.csv", header=TRUE)
getInfo(event, pred5c_100_1$x)
pred5c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_100_5.csv", header=TRUE)
getInfo(event, pred5c_100_5$x)
pred5c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_100_10.csv", header=TRUE)
getInfo(event, pred5c_100_10$x)

pred5c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_300_1.csv", header=TRUE)
getInfo(event, pred5c_300_1$x)
pred5c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_300_5.csv", header=TRUE)
getInfo(event, pred5c_300_5$x)
pred5c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_300_10.csv", header=TRUE)
getInfo(event, pred5c_300_10$x)

pred5c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_500_1.csv", header=TRUE)
getInfo(event, pred5c_500_1$x)
pred5c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_500_5.csv", header=TRUE)
getInfo(event, pred5c_500_5$x)
pred5c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TRAIN5_500_10.csv", header=TRUE)
getInfo(event, pred5c_500_10$x)


##Model 6: Dems, acc, nutrition
pred6c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_100_1.csv", header=TRUE)
getInfo(event, pred6c_100_1$x)
pred6c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_100_5.csv", header=TRUE)
getInfo(event, pred6c_100_5$x)
pred6c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_100_10.csv", header=TRUE)
getInfo(event, pred6c_100_10$x)

pred6c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_300_1.csv", header=TRUE)
getInfo(event, pred6c_300_1$x)
pred6c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_300_5.csv", header=TRUE)
getInfo(event, pred6c_300_5$x)
pred6c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_300_10.csv", header=TRUE)
getInfo(event, pred6c_300_10$x)

pred6c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_500_1.csv", header=TRUE)
getInfo(event, pred6c_500_1$x)
pred6c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_500_5.csv", header=TRUE)
getInfo(event, pred6c_500_5$x)
pred6c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_500_10.csv", header=TRUE)
getInfo(event, pred6c_500_10$x)



##External validation results
te = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Imputed_test_2019-09-21.csv", header=TRUE)
Event = ifelse(te$permth_int<=120 & te$cvdevent==1, 1, 0)


##Model 1: Dems and acc only
Pred1c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST1_100_1.csv", header=TRUE)
getInfo(Event, Pred1c_100_1$x)
Pred1c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST1_100_5.csv", header=TRUE)
getInfo(Event, Pred1c_100_5$x)
Pred1c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST1_100_10.csv", header=TRUE)
getInfo(Event, Pred1c_100_10$x)

Pred1c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST1_300_1.csv", header=TRUE)
getInfo(Event, Pred1c_300_1$x)
Pred1c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST1_300_5.csv", header=TRUE)
getInfo(Event, Pred1c_300_5$x)
Pred1c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST1_300_10.csv", header=TRUE)
getInfo(Event, Pred1c_300_10$x)

Pred1c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_100_1.csv", header=TRUE)
getInfo(Event, Pred1c_500_1$x)
Pred1c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_100_5.csv", header=TRUE)
getInfo(Event, Pred1c_500_5$x)
Pred1c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_100_10.csv", header=TRUE)
getInfo(Event, Pred1c_500_10$x)


##Model 2: Dems, acc, hei
Pred2c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_100_1.csv", header=TRUE)
getInfo(Event, Pred2c_100_1$x)
Pred2c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_100_5.csv", header=TRUE)
getInfo(Event, Pred2c_100_5$x)
Pred2c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_100_10.csv", header=TRUE)
getInfo(Event, Pred2c_100_10$x)

Pred2c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_300_1.csv", header=TRUE)
getInfo(Event, Pred2c_300_1$x)
Pred2c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_300_5.csv", header=TRUE)
getInfo(Event, Pred2c_300_5$x)
Pred2c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_300_10.csv", header=TRUE)
getInfo(Event, Pred2c_300_10$x)

Pred2c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_500_1.csv", header=TRUE)
getInfo(Event, Pred2c_500_1$x)
Pred2c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_500_5.csv", header=TRUE)
getInfo(Event, Pred2c_500_5$x)
Pred2c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST2_500_10.csv", header=TRUE)
getInfo(Event, Pred2c_500_10$x)


##Model 3: Dems, acc, ahei
Pred3c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_100_1.csv", header=TRUE)
getInfo(Event, Pred3c_100_1$x)
Pred3c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_100_5.csv", header=TRUE)
getInfo(Event, Pred3c_100_5$x)
Pred3c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_100_10.csv", header=TRUE)
getInfo(Event, Pred3c_100_10$x)

Pred3c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_300_1.csv", header=TRUE)
getInfo(Event, Pred3c_300_1$x)
Pred3c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_300_5.csv", header=TRUE)
getInfo(Event, Pred3c_300_5$x)
Pred3c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_300_10.csv", header=TRUE)
getInfo(Event, Pred3c_300_10$x)

Pred3c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_500_1.csv", header=TRUE)
getInfo(Event, Pred3c_500_1$x)
Pred3c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_500_5.csv", header=TRUE)
getInfo(Event, Pred3c_500_5$x)
Pred3c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST3_500_10.csv", header=TRUE)
getInfo(Event, Pred3c_500_10$x)


##Model 4: Dems, acc, mds
Pred4c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_100_1.csv", header=TRUE)
getInfo(Event, Pred4c_100_1$x)
Pred4c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_100_5.csv", header=TRUE)
getInfo(Event, Pred4c_100_5$x)
Pred4c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_100_10.csv", header=TRUE)
getInfo(Event, Pred4c_100_10$x)

Pred4c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_300_1.csv", header=TRUE)
getInfo(Event, Pred4c_300_1$x)
Pred4c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_300_5.csv", header=TRUE)
getInfo(Event, Pred4c_300_5$x)
Pred4c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_300_10.csv", header=TRUE)
getInfo(Event, Pred4c_300_10$x)

Pred4c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_500_1.csv", header=TRUE)
getInfo(Event, Pred4c_500_1$x)
Pred4c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_500_5.csv", header=TRUE)
getInfo(Event, Pred4c_500_5$x)
Pred4c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST4_500_10.csv", header=TRUE)
getInfo(Event, Pred4c_500_10$x)


##Model 5: Dems, acc, dash
Pred5c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_100_1.csv", header=TRUE)
getInfo(Event, Pred5c_100_1$x)
Pred5c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_100_5.csv", header=TRUE)
getInfo(Event, Pred5c_100_5$x)
Pred5c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_100_10.csv", header=TRUE)
getInfo(Event, Pred5c_100_10$x)

Pred5c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_300_1.csv", header=TRUE)
getInfo(Event, Pred5c_300_1$x)
Pred5c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_300_5.csv", header=TRUE)
getInfo(Event, Pred5c_300_5$x)
Pred5c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_300_10.csv", header=TRUE)
getInfo(Event, Pred5c_300_10$x)

Pred5c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_500_1.csv", header=TRUE)
getInfo(Event, Pred5c_500_1$x)
Pred5c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_500_5.csv", header=TRUE)
getInfo(Event, Pred5c_500_5$x)
Pred5c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred/TEST5_500_10.csv", header=TRUE)
getInfo(Event, Pred5c_500_10$x)


##Model 6: Dems, acc, nutrition
##Internal
Pred6c_100_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_100_1.csv", header=TRUE)
getInfo(Event, Pred6c_100_1$x)
Pred6c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_100_5.csv", header=TRUE)
getInfo(Event, Pred6c_100_5$x)
Pred6c_100_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_100_10.csv", header=TRUE)
getInfo(Event, Pred6c_100_10$x)

Pred6c_300_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_300_1.csv", header=TRUE)
getInfo(Event, Pred6c_300_1$x)
Pred6c_300_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_300_5.csv", header=TRUE)
getInfo(Event, Pred6c_300_5$x)
Pred6c_300_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_300_10.csv", header=TRUE)
getInfo(Event, Pred6c_300_10$x)

Pred6c_500_1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_500_1.csv", header=TRUE)
getInfo(Event, Pred6c_500_1$x)
Pred6c_500_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_500_5.csv", header=TRUE)
getInfo(Event, Pred6c_500_5$x)
Pred6c_500_10 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TEST6_500_10.csv", header=TRUE)
getInfo(Event, Pred6c_500_10$x)


##Sensitivity analyses
p1 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred3/TEST_educ_inc.csv", header=TRUE)
getInfo(Event, p1$x)

Event1 = ifelse(te$permth_int<=120 & te$hd==1, 1, 0)
p2 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred3/TEST_hd.csv", header=TRUE)
getInfo(Event1, p2$x)

Event2 = ifelse(te$permth_int<=120 & te$cereb==1, 1, 0)
p3 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred3/TEST_cereb.csv", header=TRUE)
getInfo(Event2, p3$x)




