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

##Ordering of the figures
ord = c(1, 2, 3, 4, 7, 10, 13, 16, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18)

##Supp Figure A: Internal validation GND test slopes and confidence intervals (tabGi)
tabGi = rbind(c(0.5144, 0.4941, 0.5348),
c(0.5488, 0.5279, 0.5696),
c(1.2884, 0.9315, 1.6454),
c(0.5165, 0.4962, 0.5368),
c(0.5829, 0.5354, 0.6305),
c(1.3038, 0.8937, 1.7139),
c(0.5142, 0.4993, 0.5292),
c(0.5887, 0.5440, 0.6334),
c(1.2788, 0.9201, 1.6374),
c(0.5172, 0.4991, 0.5352),
c(0.5771, 0.5530, 0.6011),
c(1.2235, 0.9059, 1.5451),
c(0.5165, 0.4896, 0.5434),
c(0.5825, 0.5494, 0.6157),
c(0.7640, 0.5804, 0.9476),
c(0.5156, 0.4991, 0.5321),
c(0.6427, 0.5795, 0.7060),
c(1.2353, 0.8154, 1.6552)
)

taGi = tabGi[ord, ]


pdf("/Users/joerigdon/Documents/Sanjay/Figures/Internal_calibration_2019-09-28.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0.45, 1.7), ylab="GND slope", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taGi[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16, lwd=3)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taGi[i, 2], y1=taGi[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i], lwd=3)
}

legend("topleft", bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=1)
legend(0.4, 1.25, lty=c(1, 2, 3), lwd=3, c("Cox", "GBM", "RF"), cex=1, bty="n")
dev.off()



##Supp Figure B: Internal validation C-statistics (tabCi)
tabCi = rbind(c(0.8607, 0.8517, 0.8698),
c(0.9113, 0.9037, 0.9189),
c(0.9210, 0.9140, 0.9279),
c(0.8608, 0.8517, 0.8699),
c(0.9107, 0.9027, 0.9187),
c(0.9336, 0.9275, 0.9397),
c(0.8610, 0.8520, 0.8701),
c(0.9147, 0.9070, 0.9225),
c(0.9327, 0.9267, 0.9387),
c(0.8609, 0.8518, 0.8700),
c(0.9166, 0.9091, 0.9241),
c(0.9238, 0.9175, 0.9302),
c(0.8615, 0.8525, 0.8706),
c(0.9183, 0.9108, 0.9258),
c(0.9766, 0.9745, 0.9788),
c(0.8750, 0.8661, 0.8838),
c(0.9228, 0.9152, 0.9303),
c(0.9967, 0.9960, 0.9973)
)

taCi = tabCi[ord, ]

pdf("/Users/joerigdon/Documents/Sanjay/Figures/Internal_discrimination_2019-09-28.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0.85, 1), ylab="C-statistic", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taCi[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16, lwd=3)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taCi[i, 2], y1=taCi[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i], lwd=3)
}

legend(-1, 0.997, bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=1)
legend(-1, 0.947, lty=c(1, 2, 3), c("Cox", "GBM", "RF"), cex=1, bty="n", lwd=3)
dev.off()


##Figure 1: External validation GND test slopes and confidence intervals (tabGe)
tabGe = rbind(c(0.5278, 0.5037, 0.5520),
c(0.5608, 0.5091, 0.6124),
c(1.1824, 0.9215, 1.4433),
c(0.5265, 0.5003, 0.5527),
c(0.5908, 0.5397, 0.6419),
c(1.1735, 0.8969, 1.4501),
c(0.5347, 0.5115, 0.5579),
c(0.5697, 0.5315, 0.6079),
c(1.1657, 0.9034, 1.4281),
c(0.5268, 0.5020, 0.5516),
c(0.5480, 0.4927, 0.6034),
c(1.1250, 0.8905, 1.3595),
c(0.5248, 0.4892, 0.5604),
c(0.6169, 0.5691, 0.6647),
c(1.1604, 0.9083, 1.4124),
c(0.4611, 0.4264, 0.4959),
c(0.8318, 0.7710, 0.8927),
c(1.0137, 0.7609, 1.2666)
)

taGe = tabGe[ord, ]

pdf("/Users/joerigdon/Documents/Sanjay/Figures/External_calibration_2019-09-28.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0.45, 1.45), ylab="GND slope", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taGe[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16, lwd=3)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taGe[i, 2], y1=taGe[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i], lwd=3)
}

legend("topleft", bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=1)
legend(0.4, 1.1, lty=c(1, 2, 3), c("Cox", "GBM", "RF"), cex=1, bty="n", lwd=3)
dev.off()



##Figure 2: External validation C-statistics (tabCe)
tabCe = rbind(c(0.8780, 0.8667, 0.8893),
c(0.9010, 0.8905, 0.9115),
c(0.9083, 0.8969, 0.9197),
c(0.8781, 0.8667, 0.8894),
c(0.8968, 0.8857, 0.9080),
c(0.9148, 0.9038, 0.9258),
c(0.8784, 0.8671, 0.8897),
c(0.9025, 0.8917, 0.9133),
c(0.9147, 0.9036, 0.9258),
c(0.8783, 0.8670, 0.8896),
c(0.9043, 0.8939, 0.9147),
c(0.9055, 0.8941, 0.9168),
c(0.8775, 0.8662, 0.8888),
c(0.9073, 0.8970, 0.9175),
c(0.9114, 0.9000, 0.9227),
c(0.8830, 0.8698, 0.8962),
c(0.9058, 0.8947, 0.9170),
c(0.9320, 0.9208, 0.9433)
)

taCe = tabCe[ord, ]

pdf("/Users/joerigdon/Documents/Sanjay/Figures/External_discrimination_2019-09-28.pdf")
par(mfrow=c(1,1))
plot(x=c(1), y=c(1), xlim=c(0, 26), ylim=c(0.85, 1), ylab="C-statistic", type="n", xaxt="n", xlab="Risk prediction approach")
axis(1, at=c(2, 5.5, 11, 20.5), c("Standard", "ML", "Nutrition", "ML+Nutrition"))
#abline(h=0, col="lightgrey")
#abline(h=0.5, col="lightgrey")
abline(h=1, col="lightgrey")
#abline(h=1.5, col="lightgrey")

points(x=c(2, 5, 6, 9:13, 16:25), y=taCe[, 1], col=c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2)), pch=16, lwd=3)

xx = c(2, 5, 6, 9:13, 16:25)
cols = c(rep("black", 3), c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), rep(c("darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), each=2))
ltys = c(1, 2, 3, rep(1, times=5), rep(c(2, 3), times=5))

##CIs
for (i in 1:18) {
    arrows(x0=xx[i], x1=xx[i], y0=taCe[i, 2], y1=taCe[i, 3], angle=90, code=3, length=0.05, lty=ltys[i], col=cols[i], lwd=3)
}

legend(-1, 1, bty="n", fill=c("black", "darkred", "purple", "blue", "darkgoldenrod3", "darkgreen"), c("ACC", "+HEI", "+AHEI", "+MDS", "+DASH", "+All"), inset=0.05, cex=1)
legend(-1, 0.96, lty=c(1, 2, 3), c("Cox", "GBM", "RF"), cex=1, bty="n", lwd=3)
dev.off()


##Supp Figure C: Partial Dependence Plots
tr = read.csv("/Users/joerigdon/Documents/Sanjay/Data/Imputed_train_2019-09-21.csv", header=TRUE)
pred6c_100_5 = read.csv("/Users/joerigdon/Documents/Sanjay/Pred2/TRAIN6_100_5.csv", header=TRUE)
DF = data.frame(pred=pred6c_100_5$x, age=tr$age, sbp=tr$sbp)

##Age
DF$ageQ = as.numeric(cut2(DF$age, g=10))
pred_age = as.numeric(unlist(by(DF$pred, DF$ageQ, function(x) 1-mean(x))))
age_mean = as.numeric(unlist(by(DF$age, DF$ageQ, function(x) mean(x))))

##SBP
DF$sbpQ = as.numeric(cut2(DF$sbp, g=10))
pred_sbp = as.numeric(unlist(by(DF$pred, DF$sbpQ, function(x) 1-mean(x))))
sbp_mean = as.numeric(unlist(by(DF$sbp, DF$sbpQ, function(x) mean(x))))


pdf('/Users/joerigdon/Documents/Sanjay/Figures/PDP_2019-09-29.pdf', width=8, height=4)
par(mfrow=c(1,2))
plot(age_mean, pred_age, type="l", xlab='Age (years)', ylab='10-year probability of CVD death', main='(a)', ylim=c(0, 0.25))
plot(sbp_mean, pred_sbp, type="l", xlab='Systolic blood pressure (mmHg)', ylab='10-year probability of CVD death', main='(b)', ylim=c(0, 0.25))
dev.off()
