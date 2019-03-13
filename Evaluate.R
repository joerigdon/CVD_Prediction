##Get predictions of event by certain time for Cox model
##mod is Cox model, data is data used to fit model, stime is survival time variable name, time is time of desired prediction
predCox = function(mod, data, stime, time) {
pr = survfit(mod, newdata=data)$surv #list of survival probability for each person (column) at each UNIQUE failure time (row)
##121st row (time=120) is the probability of event by 10 years
temp = data[, names(data)==stime]
tb = table(temp)
rownames(pr) = names(tb)

##Return predictions
index = max(which(as.numeric(as.character(rownames(pr)))<=time))
pr[index, ]
}


##Get predictions of event by certain time for GBM model
##mod is Cox model, data is data used to fit model, stime is survival time variable name, sevent is survival variable indicator (0=censored, 1=event), time is time of desired prediction
predGBM = function(mod, data, stime, sevent, time) {
prr = predict(mod, newdata=data, n.trees=100)
#the above gives us the parametric part; find the lambda_0(t) to get the predictions at time t

hh = basehaz.gbm(t=data[, names(data)==stime], delta=data[, names(data)==sevent], t.eval=c(time), f.x=prr, cumulative=TRUE) #cumulative baseline hazard at time=120 months

##For Cox model, we have h(t|x) = h0(t)*exp(XB)
##So H(t|x) = exp(XB)*integral_0^t (h0(t))
cumhaz = exp(prr)*hh

##Return vector of predicted survival at specified time
exp(-cumhaz)
}


##Get predictions of event by certain time for cforest model
##mod is Cox model, data is data used to fit model, time is time of desired prediction
predCF = function(mod, data, time) {
ex = treeresponse(mod, newdata=data)
unlist(lapply(ex, function(x) x$surv[which(x$time==max(x$time[x$time<=time]))])) #to get predicted survival at 120 months for each person
}


##GND test (can compute from predicted probabilities in pred)
##Function to get GND data set, coefficient, p-value
getGND = function(true, pred0) {
df = data.frame(true=true, pred=pred0)
df$predD = as.numeric(cut2(df$pred, g=10))
df2 = aggregate(df[, names(df) %in% c("true", "pred")], by=list(df$predD), mean)
df2$pred2 = 1-df2$pred

##Fit linear model
mod1 = lm(df2$true ~ df2$pred2)
slope = mod1$coeff[2]
ci = confint(mod1)[2, ]
#con = summary(glht(mod1, matrix(c(0, 1), nrow=1, ncol=2), rhs=1), test=adjusted("none"))
#pval = con$test$pvalues
pval = summary(mod1)$coeff[2, 4]

##Return output
output.all = list(data=df2, slope=slope, ci=ci, pval=pval)
return(output.all)
}

##Feed a function a target and predictions to get (i) C-stat + CI, and (ii) GND stat + CI
getInfo = function(true, pred) {
ROC = roc(response=true, predictor=pred, direction=">", ci=TRUE)
GND = getGND(true=true, pred0=pred)

output.all = list(C=as.numeric(ROC$ci)[c(2, 1, 3)], GND=c(GND$slope, GND$ci))
return(output.all)
}


##Function to compute multiple imputation CI
MI.conf.int = function(est, se) {
n = length(est)
Q.bar = mean(est, na.rm=TRUE)
W = mean(se^2, na.rm=TRUE)
B = sum((est-Q.bar)^2, na.rm=TRUE) / (n-1)
T = W + (1+1/n) * B
gamma = (n-1) * ( (1 + W / ((1+1/n)*B) )^2 )
crit = qt(0.975 , gamma)
l = Q.bar - crit*sqrt(T)
u = Q.bar + crit*sqrt(T)
e = Q.bar

pres = paste(round(e, 4), paste(paste(paste(paste("(", round(l, 4), sep=""), ", ", sep=""), round(u, 4), sep=""), ")", sep=""), sep=" ")

output.all = list(e=e, l=l, u=u, pres=pres)
return(output.all)
}
