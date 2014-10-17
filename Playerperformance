LM 
=========================================================

Mlm4 = lm(Salary ~ AtBat+CAtBat+Hits+Years+CHmRun+CRuns+CRBI+CWalks+League+Division+PutOuts+Assists,data=modtrial)
Pred4 = predict(Mlm4, newdata = modtrial)
summary(Mlm4)
RMSEfun(modtrial$Salary,Pred4)
RMSEfun(modtrial$Salary, mean(modtrial$Salary))

plot(modtrial$Salary,Pred4,xlab="Observed Salary",ylab="Predicted Salary",main="Pred vs Obs Salary(lm)",pch=16,col='blue')
abline(0,1,lty=2,col='red')


Mlm5 = lm(log(Salary) ~ AtBat+CHits+Years+CHmRun+CRuns+CRBI+CWalks+League+Division+PutOuts+Assists,data=modtrial)
Pred5 = predict(Mlm5, newdata = modtrial)
summary(Mlm5)
RMSEfun(modtrial$Salary,exp(Pred5))
RMSEfun(modtrial$Salary, mean(modtrial$Salary))

plot(modtrial$Salary,Pred4,xlab="W/o outlier Salary",ylab="Predicted Salary",main="Pred vs Obs Salary(lm)",pch=16,col='blue')
abline(0,1,lty=2)



MlmC = lm(Salary ~ .,data=modtrial)
PredC = predict(MlmC, newdata = modtrial)
summary(MlmC) 
RMSEfun(modtrial$Salary,PredC)
RMSEfun(modtrial$Salary, mean(modtrial$Salary))

plot(modtrial$Salary,exp(PredC),xlab="Observed Salary",ylab="Predicted Salary",main="Pred vs Obs Salary(lm)",pch=16,col='blue')
abline(0,1,lty=2,col='red')

GAMLSS
mod = gamlss(Salary~.,data=modtrial,family=GG)
pred = predict(mod,data=modtrial)
RMSEfun(modtrial$Salary,exp(pred))

plot(modtrial$Salary,exp(pred),xlab="Observed Salary",ylab="Predicted Salary",main="Pred vs Obs Salary(gamlss)",pch=16,col='red')
abline(0,1,lty=2,col='blue')
