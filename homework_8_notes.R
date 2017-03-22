#homework 8 notes
#Lauren Ashlock
#compbio
#3 8 17


###Linear Regression
###continuous dependent and independent vars

# data
xVar <- 1:10
yVar <- runif(10)
dataFrame <- data.frame(xVar,yVar)

# model
regModel <- lm(yVar~xVar,data=dataFrame) #linear regression function

# model output
print(regModel)
#output
# Coefficients:
#   (Intercept)         xVar  
# 0.61014     -0.01989

print(summary(regModel))
#output
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.43518 -0.29686  0.01578  0.27148  0.51339 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.61014    0.24536   2.487   0.0377 *
#   xVar        -0.01989    0.03954  -0.503   0.6285  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3592 on 8 degrees of freedom
# Multiple R-squared:  0.03067,	Adjusted R-squared:  -0.0905 
# F-statistic: 0.2531 on 1 and 8 DF,  p-value: 0.6285



# plot
plot(y=dataFrame$yVar,x=dataFrame$xVar,pch=21,bg="lightblue",cex=2, main="Linear Regression", xlab="Number of cats", ylab="Probability of going crazy")
#plots scatter 

abline(regModel)
#fits line to data

##ANOVA
#Continuous dependent and discrete independent

# data
xVar <- as.factor(rep(c("Control","Heated","Cooled"),each=5))
yVar <- c(rgamma(10,shape=5,scale=5),rgamma(5,shape=5,scale=10))
dataFrame <- data.frame(xVar,yVar)

# model
anovaModel <- aov(yVar~xVar,data=dataFrame) #function that runs ANOVA

# model output
print(anovaModel)
summary(anovaModel)

# plot
boxplot(yVar~xVar,data=dataFrame,col=c("grey","thistle","orchid"))

##Contingency table

#categorical independent and dependent

# data
vec1 <- c(50,66,22)
vec2 <- c(120,22,30)
dataMatrix <- rbind(vec1,vec2)
rownames(dataMatrix) <- c("Cold","Warm")
colnames(dataMatrix) <-c("Aphaenogaster",
                         "Camponotus",
                         "Crematogaster")



# model + model output
print(chisq.test(dataMatrix))

# plot
mosaicplot(x=dataMatrix,
           col=c("goldenrod","grey","black"),
           shade=FALSE)
barplot(height=dataMatrix,
        beside=TRUE,
        col=c("cornflowerblue","tomato"))

#expected data counts
chisq.test(dataMatrix)$expected

##Verify expected counts
(sum(dataMatrix[,1])*sum(dataMatrix[1,]))/sum(dataMatrix)

#compare expected vs observed

par(mfrow=c(2,1))

expected <- as.matrix(chisq.test(dataMatrix)$expected)


barplot(height=expected,
        beside=TRUE,
        col=c("cornflowerblue","tomato")) #expected

barplot(height=dataMatrix,
        beside=TRUE,
        col=c("cornflowerblue","tomato"))#observed


##Logistic regression

# data
xVar <- rgamma(n=20,shape=5,scale=5)
yVar <- rbinom(n=20,size=1,p=0.5)
dataFrame <- data.frame(xVar,yVar)

# model
logRegMod <- glm(yVar ~ xVar,
                 data=dataFrame,
                 family=binomial(link="logit"))
# model output
print(logRegMod)
summary(logRegMod)

par(mfrow=c(1,1))

# plot
plot(x=dataFrame$xVar, y=dataFrame$yVar,pch=21,bg="tan",cex=2.5)
curve(predict(logRegMod,data.frame(xVar=x),type="response"),add=TRUE,lwd=2)


