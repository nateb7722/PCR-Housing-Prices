library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(MASS)
library(GGally)
library(GPArotation)

housing2 <- as.data.frame(Final.Project.Data)
housing2$Id <- NULL
housing2$MSZoning <- as.numeric(factor(housing2$MSZoning))
housing2$Street <- as.numeric(factor(housing2$Street))
housing2$Alley <- as.numeric(factor(housing2$Alley))
housing2$LotShape <- as.numeric(factor(housing2$LotShape))
housing2$LandContour <- as.numeric(factor(housing2$LandContour))
housing2$Utilities <- as.numeric(factor(housing2$Utilities))

housing2$LotConfig <- as.numeric(factor(housing2$LotConfig))
housing2$LandSlope <- as.numeric(factor(housing2$LandSlope))
housing2$Neighborhood <- as.numeric(factor(housing2$Neighborhood))
housing2$Condition1 <- as.numeric(factor(housing2$Condition1))
housing2$Condition2 <- as.numeric(factor(housing2$Condition2))
housing2$BldgType <- as.numeric(factor(housing2$BldgType))
housing2$HouseStyle <- as.numeric(factor(housing2$HouseStyle))
housing2$RoofStyle <- as.numeric(factor(housing2$RoofStyle))
housing2$RoofMatl <- as.numeric(factor(housing2$RoofMatl))
housing2$Exterior1st <- as.numeric(factor(housing2$Exterior1st))
housing2$Exterior2nd <- as.numeric(factor(housing2$Exterior2nd))
housing2$MasVnrType <- as.numeric(factor(housing2$MasVnrType))
housing2$ExterQual <- as.numeric(factor(housing2$ExterQual))
housing2$ExterCond <- as.numeric(factor(housing2$ExterCond))
housing2$Foundation <- as.numeric(factor(housing2$Foundation))
housing2$BsmtQual <- as.numeric(factor(housing2$BsmtQual))
housing2$BsmtCond <- as.numeric(factor(housing2$BsmtCond))
housing2$BsmtExposure <- as.numeric(factor(housing2$BsmtExposure))
housing2$BsmtFinType1 <- as.numeric(factor(housing2$BsmtFinType1))
housing2$BsmtFinType2 <- as.numeric(factor(housing2$BsmtFinType2))
housing2$Heating <- as.numeric(factor(housing2$Heating))
housing2$HeatingQC <- as.numeric(factor(housing2$HeatingQC))
housing2$CentralAir <- as.numeric(factor(housing2$CentralAir))
housing2$Electrical <- as.numeric(factor(housing2$Electrical))
housing2$KitchenQual <- as.numeric(factor(housing2$KitchenQual))
housing2$Functional <- as.numeric(factor(housing2$Functional))
housing2$FireplaceQu <- as.numeric(factor(housing2$FireplaceQu))
housing2$GarageType <- as.numeric(factor(housing2$GarageType))
housing2$GarageFinish <- as.numeric(factor(housing2$GarageFinish))
housing2$GarageQual <- as.numeric(factor(housing2$GarageQual))
housing2$GarageCond <- as.numeric(factor(housing2$GarageCond))
housing2$PavedDrive <- as.numeric(factor(housing2$PavedDrive))
housing2$PoolQC <- as.numeric(factor(housing2$PoolQC))
housing2$Fence <- as.numeric(factor(housing2$Fence))
housing2$MiscFeature <- as.numeric(factor(housing2$MiscFeature))
housing2$SaleType <- as.numeric(factor(housing2$SaleType))
housing2$SaleCondition <- as.numeric(factor(housing2$SaleCondition))


housing2$LotFrontage[which(is.na(housing2$LotFrontage))] <- 0
housing2$Alley[which(is.na(housing2$Alley))] <- 0
housing2$FireplaceQu[which(is.na(housing2$FireplaceQu))] <- 0
housing2$PoolQC[which(is.na(housing2$PoolQC))] <- 0
housing2$Fence[which(is.na(housing2$Fence))] <- 0
housing2$MiscFeature[which(is.na(housing2$MiscFeature))] <- 0

housing3 <- housing2


# Check for multicolinearity 

fullFit = lm(housing3$SalePrice ~ ., data=housing3)
summary(fullFit)
vif(fullFit)

# Error: There are aliased coefficients in the model
# Could Not run VIF, So ran Alias and removed those with non-zero values
alias(fullFit)

housing3$TotalBsmtSF <- NULL
housing3$GrLivArea <- NULL


#Still could not fun VIF, removed Utilities
housing3$Utilities <- NULL

#Checking for multicollinearity issues
fullFit = lm(housing3$SalePrice ~ ., data=housing3)
summary(fullFit)
vif(fullFit)
plot(resid(fullFit))

#Checking Residuals for regression

sresid <- studres(fullFit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
plot(sresid)
spreadLevelPlot(fullFit)

## Do not seem to have constant variance among the residuals
## due to the large amount of variables, residuals, and logical correlation among some of the variables
## PCA predictions seems to be a good idea

## because Sale Price is the target variable, Deleting outliers using 1.5*IQR and checking distribution of Sale Price
housing4 <- na.exclude(housing3)
sale.price <- housing4$SalePrice
boxplot(sale.price,data=housing4, main="Sale Price Boxplot",
        xlab="Price", ylab="Frequency")


outlier.cutoff <- quantile(housing4$SalePrice,0.75) + 1.5*IQR(housing4$SalePrice)
outlier_ROT <- which(housing4$SalePrice > outlier.cutoff)
housingFinal <- housing4[-outlier_ROT, ]




### Check distribution of sale price



x <- sale.price

h<-hist(x, breaks=50, col="red", xlab="Sale Price", 
        main="Distribution of Sale Price") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

## Distribution is clearly skewed to the right
## After changing sales price to log sales price, distribution is normal, now no outliers and have normal distribution. ready for PCA

log.price <- log(sale.price)
x <- log.price

h<-hist(x, breaks=50, col="red", xlab="Sale Price", 
        main="Distribution of Log of Sale Price") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


### Running PCA
#sale.price <- housingOmit$SalePrice

## split train and test
set.seed(123)
samp <- sample(nrow(housingFinal), nrow(housingFinal)*0.75)
housingTrain <- housingFinal[samp,]
housingTrain$SalePrice = NULL
housingTest <- housingFinal[-samp,]
housingTest1 <- housingFinal[-samp,]
y_test <- housingTest$SalePrice
housingTest$SalePrice <- NULL

### looking at the plot, there seems to be 10 significant components 
p = prcomp(housingFinal, center=T, scale=T)
plot(p, main = "PCA Plot")
abline(1, 0)
####
log.sale.price <- log(housingFinal$SalePrice)

housingFinal$SalePrice <- NULL
p = prcomp(housingFinal, center=T, scale=T)


reg <- lm(log.sale.price ~ p$x[,1] + p$x[,2] + p$x[,3] + p$x[,4] + p$x[,5] + p$x[,6] + p$x[,7]  + p$x[,8] + p$x[,9] + p$x[,10])
reg
summary(reg)













