###Assignment- Linear Regression###

#Business Objective:

# 1 Understand factors that affect car price in US market
# 2 Identify variables that are significant in predicting the price of a car


#Outcome:

# Creating Price prediction model using regression analysis. 
# The model will be a good way for the management to understand the pricing dynamics of a new market. 


#=====================================================================================

# Call needed Libraries 

library(dplyr)
library(tidyr)
library(MASS)
library(car)
library(stringr)


########################################################################################

#**** Import the input file ****

rawfile <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
View(rawfile)
str(rawfile)

########################################################################################

# Check for blank and Missing Values
length(which(is.na(rawfile))) # No NA values found
sapply(rawfile, function(x) length(which(x == ""))) # No blank values
sum(duplicated(rawfile)) #No duplicates


# Variable "CarName" is split into "CompanyName" and "ModelName"

carprice <- separate(rawfile, CarName, into = c("Company","ModelName"), 
                     sep = " ", extra = "merge", fill = "right")

# Removing model name as its not neededin the model.
carprice <- carprice[,-4]

View(carprice)

#####################################################################################

# Data Understanding, Preparation and EDA

#changing categorical variables to factor values

carprice$symboling <- as.factor(carprice$symboling)
carprice$fueltype <- as.factor(carprice$fueltype)
carprice$aspiration <- as.factor(carprice$aspiration)
carprice$doornumber <- as.factor(carprice$doornumber)
carprice$carbody <- as.factor(carprice$carbody)
carprice$drivewheel <- as.factor(carprice$drivewheel)
carprice$enginelocation <- as.factor(carprice$enginelocation)
carprice$enginetype <- as.factor(carprice$enginetype)
carprice$cylindernumber <- as.factor(carprice$cylindernumber)
carprice$fuelsystem <- as.factor(carprice$fuelsystem)
carprice$Company <- as.factor(carprice$Company)

carprice[c("symboling", "Company")] <- lapply(carprice[c("symboling", "Company")], function(x) factor(x))
str(carprice)

# incorrect spelling of the same company name found
summary(carprice$Company) 

#Cleaning the Incorrect names of the Car Companies:
#This is to avoid creation of extra column when the categorical data is converted to numeric

carprice$Company <- factor(gsub("maxda", "mazda", carprice$Company))
carprice$Company <- factor(gsub("vw", "volkswagen", carprice$Company))
carprice$Company <- factor(gsub("vokswagen", "volkswagen", carprice$Company))
carprice$Company <- factor(gsub("porcshce", "porsche", carprice$Company))
carprice$Company <- factor(gsub("toyouta", "toyota", carprice$Company))

summary(carprice$Company)

#---------------------------------------------------------------------

#Standardising Text Values

column_factor <- c("fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation",
                   "enginetype", "cylindernumber", "fuelsystem" )
carprice[column_factor] <- lapply(carprice[column_factor], function(x) factor(toupper(x)))

View(carprice)

#---------------------------------------------------------------------
#outlier treatment

quantile(carprice$wheelbase,seq(0,1,0.01))
carprice$wheelbase[which(carprice$wheelbase >110)]<- 110

quantile(carprice$carlength,seq(0,1,0.01))
carprice$carlength[which(carprice$carlength < 155)] <- 155.900
carprice$carlength[which(carprice$carlength > 202.480)] <- 202.480

quantile(carprice$carwidth,seq(0,1,0.01))
#Data points are fairly consistent, ignoring it, asuming the jump to be too small 

quantile(carprice$carheight,seq(0,1,0.01))
#No outliers found

quantile(carprice$curbweight,seq(0,1,0.01))
carprice$curbweight[which(carprice$curbweight < 1819.72)] <- 1819.72

quantile(carprice$enginesize,seq(0,1,0.01))
carprice$enginesize[which(carprice$enginesize < 90 )] <- 90.00
carprice$enginesize[which(carprice$enginesize > 209.00)] <- 209.00

quantile(carprice$boreratio,seq(0,1,0.01))
#no outliers

quantile(carprice$stroke,seq(0,1,0.01))
carprice$stroke[which(carprice$stroke < 2.6400 )] <- 2.6400


quantile(carprice$compressionratio,seq(0,1,0.01))
carprice$compressionratio[which(carprice$compressionratio > 10.9400)] <- 10.9400


quantile(carprice$horsepower,seq(0,1,0.01))
carprice$horsepower[which(carprice$horsepower > 207.00)] <- 207.00


quantile(carprice$peakrpm,seq(0,1,0.01))
#no outliers

quantile(carprice$citympg,seq(0,1,0.01))
carprice$citympg[which(carprice$citympg > 38.00)] <- 38.00


quantile(carprice$highwaympg,seq(0,1,0.01))
carprice$highwaympg[which(carprice$highwaympg > 46.92)] <- 46.92

#---------------------------------------------------------------------

# creating dummy variables ----

dummy <- model.matrix(~symboling - 1, data = carprice)
dummy<-dummy[,-1]
carprice_1 <- cbind(carprice[,-2],dummy)


levels(carprice_1$fueltype)<-c(1,0)
carprice_1$fueltype <- as.numeric(carprice_1$fueltype)

levels(carprice_1$aspiration) <- c(1,0)
carprice_1$aspiration <- as.numeric(carprice_1$aspiration)

levels(carprice_1$doornumber) <- c(4,2)
carprice_1$doornumber <- as.numeric(carprice_1$doornumber)

levels(carprice_1$enginelocation) <- c(1,0)
carprice_1$enginelocation <- as.numeric(carprice_1$enginelocation)


dummy <- model.matrix(~carbody,data = carprice_1)
dummy <- dummy[,-1]
carprice_1 <- cbind(carprice_1[,-6],dummy)


dummy <- model.matrix(~drivewheel,data = carprice_1)
dummy <- dummy[,-1]
carprice_1 <- cbind(carprice_1[,-6],dummy)


dummy <- model.matrix(~enginetype,data = carprice_1)
dummy <- dummy[,-1]
carprice_1 <- cbind(carprice_1[,-12])


dummy <- model.matrix(~cylindernumber,data = carprice_1)
dummy <- dummy[,-1]
carprice_1 <- cbind(carprice_1[,-12],dummy)


dummy <- model.matrix(~fuelsystem,data = carprice_1)
dummy <- dummy[,-1]
carprice_1 <- cbind(carprice_1[,-13],dummy)


dummy <- model.matrix(~Company,data = carprice_1)
dummy <- dummy[,-1]
carprice_1 <- cbind(carprice_1[,-2],dummy)

View(carprice_1)

#---------------------------------------------------------------------

#creating new variables and metrices ---- 

#car specifications in terms of width, Length and Height

carprice_1$carWLratio <- carprice_1$carwidth/carprice_1$carlength
carprice_1$carHWratio <- carprice_1$carheight/carprice_1$carwidth
carprice_1$carHLratio <- carprice_1$carheight/carprice_1$carlength


#car performance variables in terms of mpg, power

carprice_1$citytohighwaympg <- carprice_1$citympg/carprice_1$highwaympg
carprice_1$powtoweightratio <- carprice_1$horsepower/carprice_1$curbweight

View(carprice_1)
#=========================================================================================


# MODEL BUILDING 

# Seed the sample dataset
set.seed(100)

# Divide the dataset into 70/30 ratio
trainindices= sample(1:nrow(carprice_1), 0.7*nrow(carprice_1))

train = carprice_1[trainindices,]
test = carprice_1[-trainindices,]

#Deleting unnecessary columns CarID

train <- train[,-1]

View(train)
###################################################################################

## creating a linear model with all variables

model_1 <- lm(price~.,data = train)
summary(model_1)
#Adjusted R-squared:  0.9675
#the R-squared value is very high, but very few variables seem to be significant

#using Step AIC to identify insignificant columns 
step <- stepAIC(model_1, direction="both")

#---------------------------------------------------------------------
#--- backward selection method used ----
#---------------------------------------------------------------------

model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower + peakrpm + carbodyWAGON + 
                drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                cylindernumberSIX + cylindernumberTHREE + fuelsystem2BBL +
                fuelsystemMPFI + Companybmw + Companybuick + Companydodge +
                Companyhonda + Companyisuzu + Companyjaguar + Companymazda +
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot +
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_2)
#Multiple R-squared:  0.9777,	Adjusted R-squared:  0.9698

## Let us check for multicollinearity
sort(vif(model_2))

#dropping fuelsystemMPFI due to high p-value
#VIF 9.014503 P value 0.054768

#---------------------------------------------------------------------


model_3 <-  lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower + peakrpm + carbodyWAGON +
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE + fuelsystem2BBL +
                 Companybmw + Companybuick + Companydodge + 
                 Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + Companypeugeot +
                 Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                 Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                 carHWratio + carHLratio + powtoweightratio, data = train)


summary(model_3)
#Multiple R-squared:  0.9769,	Adjusted R-squared:  0.9691  
sort (vif (model_3))

#dropping fuelsystem2BBL due to high p-value
#VIF 3.380015 p value 0.965131

#---------------------------------------------------------------------


model_4 <-  lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower + peakrpm + carbodyWAGON +
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + Companypeugeot +
                 Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                 Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_4)
#Multiple R-squared:  0.9769,	Adjusted R-squared:  0.9693  
sort (vif (model_4))

#dropping peakrpm due to high p-value
#VIF  5.164844 p value 0.116367

#---------------------------------------------------------------------

model_5 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower +  carbodyWAGON + 
                drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                cylindernumberSIX + cylindernumberTHREE  + 
                Companybmw + Companybuick + Companydodge + 
                Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot +
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_5)
#Multiple R-squared:  0.9764,	Adjusted R-squared:  0.9689 
sort(vif(model_5))

#dropping carbodyWAGON due to high p-value
#p value 0.106880


#---------------------------------------------------------------------

model_6 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower +  
                drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR +
                cylindernumberSIX + cylindernumberTHREE  + 
                Companybmw + Companybuick + Companydodge + 
                Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot +
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_6)
#Multiple R-squared:  0.9758,	Adjusted R-squared:  0.9685 
sort(vif(model_6))

#companyisuzu has least VIF and high p-value. It is being dropped due to its insignificance

#---------------------------------------------------------------------

model_7 <-lm(formula = price ~ aspiration + enginelocation + carlength + 
               enginesize + stroke + horsepower +  
               drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR +
               cylindernumberSIX + cylindernumberTHREE  + 
               Companybmw + Companybuick + Companydodge + 
               Companyhonda +  Companyjaguar + Companymazda + 
               Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
               Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
               Companytoyota + Companyvolkswagen + Companyvolvo + carWLratio + 
               carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_7)
#Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9685 
sort(vif(model_7))

#companyvolvo is dropped due to high p-value
#p-value 0.200294

#---------------------------------------------------------------------

model_8 <- lm(formula = price ~ aspiration + enginelocation + carlength +
                enginesize + stroke + horsepower +  
                drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                cylindernumberSIX + cylindernumberTHREE  + 
                Companybmw + Companybuick + Companydodge + 
                Companyhonda +  Companyjaguar + Companymazda +
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen  + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_8)
#Multiple R-squared:  0.9752,	Adjusted R-squared:  0.9683
sort(vif(model_8))

#droping companyhonda to high p-value 0.207707

#---------------------------------------------------------------------

model_9 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                enginesize + stroke + horsepower +  
                drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR +
                cylindernumberSIX + cylindernumberTHREE  + 
                Companybmw + Companybuick + Companydodge + 
                Companyjaguar + Companymazda + 
                Companymercury + Companymitsubishi + Companynissan + Companypeugeot + 
                Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                Companytoyota + Companyvolkswagen  + carWLratio + 
                carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_9)
#Multiple R-squared:  0.9748,	Adjusted R-squared:  0.9681
sort(vif(model_9))

#dropping companypeugeot due to high p-value - 0.059191

#---------------------------------------------------------------------

model_10 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + Companysubaru + 
                 Companytoyota + Companyvolkswagen  + carWLratio + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_10)
#Multiple R-squared:  0.974,	Adjusted R-squared:  0.9674 
sort(vif(model_10))

#companysabaru can be droped due to its insignificance

#---------------------------------------------------------------------

model_11 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen  + carWLratio +
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_11)
#Multiple R-squared:  0.9651,	Adjusted R-squared:  0.9565  
sort(vif(model_11))


#Adjusted r square value has dropped significantly.
#carwlratio can be dropped

#---------------------------------------------------------------------

model_12 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 enginesize + stroke + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_12)
#Multiple R-squared:  0.9642,	Adjusted R-squared:  0.9558
sort(vif(model_12))

#no significant drop in adjusted r square
#enginesize is dropped due to high p-value

#---------------------------------------------------------------------

model_13 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 stroke + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_13)
#Multiple R-squared:  0.964,	Adjusted R-squared:  0.9559 
sort(vif(model_13))

#adjusted r square increased by 0.001 
#stroke can be removed due to high p-value

#---------------------------------------------------------------------

model_14 <- lm(formula = price ~ aspiration + enginelocation + carlength +
                 horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_14)
#Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9561  
sort(vif(model_14))

#aspiration can be removed due to high p-value

#---------------------------------------------------------------------

model_15 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX + cylindernumberTHREE  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_15)
#Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9564 
sort(vif(model_15))

#dropping cylindernumber3 due to high p-value

#---------------------------------------------------------------------

model_16 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault + Companysaab + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_16)
#Multiple R-squared:  0.9631,	Adjusted R-squared:  0.956 
sort(vif(model_16))

#droping companysaab due to high p-value

#---------------------------------------------------------------------

model_17 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + Companynissan + 
                 Companyplymouth + Companyrenault  + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_17)
#Multiple R-squared:  0.9631,	Adjusted R-squared:  0.9563
sort(vif(model_17))

#removing companynissan due to high p-value

#---------------------------------------------------------------------

model_18 <- lm(formula = price ~ enginelocation + carlength + 
                 horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + Companymercury + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 Companytoyota + Companyvolkswagen + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_18)
#Multiple R-squared:  0.963,	Adjusted R-squared:  0.9565  
sort(vif(model_18))

#droping companyvolkswagen due to high p-value

#---------------------------------------------------------------------

model_19 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi +
                 Companyplymouth + Companyrenault  + Companytoyota  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_19)
#Multiple R-squared:  0.9629,	Adjusted R-squared:  0.9568  
sort(vif(model_19))

#droping company toyota due to high p-value

#---------------------------------------------------------------------

model_20 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + 
                 Companybmw + Companybuick + Companydodge + 
                 Companyjaguar + Companymazda + 
                 Companymercury + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_20)
#Multiple R-squared:  0.9617,	Adjusted R-squared:  0.9558 
sort(vif(model_20))

#removing companymazda due to high p-value

#---------------------------------------------------------------------

model_21 <- lm(formula = price ~ enginelocation + carlength + horsepower + 
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  +  Companymercury + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_21)
#Multiple R-squared:  0.9608,	Adjusted R-squared:  0.9551 
sort(vif(model_21))

#removing companymercury due to high p-value

#---------------------------------------------------------------------

model_22 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  + Companymitsubishi + 
                 Companyplymouth + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_22)
#Multiple R-squared:  0.9602,	Adjusted R-squared:  0.9548 
sort(vif(model_22))

#removing companyplymouth due to high p-value

#---------------------------------------------------------------------

model_23 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  +  Companymitsubishi + Companyrenault  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_23)
#Multiple R-squared:  0.9597,	Adjusted R-squared:  0.9546 
sort(vif(model_23))

#removing companyrenault due to high p-value

#---------------------------------------------------------------------

model_24 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick + Companydodge + 
                 Companyjaguar  +  Companymitsubishi  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_24)
#Multiple R-squared:  0.9585,	Adjusted R-squared:  0.9536 
sort(vif(model_24))

#removing companydodge due to high p-value

#---------------------------------------------------------------------

model_25 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick +  
                 Companyjaguar  +  Companymitsubishi  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_25)
#Multiple R-squared:  0.9579,	Adjusted R-squared:  0.9533 
sort(vif(model_25))

#removing companymitsubishi due to high p-value among all other variables

#---------------------------------------------------------------------

model_26 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 drivewheelRWD + cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick +  Companyjaguar  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_26)
#Multiple R-squared:  0.9564,	Adjusted R-squared:  0.952 
sort(vif(model_26))

#removing drivewheelRWD due to high p-value

#---------------------------------------------------------------------

model_27 <- lm(formula = price ~ enginelocation + carlength + horsepower +  
                 cylindernumberFIVE + cylindernumberFOUR + 
                 cylindernumberSIX  + Companybmw + Companybuick +  Companyjaguar  + 
                 carHWratio + carHLratio + powtoweightratio, data = train)

summary(model_27)
#Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9511 

sort(vif(model_27))
# All variables are now highly significant, VIF values are also low and has p < 0.05.

#---------------------------------------------------------------------

#### Model evaluation ####


#predicting price using model_27

predict_price <- predict(model_27,test[,-18])
test$test_price <- predict_price
r <- cor(test$test_price,test$price)
rsquared <- r^2

summary(model_27)

#r squared from test dataset is 0.877 and initial model/traning data is 0.9511. 
#The difference is approx 7.79%, which is reasonably an accurate model

#The beta values/estimate intercept is -71548.19
#The final adjusted r square and the r square are similar.
#R-squared:  0.9552,	Adjusted R-squared:  0.9511

#---------------------------------------------------------------------

#Below are the variables that are part of the final model

#1.enginelocation
#2.carHWratio
#3.horsepower
#4.Companybmw
#5.Companybuick
#6.Companyjaguar
#7.powtoweightratio
#8.cylindernumberFIVE
#9.cylindernumberFOUR
#10.cylindernumberSIX 
#11.carlength
#12.carHLratio


############################################## Results/Outcomes ###########################################


# Model_27 predicts car price with sufficent accuracy, contains only highly significant and has little
# to no multicollinearity


# Key variables used for car price prediction

# 1. Engine location - price varies with location of engine - rear/front
# 2. Luxury car brand - bmw, buick and jaguar have significantly higher prices than other cars
# 3. Number of cylinders, car featues like length,height and weight, and engine power are key engine parameters controlling price

# For the negatively dependent variables, increase in value would decrease price for cars



