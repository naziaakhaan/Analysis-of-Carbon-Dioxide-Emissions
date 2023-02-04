########################################    STATISTICAL ANALYSIS     ###############################################

############################# Descriptive Statistical Analysis #############################                                  ##########################

install.packages("tidyverse")
install.packages("dyplr")
install.packages("psych")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("car")
install.packages("RVAideMemoire")

library(car)
library(caret)
library(ggplot2)
library(tidyverse)
library(dyplr)
library(psych)
library(corrplot)
library( RVAideMemoire)

####################### Read the data set ##################################################

co2_data <- read.csv("CO2EMISSIONANALYSIS.csv",header=TRUE)


#################### DATA Exploration  ##############################################
###################                    #############################################

head(co2_data)
tail(co2_data)
summary(co2_data)
str(co2_data)
names(co2_data)
dim(co2_data)
describe(co2_data)



#################################To check NA values#############################################################

sum(is.na(co2_data))


################################# Descriptive statistical Analysis  ##############################################

############### Extract numerical variables from dataset #####################################################
new_co2_data<-co2_data[,c(3,4,5,6,7,8,9)]
names(new_co2_data)
sum(is.na(new_co2_data))
summary(new_co2_data)
describe(new_co2_data)
dim(new_co2_data)
boxplot(new_co2_data)


########################## data visualization with histogram #####################################3
hist(new_co2_data$Solid.fuel)
hist(new_co2_data$Transport)
hist(new_co2_data$Liquid.fuel)
hist(new_co2_data$Gaseous.fuel)
hist(new_co2_data$Electricity)
hist(new_co2_data$Industries)
hist(new_co2_data$Total.co2.emission)

############################# Data Transformation   ##############################################################


transform_co2_data<-log(new_co2_data)
transform_co2_data
boxplot(transform_co2_data)
describe(transform_co2_data)
################## data visualization after log transformation ########################################33

hist(transform_co2_data$Solid.fuel)
hist(transform_co2_data$Transport)
hist(transform_co2_data$Liquid.fuel)
hist(transform_co2_data$Gaseous.fuel)
hist(transform_co2_data$Electricity)

hist(transform_co2_data$Industries)
hist(transform_co2_data$Total.co2.emission)

################################# CLEAN DATA AFTER REMOVING OUTLIERS     ##################################################

outliers <- boxplot(transform_co2_data$Total.co2.emission, plot=FALSE)$out
outliers
clean_data<-transform_co2_data
dim(clean_data)
clean_data<- clean_data[-which(clean_data$Total.co2.emission %in% outliers),]
clean_data
boxplot(clean_data$Total.co2.emission)
boxplot(clean_data)

describe(clean_data)


################ Removing Remaining Outliers ###################################

Q <- quantile(clean_data$Total.co2.emisson,probs=c(.25, .75),na.rm = FALSE)
iqr <- IQR(clean_data$Total.co2.emission)
summary(clean_data$Total.co2.emission)
up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(clean_data,clean_data$Total.co2.emission > low & clean_data$Total.co2.emission < up)
dim(clean_data)
summary(clean_data)
describe(clean_data)
clean_data$Total.co2.emission=ifelse(clean_data$Total.co2.emission>15.03,15.03 ,clean_data$Total.co2.emission)
boxplot(clean_data$Total.co2.emission)
boxplot(clean_data)

######################### data visualization after cleaning ####################################################################

hist(clean_data$Solid.fuel)
hist(clean_data$Transport)
hist(clean_data$Liquid.fuel)
hist(clean_data$Gaseous.fuel)
hist(clean_data$Electricity)
hist(clean_data$Industries)
hist(clean_data$Total.co2.emission)


######################################################################################




############################  CORRELATION ANALYSIS #############################################################


head(co2_data)
new_co2_data<-co2_data[,c(3,4,5,6,7,8,9)]
new_co2_data
round(cor(new_co2_data), digits = 2)

corrplot(cor(new_co2_data), method = "number")
corrplot(cor(new_co2_data), method = "pie")


#########################        Anova  Test        ######################



data_anova<-co2_data[,c(9:10)]
data_anova


boxplot(Total.co2.emission ~ Continent , data=data_anova, Continent=c("Asia", "Europe"),
        xlab="continents", ylab="total co2 Emission",
        main="Total co2 emission from Asia and Europe")


byf.shapiro(Total.co2.emission ~ Continent  , data=data_anova)


bartlett.test(Total.co2.emission ~ Continent , data=data_anova)


############################     REGRESSION ANALYSIS      ####################################

#check the  data set  

str(new_co2_data)
head(new_co2_data)
tail(new_co2_data)


###############    checking correlation between numerical variables ###############


cor(new_co2_data)

corrplot(cor(new_co2_data))


###################      Forward Step wise regression analysis Y=total co2 emission and X =Solid fuel emission

model_1 <-lm(Total.co2.emission ~ Solid.fuel, new_co2_data)
summary.lm(model_1)


########################     Adding industries emission variable to the  previous model     ############################


model_2 <-lm(Total.co2.emission ~ Solid.fuel + Industries, new_co2_data)
summary.lm(model_2)

############# ######       Adding electricity variable  to the model    ###################################   


model_3 <-lm(Total.co2.emission ~ Solid.fuel + Industries+Electricity , new_co2_data)
summary.lm(model_3)


#########################     Adding liquid variable to the model      #######################


model_4 <-lm(Total.co2.emission ~ Solid.fuel + Industries+Electricity +Liquid.fuel, new_co2_data)
summary.lm(model_4)

#####################      Checking the Assumptions  of model2     ########################

####################       linearity check        ######################################3

pairs(new_co2_data[,c(7,1,4)], lower.panel = NULL, pch = 19,cex = 0.2)

#############       Residual independence  ##############################


plot(model_2, 1)


#############  Normality of residuals      #########################

plot(model_2, 2)

###############     Equal variances of the residuals (Homoscedasticity)  ###################


plot(model_2, 3)

################   multi collinearity check #########################################

vif(model_2)



#######################           Time Series analysis          ######################

#Convert to time series format

install.packages("TTR")
library(TTR)
install.packages("forecast")
library(forecast)


tseries<-ts(new_co2_data$Total.co2.emission,start=2001,frequency=10)
tseries
plot.ts(tseries)
attributes(tseries)
############### smoothing time series  ######################
tseriesSMA3 <- SMA(tseries,n=3)
plot.ts(tseriesSMA3)
tseriesSMA10 <- SMA(tseries,n=10)
plot.ts(tseriesSMA10)

################# forecast using smoothing time series ############################
co2seriesforecasts <- HoltWinters(tseries, beta=FALSE, gamma=FALSE)
co2seriesforecasts$fitted
plot(co2seriesforecasts)
co2seriesforecasts$SSE

HoltWinters(tseries, beta=FALSE, gamma=FALSE, l.start=4735107)
co2seriesforecast2 <- forecast(co2seriesforecasts, h=100,)
co2seriesforecast2
plot(co2seriesforecast2)

acf(co2seriesforecast2$residuals, lag.max=10, na.action = na.pass)
Box.test(co2seriesforecast2$residuals, lag=10, type="Ljung-Box")
plot.ts(co2seriesforecast2$residuals)

####################### function for  plotforecasterrors ##############################3

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

co2seriesforecast2$residuals <-co2seriesforecast2$residuals[!is.na(co2seriesforecast2$residuals)]
plotForecastErrors(co2seriesforecast2$residuals)



##################### ARIMA MODEL ################

plot(tseries)
model<- auto.arima(tseries)
model
attributes(model)
model$coef

########## acf and pacf plots #############

acf(model$residuals,main='correlogram')

pacf(model$residuals,main='partial correlogram')


################## ljung box test ###########

Box.test(model$residuals,lag=10,type="Ljung-Box")

#### RESIDUAL PLOTS#############

hist(model$residuals,
     col='red',
     xlab='Error',
     main='histogram of residuals',
     freq=FALSE)
lines(density(model$residuals))

# forecast#############


f<-forecast(model,48)
autoplot(f)
accuracy(f)


##############################       HYPOTHESIS TESTING     #########################################


################################### ASSESSING THE NORMALITY OF THE DATA   ####################################


########################   QQ PLOT VISUAL METHOD FOR NORMALITY OF DATA     #######################################

################     Visually confirm if the plot appears to be on a normal curve   ###############


ggplot(mapping = aes(sample=transform_co2_data$Total.co2.emission))+
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

########## Shapiro test to check the normality  of the data ##############################


shapiro.test(transform_co2_data$Total.co2.emission)


############### one sample T-test ##########################

hist(transform_co2_data$Total.co2.emission)


mean(transform_co2_data$Total.co2.emission)
sd(transform_co2_data$Total.co2.emission)



t.test(transform_co2_data$Total.co2.emission, mu=15, alternative="less")



############## Two sample T test  or  paired sample T test #####################

data_Asia<-co2_data[co2_data$Continent=="Asia",]
data_Asia
names(data_Asia)
data_Asia1<-data_Asia[,c(3,4,5,6,7,8,9)]
names(data_Asia1)
describe(data_Asia1)
summary(data_Asia1)
boxplot(data_Asia1$Total.co2.emission)

mean(data_Asia1$Total.co2.emission)


data_Europe<-co2_data[co2_data$Continent=="Europe",]
data_Europe
names(data_Europe)
data_Europe1<-data_Europe[,c(3,4,5,6,7,8,9)]
data_Europe1
names(data_Europe1)
describe(data_Europe1)

summary(data_Europe1)
boxplot(data_Europe1$Total.co2.emission)
mean(data_Europe1$Total.co2.emission)

mean(data_Europe1$Total.co2.emission)

boxplot(data_Asia1$Total.co2.emission, data_Europe1$Total.co2.emission, names=c("Europe", "Asia"),
        xlab="European and Asian", ylab="Weight",
        main="Total CO2 emission by European and Asian cOuntries")

t.test(data_Asia1$Total.co2.emission, data_Europe1$Total.co2.emission, paired=TRUE)

###############################################################################################################












  
         
      



