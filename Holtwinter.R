####################################################################################
#This is the second R file on forecasting for students in the IDS 552 Supply Chain Management
#elective taught by Prof. Selva Nadarajah
#
#The purpose of this file is to show that much of the code in the previous forecasting R file 
#can be simplified. Of course, the reason for going into details in the previous file is to 
#make sure you know what you are using. Now that you do, we can make life easier.  

#The code illustrates a comparison between the following models: holt winters additive, 
#holt winters multiplicative, and ARIMA. 

#Reminder: To learn about the inputs and outputs of a function, you can type ? followed by the 
#name of the function. For example, the holt winters function we will use below is called hw. To learn about this
#function type ?hw and press enter in the console. The details of this function will appear in the help panel in the 
#right hand side.

#After going through the template, think about how cumbersome it would be to do this comparison 
#in excel as suggested in your textbook.

Tractor_Sales <- read.csv("D:/MSBA/Spring2017/IDS 552/Assignments/2/Tractor-Sales.csv")
#View(Tractor_Sales)
####################################################################################

library("forecast") #load forecast library

###################Load data#####################
#Load the data using the Import DataSet dropdown menu in the environment tab (top right)
demandData <- Tractor_Sales

###########Extract time series and plot#########
dataFreq= 12 #Data frequency of time series. Set to 12 and 4 for monthly and quaterly data, respectively
startEntry= c(2003,01) #Time stamp of first entry in time series e.g. c(2012,2) implies second quarter of 2012 if data frequency equals 4

demandTS <- ts(demandData$Demand, frequency=dataFreq, 
               start=startEntry) #create a time series

###########Prepare time series for forecasting#########
###We partition the time series into a training set for 
###forecasting and a test set to evaluate accuracy
trainSetStart= c(2003,01) #training set start location in time series (typically the first entry)
trainSetEnd= c(2012,12) #training set end location in time series (typically covers 70% of time series)
testSetStart= c(2013,01) #test set start location in time series (typically location of entry after training set ends)
testSetEnd= c(2014,12) #test set end location in time series (typically end of time series)

demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd) #extract training set
demandTest <- window(demandTS,start=testSetStart,end=testSetEnd) #extract test set

#View(demandTrain)
#View(demandTest)

###########Forecast#########
numForcPeriods = 24 #number of periods to forecast in the future (Note: If you are computing errors with respect to testing data then this value 
#should be equal to the duration of the testing data)

fit_HWA <- hw(demandTrain,seasonal="additive",h=numForcPeriods) #Train Holt-Winters multiplicative forecasting object; note that we are simultaneously building the model and foreacsting
fit_HWM <- hw(demandTrain,seasonal="multiplicative",h=numForcPeriods) #Train Holt-Winters multiplicative forecasting object

#AutoARIMA <- auto.arima(demandTrain) #Unlike the holt winters function hw, auto.arima finds the best ARIMA model but does not forecast
#fit_AutoARIMA <- forecast(AutoARIMA,h=numForcPeriods) #We are using the best ARIMA model in the previous step to forecast here

plot(demandTS,main = "Tractor Sales",
     xlab="Year.Month",ylab="Tractor demand") #plot time series.

accuracy(fit_HWA,demandTest) #Test accuracy of Holt winter's additive model.. The second ardument is the testing data set
accuracy(fit_HWM,demandTest) #Test accuracy of Holt winter's multiplicative model...
#accuracy(fit_AutoARIMA,demandTest) #Test accuracy of ARIMA model...

#plotting the holtwinters additive forecast and prediction intervals. To plot a different foreast
#change fit_HWA 
plot(fit_HWA, main="Additive Model-Plot of training demand, 
     testing demand, and forecast with 80% and 95% 
     prediction intervals",xlab="Year.Month", 
     ylab="Demand")
lines(demandTest,col=2) #add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), 
       legend=c("Training Demand","Forecast","Testing Demand")) #create plot legend

#plotting the holtwinters multiplicative forecast and prediction intervals.
plot(fit_HWA, main="Multiplicative Model-Plot of training demand, 
     testing demand, and forecast with 80% and 95% 
     prediction intervals",xlab="Year.Month", 
     ylab="Demand")
lines(demandTest,col=2) #add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), 
       legend=c("Training Demand","Forecast","Testing Demand")) #create plot legend

