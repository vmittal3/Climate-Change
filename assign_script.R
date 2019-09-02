#################################################################
# Author - Vaibhav Mittal
# Submission for Assignment 1 - Data Science (DSc) - Monsoon 2019
# IIITD

#################################################################
# libraries required
library(ggplot2)
library(ggthemes)
library(corrplot)
library(tidyverse)
library(data.table)
library(reshape2)

#################################################################
# read, preprocess, and plot the global CO2 data file from NASA
co2 = read.csv('co2_gr_mlo.txt', sep = " ")
co2 = subset(co2, select = c(X.4))
plot(row.names(co2), co2$X.4, type = 'l', xlab = 'Year',
     ylab = expression('Annual Increase (in CO '[2]*' metric tonnes)'), 
     main = 'Gradual Increase in Atmospheric Carbon Dioxide')
grid(NULL, NULL)

#################################################################
# read and preprocess Kaggle's Indian Climate Dynamics data file
temp = read.csv('GlobalLandTemperaturesByCity.csv')
india = temp[temp$Country=='India',]
major = india[india$City %in% c('Delhi', 'Calcutta','Bangalore','Bombay','Madras','Hyderabad','Chandigarh'),]
major$dt = gsub("-.*","", major$dt)


#################################################################
# read, preprocess, plot Five-Year Plan data file
# from https://doi.org/10.1080/21681376.2014.943804
budget = read.csv("per capita resource allocation.csv")

# compare Bihar's data. 
initial = data.frame(cbind(budget$States.fiscal.year, budget$Bihar, budget$All.India))
initial = data.frame(initial[,-1])
initial = data.frame(initial[-11,])
x = rep(1:10)
y1 = budget$Bihar[1:10]
y2 = budget$All.India[1:10]
initial = data.frame(x, y1, y2)
initial = melt(data = initial, id.vars = 'x')
ggplot(data = initial, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Number of Five-Year Plan") + ylab("Per Capita Resource Allocation (in Rupees)") +
  ggtitle("Central Govt's Funding to Bihar as per the Five-Year Plans") +
  scale_color_manual(labels = c("Bihar", "All India"), values = c("blue", "red"))

# go for an initial analysis
x = rep(1:4)
y1 = budget$Bihar[1:4]
y2 = budget$All.India[1:4]
initial = data.frame(x, y1, y2)
initial = melt(data = initial, id.vars = 'x')
ggplot(data = initial, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Number of Five-Year Plan") + ylab("Per Capita Resource Allocation (in Rupees)") +
  ggtitle("Central Govt's Funding to Bihar as per the Five-Year Plans") +
  scale_color_manual(labels = c("Bihar", "All India"), values = c("blue", "red"))

#################################################################
# read, preprocess, and plot forest coverage file from data.gov.in
tree = read.csv("district_forest_cover.csv")
main_tree = tree[tree$District == 'Total',]
main_tree$Percentage.to.Geographical.Area = as.numeric(as.character(main_tree$Percentage.to.Geographical.Area))
main_tree$Percentage.to.Geographical.Area[4] = 7.10 # the value given for Bihar is 723%. This has been recalculated.
main_tree = main_tree[order(main_tree$Percentage.to.Geographical.Area),]
low_tree = head(main_tree); high_tree = tail(main_tree)

ggplot(low_tree) + geom_col(aes(low_tree$State.UTs, y = low_tree$Percentage.to.Geographical.Area)) + 
  geom_text(aes(x = low_tree$State.UTs, y = low_tree$Percentage.to.Geographical.Area + 0.4, label = round(low_tree$Percentage.to.Geographical.Area, 2))) +
  xlab("States/UTs") + ylab("Forest Coverage in Percentage to Geographical Area") +
  theme_economist() + ggtitle("States with Lowest Forest Coverage")

ggplot(high_tree) + geom_col(aes(high_tree$State.UTs, y = high_tree$Percentage.to.Geographical.Area)) + 
  geom_text(aes(x = high_tree$State.UTs, y = high_tree$Percentage.to.Geographical.Area + 3, label = round(high_tree$Percentage.to.Geographical.Area, 2))) +
  xlab("States/UTs") + ylab("Forest Coverage in Percentage to Geographical Area") +
  theme_economist() + ggtitle("States with Highest Forest Coverage")


#################################################################
# read, preprocess, and plot the socio-economic factor file from data.gov.in
socio = read.csv("ranksallstatesr1_1.csv")
socio = subset(socio, select = -c(Sr..No, Source, Periodicity..Latest.available.data))
colnames(socio)[2] = "Andhra Pradesh"
colnames(socio)[3] = "Arunachal Pradesh"
colnames(socio)[11] = "Himachal Pradesh"
colnames(socio)[12] = "Jammu and Kashmir"
colnames(socio)[16] = "Madhya Pradesh"
colnames(socio)[26] = "Tamil Nadu"
colnames(socio)[28] = "Uttar Pradesh"
colnames(socio)[30] = "West Bengal"
row.names(socio) = socio[,1]
socio = socio[,-1]
socio = data.frame(t(socio))
socio2= data.matrix(socio)
socio2[is.na(socio2)] = 0
corrplot(cor(socio2), tl.cex = 0.5, type = 'upper')
# forest cover percentage (last row) is highly negatively correlated with literacy rate,
# sex ratio, mortality rate and access to clean drinking water!
# again BiMaRU states rank the lowest in these measures.
# positive correlations seen with school enrolment as well as dropouts!


#################################################################
# read, preprocess, and plot Indian CO2 data file from World Bank
co2_med = read.csv('API_EN.ATM.CO2E.PC_DS2_en_csv_v2_103958.csv', skip = 4)
india = co2_med[108,]
india = data.frame(t(subset(india, select = -c(Country.Code, Indicator.Name, Indicator.Code, Country.Name))))
india = data.frame(india[1:55,])
row.names(india) = rep(1961:2015)
plot(y = india$india.1.55..., x = row.names(india), type = 'l', xlab = 'Years', 
     ylab = expression('Atmospheric CO'[2]* ' level in ppm'),
     main = expression('The Rising CO'[2]* ' levels in India'))
grid(NULL,NULL)

#################################################################
# read, preprocess, and plot temperature anomaly file from NASA
gtemp = read.csv('GLB.Ts+dSST.csv', skip = 1)
colnames(gtemp)[14] = "Jan-Dec"
colnames(gtemp)[15] = "Dec-Nov"
colnames(gtemp)[16] = "Dec-Feb"
colnames(gtemp)[17] = "Mar-May"
colnames(gtemp)[18] = "Jan-Jun"
colnames(gtemp)[19] = "Sep-Nov"
pairs(gtemp[,14:19], col = 'red')