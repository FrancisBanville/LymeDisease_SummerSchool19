

#######################################
#### Lyme Disease mixed model SS19 ####
#######################################

setwd("C:/Users/maxim/OneDrive/Bureau/BIO6065-Ecole_ete/LymeDisease_SummerSchool19")
library(data.table)
library(revgeo)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(scales)
library(stats)
library(stringr)
library(lubridate)
library("lme4")
library("nlme")
library("Matrix")
library("vegan")

###----------------------------###
### 1. Import Data and arrange ###
###----------------------------###

# 1.1 infection rate table
infection_rate <- fread("Data_infection.csv", header = T) # look back at data source on internet****
infection_rate[,c("2007","2008","2009"):=NULL]
infection_rate <- data.table(gather(infection_rate, Year, infection_rate, c(2:9)))
#------------------------------------

## 1.2 Deer table (source)
WT_deer <- fread("CERF-BON.csv", header = T)
setnames(WT_deer, "year", "Year")
setnames(WT_deer, "STATE", "State")
setnames(WT_deer, "COUNTY", "County")
WT_deer <- WT_deer[Year>2009 & Year<2018]

WT_deer = WT_deer %>%
  group_by(Year, State) %>%
  unique() %>%
  select(-3) %>%
  count()
WT_deer <- data.table(WT_deer)
#-----------------------------------

## 1.3 Mice table (source)
WF_mice <- fread("SOURIS-BON.csv", header = T)
WF_mice <- WF_mice[Year>2009 & Year<2018]

WF_mice = WF_mice %>%
  group_by(Year, State) %>%
  unique() %>%
  select(-3) %>%
  count()
WF_mice <- data.table(WF_mice)
#-----------------------------------

## 1.4 Temperature and precipitation tables (source)
precipitations <- fread(file.choose(), header = T, sep = ",") # precip.txt
precipitations <- precipitations[Year>2009 & Year<2018]
precipitations[,V1:=NULL]
# precipitations[,precip_Z:=(precip_mm-mean(precip_mm))/sd(precip_mm), by = State]
# precipitations[,precip_mm:=NULL]

temperature <- fread(file.choose(), header = T, sep = ",") # avg_temp.txt
temperature <- temperature[Year>2009 & Year<2018]
temperature[,V1:=NULL]
# temperature[,avgTemp_Z:=(avg_temp_C-mean(avg_temp_C))/sd(avg_temp_C), by = State]
# temperature[,avg_temp_C:=NULL]
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------


###-----------------###
### 2. Merge tables ###
###-----------------###

county_total <- fread(file.choose("C:/Users/maxim/OneDrive/Bureau/BIO6065-Ecole_ete/LymeDisease_SummerSchool19/DataSets/county_total.csv"), header = T, sep = ";")
setnames(county_total, "state", "State")

# 2.1 Merge Mice and Deer tables with county_total, and divide freq variable by total_county variable
WT_deer1 <- merge(WT_deer, county_total, by = "State")
WF_mice1 <- merge(WF_mice, county_total, by = "State")

WT_deer1[,Deerprop_county:=(freq/total_county)]
WF_mice1[,Miceprop_county:=(freq/total_county)]

WT_deer1[,c("freq","total_county"):=NULL]
WF_mice1[,c("freq","total_county"):=NULL]

# WT_deer1[,Deerprop_countyZ:=(Deerprop_county-mean(Deerprop_county))/sd(Deerprop_county), by = State]
# WF_mice1[,Miceprop_countyZ:=(Miceprop_county-mean(Miceprop_county))/sd(Miceprop_county), by = State]
#-------------------------------------------

# 2.2 Arrange all variables as year = numeric, State = factor, all else = numeric
WT_deer1[,State:= as.factor(State)]
WF_mice1[,State:= as.factor(State)]
infection_rate[,":="(State = as.factor(State), Year = as.integer(Year), infection_rate = as.numeric(infection_rate))]
precipitations[,State:= as.factor(State)]
temperature[,State:= as.factor(State)]

# 2.3 merge for final table
infection_data <- merge(infection_rate, precipitations, by=c("Year", "State"), all = T)
infection_data <- merge(infection_data, temperature, by=c("Year", "State"), all = T)
infection_data <- merge(infection_data, WT_deer1, by=c("Year", "State"), all = T)
infection_data <- merge(infection_data, WF_mice1, by=c("Year", "State"), all = T)

# take out Alaska, Hawaii, and District of columbia
infection_data <- infection_data[State!="Alaska"]
infection_data <- infection_data[State!="Hawaii"]
infection_data <- infection_data[State!="District of Columbia"]

# Replace NAs in proportion variables with 0 values
infection_data$Deerprop_county <- replace_na(infection_data$Deerprop_county, 0)
infection_data$Miceprop_county <- replace_na(infection_data$Miceprop_county, 0)
# infection_data$Deerprop_countyZ <- replace_na(infection_data$Deerprop_countyZ, 0)
# infection_data$Miceprop_countyZ <- replace_na(infection_data$Miceprop_countyZ, 0)

# standardize infection rate by State
# infection_data[,infection_rateZ:=(infection_rate-mean(infection_rate))/sd(infection_rate)]
# check again if there are NA values
# anyNA(infection_data$infection_rateZ) # good to go
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------


###---------------------------------------###
### 3. Explore data + distribution graphs ###
###---------------------------------------###

anyNA(infection_data) # No NA values
hist(infection_data$infection_rate)


###---------------------###
### 4. Build the models ###
###---------------------###

plot(infection_data)

# linear model of prevalence againts precipitation
lm_precipitation = data.frame()
colnames(lm_precipitation) = c("annee", "coeff", "lower", "upper")
for (i in 2010:2017) {
lm_model <- lm(infection_rate ~ precip_mm, data = infection_data[Year==i])
lm_precipitation[i-2009, "annee"] = i
lm_precipitation[i-2009, "coeff"] = summary(lm_model)$coefficients[2,1]
lm_precipitation[i-2009, "lower"] = confint(lm_model)[2,1]
lm_precipitation[i-2009, "upper"] = confint(lm_model)[2,2]
}

ggplot(lm_precipitation, aes(x = annee, y = coeff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  labs(x = "Année", y = "Regression coefficient") +
  geom_smooth(col = "red")


# linear model of prevalence againts mean temperature
lm_temperature = data.frame()
colnames(lm_temperature) = c("annee", "coeff", "lower", "upper")
for (i in 2010:2017) {
  lm_model <- lm(infection_rate ~ avg_temp_C, data = infection_data[Year==i])
  lm_temperature[i-2009, "annee"] = i
  lm_temperature[i-2009, "coeff"] = summary(lm_model)$coefficients[2,1]
  lm_temperature[i-2009, "lower"] = confint(lm_model)[2,1]
  lm_temperature[i-2009, "upper"] = confint(lm_model)[2,2]
}

ggplot(lm_temperature, aes(x = annee, y = coeff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  labs(x = "Année", y = "Regression coefficient") +
  geom_smooth(col = "red")




# linear model of prevalence againts deer proportion
lm_deer = data.frame()
colnames(lm_deer) = c("annee", "coeff", "lower", "upper")
for (i in 2010:2017) {
  lm_model <- lm(infection_rate ~ Deerprop_county, data = infection_data[Year==i])
  lm_deer[i-2009, "annee"] = i
  lm_deer[i-2009, "coeff"] = summary(lm_model)$coefficients[2,1]
  lm_deer[i-2009, "lower"] = confint(lm_model)[2,1]
  lm_deer[i-2009, "upper"] = confint(lm_model)[2,2]
}

ggplot(lm_deer, aes(x = annee, y = coeff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  labs(x = "Année", y = "Regression coefficient") +
  geom_smooth(col = "red")


# linear model of prevalence againts mouse proportion
lm_mouse = data.frame()
colnames(lm_mouse) = c("annee", "coeff", "lower", "upper")
for (i in 2010:2017) {
  lm_model <- lm(infection_rate ~ Miceprop_county, data = infection_data[Year==i])
  lm_mouse[i-2009, "annee"] = i
  lm_mouse[i-2009, "coeff"] = summary(lm_model)$coefficients[2,1]
  lm_mouse[i-2009, "lower"] = confint(lm_model)[2,1]
  lm_mouse[i-2009, "upper"] = confint(lm_model)[2,2]
}

ggplot(lm_mouse, aes(x = annee, y = coeff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  labs(x = "Année", y = "Regression coefficient") +
  geom_smooth(col = "red")





#### Multiple regression

m0 = lm(infection_rate ~ 1, data = infection_data)
mtot = lm(infection_rate ~ precip_mm + avg_temp_C + Deerprop_county + Miceprop_county, data = infection_data)

res = step(m0, scope = formula(mtot), direction = "both")
res
anova(mtot)


































ggplot(CountbyCounty, aes(x = LogCount)) +
  geom_histogram(bins = 20, color = "black") +
  labs(x = "\nLog count", y = "Frequency\n") +
  scale_x_continuous(minor_breaks = NULL, limits = c(0,4), breaks = seq(0,4,0.5), expand = c(0,0)) +
  scale_y_continuous(minor_breaks = NULL, limits = c(0,5500), breaks = seq(0,5500,500), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        title = element_text(size = 30),
        axis.line = element_line(), 
        panel.grid.major = element_blank(), # no grids in panel
        panel.grid.minor = element_blank(), # no grids in panel
        panel.background = element_rect(fill = "white", color  =  NA),  # panel background
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,1), "cm"))
