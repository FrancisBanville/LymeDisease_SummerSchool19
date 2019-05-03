
#######################################
#### Lyme Disease mixed model SS19 ####
#######################################

setwd("C:/Users/maxim/OneDrive/Bureau/BIO6065-Ecole_ete/LymeDisease_SummerSchool19")
library(data.table)
library(tidyr)


# 1. Import Data and arrange
#----------------------------

# Incidence rate table
incidence_rate <- fread("Data_infection.csv", header = T)
incidence_rate <- gather(incidence_rate, Year, Inf.rate, c(2:12))

# Count by county table
CountbyCounty <- fread("count_by_county.csv", header = T)
CountbyCounty <- setnames(CountbyCounty, c(5:22), 
                          c("2000","2001","2002","2003","2004","2005","2006","2007","2008",
                            "2009","2010","2011","2012","2013","2014","2015","2016","2017"))
CountbyCounty <- CountbyCounty[,-c(3:4)]
CountbyCounty <- gather(CountbyCounty, Year, Count, c(3:20))
str(CountbyCounty)

# Mice table
WT_deer <- fread("GBIF-cerf.csv", header = T)

# Cervids table
WF_mice <- fread("GBIF-souris.csv", header = T)
