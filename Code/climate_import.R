# Library
library(dplyr)
library(tidyverse)

extract_data = function(path) {
  filelist = list.files(path = path, pattern = ".*.txt", full.names = TRUE)
  datalist = lapply(filelist, function(x) read.table(x, header=T, skip = 4, sep = ",")) 
  namelist = lapply(filelist, function(x) read.table(x, nrow = 1, sep = ",")) 
  
  for (i in 1:48) {
    datalist[[i]][,3] = namelist[[i]][1]
  }
  
  alldata = reduce(datalist, union) 
  return(alldata)
}

avg_temp = extract_data("C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/avg_temp")
min_temp = extract_data("C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/min_temp")
precip = extract_data("C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/precip")
deg_days = extract_data("C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/deg_days")

tidying_general = function(dataframe, unit) {
  dataframe = dataframe[,c(3,1,2)] %>%
    separate(col = Date, into = c("Year", "Month"), sep = 4) 
  colnames(dataframe) = c("State", "Year", "Month", unit) 
  dataframe[,1] = as.factor(dataframe[,1])
  dataframe[,2] = as.numeric(dataframe[,2])
  dataframe[,3] = as.numeric(dataframe[,3])
  dataframe[,4] = as.numeric(dataframe[,4])
  dataframe = dataframe %>% arrange(State, Year, Month)
  return(dataframe)
}

avg_temp_tidyg = tidying_general(avg_temp, "avg_temp_C")
min_temp_tidyg = tidying_general(min_temp, "min_temp_C")
precip_tidyg = tidying_general(precip, "precip_mm")
deg_days_tidyg = tidying_general(deg_days, "deg_days_C_days")

avg_temp_tidy = avg_temp_tidyg %>%
  select(-3) %>%
  mutate(avg_temp_C = round(((avg_temp_C - 32)*5/9),3))

min_temp_tidy = min_temp_tidyg %>%
  mutate(min_temp_C = round(((min_temp_C - 32)*5/9),3))

precip_tidy = precip_tidyg %>%
  select(-3) %>%
  mutate(precip_mm = round(precip_mm * 25.4,2))

deg_days_tidy = deg_days_tidyg %>%
  select(-3) %>%
  mutate(deg_days_C_days = round(((deg_days_C_days - 32)*5/9),1))


write.csv(avg_temp_tidy, file = "C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/avg_temp.txt")
write.csv(min_temp_tidy, file = "C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/min_temp.txt")
write.csv(precip_tidy, file = "C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/precip.txt")
write.csv(deg_days_tidy, file = "C:/Users/Francis Banville/Documents/github/LymeDisease_SummerSchool19/DataSets/deg_days.txt")



