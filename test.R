
source("getDataFunctions.R")
# test
# voor nutrienten:
parID <- c(26,2829,1996,528,2370,529,530,531,1843,1019,828,834,866,1010,3217,833,1021,1022,1972)
startyear = 2018
endyear = 2019
df <- getSMdata(startyear = startyear, endyear = endyear, parID = parID)
as.character(max(df$value))
summary(df)
