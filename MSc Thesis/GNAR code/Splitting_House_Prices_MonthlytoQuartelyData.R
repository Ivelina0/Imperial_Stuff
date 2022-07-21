library("data.table")
library("dplyr")
library("tidyverse")

HPData <- read.csv("HousePricesbyUKRegion_monthlydata.csv")

############## GET THREE POINT AVR of HP Index ##############
dim(HPData) # 649   5
NewCols <- 649%/%3 # 216 Quarters. Ignoring April month data since no Quarters
mydf <- HPData[1:648,2:5]
GroupLabels <- 0:(nrow(mydf) - 1)%/% 3 # 215
mydf$Group <- GroupLabels
Avgs <- mydf %>% group_by(Group) %>% summarize(HP.Indx.England = mean(HP..Index.England),
                                               HP.Indx.NorIre = mean(HP.Index.Northern.Ireland),
                                               HP.Indx.Scot = mean(HP.Index.Scotland),
                                               HP.Indx.Wales = mean(HP.Index.Wales))
dim(Avgs) # 216   5
write.csv(Avgs,"HousePricesIndexbyCounty.csv", row.names = FALSE)

########### CONCAT DATA1 and AVrgs ###############
Avgs$DATE <- dates[(269-215):269]
Avgs <- data.frame(Avgs)[,2:6]

dim(merge(Data1, Avgs, by.x = "DATE", all.x = TRUE)) # 269  14
df <- merge(Data1, Avgs, by.x = "DATE", all.x = TRUE)

tail(df)
summary(df[,2:14])
