## NEW DATA

## GDP, HP, BoP splines
file_place <- "~/Imperial_College_London/Project code & data/CleanerCode/DATA/Quaterly Dataset/UK_Macroeconomics_Variables_TSOriginal.csv"
df <- read.csv(file = file_place)
dim(df) #268  13
colnames(df)

## Original Quaterly Data 
gdp_data <- df[,3]
BoP_data <- df[,2]

length(gdp_data)
#gdp_data[267:268]

# GDP Q1 2022: 569055
gdp_df <- c(gdp_data, c(569055) )
length(gdp_df) #269
# BoP Q1 2022: -51673
BoP_df <- c(BoP_data, -51673 )
length(BoP_df) 

df[1,c(1,3)]
df[268,c(1,3)]

## Splines Q to M

gdp_splines <- qtrly.toMonthly(gdp_df, start_date="1955-01-01", end_date="2022-03-01")
BoP_splines <- qtrly.toMonthly(BoP_df, start_date="1955-01-01", end_date="2022-03-01")

length(gdp_splines[[1]]) #802
gdp_splines[[1]]
class(fortify.zoo(gdp_splines[[1]]))
as.vector(gdp_splines[[1]])

gdp_BoP <- cbind(as.vector(gdp_splines[[1]]), as.vector(BoP_splines[[1]]))

write.csv(gdp_BoP,"GDP_BoP_QtoM.csv", row.names = FALSE)




