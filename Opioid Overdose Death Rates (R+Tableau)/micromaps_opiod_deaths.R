library(imputeR)
library(micromapST)
library(tidyverse)
library(janitor)



base_dir <- "C:\\Users\\chait\\Desktop\\Fall 2019 - Courses\\STAT 515 - R programing\\R midterm project\\datasets\\"
year_wise_data <- "C:\\Users\\chait\\Desktop\\Fall 2019 - Courses\\STAT 515 - R programing\\R midterm project\\datasets\\year_wise_data\\"


#data pre-processing
files_list <- list.files(path = year_wise_data,pattern = "raw")
df <- read.csv(paste(base_dir,"data.csv",sep=""),check.names=FALSE)
names(df)[names(df) == '﻿Location'] <- 'Location'

copy_columns_list <- list("All Drug Overdose Death Rate (Age-Adjusted)","Percent Change in Opioid Overdose Death Rate from Prior Year","Percent Change in All Drug Overdose Death Rate from Prior Year")
  
for (column in copy_columns_list)
{
    for (file in files_list)
    {
      year <- substr(file, 10, 13)
      temp_df <- read.csv(paste(year_wise_data,file,sep=""),check.names=FALSE)
      col_name = paste(year,"  ",column,sep="")
      df[col_name] <- temp_df[column]
    }
}

  
df <-  data.frame(lapply(df, function(x)
{
    gsub("NSD",0,x)
}
))
  
df <-  data.frame(lapply(df, function(x)
{
    gsub("NR",0,x)
}
))
  
for (i in 1:ncol(df))
{
    df[,i] <- as.numeric(as.character(df[,i]))
}
  
  
for(every_row in 1:nrow(df))
{
    df[every_row,"avg_opioid_death_growth_rate"] <- sum(df[every_row,40:58])/19 
    df[every_row,"avg_deaths_due_to_opioid"] <- sum(df[every_row,2:20])/19
    df[every_row,"avg_deaths_due_to_all_drugs"] <- sum(df[every_row,21:39])/19
    df[every_row,"avg_opioid_death_rate_2009_2013"] <- sum(df[every_row,12:16])/5
    df[every_row,"avg_opioid_death_rate_2013_2017"] <- sum(df[every_row,16:20])/5
}
  
df["percentage_of_opioid_death_rates_amongst_all_drug_death_rates"] <- (df["avg_deaths_due_to_opioid"]*100)/df["avg_deaths_due_to_all_drugs"]
df["percentage_increase_in_avg_opioid_death_rate_between_2009_2013_and_2013_2017"] <- ((df["avg_opioid_death_rate_2013_2017"]- df["avg_opioid_death_rate_2009_2013"])/(df["avg_opioid_death_rate_2009_2013"])*100)

df <- clean_names(df)
names(df) <- gsub("x","",names(df),fixed=TRUE)
df = df[-1,]
  
row_list <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio",
                "Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
  
df["location"] <- row_list
write.csv(df, file = paste(base_dir,"insights_into_drug_deaths.csv",sep=""),row.names = FALSE)





#preparing data for micromaps

micromap_df <- read_csv(file=paste(base_dir,"insights_into_drug_deaths.csv",sep=""))
micromap_df <- micromap_df[c(-1,-21:-ncol(micromap_df))]
  
col_list <- c("Y1999","Y2000","Y2001","Y2002","Y2003","Y2004","Y2005","Y2006","Y2007","Y2008","Y2009","Y2010","Y2011",
                "Y2012","Y2013","Y2014","Y2015","Y2016","Y2017")
  
row_list <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY",
                "LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                "OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  
colnames(micromap_df) <- col_list
row.names(micromap_df) <- row_list
write.csv(micromap_df, file = paste(base_dir,"micromap_data.csv",sep=""),row.names=TRUE)
newDf <- read.csv(file = paste(base_dir,"micromap_data.csv",sep=""))
names(newDf)[names(newDf) == 'X'] <- 'Location'
write.csv(newDf, file = paste(base_dir,"micromap_data.csv",sep=""),row.names=FALSE)





#plotting time series micromaps

tempTS <- read.table(file=paste(base_dir,"micromap_data.csv",sep=""),sep=",",header = T)
yrmat <-matrix(rep(1999:2017,51),nrow=51,ncol=19,byrow=T)
ratemat<-as.matrix(tempTS[,c(2:20)])
workmat<-cbind(yrmat,ratemat)
TSdata <-NULL
TSdata <-array(workmat,dim=c(51,19,2))
  
rownames(TSdata)<-as.character(tempTS$stab)
rownames(TSdata)<-as.character(tempTS$Location)
temprates <- data.frame(TSdata[,,2])
  
panelDesc <- data.frame(
  type=c("mapcum","id","ts","dot"),
  lab1=c("","","Time Series",""),
  lab2=c("","","Annual Death Rate per 100,000","Most Recent Death Rate (2017)"),
  lab3=c("","","Years","Death Rate per 100,000"),
  lab4=c("","","Rate",""),
  col1=c(NA,NA,NA,19),
  panelData =c(NA,NA,"TSdata",NA)
)
  
ExTitle <- c("Time Series Plots",
             "Annual Opioid Overdose Death Rates (Age-Adjusted), 1999-2017")


pdf(file=paste(base_dir,"time_series_micromap_opioid_deaths.pdf",sep=""),width=9,height=12)
micromapST(temprates,panelDesc,sortVar=19,ascend=FALSE,title=ExTitle)
dev.off()



