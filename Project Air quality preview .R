#installing spark
install.packages("sparklyr")

#instal local version of spark
library(sparklyr)
spark_install(version = "2.1.0")

#connecting to spark 
sc <- spark_connect(master = "local") #local 
Sys.setenv(JAVA_HOME = "/path/to/java/installation") #cluster 

Sys.which("java")

#import data
airquality2020 <- read.csv("~/Desktop/waqi-covid19-airqualitydata-2020.csv", comment.char="#")
airquality2019 <- read.csv("~/Desktop/waqi-covid19-airqualitydata-2019Q1.csv", comment.char="#")
airquality20192 <- read.csv("~/Desktop/waqi-covid19-airqualitydata-2019Q2.csv", comment.char="#")

#look at data 
library(psych)
describe(airquality2020)
head(airquality2020) # start date: 2020-01-08
tail(airquality2020) # end data: 2020-04-05
head(airquality2019) # start date: 2019-01-16
tail(airquality2019) # end data: 2019-03-29

#merge q1 and q2
#airquality2019 =rbind(airquality2019,airquality20192)

#separate dates
#library(tidyr)
#airquality2019=separate(airquality2019, "Date", c("Year", "Month", "Day"), sep = "-")

#subset no2 data 
airquality2019no2=airquality2019[airquality2019$Specie=="no2",]
airquality2020no2=airquality2020[airquality2020$Specie=="no2",]

#table of highest average max no2 levels per city (2019)
library(plyr)
result<-tapply( airquality2019no2$median, airquality2019no2$City, mean )
sort(result,decreasing=TRUE)
    # top 5 for max are jerusalem, izmit, tel aviv, delhi, samsun, seoul
    # top 5 for median kayseri, milan, oaxaca, seoul, modena
result2<-tapply( airquality2020no2$median, airquality2020no2$City, mean )
sort(result2,decreasing=TRUE)

# comparing no2 levels in soeul 2019 and 2020
airquality2019Seoul = airquality2019no2[airquality2019no2$City=="Seoul",]
airquality2020Seoul = airquality2020no2[airquality2020no2$City=="Seoul",]

# mean median of seoul 2019/2020
mean(airquality2019Seoul$median) #34.84667
mean(airquality2020Seoul$median) #28.04906

# average max 
mean(airquality2019Seoul$max) #92.72333
mean(airquality2020Seoul$max) #77.90094

#histogram of 2020 and 2019 median no2
par(mfrow=c(1,2))
hist(airquality2019Seoul$median)
hist(airquality2020Seoul$median)

#order the data
library(tidyverse)
airquality2020Seoul=airquality2020Seoul %>% arrange(Date)
airquality2019Seoul=airquality2019Seoul %>% arrange(Date)
airquality2019Seoul=airquality2019Seoul[1:103,]

library(readr)
library(ggplot2)
library(scales)

airquality2019 <- read.csv("~/Desktop/waqi-covid19-airqualitydata-2019Q1.csv", comment.char="#")
seoul2019=airquality2019[airquality2019$City=="Seoul",]
seoul2019$Date = as.Date(seoul2019$Date)

#remove dew, humidity, percipitation,pressure, temp, wd, wind quest, wind sped
seoul2019 = seoul2019[seoul2019$Specie=="co"|seoul2019$Specie=="no2"|seoul2019$Specie=="o3"|seoul2019$Specie=="pm10"|seoul2019$Specie=="pm25"|seoul2019$Specie=="so2",]

#graph different air quality meaurements in seoul 2019
g1= ggplot(data=seoul2019,aes(x=Date,y=median,color=Specie))+
  geom_line()
theme_bare= theme(panel.background=element_blank())
g1 = g1 + theme_bare+labs(title="Seoul 2019 air Quality")+
  scale_x_date(labels=date_format("%Y-%m"),breaks ="1 month")

#same for 2020
seoul2020=airquality2020[airquality2020$City=="Seoul",]
seoul2020$Date = as.Date(seoul2020$Date)
seoul2020 = seoul2020[seoul2020$Specie=="co"|seoul2020$Specie=="no2"|seoul2020$Specie=="o3"|seoul2020$Specie=="pm10"|seoul2020$Specie=="pm25"|seoul2020$Specie=="so2",]

#graph seoul 2020
g2= ggplot(data=seoul2020,aes(x=Date,y=median,color=Specie))+
  geom_line()
theme_bare= theme(panel.background=element_blank())
g2 = g2 + theme_bare+labs(title="Seoul 2020 air Quality")+
  scale_x_date(labels=date_format("%Y-%m"),breaks ="1 month")

#la 2019
la2019=airquality2019[airquality2019$City=="Los Angeles",]
la2019$Date = as.Date(la2019$Date)
la2019 = la2019[la2019$Specie=="co"|la2019$Specie=="no2"|la2019$Specie=="o3"|la2019$Specie=="pm10"|la2019$Specie=="pm25"|la2019$Specie=="so2",]

#graph la 2019
g3= ggplot(data=la2019,aes(x=Date,y=median,color=Specie))+
  geom_line()
theme_bare= theme(panel.background=element_blank())
g3 = g3 + theme_bare+labs(title="La 2019 air Quality")+
  scale_x_date(labels=date_format("%Y-%m"),breaks ="1 month")

#2020 la
la2020=airquality2020[airquality2020$City=="Los Angeles",]
la2020$Date = as.Date(la2020$Date)
la2020 = la2020[la2020$Specie=="co"|la2020$Specie=="no2"|la2020$Specie=="o3"|la2020$Specie=="pm10"|la2020$Specie=="pm25"|la2020$Specie=="so2",]

#graph la 2020
g4= ggplot(data=la2020,aes(x=Date,y=median,color=Specie))+
  geom_line()
theme_bare= theme(panel.background=element_blank())
g4 = g4 + theme_bare+labs(title="La 2020 air Quality")+
  scale_x_date(labels=date_format("%Y-%m"),breaks ="1 month")

hist(airquality2019$median- airquality2020$median)

# merging data
head(df1)
df1=airquality2019
df1$Date<-NULL

df2=airquality2020
df2$Date<-NULL

df3 = merge(df1, df2, by.x=c("City", "Specie"), by.y=c("City", "Specie"))
head(d)

