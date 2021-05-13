library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)
data <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv', colClasses = c('date'='Date'))

##########################################################################################
#                                                                                        #
#                   RUN THE ABOVE ONLY THE FIRST TIME TO SAVE TIME                       #
#                                                                                        #
#                CHANGE THE BELOW, MIND THE QUOTES, MAX 8 COUNTRIES!                     #
##########################################################################################

countries = c('USA','GBR','CAN','DEU')

##########################################################################################

df = data[which(data$iso_code%in%countries),c(1,3,4,36)]

df['daily_first_doses'] = NA
for (j in 1:length(countries)) {
  i = Sys.Date() - 90
  temp = df[df['iso_code']==countries[j],]
  while (i<Sys.Date()){
    if ((i-7)%in%temp$date){
      temp[temp$date==i,5] = (temp[temp$date==i,4] - temp[temp$date==i-7,4])/(7*data[data[,1]==countries[j],45][1])
    }
    i = i + 1
  }
  df[df['iso_code']==countries[j],] = temp
}

colours = c("red", "blue", "black",'cyan','darkgoldenrod2','darkorchid2','hotpink2','tan2')
ggplot(df, aes(date,daily_first_doses, color=location))+
  xlim(min(df$date[!is.na(df$daily_first_doses)]),NA)+
  geom_line(size=1.5)+
  ggtitle('Daily First Doses per Capita (7-day average)')+ xlab(NULL)+ ylab(NULL)+
  scale_y_continuous(labels = scales::percent,breaks = scales::pretty_breaks(n = 10))+
  scale_color_manual(name='',values = colours, breaks = waiver())+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5))
