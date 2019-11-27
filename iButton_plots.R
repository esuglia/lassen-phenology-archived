#specific sites, R's built-in plotting function
lv1=read.csv("lv1.csv")
lv1$date = as.Date(lv1$date, "%m/%d/%Y")

par(mfrow=c(3,3))
plot(lv1$date,lv1$temp,type="p", pch=20, main="LV1 Temperature Over Time", ylab="Temperature (degrees C)", xlab="Month")

lv2p1=read.csv("lv2_plot1.csv")
lv2p1$date = as.Date(lv2p1$date, "%m/%d/%Y")
plot(lv2p1$date,lv2p1$temp,type="p", pch=20, main="LV2 plot 1 Temperature Over Time", ylab="Temperature (degrees C)", xlab="Month")

#all ibutton data combined, ggplot
library(ggplot2)
library(plyr)
library(reshape2)

#file ibutton_data is original file with all data. This includes data collected AFTER ibuttons were removed from the field, which get some really high temps and are not accurate.
#file ibutton_data_12.5.18 has removed all data taken on 6/30/18 and after (goes through 7/2).

data=read.csv("ibutton_data_12.5.18.csv")
data$date = as.Date(data$date, "%m/%d/%Y") #makes sure R reads this column as a date. The % signs and numbers of m's, d's, or Y's, as well as the capitalization of these letters, have their own meaning that signify how the date is formatted.

data$pop_plot=factor(data$pop_plot,levels=c("LV3_plot_1","LV3_plot_2","LV2_plot_1","LV2_plot_2","LV1","LVTR1","LVTR2"))

ggplot(data=data,aes(x=data$date,y=data$temp)) +
  geom_point(color='deepskyblue4',shape=20,size=0.5) +
  geom_line(color='deepskyblue4') +
  facet_wrap(~pop_plot) +
  labs(title="Lassen Temperature Data 2017-2018", x = "Date", y = "Temperature (degrees C)") +
  scale_x_date(date_breaks="1 month", date_labels="%b") + #breaks up and labels x axis with ticks for each month. %b gives month in characters rather than numbers. %m would give month in numbers. %b %d would give month in characters, then day, ie. "Jun 19"
  theme(panel.grid.minor=element_blank()) #graphs only major gridlines





