## ibutton data

# Libraries ----
library(wesanderson)
library(RColorBrewer)
library(cowplot)
library(reshape2)
#library(scales)
#library(vistime)
library(directlabels)
library(export)
library(gridExtra)
#library(lattice)
#library(vioplot)
library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)

# old analyses ----

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







# 2019 analyses ----
# clean data ----

ibutton = read_csv("ibutton_data.csv")

# clean master file
ibutton$date = as.Date(ibutton$date, "%m/%d/%y")

ibutton = ibutton %>%
  separate(pop_plot, sep = "_", into = c("pop", "var", "plot"), remove = FALSE) %>%
  select(-var) %>%
  mutate(site = case_when(
    pop == "LVTR2.5" ~ "LVTR",
    pop == "LVTR2.25" ~ "LVTR",
    pop == "LVTR2" ~ "LVTR",
    pop == "LVTR1.25" ~ "LVTR",
    pop == "LVTR1.5" ~ "LVTR",
    pop == "LVTR1.75" ~ "LVTR",
    pop == "LVTR1" ~ "LVTR",
    pop == "LV1" ~ "LV1",
    pop == "LV2" ~ "LV2",
    pop == "LV3" ~ "LV3",)) %>%
mutate(plot = if_else(is.na(plot), 
    case_when(
      pop == "LVTR2.5" ~ "2.5",
      pop == "LVTR2.25" ~ "2.25",
      pop == "LVTR2" ~ "2",
      pop == "LVTR1.25" ~ "1.25",
      pop == "LVTR1.5" ~ "1.5",
      pop == "LVTR1.75" ~ "1.75",
      pop == "LVTR1" ~ "1",
      pop == "LV1" ~ "0"),
    plot)) %>%
mutate(jday = yday(date)) %>%
mutate(year = year(date)) %>%
mutate(month = month(date)) %>%
mutate(year_month = floor_date(date, "month")) %>%
unite("month_year", month, year, sep = "_", remove = FALSE) %>%
  mutate(elevation =
  case_when(
    pop=="LVTR2.5" ~ 2853,
    pop=="LVTR2.25" ~ 2845,
    pop=="LVTR2" ~ 2808,
    pop=="LVTR1.75" ~ 2773,
    pop=="LVTR1.5" ~ 2761,
    pop=="LVTR1" ~ 2756,
    pop=="LV3" ~ 2353,
    pop=="LV2" ~ 2500,
    pop=="LV1" ~ 2593
  ))

ggplot(ibutton, aes(date, temp, group = plot, color = plot)) +
  geom_line() +
  facet_wrap(~pop_plot)

# at the plot level
meantempplot = ibutton %>%
  group_by(date, year_month, month, year, pop, plot, pop_plot) %>%
  summarize(
    maxdaily = max(temp),
    mindaily = min(temp),
    meandaily = mean(temp),
    elevation = median(elevation)
  ) %>%
  group_by(year_month, month, year, pop, plot, site, elevation, pop_plot) %>%
  summarize(
    meanmaxdaily = mean(maxdaily),
    semeanmaxdaily = se(maxdaily),
    lowermeanmaxdaily = meanmaxdaily - semeanmaxdaily,
    uppermeanmaxdaily = meanmaxdaily + semeanmaxdaily
  ) %>%
  drop_na(site)

# at the site level
meantempsite = ibutton %>%
  group_by(date, year_month, month, year, site) %>%
  summarize(
    elevation = median(elevation),
    maxdaily = max(temp),
    mindaily = min(temp),
    meandaily = mean(temp)
  ) %>%
  group_by(year_month, month, year, site, elevation) %>%
  summarize(
    meanmaxdaily = mean(maxdaily),
    semeanmaxdaily = se(maxdaily),
    lowermeanmaxdaily = meanmaxdaily - semeanmaxdaily,
    uppermeanmaxdaily = meanmaxdaily + semeanmaxdaily
  ) %>%
  drop_na(site)

# the following code does something very strange to mismatch the sites with their elevations...
#meantempsite$site=factor(meantemp$site,levels=c("LVTR", "LV1", "LV2", "LV3"))
#meantempplot$site=factor(meantempplot$site,levels=c("LVTR", "LV1", "LV2", "LV3"))

meantempplot$year = as.factor(meantempplot$year) # just doing this b/c can't map shape onto a continuous variable; really I just need to change the format of the dataframe
#meantemp$month = as.factor(meantemp$month)
1
# colors = elevation
tempelev = ggplot(meantempsite, aes(year_month, meanmaxdaily, group = site, color = elevation)) +
  ggtitle ("Date vs maximum daily temperature averaged by month") +
  ylab("Average mean max daily temp per month") +
  xlab("Date") +
  scale_color_continuous(low = "orange", high = "blue") +
  #geom_smooth(se = FALSE) +
  geom_line() +
  geom_point(size = 3) + # aes(shape = plot),
  geom_errorbar(aes(ymin = lowermeanmaxdaily, ymax = uppermeanmaxdaily), size = 0.5, width = 5)

# colors = sites
ggplot(meantempsite, aes(year_month, meanmaxdaily, group = site, color = site)) +
  #scale_color_continuous() +
  geom_smooth(se = FALSE) +
  #geom_line() +
  geom_point(size = 3) + # aes(shape = plot),
  geom_errorbar(aes(ymin = lowermeanmaxdaily, ymax = uppermeanmaxdaily), size = 0.5, width = 5)

# group = plot
ggplot(meantempplot, aes(year_month, meanmaxdaily, group = pop_plot, color = pop_plot)) +
  #facet_wrap(~site) +
  #scale_color_continuous() +
  #geom_smooth(se = FALSE) +
  geom_line() +
  geom_point(size = 3) + # aes(shape = plot),
  geom_errorbar(aes(ymin = lowermeanmaxdaily, ymax = uppermeanmaxdaily), size = 0.5, width = 5)

# all years in one plot
monthvstemp = ggplot(meantempplot, aes(month, meanmaxdaily, group = year, color = year)) +
  ggtitle ("Month of year vs maximum daily temperature averaged by month") +
  ylab("Average mean max daily temp per month") +
  xlab("Month") +
  #facet_wrap(~site) +
  #scale_color_continuous() +
  geom_smooth(aes(group = year, color = year), se = FALSE) +
  #geom_line() +
  #geom_jitter() #+
  geom_point(size = 3) + # aes(shape = plot),
  geom_errorbar(aes(ymin = lowermeanmaxdaily, ymax = uppermeanmaxdaily), size = 0.2, width = 0.1)

