
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


# Study system graphs ----

# *** Variability in snowpack across years & sites

setwd("~/Box Sync/Graduate School/PhD Research/Ch. 2) Lassen Snowmelt & Phenology/Data analysis/lassen-phenology")
load("cleaning1.R")

snowvar = d1 %>%
  subset(year %in% 2018:2019) %>%
  group_by(site, meltdate, jmeltdate, jday, year) %>%
  summarize(meanmelt = mean(meltdate)) %>%
  drop_na(meltdate) %>%
  select(-meanmelt)

ggplot(data = snowvar, mapping = aes(jday, jmeltdate, group = site)) +
  geom_point() +
  geom_line()

# create a dataframe that replaces NAs with zeros in buds, flrs, and fruits columns
d2 <- d1 %>%
  mutate(flrs = replace_na(flrs, 0)) %>%
  mutate(fruits = replace_na(fruits, 0)) %>%
  mutate(buds = replace_na(buds, 0))

# Phenology over time (visualizations only) ----

#NOTES FOR FUTURE IMPROVEMENTS ON ANALYSES
# try something like reprostruct = d %>% filter(d1, variable = c(flrs, fruits))  so you only have flrs and fruits; need to construct df in long format so that you can separate out the ggplot categories based on the value of a column

# *** number of fruits each individual had over time 

ggplot(data = d1, mapping = aes(jday, fruits, color = site)) +
  #geom_point() +
  #geom_jitter() +
  geom_count() +
  facet_wrap(~year)

# I MISSED FIRST FLOWERING TIME FOR SEVERAL POPS; MAY BE BETTER TO COMPARE USING PEAK FLOWERING TIME
flr = d1 %>% 
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  group_by(site, plot, year, uniqueID) %>% 
  summarise(
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0]),
    lastflr = max(jday[is.na(flrs) == FALSE & flrs > 0]),
    branchiness = max(totalbranch),
    fitness = max(fruits),
    peakflr = min(jday[which(flrs == max(flrs))]) # if there are multiple days that have the same max value, need to specify one of those
  )

# fitness ~ peakflr stats & graph ----
flrlm = lm(fitness ~ peakflr, data = flr)
summary(flrlm)
anova(flrlm)
ggplot(data = flr, mapping = aes(peakflr, fitness)) +
  geom_smooth(method = lm)

# graph: peak vs last flowering time (both years included) 
# MAY BE MORE USEFUL TO LOOK AT ONE YEAR AT A TIME
p1 <- ggplot(data = flr, mapping = aes(peakflr, fill = site)) +
  geom_histogram() +
  scale_y_continuous(limits = c(0, 150))
p2 <- ggplot(data = flr, mapping = aes(lastflr, fill = site)) +
  geom_histogram()
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12, ncol = 1)

# graph: first vs last flowering time
ggplot(data = flr, mapping = aes(firstflr, lastflr, color = site)) +
  #geom_point()
  facet_wrap(~year) +
  geom_count()
#geom_jitter()

#graph: first flowering time (histogram)
ggplot(data = flr, mapping = aes(firstflr, fill = site)) +
  facet_wrap(~year) +
  geom_histogram()
# is there a better way to visualize this?

#graph: last flowering time (histogram)
ggplot(data = flr, mapping = aes(lastflr, fill = site)) +
  facet_wrap(~year) +
  geom_histogram()    

# *** pop level sums and means across time ----
# population level sums across time
popsumflr = d1 %>% 
  drop_na(buds) %>%
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  drop_na(fruits) %>%
  group_by(site, year, date, jday, elevation) %>% 
  summarise(
    sumbud = sum(buds),
    sumflr = sum(flrs),
    sumfruit = sum(fruits),
    herb = sum(anyherb)*20
  )

# population level means across time
popmeanflr = d1 %>%
  #drop_na(buds) %>%
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  #drop_na(fruits) %>%
  group_by(site, year, date, jday, elevation) %>% 
  summarise(
    flrs = mean(flrs),
    fruits = mean(fruits),
    buds = mean(buds)
  )

# graphs: total coflowering across sites (2017 and 2018 graphs are separate)
popsumflr %>% subset(year == 2017) %>%
  ggplot(aes(jday, sumflr, elevation)) +
  geom_line() +
  geom_area() +
  facet_wrap(~site, ncol = 1)
# in 2018 there was really low flowering overall at the peak trail sites

popsumflr %>% subset(year == 2018) %>%
  ggplot(aes(jday, sumflr, elevation)) +
  geom_line() +
  geom_area() +
  facet_wrap(~site, ncol = 1)
# number of flowers each individual had over time

popsumflr %>% subset(year == 2019) %>%
  ggplot(aes(jday, sumflr, elevation)) +
  geom_line() +
  geom_area() +
  facet_wrap(~site, ncol = 1)

# graphs: mean coflowering across sites (2017 and 2018 graphs are separate)
popmeanflr %>% subset(year == 2018) %>%
  ggplot(aes(jday, flrs)) +
  geom_line() +
  geom_area(aes(fill = "flrs", alpha = 0.04)) +
  geom_line(aes(jday, fruits)) +
  geom_area(aes(jday, fruits, fill = "fruits", alpha = 0.04)) +
  geom_line(aes(jday, buds)) +
  geom_area(aes(jday, buds, fill = "buds", alpha = 0.04)) +
  facet_wrap(~site, ncol = 1)
# graph is called "mean flrs and fruits over time" --> remember that it's only including those plants that both produced fruits and flowers

popmeanflr %>% subset(year == 2018) %>%
  ggplot(aes(date, flrs, group = site, color = elevation, label = site)) +
  geom_line() +
  geom_point() +
  geom_dl(method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  scale_x_date(limits = as.Date(c("2018-06-25", "2018-09-05"))) +
  #geom_text() +
  # ADD STANDARD ERROR BARS
  scale_color_continuous(low = "orange", high = "blue") +
  #scale_color_brewer(name = "Site",
  #palette = "Dark2") +
  ggtitle("Date vs flower abundance") +
  xlab("Date") +
  ylab("Mean Flower Abundance") +
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16))

popmeanflr %>% subset(year == 2019) %>%
  ggplot(aes(date, flrs, group = site, color = elevation, label = site)) +
  geom_line() +
  geom_point() +
  geom_dl(method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  scale_x_date(limits = as.Date(c("2019-07-15", "2019-10-10"))) +
  #geom_text() +
  # ADD STANDARD ERROR BARS
  scale_color_continuous(low = "orange", high = "blue") +
  #scale_color_brewer(name = "Site",
  #palette = "Dark2") +
  ggtitle("Date vs flower abundance") +
  xlab("Date") +
  ylab("Mean Flower Abundance") +
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16))  

popsumflr %>% subset(year == 2018) %>%
  ggplot(aes(date, sumflr, group = site, color = elevation, label = site)) +
  geom_line() +
  geom_point() +
  geom_dl(method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  scale_x_date(limits = as.Date(c("2018-06-25", "2018-09-05"))) +
  #geom_text() +
  # ADD STANDARD ERROR BARS
  scale_color_continuous(low = "orange", high = "blue") +
  #scale_color_brewer(name = "Site",
  #palette = "Dark2") +
  ggtitle("Date vs flower abundance") +
  xlab("Date") +
  ylab("Summed Flower Abundance") +
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16))

popsumflr %>% subset(year == 2019) %>%
  ggplot(aes(date, sumflr, group = site, color = elevation, label = site)) +
  geom_line() +
  geom_point() +
  geom_dl(method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  scale_x_date(limits = as.Date(c("2019-07-15", "2019-10-10"))) +
  #geom_text() +
  # ADD STANDARD ERROR BARS
  scale_color_continuous(low = "orange", high = "blue") +
  #scale_color_brewer(name = "Site",
  #palette = "Dark2") +
  ggtitle("Date vs flower abundance") +
  xlab("Date") +
  ylab("Summed Flower Abundance") +
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16))

popmeanflr %>% subset(year == 2017) %>%
  ggplot(aes(jday, flrs)) +
  geom_line() +
  geom_area(aes(fill = "flrs", alpha = 0.04)) +
  geom_line(aes(jday, fruits)) +
  geom_area(aes(jday, fruits, fill = "fruits", alpha = 0.04)) +
  #geom_line(aes(jday, buds)) +
  #geom_area(aes(jday, buds, fill = "buds", alpha = 0.04)) +
  facet_wrap(~site, ncol = 1)

popsumflr %>% subset(year == 2018) %>%
  ggplot(aes(jday, sumflr)) +
  geom_line() +
  geom_area(aes(fill = "flrs", alpha = 0.01)) +
  geom_line(aes(jday, sumfruit)) +
  geom_area(aes(jday, sumfruit, fill = "fruits", alpha = 0.01)) +
  geom_line(aes(jday, herb)) +
  geom_area(aes(jday, herb, fill = "herb", alpha = 0.01)) +
  geom_line(aes(jday, sumbud)) +
  geom_area(aes(jday, sumbud, fill = "buds", alpha = 0.01)) +
  facet_wrap(~site, ncol = 1)
# graph is called sum flrs and fruits over time; does NOT subset only plants that produced flrs and fruits; rather, includes those that are zeros

popsumflr %>% subset(year == 2018) %>%
  ggplot(aes(date, sumflr, group = site, color = elevation)) +
  geom_line() +
  geom_point() +
  # ADD STANDARD ERROR BARS
  #geom_point(data = d2, aes(jday, flrs, group = site, color = elevation)) +
  scale_color_continuous(low = "orange", high = "blue")
#geom_area(aes(fill = "flrs", alpha = 0.01)) +
#geom_line(aes(jday, sumfruit)) +
#geom_area(aes(jday, sumfruit, fill = "fruits", alpha = 0.01)) +
#geom_line(aes(jday, herb)) +
#geom_area(aes(jday, herb, fill = "herb", alpha = 0.01)) +
#geom_line(aes(jday, sumbud)) +
#geom_area(aes(jday, sumbud, fill = "buds", alpha = 0.01)) +
#facet_wrap(~site, ncol = 1)
# FIX ELEVATION FOR ONE SITE

# LVTR2 seems to have the highest overall numbers of flowers. Could this be due to branchiness? Did analysis of max branchiness vs fitness below to find out

# *** FOR POSTER * percent flring individuals by pop over time, #herb vs non-herb ----

#herb = d1 %>% 
#  group_by(uniqueID) %>%
#  summarize(
#    herbind = max(anyherb)
#  )

#dherb = d1 %>%
#  full_join(herb, by = "uniqueID")

# create a tibble that summarizes number of reproductive individuals in a population on each date in each year
repro = d2 %>%
  drop_na(flrs) %>%
  #drop_na(jmeltdate) %>%
  group_by(uniqueID, date, jday, pop, year) %>%
  summarize(
    repro = ifelse(
      flrs>0,1,0
    )) %>%
  group_by(uniqueID, pop, year) %>%
  summarize(
    reproind = max(repro)
  ) %>% 
  subset(reproind == 1) %>%
  group_by(pop, year) %>%
  add_tally() %>%
  select(-reproind, -uniqueID) %>%
  unite(pop_year, pop, year) %>%
  distinct() # removes duplicate rows

# create a tibble that summarizes numer of flowering individuals per site on each date in each year
d2 = d1 %>%
  mutate(pop = case_when(
    site == "LVTR2.5" ~ "LVTR",
    site == "LVTR2.25" ~ "LVTR",
    site == "LVTR2" ~ "LVTR",
    site == "LVTR1.25" ~ "LVTR",
    site == "LVTR1.5" ~ "LVTR",
    site == "LVTR1.75" ~ "LVTR",
    site == "LVTR1" ~ "LVTR",
    site == "LV1" ~ "LV1",
    site == "LV2" ~ "LV2",
    site == "LV3" ~ "LV3",))

d3 = d2 %>%
  drop_na(flrs) %>%
  drop_na(jmeltdate) %>%
  group_by(pop, year, date, jday, uniqueID) %>%
  summarise(
    flrs = sum(flrs)) %>%
  group_by(pop, year, date, jday) %>%
  summarize(
    flrind = length(which(flrs>0 & is.na(flrs) == FALSE))
  ) %>%
  unite(pop_year, pop, year, remove = FALSE)

# create a tibble that summarizes first, peak, and last flowering dates for each site
d4 = d2 %>%
  drop_na(flrs) %>%
  drop_na(jmeltdate) %>%
  group_by(pop, year) %>%
  summarise(
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0]),
    peakflr = median(jday[which(flrs == max(flrs))]),
    lastflr = max(jday[is.na(flrs) == FALSE & flrs > 0])
  ) %>%
  mutate(firstflr = na_if(firstflr, Inf)) %>%
  mutate(firstflr = na_if(firstflr, -Inf)) %>%
  mutate(peakflr = na_if(peakflr, Inf)) %>%
  mutate(peakflr = na_if(peakflr, -Inf)) %>%
  mutate(lastflr = na_if(lastflr, Inf)) %>%
  mutate(lastflr = na_if(lastflr, -Inf)) %>%
  drop_na(firstflr) %>%
  drop_na(peakflr) %>%
  drop_na(lastflr) %>%
  unite(pop_year, pop, year)

# merge together the various tibbles   
d5 = full_join(repro, d3, by = "pop_year") %>%
  mutate(percflr = ((flrind/n)*100)) %>%
  group_by(pop, year, pop_year, date, jday) %>%
  summarize(
    meanpercflr = mean(percflr)
  ) %>%
  full_join(d4, by = "pop_year", remove=FALSE) %>%
  drop_na(meanpercflr) %>%
  mutate(meltdate = case_when(
    year == 2018 & pop == "LVTR" ~ "2018-06-01",
    year == 2018 & pop == "LVTR" ~ "2018-06-01",
    year == 2018 & pop == "LV1" ~ "2018-06-20",
    year == 2018 & pop == "LV2" ~ "2018-05-25",
    year == 2018 & pop == "LV3" ~ "2018-05-10",
    year == 2019 & pop == "LVTR" ~ "2019-06-18",
    year == 2019 & pop == "LV1" ~ "2019-08-06",
    year == 2019 & pop == "LV2" ~ "2019-06-28",
    year == 2019 & pop == "LV3"~ "2019-07-05")) %>%
  mutate(jmeltdate = yday(meltdate))

#year == 2018 & site == "LVTR2.5" ~ "NA",
#year == 2018 & site == "LVTR2" ~ "2018-06-01",
#year == 2018 & site == "LVTR1" ~ "2018-06-01",
#year == 2019 & site == "LVTR2.5" ~ "2019-06-13",
#year == 2019 & site == "LVTR2.25" ~ "2019-06-17",
#year == 2019 & site == "LVTR2" ~ "2019-06-19",
#year == 2019 & site == "LVTR1.75" ~ "2019-06-28",
#year == 2019 & site == "LVTR1.5" ~ "2019-07-05",
#year == 2019 & site == "LVTR1" ~ "2019-06-15",

#lvtrmeltdates = c("2019-06-13", "2019-06-17", "2019-06-19", "2019-06-28", "2019-07-05", "2019-06-15")
#lvtrmeltdates = as.Date(lvtrmeltdates)
#lvtrmeltdates = yday(lvtrmeltdates)
#median(lvtrmeltdates)

#%>%
#mutate(herbind = case_when(

# herbind == 0 ~ "no",
#  herbind == 1 ~ "yes"
#  ))

# *FOR POSTER* graph: coflowering separating out pops
#popflrpercsnow$meltdate = ymd(popflrpercsnow$meltdate)
#popflrperc18$date = as.Date(popflrperc18$date)
#as.numeric(as.Date(

# "LVTR2.25", "LVTR2", "LVTR1.75", "LVTR1.5", "LVTR1",
d5$pop=factor(d5$pop,levels=c("LVTR", "LV1", "LV2", "LV3"))

d5$meltdate = as.Date(d5$meltdate)

d5plot18 = 
  d5 %>% 
  subset(year == 2018) %>%
  ggplot(aes(jday, meanpercflr)) + #group = herbind
  #scale_color_continuous(low = "orange", high = "blue") +
  geom_point() +
  geom_line() + #aes(linetype = herbind)
  facet_wrap(~pop, ncol = 1) +
  theme(axis.line = element_line(colour = "black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold")) +
  ggtitle("2018") +
  xlab("Date") +
  ylab("Percent Flowering") +
  geom_vline(aes(xintercept = jmeltdate), color = "gray39", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = peakflr), color = "cadetblue", linetype = "solid", size = 2, alpha = 0.5) +
  geom_vline(aes(xintercept = lastflr), color = "red", linetype = "solid", size = 2, alpha = 0.5) #+
geom_rect( 
  aes(xmin = as.Date("05-05", "%m-%d"), 
      xmax = as.Date("09-05",  "%m-%d"), # not sure why but the extra 0 is necessary!
      #xmin = as.Date("2018-05-05", "%Y-%m-%d"), 
      #xmax = as.Date("20018-09-05",  "%Y-%m-%d"), # not sure why but the extra 0 is necessary!
      ymin = -Inf, 
      ymax = Inf),
  fill = "gray", 
  alpha = 0.5)

d5plot19 = d5 %>% 
  subset(year == 2019) %>%
  ggplot(aes(jday, meanpercflr), nrow = 1) + #group = herbind
  #scale_color_continuous(low = "orange", high = "blue") +
  geom_point() +
  geom_line() + #aes(linetype = herbind)
  facet_wrap(~pop, ncol = 1) +
  theme(axis.line = element_line(colour = "black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold")) +
  ggtitle("2019") +
  xlab("Date") +
  ylab("Percent Flowering") +
  geom_vline(aes(xintercept = jmeltdate), color = "gray39", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = peakflr), color = "cadetblue", linetype = "solid", size = 2, alpha = 0.5) +
  geom_vline(aes(xintercept = lastflr), color = "red", linetype = "solid", size = 2, alpha = 0.5) #+
geom_rect(
  aes(xmin = as.Date("2019-06-05", "%Y-%m-%d"), 
      xmax = as.Date("20019-10-05",  "%Y-%m-%d"), # not sure why but the extra 0 is necessary!
      ymin = -Inf, 
      ymax = Inf),
  fill = "gray", 
  alpha = 0.5)

require(grid)
grid.arrange(d5plot18, d5plot19, nrow = 1, top=textGrob("Date vs percent population flowering", gp=gpar(fontsize=20,font=8)))

# save graph as a ppt file in directory
#graph2ppt(file="date vs percent flowering with meltdate and herb.pptx", width = 9, height = 9, append=TRUE)

# graph: coflowering with pops all together
popflrperc %>%
  ggplot(aes(date, meanpercflr, group = site, color = elevation, label = site)) +
  geom_line() +
  geom_point() +
  geom_dl(method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  scale_x_date(limits = as.Date(c("2018-06-25", "2018-09-05"))) +
  #geom_text() +
  # ADD STANDARD ERROR BARS
  scale_color_continuous(low = "orange", high = "blue") +
  #scale_color_brewer(name = "Site",
  #palette = "Dark2") +
  ggtitle("Date vs percent of population flowering") +
  xlab("Date") +
  ylab("Percent Flowering") +
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# *** fitness over time ----

fruits = d1 %>% 
  drop_na(fruits) %>% # to do a summarize, can't have NAs in column
  group_by(site, plot, year, uniqueID) %>% 
  summarise(
    firstfruit = min(jday[is.na(fruits) == FALSE & fruits > 0]),
    lastfruit = max(jday[is.na(fruits) == FALSE & fruits > 0]),
    fitness = max(fruits),
    branchiness = max(totalbranch)
  )

# graph: first vs last fruit
ggplot(data = fruits, mapping = aes(firstfruit, lastfruit, color = site)) +
  #geom_point()
  facet_wrap(~year) +
  #geom_jitter()
  geom_count()

# graph: fitness over time
ggplot(data = fruits, mapping = aes(fitness, fill = site)) +
  facet_wrap(~year) +
  geom_bar()

# Branchiness vs fitness (2017 & 2018) ---- 

#branchiness vs #flrs/#fruits/fitness
ggplot(data = fruits, mapping = aes(branchiness, fitness)) +
  facet_wrap(~year) +
  geom_smooth() +
  geom_count()
#geom_smooth()
# it appears that branchiness does affect fitness. Let's do an anova:

# anova
branchfitnesslm = lm(fitness ~ branchiness, data = fruits)
summary(branchfitnesslm)
anova(branchfitnesslm)
# Those with more (max) total branches had significantly higher fecundity.

# Snowmelt timing ~ phenology (2018 & 2019 only) ----

# *FOR POSTER* snowmelt ~ flowering phenology ----

se = function(x) sqrt(var(x)/length(x)) 

## flowering phenology
flrsnow0 = d1 %>% 
  drop_na(jmeltdate) %>%
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  group_by(site, jmeltdate, meltdate, elevation, year, uniqueID) %>%
  summarise(
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0]),
    lastflr = max(jday[is.na(flrs) == FALSE & flrs > 0]),
    peakflr = median(jday[which(flrs == max(flrs))])
  ) %>%
  mutate(firstflr = na_if(firstflr, Inf)) %>%
  mutate(firstflr = na_if(firstflr, -Inf)) %>%
  mutate(peakflr = na_if(peakflr, Inf)) %>%
  mutate(peakflr = na_if(peakflr, -Inf)) %>%
  mutate(lastflr = na_if(lastflr, Inf)) %>%
  mutate(lastflr = na_if(lastflr, -Inf)) %>%
  drop_na(firstflr) %>%
  drop_na(peakflr) %>%
  drop_na(lastflr) %>%
  unite(site_year, site, year, remove = FALSE)


flrsnowpopmean = flrsnow0 %>%
  group_by(site, year, elevation, jmeltdate) %>%
  summarise(
    meanpeak = mean(peakflr),
    meanfirst = mean(firstflr),
    sepeak = se(peakflr),
    sefirst = se(firstflr),
    lowerpeak = meanpeak - sepeak,
    upperpeak = meanpeak + sepeak,
    lowerfirst = meanfirst - sefirst,
    upperfirst = meanfirst + sefirst
  ) %>%
  unite(site_year, site, year, remove = FALSE)

flrsnow0statsum = full_join(flrsnow0, flrsnowpopmean, by = "site_year")

#flrsnow0$firstflr[is.na(flrsnow0$firstflr)] <- "X"
#drop_na(firstflr) %>%
#mutate(firstflr = replace_na(firstflr, 0))

flrsnow = flrsnow0 %>%
  gather(key = "variable", value = "value", "peakflr", "lastflr", "firstflr", -site, -jmeltdate, -elevation, -uniqueID, -year, na.rm=FALSE) %>%
  drop_na(value)

#remove_missing(flrsnow, finite = TRUE)

#write.csv(flrsnow, "flrsnow.csv")
flrsnow$variable <- factor(flrsnow$variable, levels = c("peakflr", "lastflr"))

# graph: individual meltdate ~ peak, and last flowering time
flrsnow18 = flrsnow %>%
  subset(year == 2018)

flrsnow19 = flrsnow %>%
  subset(year == 2019)

plotflrsnow18 = ggplot(data = flrsnow18, mapping = aes(jmeltdate, value, color = variable)) +
  ggtitle("2018 Snowmelt date vs flowering phenology") +
  xlab("Snowmelt Date") +
  ylab("Flowering Phenology Date") +
  scale_color_brewer(name="Flowering\nPhenology\nMetric",
                     palette = "Dark2",
                     labels=c("Peak Flowering", "Last Flowering")) +
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(method = lm) +
  geom_jitter(data = flrsnow18, mapping = aes(jmeltdate, value, color = variable, alpha = 0.1), na.rm=TRUE)

plotflrsnow19 = ggplot(data = flrsnow19, mapping = aes(jmeltdate, value, color = variable)) +
  ggtitle("2019 Snowmelt date vs flowering phenology") +
  xlab("Snowmelt Date") +
  ylab("Flowering Phenology Date") +
  scale_color_brewer(name="Flowering\nPhenology\nMetric",
                     palette = "Dark2",
                     labels=c("Peak Flowering", "Last Flowering")) +
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(method = lm) +
  geom_jitter(data = flrsnow19, mapping = aes(jmeltdate, value, color = variable, alpha = 0.1), na.rm=TRUE)


require(gridExtra)
grid.arrange(plotflrsnow18, plotflrsnow19, nrow=2)

# save graph as a ppt file in directory
#graph2ppt(file="snowmelt vs peak and last flring time.pptx", width = 9, height = 9, append=TRUE)

# *** graphs for meltdate ~ flring time ----
# graph: firstflr ~ meltdate
ggplot(data = flrsnow0) +
  geom_smooth(method = lm, aes(jmeltdate, firstflr)) +
  geom_smooth(method = lm, aes(jmeltdate, peakflr)) +
  geom_smooth(method = lm, aes(jmeltdate, lastflr))

# graph: peakflr ~ meltdate 
ggplot(data = flrsnow0, mapping = aes(jmeltdate, peakflr, color = elevation)) +
  geom_count() +
  geom_smooth(method = lm, aes(jmeltdate, peakflr))
#geom_jitter()

# graph: lastflr ~ meltdate
ggplot(data = flrsnow0, mapping = aes(jmeltdate, lastflr, color = elevation)) +
  geom_smooth(method = lm, aes(jmeltdate, lastflr)) +
  #geom_point()
  #geom_jitter()
  geom_count()

# firstflr ~ jmeltdate model selection ----
# firstflr ~ jmeltdate fixed model & anova
firstflrlm = lm(firstflr ~ jmeltdate, data = flrsnow0)
summary(firstflrlm)
anova(firstflrlm)
# later snowmelt significantly delays first flowering 

m14 = lmer(firstflr ~ jmeltdate + (1|site), data = flrsnow0, REML = FALSE)
m15 = lmer(firstflr ~ jmeltdate + (1 + jmeltdate|site), data = flrsnow0, REML = FALSE)
m16 = lmer(firstflr ~ jmeltdate + (1|year), data = flrsnow0, REML = FALSE)
m17 = lmer(firstflr ~ jmeltdate + (1|site) + (1|year), data = flrsnow0, REML = FALSE)
m18 = lmer(firstflr ~ jmeltdate*year + (1|site), data = flrsnow0, REML = FALSE)
m19 = lmer(firstflr ~ jmeltdate*site + (1|year), data = flrsnow0, REML = FALSE) # best fit
m20 = lmer(firstflr ~ jmeltdate*site + (1|year) + (1|site), data = flrsnow0, REML = FALSE)
m21 = lmer(firstflr ~ jmeltdate*site + (1 + site|year), data = flrsnow0, REML = FALSE)
# some warning messages

AIC(firstflrlm, m14, m15, m16, m17, m18, m19, m20, m21)
AICc(firstflrlm, m14, m15, m16, m17, m18, m19, m20, m21)
BIC(firstflrlm, m14, m15, m16, m17, m18, m19, m20, m21)

r.squaredGLMM(m14)
r.squaredGLMM(m15) 
r.squaredGLMM(m16) 
r.squaredGLMM(m17) 
r.squaredGLMM(m18) 
r.squaredGLMM(m19) 
r.squaredGLMM(m20) 
r.squaredGLMM(m21) 
# including random effects does not necessarily appear to help
# why are the last three identical? Ask Andrew about error messages

# peakflr ~ jmeltdate model selection ----
# peakflr ~ jmeltdate fixed effects model & anova
peakflrlm = lm(peakflr ~ jmeltdate, data = flrsnow0)
summary(peakflrlm)
anova(peakflrlm)

# mixed models for meltdate ~ peakflr
m6 = lmer(peakflr ~ jmeltdate + (1|site), data = flrsnow0, REML = FALSE)
m7 = lmer(peakflr ~ jmeltdate + (1 + jmeltdate|site), data = flrsnow0, REML = FALSE)
m8 = lmer(peakflr ~ jmeltdate + (1|year), data = flrsnow0, REML = FALSE)
m9 = lmer(peakflr ~ jmeltdate + (1|site) + (1|year), data = flrsnow0, REML = FALSE)
m10 = lmer(peakflr ~ jmeltdate*year + (1|site), data = flrsnow0, REML = FALSE) # best fit
m11 = lmer(peakflr ~ jmeltdate*site + (1|year), data = flrsnow0, REML = FALSE)
m12 = lmer(peakflr ~ jmeltdate*site + (1|year) + (1|site), data = flrsnow0, REML = FALSE)
m13 = lmer(peakflr ~ jmeltdate*site + (1 + site|year), data = flrsnow0, REML = FALSE)

AIC(peakflrlm, m6, m7, m8, m9, m10, m11, m12, m13)
AICc(peakflrlm, m6, m7, m8, m9, m10, m11, m12, m13)
BIC(peakflrlm, m6, m7, m8, m9, m10, m11, m12, m13)

r.squaredGLMM(m6)
r.squaredGLMM(m7) 
r.squaredGLMM(m8) 
r.squaredGLMM(m9) 
r.squaredGLMM(m10) 
r.squaredGLMM(m11) 
r.squaredGLMM(m12) 
r.squaredGLMM(m13) 
# including random effects improves all models

# lastflr ~ jmeltdate model selection ----
# lastflr ~ jmeltdate
lastflrlm = lm(lastflr ~ jmeltdate, data = flrsnow0)
summary(lastflrlm)
anova(lastflrlm)    
# later snowmelt significantly delays last flowering, but to a lesser degree than it delays first flowering. Implies there must be some mechanism for plants to "catch up", OR that flowering just has a hard end date beyond which the plant can't produce more (late season drought? (THIS MAY NOT BE TRUE ANYMORE ONCE 2019 DATA INCLUDED)

# mixed models for meltdate ~ lastflr
m22 = lmer(lastflr ~ jmeltdate + (1|site), data = flrsnow0, REML = FALSE)
m23 = lmer(lastflr ~ jmeltdate + (1 + jmeltdate|site), data = flrsnow0, REML = FALSE)
m24 = lmer(lastflr ~ jmeltdate + (1|year), data = flrsnow0, REML = FALSE)
m25 = lmer(lastflr ~ jmeltdate + (1|site) + (1|year), data = flrsnow0, REML = FALSE)
m26 = lmer(lastflr ~ jmeltdate*year + (1|site), data = flrsnow0, REML = FALSE) # best fit BIC
m27 = lmer(lastflr ~ jmeltdate*site + (1|year), data = flrsnow0, REML = FALSE) # best fit AIC
m28 = lmer(lastflr ~ jmeltdate*site + (1|year) + (1|site), data = flrsnow0, REML = FALSE)
m29 = lmer(lastflr ~ jmeltdate*site + (1 + site|year), data = flrsnow0, REML = FALSE)

AIC(lastflrlm, m22, m23, m24, m25, m26, m27, m28, m29)
AICc(lastflrlm, m22, m23, m24, m25, m26, m27, m28, m29)
BIC(lastflrlm, m22, m23, m24, m25, m26, m27, m28, m29)
# BIC says model 26 is best

r.squaredGLMM(m22)
r.squaredGLMM(m23) 
r.squaredGLMM(m24) 
r.squaredGLMM(m25) 
r.squaredGLMM(m26) 
r.squaredGLMM(m27) 
r.squaredGLMM(m28) 
r.squaredGLMM(m29) 

# population meltdate ~ peak and last flring time graph ----
flrsnowpop = d1 %>% 
  drop_na(meltdate) %>%
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  group_by(site, plot, meltdate, elevation, year) %>% 
  summarise(
    #firstflr = mean(min(jday[is.na(flrs) == FALSE & flrs > 0])),
    lastflr = median(max(date[is.na(flrs) == FALSE & flrs > 0])),
    peakflr = median(date[which(flrs == max(flrs))])
  )

flrsnowpop = flrsnowpop %>%
  gather(key = "variable", value = "value", "peakflr", "lastflr", -site, -plot, -meltdate, -elevation, na.rm=FALSE) %>%
  drop_na(value) %>%
  drop_na(meltdate)

#remove_missing(flrsnow, finite = TRUE)

flrsnowpop$variable <- factor(flrsnowpop$variable, levels = c("peakflr", "lastflr"))

flrsnowpop %>%
  subset(year == 2018) %>%
  ggplot(mapping = aes(meltdate, value, color = variable)) +
  #facet_wrap(~year) +
  ggtitle("Snowmelt date vs flowering phenology") +
  xlab("Snowmelt Date") +
  ylab("Flowering Phenology Date") +
  scale_color_brewer(name="Flowering\nPhenology\nMetric",
                     palette = "Dark2",
                     labels=c("Peak Flowering", "Last Flowering")) +
  theme(legend.title = element_text(size=18),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(method = lm) +
  geom_count(aes(color = variable))
#geom_jitter(aes(color = variable, alpha = 0.1)) +
scale_y_date(limits = as.Date(c("2018-06-25", "2018-09-05")))

flrsnowpop %>%
  subset(year == 2019) %>%
  ggplot(mapping = aes(meltdate, value, color = variable)) +
  #facet_wrap(~year) +
  ggtitle("Snowmelt date vs flowering phenology") +
  xlab("Snowmelt Date") +
  ylab("Flowering Phenology Date") +
  scale_color_brewer(name="Flowering\nPhenology\nMetric",
                     palette = "Dark2",
                     labels=c("Peak Flowering", "Last Flowering")) +
  theme(legend.title = element_text(size=18),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(method = lm) +
  geom_count(aes(color = variable))
#geom_jitter(aes(color = variable, alpha = 0.1)) +
#scale_y_date(limits = as.Date(c("2018-06-25", "2018-09-05")))

# *FOR POSTER* snowmelt timing ~ elevation ----

#geom_point(aes(elevation, firstflr, size = 0.1, color = site), pch = 7)
# take means and make a spline
# this is a great graph for getting across the points I want to make about flowering duration in relation to snowmelt. NOTE: WHEN I ADDED FIRST FLR, I REALIZED I HAD PEAK AND FIRST AS THE SAME DATES FOR SOME POPS. THIS PROBABLY MEANS I ACTUALLY MISSED TRUE PEAK FLOWERING IN ADDITION TO FIRST FLOWERING FOR THOSE POPULATIONS

# graph: boxplot of elevation vs peakflr for individuals 
ggplot(data = flrsnow0, mapping = aes(x = elevation, y = peakflr, color = site)) +
  geom_boxplot()

# panel graph of elevation vs meltdate, temperature, and chilling hours/photoperiod (depending on what you get to) for proposal ----
# graph: elevation vs meltdate ----
# can add first, peak, and last flowering for individuals across populations
flrsnowpop1 = flrsnowpop %>%
  spread(variable, value)

ggplot(data = flrsnowpopmean, aes(x = elevation, y = jmeltdate, color = site, group = year)) +
  #facet_wrap(~year, ncol = 1) +
  geom_point(aes(shape = year), size = 4) +
  #geom_point(aes(elevation, meanpeak)) +
  #geom_errorbar(aes(ymin = lowerpeak, ymax = upperpeak)) +
  #scale_color_continuous(low = "orange", high = "blue") +
  ggtitle("Elevation vs snowmelt date") +
  xlab("Elevation") +
  ylab("Snowmelt date") +
  theme(legend.title = element_text(size=18),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())# +
#coord_flip()

# save graph as a ppt file in directory
graph2ppt(file="elevation vs snowmelt date combined yrs.pptx", width = 9, height = 9, append=TRUE)

# elevation vs temperature ----


# *FOR POSTER*
# anova

#lvtrel = c(2853, 2845, 2808, 2773, 2761, 2756)
#median(lvtrel) # 2790.5

ggplot(data = flrsnow0, aes(x = elevation, y = peakflr)) +
  facet_wrap(~year) +
  geom_jitter(aes(alpha = 0.2, group = site, color = elevation)) +
  scale_color_continuous(low = "orange", high = "blue") +
  geom_smooth(method = lm) +
  theme(axis.line = element_line(colour = "black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold")) +
  ggtitle("Elevation vs peak flowering date") +
  xlab("Elevation") +
  ylab("Peak Flowering Date")

graph2ppt(file="elevation vs peak flowering date.pptx", width = 9, height = 9, append=TRUE)


elevpeakflrlm = lm(peakflr ~ elevation*year, data = flrsnow0)
summary(elevpeakflrlm)
anova(elevpeakflrlm)
# not significant

# Snowmelt timing ~ fitness (2018 only) ----

fitnesssnow = d1 %>% 
  drop_na(jmeltdate) %>%
  drop_na(fruits) %>% # to do a summarize, can't have NAs in column
  group_by(site, plot, year, jmeltdate, elevation, meltdate, uniqueID) %>% 
  summarise(
    fitness = max(fruits)
  )

# graph

fitnesssnow18 = fitnesssnow %>%
  filter(year == 2018)
fitnesssnow18plot = ggplot(data = fitnesssnow18, mapping = aes(meltdate, fitness)) +
  #facet_wrap(~year) +
  geom_smooth(method = lm) +
  geom_jitter(aes(alpha = 0.2, group = site, color = elevation)) +
  scale_color_continuous(low = "orange", high = "blue") +
  ggtitle("Snowmelt date vs fitness") +
  xlab("Snowmelt Date") +
  ylab("Fecundity") +
  theme(legend.title = element_text(size=18),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

fitnesssnow19 = fitnesssnow %>%
  filter(year == 2019)
fitnesssnow19plot = ggplot(data = fitnesssnow19, mapping = aes(meltdate, fitness)) +
  #facet_wrap(~year) +
  geom_smooth(method = lm) +
  geom_jitter(aes(alpha = 0.2, group = site, color = elevation)) +
  scale_color_continuous(low = "orange", high = "blue") +
  ggtitle("Snowmelt date vs fitness") +
  xlab("Snowmelt Date") +
  ylab("Fecundity") +
  theme(legend.title = element_text(size=18),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

require(grid)
grid.arrange(fitnesssnow18plot, fitnesssnow19plot, nrow = 1)
#graph2ppt(file="Snowmelt date vs fitness.pptx", width = 9, height = 9, append=TRUE)

# Fitness ~ snowmelt statistical analyses ----

# fixed effects model & anova
fitnesssnowlm = lm(fitness ~ jmeltdate, data = fitnesssnow)
summary(fitnesssnowlm)
anova(fitnesssnowlm)
# site is important. not sure how to interpret this result

# mixed model
m1 = lmer(fitness ~ jmeltdate + (1 + jmeltdate|year), data = fitnesssnow, REML = FALSE)
m2 = lmer(fitness ~ jmeltdate + (1|site), data = fitnesssnow, REML = FALSE)
m2.5 = lmer(fitness ~ jmeltdate + (1|jmeltdate), data = fitnesssnow, REML = FALSE)
m3 = lmer(fitness ~ jmeltdate + (1|year), data = fitnesssnow, REML = FALSE)
m4 = lmer(fitness ~ jmeltdate + (1 + jmeltdate|year) + (1|site), data = fitnesssnow, REML = FALSE)
m5 = lmer(fitness ~ jmeltdate*year +  (1|site), data = fitnesssnow, REML=FALSE)

display(m1)
display(m2)
AIC(fitnesssnowlm, m1, m2, m2.5, m3, m4, m5)
AICc(fitnesssnowlm, m1, m2, m2.5, m3, m4, m5)
BIC(fitnesssnowlm, m1, m2, m2.5, m3, m4, m5)

# Do we include the random effects or not?
# Notes for me from the help page:
# The marginal R2 value represents the variance explained by the fixed effects, defined as: 
# R_GLMM(m)² = (σ_f²) / (σ_f² + σ_α² + σ_ε²)
# The conditional R2 value is interpreted as the variance explained by the entire model, including both fixed and random effects, and is calculated according to the equation: 
# R_GLMM(c)² = (σ_f² + σ_α²) / (σ_f² + σ_α² + σ_ε²)
# where σ_f² is the variance of the fixed effect components, σ_α² is the variance of the random effects, and σ_ε² is the “observation-level” variance.

r.squaredGLMM(m1) # R2m        R2c
# 0.0560748  0.1495234
r.squaredGLMM(m2) 
r.squaredGLMM(m2.5)
r.squaredGLMM(m3) 
r.squaredGLMM(m4) 
r.squaredGLMM(m5) 
# all models perform better when including random effects

# Fitness ~ days from snowmelt to first flower (2018 -2019) ----

# TALK TO JENNY: HERE I AM REMOVING THOSE COLUMNS WHICH HAVE NAS FOR BOTH FLRS AND FRUITS; HOW DO I SOLVE THIS PROBLEM?

snowfirstflr = d1 %>% 
  drop_na(jmeltdate) %>%
  drop_na(fruits) %>% # to do a summarize, can't have NAs in column
  drop_na(flrs) %>%
  group_by(site, plot, jmeltdate, uniqueID, year) %>% 
  summarise(
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0]),
    lastflr = max(jday[is.na(flrs) == FALSE & flrs > 0]),
    peakflr = median(jday[which(flrs == max(flrs))]),
    flrduration = lastflr - firstflr,
    fitness = max(fruits)
  ) %>%
  mutate(snowtoflr = firstflr - jmeltdate) %>%
  mutate(firstflr = na_if(firstflr, Inf)) %>%
  mutate(firstflr = na_if(firstflr, -Inf)) %>%
  mutate(peakflr = na_if(peakflr, Inf)) %>%
  mutate(peakflr = na_if(peakflr, -Inf)) %>%
  mutate(lastflr = na_if(lastflr, Inf)) %>%
  mutate(lastflr = na_if(lastflr, -Inf))%>%
  mutate(flrduration = na_if(flrduration, Inf)) %>%
  mutate(flrduration = na_if(flrduration, -Inf)) %>%
  drop_na(firstflr) %>%
  drop_na(peakflr) %>%
  drop_na(lastflr) %>%
  drop_na(flrduration) %>%
  drop_na(snowtoflr)

ggplot(data = snowfirstflr, mapping = aes(snowtoflr, fitness)) +
  facet_wrap(~year) +
  geom_smooth(method = lm)
#geom_count()
#geom_line()

# anova
snowfirstflrlm = lm(fitness ~ snowtoflr*year, data = snowfirstflr)
summary(snowfirstflrlm)
anova(snowfirstflrlm)
# relationship between fitness and days from snowmelt to first flowering are totally dependent on year!

# *** flowering duration ~ meltdate ----

# graph of meltdate ~ flowering duration
ggplot(data = snowfirstflr, mapping = aes(jmeltdate, flrduration)) +
  facet_wrap(~year) +
  geom_smooth(method = lm)
#geom_count()

# anova of meltdate ~ flowering duration
meltdatedurationlm = lm(flrduration ~ jmeltdate, data = snowfirstflr)
summary(meltdatedurationlm)
anova(meltdatedurationlm)
# this is a pretty interesting result. Flowering duration is more influenced when snowmelt is later (or is it something else about that year?)

# *** meltdate vs snowtoflr ----
# graph
ggplot(data = snowfirstflr, mapping = aes(jmeltdate, snowtoflr)) +
  facet_wrap(~year) +
  geom_smooth(method = lm)

meltdatesnowtoflrlm = lm(snowtoflr ~ jmeltdate, data = snowfirstflr)
summary(meltdatesnowtoflrlm)
anova(meltdatesnowtoflrlm)
AIC(meltdatesnowtoflrlm)
# flower much faster when snow melts later. This could either be developmental (growth is faster in warmer temps) or due to anticipatory/photoperiod cuing to synchronize flowering with other plants.

# Flowering duration ~ fitness (2017 & 2018) ----

# use d2 so include zeros for #s of reproductive structures
# start with just 2018
phentimeline = d2 %>% 
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  drop_na(fruits) %>%
  group_by(site, plot, year, elevation, uniqueID) %>% 
  summarise(
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0]),
    lastflr = max(jday[is.na(flrs) == FALSE & flrs > 0]),
    flrduration = lastflr - firstflr,
    branchiness = max(totalbranch),
    peakflr = median(jday[which(flrs == max(flrs))]),
    fitness = max(fruits)
  ) %>%
  mutate(firstflr = na_if(firstflr, Inf)) %>%
  mutate(firstflr = na_if(firstflr, -Inf)) %>%
  mutate(peakflr = na_if(lastflr, Inf)) %>%
  mutate(peakflr = na_if(lastflr, -Inf)) %>%
  mutate(flrduration = na_if(flrduration, Inf)) %>%
  mutate(flrduration = na_if(flrduration, -Inf)) %>%
  drop_na(firstflr) %>%
  drop_na(peakflr) %>%
  drop_na(lastflr) %>%
  unite(site_plot, site, plot, sep = "_", remove = FALSE) #%>%
drop_na(flrduration) %>%
  mutate(snowtoflr = firstflr - meltdate) %>%
  drop_na(snowtoflr)

# *** visualizing flowering duration ----
# timelineS is a useful package that creates timelines: https://www.rdocumentation.org/packages/timelineS/versions/0.1.1
#install.packages("timelineS") 
library("timelineS")

## All individuals

# graph for sites (with all individuals included; firstflr here is the first time any individual flowered in the pop)
timelineG(df = phentimeline, start = "firstflr", end = "lastflr", names = "site_plot", group1 = "year")

# graph across all individuals
timelineG(df = phentimeline, start = "firstflr", end = "lastflr", names = "uniqueID", group1 = "site", group2 = "year")

# graph flring duration ~ fitness

ggplot(data = phentimeline, mapping = aes(flrduration, fitness, color = site)) +
  facet_wrap(~year) +
  geom_count()
#geom_line()
#geom_smooth()

# anova
flrdurationfitnesslm = lm(flrduration ~ fitness, data = phentimeline)
summary(flrdurationfitnesslm)
anova(flrdurationfitnesslm)
# longer flowering duration significantly increases fitness


## Population-level means for flowering phenology metrics

popsnowfirstflr = snowfirstflr %>% 
  group_by(site, meltdate) %>% 
  drop_na(meltdate) %>%
  summarise(
    firstflr = mean(firstflr),
    lastflr = mean(lastflr),
    peakflr = mean(peakflr),
    flrduration = mean(lastflr - firstflr),
    fitness = mean(fitness)
  )

popsnowfirstflr <- popsnowfirstflr %>% 
  mutate(snowtoflr = firstflr - meltdate) %>%
  drop_na(meltdate)

# population level timeline
# include elevation later?
# don't include meltdate yet
popphentimeline = phentimeline %>% 
  group_by(site, plot, year) %>% 
  #drop_na(meltdate) %>%
  summarise(
    firstflr = median(firstflr),
    lastflr = median(lastflr),
    peakflr = median(peakflr),
    flrduration = median(lastflr - firstflr),
    fitness = mean(fitness)
  ) %>%
  unite(site_plot, site, plot, sep = "_", remove = FALSE)

popphentimeline <- popphentimeline %>% 
  mutate(snowtoflr = firstflr - meltdate) %>%
  drop_na(meltdate)

timelineG(df = popphentimeline, start = "firstflr", end = "lastflr", names = "site_plot", group1 = "year") 



# Herbivory ~ phenology ----

herbphen = d1 %>% 
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  drop_na(fruits) %>%
  group_by(year, site, plot, elevation, jmeltdate, uniqueID) %>% 
  summarise(
    firstflr = min(jday[is.na(flrs) == FALSE]),
    lastflr = max(jday[is.na(flrs) == FALSE]),
    peakflr = min(jday[which(flrs == max(flrs))]),
    fitness = max(fruits),
    flrduration = lastflr - firstflr,
    branchiness = max(totalbranch),
    herbpres = max(anyherb), # will be 1 if herb; 0 if not
    firstherb = min(jday[which(anyherb > 0)])
  ) %>%
  mutate(firstherb = na_if(firstherb, Inf))

# *** presence of herbivory ~ flowering phenology ----

# graph
ggplot(data = herbphen, mapping = aes(herbpres, lastflr, color = site, group = site))     +
  #geom_count() +
  geom_smooth(method = lm) +
  facet_wrap(~year)

# anova
herbpreslastflrlm = lm(herbpres ~ lastflr, data = herbphen)
summary(herbpreslastflrlm)
anova(herbpreslastflrlm)

# *** population level presence of herbivory ~ flowering phenology

herbphenpop = herbphen %>% 
  group_by(year, site, plot, elevation, jmeltdate) %>% 
  summarise(
    herbprespop = mean(herbpres), 
    firstflrpop = mean(firstflr),
    peakflrpop = mean(peakflr),
    lastflrpop = mean(lastflr),
    flrdurationpop = mean(flrduration)
  ) #%>%

# graph

ggplot(data = herbphenpop, mapping = aes(herbprespop, flrdurationpop)) +
  geom_smooth(method = lm) +
  geom_point(aes(color = site, group = site)) +
  facet_wrap(~year)

# *** presence of herbivory ~ flowering duration ----

# graph
ggplot(data = herbphen, mapping = aes(herbpres, flrduration, color = site, group = site))     +
  geom_smooth(method = lm) +
  facet_wrap(~year)

# anova
herbpresflrdurationlm = lm(herbpres ~ flrduration, data = herbphen)
summary(herbpresflrdurationlm)
anova(herbpresflrdurationlm)
# lengthens flowering duration a little bit but not by much, and not significantly

# Herbivory ~ fitness ----
# *** presence of herbivory ~ fitness ----

# graph
ggplot(data = herbphen, mapping = aes(herbpres, fitness, color = site))     +
  geom_smooth(method = lm) +
  facet_wrap(~year)

# anova
herbphen18 = herbphen %>% filter(year == 2018)
herbpresfitnesslm18 = lm(herbpres ~ fitness, data = herbphen18)
summary(herbpresfitnesslm18)
anova(herbpresfitnesslm18)
# SUPER different results depending on the year. In 2017, herbivory depresses fitness, but in 2018, it increases it mostly because of LVTR2

# *** timing of first herbivory ~ fitness ----

# graph
ggplot(data = herbphen, mapping = aes(firstherb, fitness, color = site))     +
  geom_count() +
  facet_wrap(~year)

# anova
firstherbfitnesslm = lm(firstherb ~ fitness + year, data = herbphen)
summary(firstherbfitnesslm)
anova(firstherbfitnesslm)
# not sure how to interpret these stats

# Snowmelt timing ~ herbivory (2018 only) ----

snowherb = d1 %>% 
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  drop_na(fruits) %>%
  drop_na(jmeltdate) %>%
  group_by(site, plot, jmeltdate, elevation, uniqueID) %>% 
  summarise(
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0]),
    lastflr = max(jday[is.na(flrs) == FALSE & flrs > 0]),
    peakflr = min(jday[which(flrs == max(flrs))]),
    fitness = max(fruits),
    flrduration = lastflr - firstflr,
    branchiness = max(totalbranch),
    herbpres = max(anyherb), # will be 1 if herb; 0 if not
    firstherb = min(jday[which(anyherb > 0)]),
    midherb = median(jday[which(anyherb > 0)]) # median date of herbivory occurrence()
  ) %>%
  mutate(firstherb = na_if(firstherb, Inf)) %>%
  mutate(midherb = na_if(midherb, Inf)) %>%
  mutate(firstflr = na_if(firstflr, Inf)) %>%
  mutate(peakflr = na_if(peakflr, Inf)) %>%
  mutate(lastflr = na_if(lastflr, Inf))


snowherbpop = d1 %>% 
  drop_na(flrs) %>% # to do a summarize, can't have NAs in column
  drop_na(fruits) %>%
  drop_na(jmeltdate) %>%
  group_by(site, plot, jmeltdate, elevation) %>% 
  summarise(
    firstflr = mean(min(jday[is.na(flrs) == FALSE & flrs > 0])),
    lastflr = mean(max(jday[is.na(flrs) == FALSE & flrs > 0])),
    peakflr = mean(min(jday[which(flrs == max(flrs))])),
    fitness = mean(max(fruits)),
    flrduration = mean(lastflr - firstflr),
    branchiness = mean(max(totalbranch)),
    herbpres = mean(anyherb), # will be 1 if herb; 0 if not
    firstherb = mean(min(jday[which(anyherb > 0)])),
    midherb = mean(median(jday[which(anyherb > 0)])) # median date of herbivory occurrence()
    
    # *FOR POSTER* snowmelt ~ presence of herbivory (NOT SIGNIFICANT) ----
  ) %>%
  mutate(firstherb = na_if(firstherb, Inf)) %>%
  mutate(midherb = na_if(midherb, Inf)) %>%
  drop_na(jmeltdate)

ggplot(data = snowherb, mapping = aes(jmeltdate, herbpres)) +
  ggtitle("Snowmelt date vs presence of herbivory") +
  xlab("Snowmelt Date") +
  ylab("Presence of herbivory") +
  #scale_color_brewer(name="Count",
  #                   palette = "Dark2") +
  theme(legend.title = element_text(size=18),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(method = lm) +
  geom_count()

graph2ppt(file="snowmelt vs presence of herbivory.pptx", width = 9, height = 9, append=TRUE)

# anova
## LOGISTIC REGRESSION FOR BINARY DATA
snowherbpreslm = lm(jmeltdate ~ herbpres, data = snowherb)
summary(snowherbpreslm)
anova(snowherbpreslm)
# meltdate does not affect likelihood of herbivory

# *** snowmelt ~ firstherb ----

# graph
ggplot(data = snowherb, mapping = aes(jmeltdate, firstherb)) +
  geom_smooth()

# anova
snowfirstherblm = lm(jmeltdate ~ firstherb, data = snowherb)
summary(snowfirstherblm)
anova(snowfirstherblm)
# later snowmelt significantly delays first occurrence of herbivory

# *** snowmelt*firstherb ~ fitness ----

# anova
snowfirstherbfitnesslm = lm(jmeltdate*firstherb ~ fitness, data = snowherb)
summary(snowfirstherbfitnesslm)
anova(snowfirstherbfitnesslm)
# no significant effects of interaction between snowmelt date and date of first herbivory occurrence on fitness (fecundity)

# *** snowmelt*midherb ~ fitness ----

# anova
snowmidherbfitnesslm = lm(jmeltdate*midherb ~ fitness, data = snowherb)
summary(snowmidherbfitnesslm)
anova(snowmidherbfitnesslm)
# no significant effects of interaction between snowmelt date and median date of herbivory occurrence on fitness (fecundity)

# *** snowmelt*herbpres ~ fitness ----

snowherbpresfitnesslm = lm(jmeltdate*herbpres ~ fitness, data = snowherb)
summary(snowherbpresfitnesslm)
anova(snowherbpresfitnesslm)
