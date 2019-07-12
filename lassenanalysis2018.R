setwd("~/Box Sync/Graduate School/PhD Research/Lassen Snowmelt & Phenology/Data")

####### Old analysis code #######


#nearly the same as code from 2017; see lassenanalysis2017.R for commented code
phen=read.csv("phen2018_cleaning.csv")

library(reshape2)
library(tidyverse)
library(reshape2)
library(plyr)
library(tidyr)

phen <- subset(phen, phen.1!= "D")
print (phen)

phenm=melt(phen,id.vars=c("uniqueID","site","emerg.date", "plot", "quad","ind"),measure.vars = c(grep("^flrs",names(phen),value = TRUE),grep("^buds",names(phen),value = TRUE),grep("^fruits",names(phen),value = TRUE)))

#colnames(phenm)[7] <- c('date')
names(phenm)
phenm$date
head(phenm)
dim(phenm)

phenm$site=factor(phenm$site,levels=c("LV3","LV2","LV1","LVTR1","LVTR2","LVTR2.5"))

phenm$date <- sapply(strsplit(as.character(phenm$variable), split='_', fixed=TRUE), function(x) (x[2]))
phenm$date <- as.Date(phenm$date,"%m.%d.%y")
phenm$repro <- sapply(strsplit(as.character(phenm$variable), split='_', fixed=TRUE), function(x) (x[1]))
phenm$repro=as.factor(phenm$repro)

phenm$value=as.numeric(phenm$value)

ggplot(phenm,aes(x=date,y=value,group=repro,color=repro)) +
  ylim(0,12) +
  scale_color_manual(values=c('seagreen','orange','orchid4')) +
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar",width=.5) +
  stat_summary(fun.y=mean, geom="point") +
  facet_wrap(~site)



###### Work done starting 2/18/2019 ######




#### Master data file: cleaning and formatting ####


## Adding even-numbered trips to master datasheet ###
#First, I entered data for odd-numbered trips (1, 3, 5, & 7) to do the above data analysis
#Danielle Davisson and Ylana Nguyen helped enter data for even-numbered trips

#need to merge by uniqueID
#first, do some cleaning of the master data file within R
phen2018 = read.csv("lassenphendata2018_cleaning_ES20190218.csv")
phen2018 = subset(phen2018, phen_01.07.2018!= "D" | is.na(phen_01.07.2018)) #remove plants that did not perennate to simplify data visualization and analysis
#library(dplyr)
phen2018 = phen2018 %>% select(-starts_with("notes"),-starts_with("protocol"),-c("itero.per", "emerg.date"))
#note - trip 3 2018 has a few plants with herb type "3" (pods herbivorized)

#merge trip 2
trip2 = read.csv("2018trip2_cleaned_ES20190218.csv")
trip2 = subset(trip2, phen_10.07.2018!= "D" | is.na(phen_10.07.2018))

fullphen2018 = merge(phen2018, trip2, by.x="uniqueID", by.y="uniqueID.2", all.x=TRUE, all.y=TRUE)
write.csv(fullphen2018, '2018data.csv')

df18 = read.csv("2018data_cleaned.csv")
#df18 = df18 %>% select(-"X") #after merge may need to remove first column

## TRIP 6 WAS MISLABELLED AS TRIP 4 -- IGNORE THIS PART
#adding trip 4
trip4 = read.csv("trip4_cleaning_ES20190219.csv")
trip4 = trip4 %>% select(-starts_with("notes"),-starts_with("protocol"),-starts_with("ind"),-starts_with("emerg")-starts_with("itero")-starts_with("site")-starts_with("protocol")-starts_with("plot"),-starts_with("protocol"),-starts_with("protocol"))

df18_w.4 = merge(df18, trip4, by.x="uniqueID", by.y="uniqueID.4", all.x=TRUE, all.y=TRUE)
write.csv(df18_w.4, '2018data_w.4.csv')

df18 = read.csv("2018data_w.4.csv")
df18 = df18 %>% select(-"X") #after merge may need to remove first column

#adding trip 6
trip6 = read.csv("trip6.csv")
trip6 = trip6 %>% select(-starts_with("notes"),-starts_with("protocol"),-starts_with("ind"),-starts_with("emerg")-starts_with("itero")-starts_with("site")-starts_with("protocol")-starts_with("plot"))
trip6=trip6 %>% select(-"X")

df18_w.6 = merge(df18, trip6, by.x="uniqueID", by.y="uniqueID.6", all.x=TRUE, all.y=TRUE)
write.csv(df18_w.6, '2018data_w.6.csv')

df18 = read.csv("2018data_w.6.csv")
df18 = df18 %>% select(-"X") #after merge may need to remove first column
df18 = df18 %>% select(-"emerg.date")

df18$site=df18$site.x
df18$quad=df18$quad.x 
df18$plot=df18$plot.x

df18 = df18 %>% select(-"site.x", -"site.y", -"quad.x", -"quad.y", -"plot.x", -"plot.y")
df18 = df18 %>% select(-starts_with("emerg.date"), -starts_with("date"))
df18 = df18 %>% select(-"site_27.07.2018", -"plot_27.07.2018",-"quad_27.07.2018")

############ ALL INDIVIDUALS ###############


######  Data formatting ######

df18m=melt(df18,id.vars=c("uniqueID", "site", "plot", "quad", "ind", "phen"),measure.vars = c(grep("^flrs",names(df18),value = TRUE),grep("^buds",names(df18),value = TRUE),grep("^fruits",names(df18),value = TRUE), grep("^herb",names(df18),value = TRUE))) #changed data from wide to long format. measure.vars are counted in the value column.

df18m$date <- sapply(strsplit(as.character(df18m$variable), split='_', fixed=TRUE), function(x) (x[2])) #this turns i.e. "date.1" into "date_06-30-17"
df18m$date <- as.Date(df18m$date,"%d.%m.%Y") #tells R to recognize this column as dates, and what format the dates are in to begin with
df18m$var <- sapply(strsplit(as.character(df18m$variable), split='_', fixed=TRUE), function(x) (x[1])) #making a vector of the "_flrs" (or buds, fruits) portion of the "date_flrs" column and splitting by the "_"
df18m$var=as.factor(df18m$var) #now, putting the vector into its own column, called "repro"

df18m$value=as.numeric(df18m$value)

#Split variable column into four separate columns for each type of variable

df18c = dcast(df18m, uniqueID + site + plot + quad + date + ind ~ var, value.var = 'value', fun.aggregate=max) #dcast used for data frame output (vs. acast for matrix/vector output)

df18c$herb = as.factor(df18c$herb)
df18c$date = as.Date(df18c$date, "%m/%d/%y")

df18c = df18c %>% mutate (melt_date = case_when(
  df18c$site=="LVTR2.5" ~ "NA",
  df18c$site=="LVTR2" ~ "01_06_2018",
  df18c$site=="LVTR1" ~ "01_06_2018",
  df18c$site=="LV1" ~ "20_06_2018",
  df18c$site=="LV2" & df18c$plot==1 ~ "29_05_2018",
  df18c$site=="LV2" & df18c$plot==2 ~ "21_05_2018",
  df18c$site=="LV3" & df18c$plot==1 ~ "17_05_2018",
  df18c$site=="LV3" & df18c$plot==1.5 ~ "03_05_2018"))

#write.csv(df18c, "df18c.csv")

#df18c = read.csv("df18c.csv")
df18c$melt_date = as.Date(df18c$melt_date,"%d_%m_%Y")
df18c = subset(df18c, uniqueID != "LV3 P2 QALL GONE; BANK ERODED AWAY #NA") #got rid of some useless rows
#df18c = df18c %>% select(-"X")

########## MAX FRUIT (ie FECUNDITY) ###########

## First, remove individuals that never made buds (did not attempt to reproduce) ##

df18$buds[df18c$buds = NA] = 0

df18r = df18 %>% mutate (repro = ifelse (buds_01.07.2018=="0"& buds_05.08.2018=="0" & buds_10.07.2018=="0" & buds_27.08.2018=="0" & buds_18.07.2018=="0" & buds_27.07.2018=="0", T, F))

#df18r = df18 %>% mutate (repro = ifelse (buds_01.07.2018=NA & buds_05.08.2018 = NA & buds_10.07.2018=NA & buds_27.08.2018=NA & buds_18.07.2018=NA & buds_27.07.2018=NA, T, F))

df18r = subset(df18r, is.na(repro)==FALSE) 
df18r = df18r %>% select(-"repro")

df18mr=melt(df18r,id.vars=c("uniqueID", "site", "plot", "quad", "ind"),measure.vars = c(grep("^flrs",names(df18r),value = TRUE),grep("^buds",names(df18r),value = TRUE),grep("^fruits",names(df18r),value = TRUE), grep("^herb",names(df18r),value = TRUE))) #changed data from wide to long format. measure.vars are counted in the value column.

df18mr$date <- sapply(strsplit(as.character(df18mr$variable), split='_', fixed=TRUE), function(x) (x[2])) #somehow I think this turns i.e. "date.1" into "date_06-30-17", but not sure how
df18mr$date <- as.Date(df18mr$date,"%d.%m.%Y") #tells R to recognize this column as dates, and what format the dates are in to begin with
df18mr$var <- sapply(strsplit(as.character(df18mr$variable), split='_', fixed=TRUE), function(x) (x[1])) #making a vector of the "_flrs" (or buds, fruits) portion of the "date_flrs" column and splitting by the "_"
df18mr$var=as.factor(df18mr$var) #now, putting the vector into its own column, called "repro"

df18mr$value=as.numeric(df18mr$value)

df18cr = dcast(df18mr, uniqueID + site + plot + quad + date + ind ~ var, value.var = 'value', fun.aggregate=max) #Split variable column into four separate columns for each type of variable

df18cr$herb = as.factor(df18cr$herb)

#df18cr[df18cr == 0] <- NA
df18cr$fruits[is.na(df18cr$fruits)] <- 0
df18cr$flrs[is.na(df18cr$flrs)] <- 0
df18cr$buds[is.na(df18cr$buds)] <- 0

#### Using repro only, plot individuals' # flrs over time ####

ggplot(data = df18cr) +
  geom_point(mapping = aes(x=date, y=flrs, group = uniqueID, color = site)) +
  geom_jitter(mapping = aes(x=date, y=flrs, group = uniqueID, color = site)) +
  geom_line(mapping = aes(x=date, y=flrs, group = uniqueID, color = site)) +
  facet_wrap(~site)

# ^ that was a little too hard to visualize - try looking at means:

df18cr = df18cr %>% mutate (elevation = case_when(
  #site=="LVTR2.5" ~ "NA",
  site=="LVTR2" ~ 2795,
  site=="LVTR1" ~ 2756.5,
  site=="LV3" ~ 2353,
  site=="LV2" ~ 2500,
  site=="LV1" ~ 2593))

ggplot(data=df18cr) +
  geom_point(mapping=aes(x=elevation, y=melt_date, group=plot, color=site, size=0.1)) +
  theme(text = element_text(size=20))

meanrepro = ddply(df18cr,.(site, date, melt_date), summarise,
                  flrs = mean(flrs),
                  buds = mean(buds),
                  fruits = mean(fruits))

meanrepro = meanrepro %>% mutate (elevation = case_when(
  #site=="LVTR2.5" ~ "NA",
  site=="LVTR2" ~ 2795,
  site=="LVTR1" ~ 2756.5,
  site=="LV3" ~ 2353,
  site=="LV2" ~ 2500,
  site=="LV1" ~ 2593))

ggplot(data=meanrepro) +
  geom_point(mapping=aes(x=elevation, y=melt_date, color="site")) +
  theme(text = element_text(size=20))

ggplot(data = meanrepro) +
  geom_line(mapping=aes(x=date, y=flrs, color="red")) +
  geom_line(mapping=aes(x=date, y=buds, color="green")) +
  geom_line(mapping=aes(x=date, y=fruits, color="blue")) +
  facet_wrap(~site)

#df18cr = subset(df18cr, herb!= "NA")

## Add melt date to dataframe

df18cr = df18cr %>% mutate (melt_date = case_when(
  df18cr$site=="LVTR2.5" ~ "NA",
  df18cr$site=="LVTR2" ~ "01_06_2018",
  df18cr$site=="LVTR1" ~ "01_06_2018",
  df18cr$site=="LV1" ~ "20_06_2018",
  df18cr$site=="LV2" & df18cr$plot==1 ~ "29_05_2018",
  df18cr$site=="LV2" & df18cr$plot==2 ~ "21_05_2018",
  df18cr$site=="LV3" & df18cr$plot==1 ~ "17_05_2018",
  df18cr$site=="LV3" & df18cr$plot==1.5 ~ "03_05_2018"))

fecundity = ddply(df18cr,.(site, plot, melt_date, uniqueID), summarise,
                 fecundity = max(fruits),
                 herb = herb)

## After many tries, decided to drop the NAs in column manually...
#maxfruit = maxfruit[-c(3,5,7,9,10,11,13,15,19,28,31,32,33,34,41,42,47,48,49,50,54,55,56,58,59,60,61,67,73,74,75,76,79,80,84,88,90,91,92,93,94,97,101,104,106,107,108,115,123,125,130,136,137,140,141,142,143,145,146,148,149,151,152,153,154,155,156,160,166,171,172,173,174,176,177,181,183,184,189,190,191,192,193,194,195,198,199,203,204,205,209,212,213,214,215,217,222,228,241,247,261,267,268,269,270,271,272,281,283,285,287,289,291), ]

fecundity$Jmelt_date <- as.POSIXlt(fecundity$melt_date, format="%d_%m_%Y")$yday

ggplot(data = fecundity) +
  geom_point(mapping = aes(x=Jmelt_date, y=fecundity, group = uniqueID, color = site, size=0.1)) +
  geom_jitter(mapping = aes(x=Jmelt_date, y=fecundity, group = uniqueID, color = site, size =0.1)) +
  theme(text = element_text(size=20))

fecunditymodel = lm(fecundity ~ Jmelt_date, data=fecundity)
summary(fecunditymodel)
anova(fecunditymodel)

### Trying to add herbivory to repro only inds

df18cr = df18cr %>% mutate (type1herb = case_when(
  df18cr$herb=="0" ~0,
  df18cr$herb=="1" ~1,
  df18cr$herb=="1.5" ~1,
  df18cr$herb=="2" ~0,
  df18cr$herb=="3" ~0
))

#df18ch$type1herb = as.factor(df18ch$type1herb)
#levels(df18ch$type1herb)

#type 2 herbivory
df18cr = df18cr %>% mutate (type2herb = case_when(
  df18cr$herb=="0" ~0,
  df18cr$herb=="1" ~0,
  df18cr$herb=="1.5" ~1,
  df18cr$herb=="2" ~1,
  df18cr$herb=="3" ~0
))

df18cr$type1herb[is.na(df18cr$type1herb)] <- 0
df18cr$type2herb[is.na(df18cr$type2herb)] <- 0

df18cr = df18cr %>% select(-c("buds","flrs","herb","ind","quad"))

df18cr$melt_date <- as.POSIXlt(df18cr$melt_date, format="%d_%m_%Y")$yday

df18cr$date <- as.POSIXlt(df18cr$date, format="%d_%m_%Y")$yday

df18cr = df18cr %>% mutate (plottotal = case_when(
  site=="LVTR2.5" ~ sum(with(df18cr, site == "LVTR2.5")),
  site=="LVTR2" ~ sum(with(df18cr, site == "LVTR2")),
  site=="LVTR1" ~ sum(with(df18cr, site == "LVTR1")),
  site=="LV3" & df18cr$plot==1.0 ~ sum(with(df18cr, site == "LV3" & plot==1.0)),
  site=="LV3" & df18cr$plot==1.5 ~ sum(with(df18cr, site == "LV3" & plot==1.5)),
  site=="LV2" & df18cr$plot==1 ~ sum(with(df18cr, site == "LV2" & plot==1)),
  site=="LV2" & df18cr$plot==2 ~ sum(with(df18cr, site == "LV2" & plot==2)),
  site=="LV1" & df18cr$plot==1 ~ sum(with(df18cr, site == "LV1" & plot==1)),
  site=="LV1" & df18cr$plot==2 ~ sum(with(df18cr, site == "LV1" & plot==2))))

df18cr = df18cr %>% mutate (perc1 = sum(type1herb)/plottotal)

fecundherb = ddply(df18cr,.(site, plot, melt_date, uniqueID), summarise,
                 perc1 = sum(type1herb)/plottotal,
                 perc2 = sum(type2herb)/plottotal,
                 fecundity = max(fruits))

###### Max fruit time #######

df18c$fruits[df18c$fruits == 0] = NA

maxfruit = ddply(df18c,.(site, plot, melt_date, uniqueID), summarise,
                     maxfruit = max(date[is.na(flrs)==FALSE]))

## After many tries, decided to drop the NAs in the maxfruit column manually...
maxfruit = maxfruit[-c(3,5,7,9,10,11,13,15,19,28,31,32,33,34,41,42,47,48,49,50,54,55,56,58,59,60,61,67,73,74,75,76,79,80,84,88,90,91,92,93,94,97,101,104,106,107,108,115,123,125,130,136,137,140,141,142,143,145,146,148,149,151,152,153,154,155,156,160,166,171,172,173,174,176,177,181,183,184,189,190,191,192,193,194,195,198,199,203,204,205,209,212,213,214,215,217,222,228,241,247,261,267,268,269,270,271,272,281,283,285,287,289,291), ]

maxfruit$Jmelt_date <- as.POSIXlt(maxfruit$melt_date, format="%d_%m_%Y")$yday

maxfruit$Jmaxfruit <- as.POSIXlt(maxfruit$maxfruit, format="%d_%m_%Y")$yday

ggplot(data = maxfruit) +
  geom_point(mapping = aes(x=Jmelt_date, y=Jmaxfruit, group = uniqueID, color = site)) +
  geom_jitter(mapping = aes(x=Jmelt_date, y=Jmaxfruit, group = uniqueID, color = site))

indmaxfruitmodel = lm(Jmelt_date ~Jmaxfruit, data=maxfruit)
summary(indmaxfruitmodel)
anova(indmaxfruitmodel)

##### First flowering date (all individuals) #####

## THIS MAY NOT BE A GREAT METRIC BECAUSE FOR SOME POPS, DID NOT NECESSARILY  CATCH real FIRST FLOWER FOR MANY OR ALL INDIVIDUALS

df18c$flrs[df18c$flrs == 0] = NA

df18cflrdate = ddply(df18c,.(site, plot, melt_date, uniqueID), summarise,
      firstflr = min(date[is.na(flrs)==FALSE]))

## All of the following lines are failed attempts to remove the NA values in the firstflr column :(

#df18cflrdate = df18cflrdate %>% drop_na(firstflr)
#df18cflrdate = subset(df18cflrdate, is.na(firstflr) == FALSE)
#df18cflrdate = df18cflrdate %>% drop_na(firstflr)
#df18cflrdate = subset(df18cflrdate, firstflr != NA)

#this one worked for the melt_date column (there are no values for LVTR2.5 because that ibutton was lost)
df18cflrdate = df18cflrdate %>% drop_na(melt_date)

## After many tries, decided to drop the NAs in the firstflr column manually...
df18cflrdate = df18cflrdate[-c(3,5,7,9,10,11,13,15,19,28,31,32,33,34,41,42,47,48,49,50,54,55,56,58,59,60,61,67,73,74,75,76,79,80,84,88,90,91,92,93,94,97,101,104,106,107,108,115,123,125,130,136,137,140,141,142,143,145,146,148,149,151,152,153,154,155,156,160,166,171,172,173,174,176,177,181,183,184,189,190,191,192,193,194,195,198,199,203,204,205,209,212,213,214,215,217,222,228,241,247,261,267,268,269,270,271,272,281,283,285,287,289,291), ]

ggplot(data = df18cflrdate) +
  #geom_point(mapping = aes(x=melt_date, y=firstflr, group = uniqueID, color = site, size=0.05)) +
  geom_jitter(mapping = aes(x=melt_date, y=firstflr, group = uniqueID, color = site, size=0.1, stroke=0)) +
  theme(text = element_text(size=20))

#boxplot(df18cflrdate$melt_date, df18cflrdate$first_flr)

#summarize by condensing according to plot rather than uniqueID
meanfirstflr = ddply(df18cflrdate,.(site, melt_date, plot), summarise,
  meanflr = mean(firstflr))

ggplot(data = meanfirstflr) +
  geom_point(mapping = aes(x=melt_date, y=meanflr, group = plot, color= site, size = 0.1, stroke = 0)) +
  theme(text = element_text(size=20))

  #geom_jitter(mapping = aes(x = melt_date, y = meanflr, group = plot, color = plot))

meanfirstflr$Jmelt_date <- as.POSIXlt(meanfirstflr$melt_date, format="%d_%m_%Y")$yday

meanfirstflr$Jmeanflr <- as.POSIXlt(meanfirstflr$meanflr, format="%d_%m_%Y")$yday

df18cflrdate$Jfirstflr <- as.POSIXlt(df18cflrdate$firstflr, format="%d_%m_%Y")$yday

df18cflrdate$Jmelt_date <- as.POSIXlt(df18cflrdate$melt_date, format="%d_%m_%Y")$yday

###### Linear models for first flowering date (all inds) #####

# Something weird here - can I use dates as formatted, or do I need Julian dates?
meanflrmodel = lm(Jmelt_date ~ Jmeanflr, data=meanfirstflr)
indflrmodel = lm(Jmelt_date ~Jfirstflr * site, data=df18cflrdate)
summary(indflrmodel)
anova(indflrmodel)
summary(meanflrmodel)
anova(meanflrmodel)

  ### If code worked, would want to evaluate significance using this code:
indsum = summary(indflrmodel)
indcoeffs = indsum$coefficients #... and so on from example below:

  ### Example code for finding t stat & p vals: http://r-statistics.co/Linear-Regression.html

#modelSummary <- summary(linearMod)  # capture model summary as an object
#modelCoeffs <- modelSummary$coefficients  # model coefficients
#beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
#std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
#t_value <- beta.estimate/std.error  # calc t statistic
#p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
#f_statistic <- linearMod$fstatistic[1]  # fstatistic
#f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
#model_p <- pf(f[1], f[2], f[3], lower=FALSE)


##### First fruit date (all individuals) ####


df18c$fruits[df18c$fruits == 0] = NA

df18cfruitdate = ddply(df18c,.(site, plot, melt_date, uniqueID), summarise,
  firstfruit = min(date[is.na(fruits)==FALSE]))

df18cfruitdate$Jfirstfruit <- as.POSIXlt(df18cfruitdate$firstfruit, format="%d_%m_%Y")$yday

df18cfruitdate$Jmelt_date <- as.POSIXlt(df18cfruitdate$melt_date, format="%d_%m_%Y")$yday

#df18cfruitdate = df18cfruitdate %>% drop_na(firstfruit)

df18cfruitdate = df18cfruitdate[-c(1,2,3,5,7,10,13,15,19,21,28,29,31,32,33,34,41,42,47,48,49,50,54,55,56,58,59,60,61,67,73,74,75,76,28,79,88,90,91,92,93,94,96,97,99,101,104,106,107,108,109,115,127,129,130,133,134,136,137,138,140,141,142,145,146,148,149,151,152,153,154,155,156,157,171,172,173,176,177,181,183,184,189,190,191,192,193,194,198,199,203,204,205,209,212,213,214,215,217,222,224,228,241,247,261,267,268,269,270,271,272,281,282,283,285,287,289,290), ]

ggplot(data = df18cfruitdate) +
  geom_point(mapping = aes(x=melt_date, y=firstfruit, group = uniqueID, color=uniqueID)) +
  geom_jitter(mapping = aes(x=melt_date, y=firstfruit, group = uniqueID, color=uniqueID))
  #facet_wrap(~site)

ggplot(data = df18cfruitdate) +
  geom_point(mapping = aes(x=melt_date, y=firstfruit, group = plot, color=site)) +
  geom_jitter(mapping = aes(x=melt_date, y=firstfruit, group = plot, color=site))

########  Max flowering (all individuals) #########

maxflrdate = ddply(df18c,.(site, plot, melt_date, uniqueID), summarise,
                       maxflr = max(date[is.na(flrs)==FALSE]))

maxflrdate = maxflrdate %>% drop_na(melt_date)


# same issue as previously - can't figure out how to drop NAs
#maxflrdate = maxflrdate %>% drop_na(maxflr)
#maxflrdate = subset(maxflrdate, is.na(maxflr) == FALSE)
#df18cflrdate = df18cflrdate %>% drop_na(firstflr)
#df18cflrdate = subset(df18cflrdate, firstflr != NA)

maxflrdate = maxflrdate[-c(3,5,7,9,10,11,13,15,19,28,31,32,33,34,41,42,47,48,49,50,54,55,56,58,59,60,61,67,73,74,75,76,79,80,84,88,90,91,92,93,94,97,101,104,106,107,108,115,123,125,130,136,137,140,141,142,143,145,146,148,149,151,152,153,154,155,156,160,166,171,172,173,174,176,177,181,183,184,189,190,191,192,193,194,195,198,199,203,204,205,209,212,213,214,215,217,222,228,241,247,261,267,268,269,270,271,272,281,283,285,287,289), ]

##### ADD JULIAN DATE ######

ggplot(data = maxflrdate) +
  geom_point(mapping = aes(x=melt_date, y=maxflr, group = uniqueID, color = site)) +
  geom_jitter(mapping = aes(x=melt_date, y=maxflr, group = uniqueID, color = site))


# Mean max flr date by plot
meanmaxflr = ddply(maxflrdate,.(site, melt_date, plot), summarise,
                     meanmaxflr = mean(maxflr))

ggplot(data = meanmaxflr) +
  geom_point(mapping = aes(x=melt_date, y=meanmaxflr, group = plot, color= site))

#### Linear models of max flr date ####

meanmaxmodel = lm(melt_date ~ meanmaxflr, data=meanmaxflr)
summary(meanmaxmodel)
indmaxmodel = lm(melt_date ~ maxflr, data=maxflrdate)
summary(indmaxmodel)

##### General plotting with all individuals #####
## Didn't really get anywhere with this yet

ggplot(data = df18c) +
  geom_point(mapping = aes(x=date, y=flrs, group = ind, color=ind)) +
  geom_jitter(mapping = aes(x=date, y=flrs, group = ind, color=ind)) +
  facet_wrap(~site)

#stat_summary() +
#geom_line(aes(x=date,y=flrs)) +
#geom_jitter(aes(x=date,y=flrs)) +

ggplot(phenm,aes(x=date,y=value,group=repro,color=repro)) +
stat_summary(fun.y=mean, geom="line") + #plotting the means with points
  stat_summary(fun.data = mean_se, geom = "errorbar",width=.5) +
  stat_summary(fun.y=mean, geom="point") + #plotting the means as a line graph
  facet_wrap(~site)


##-----------------------------------------------------------------



###### HERBIVORIZED INDIVIDUALS ONLY #######
### Formatting ###

#Remove all individuals that were not herbivorized

#df18$herb_01.07.2018 = 
#df18$herb_01.07.2018[is.na(df18$herb_01.07.2018)] <- 0

df18h = df18 %>% mutate (anyherb = ifelse (herb_01.07.2018=="0"& herb_05.08.2018=="0" & herb_10.07.2018=="0" & herb_27.08.2018=="0" & herb_18.07.2018=="0" & herb_16.08.2018=="0", T, F))

df18h$anyherb [is.na(df18h$anyherb)] <- FALSE

df18h = subset(df18h, anyherb!= "TRUE") 
df18h = df18h %>% select(-"anyherb")

df18mh=melt(df18h,id.vars=c("uniqueID", "site", "plot", "quad", "ind"), measure.vars = c(grep("^flrs",names(df18h),value = TRUE),grep("^buds",names(df18h),value = TRUE),grep("^fruits",names(df18h),value = TRUE), grep("^herb",names(df18h),value = TRUE), grep("^phen", names(df18h), value = TRUE))) #changed data from wide to long format. measure.vars are counted in the value column.

df18mh$date <- sapply(strsplit(as.character(df18mh$variable), split='_', fixed=TRUE), function(x) (x[2])) #somehow I think this turns i.e. "date.1" into "date_06-30-17", but not sure how
df18mh$date <- as.Date(df18mh$date,"%d.%m.%Y") #tells R to recognize this column as dates, and what format the dates are in to begin with
df18mh$var <- sapply(strsplit(as.character(df18mh$variable), split='_', fixed=TRUE), function(x) (x[1])) #making a vector of the "_flrs" (or buds, fruits) portion of the "date_flrs" column and splitting by the "_"
df18mh$var=as.factor(df18mh$var) #now, putting the vector into its own column, called "repro"

#df18mh$value=as.numeric(df18mh$value)

df18ch = dcast(df18mh, uniqueID + site + plot + quad + date + ind ~ var, value.var = 'value', fun.aggregate=max) #Split variable column into four separate columns for each type of variable

df18ch$herb[df18ch$herb == 0] = NA

df18ch$herb = as.numeric(df18ch$herb)

df18ch = subset(df18ch, herb!= "NA")

#df18ch$herb=as.factor(df18ch$herb)
df18ch$buds=as.numeric(df18ch$buds)
df18ch$flrs=as.factor(df18ch$flrs)
df18ch$fruits=as.factor(df18ch$fruits)
df18ch$phen=as.factor(df18ch$phen)

df18ch$fruits[is.na(df18ch$fruits)] <- 0
df18ch$flrs[is.na(df18ch$flrs)] <- 0
df18ch$buds[is.na(df18ch$buds)] <- 0

### LATER: NEED TO AUTOMATE BELOW PROCESS USING DDPLYR THE SAME WAY YOU FOUND FIRST FLR DATE ###

df18ch = df18ch %>% mutate (melt_date = case_when(
  df18ch$site=="LVTR2.5" ~ "NA",
  df18ch$site=="LVTR2" ~ "01_06_2018",
  df18ch$site=="LVTR1" ~ "01_06_2018",
  df18ch$site=="LV1" ~ "20_06_2018",
  df18ch$site=="LV2" & df18ch$plot==1 ~ "29_05_2018",
  df18ch$site=="LV2" & df18ch$plot==2 ~ "21_05_2018",
  df18ch$site=="LV3" & df18ch$plot==1 ~ "17_05_2018",
  df18ch$site=="LV3" & df18ch$plot==1.5 ~ "03_05_2018"))

df18ch$melt_date = as.Date(df18ch$melt_date,"%d_%m_%Y")

###### Make new columns for each type of herbivory ######

#adding column for type 1 herbivory with binary Y/N option
df18ch = df18ch %>% mutate (type1herb = case_when(
  df18ch$herb=="0" ~0,
  df18ch$herb=="1" ~1,
  df18ch$herb=="1.5" ~1,
  df18ch$herb=="2" ~0,
  df18ch$herb=="3" ~0
))

#df18ch$type1herb = as.factor(df18ch$type1herb)
levels(df18ch$type1herb)

#type 2 herbivory
df18ch = df18ch %>% mutate (type2herb = case_when(
  df18ch$herb=="0" ~0,
  df18ch$herb=="1" ~0,
  df18ch$herb=="1.5" ~1,
  df18ch$herb=="2" ~1,
  df18ch$herb=="3" ~0
))

#df18ch$type2herb = as.factor(df18ch$type2herb)
#levels(df18ch$type2herb)

#type 3 herbivory
#df18ch = df18ch %>% mutate (type3herb = case_when(
 # df18ch$herb=="0" ~"0",
  #df18ch$herb=="1" ~"0",
  #df18ch$herb=="1.5" ~"0",
  #df18ch$herb=="2" ~"0",
  #df18ch$herb=="3" ~"1"
#))

#df18ch$type3herb = as.factor(df18ch$type3herb)
#levels(df18ch$type3herb)

#### Did snowmelt date impact frequency of herbivory? ####

# Condense based on plot #
#df18ch$type1herb = as.numeric(df18ch$type1herb)
#df18ch$type2herb = as.numeric(df18ch$type2herb)
#df18ch$type3herb = as.numeric(df18ch$type3herb)

##### March 28th: Timing of Herbivory Across Populations ######

## Need to add a column for first time experiencing each type of herbivory for each individual
summary(df18ch)
#df18ch$type1herb[df18ch$type1herb == 0] = NA
#df18ch$type2herb[df18ch$type2herb == 0] = NA

df18ch$type1herb=as.numeric(df18ch$type1herb)
df18ch$type2herb=as.numeric(df18ch$type2herb)

df18ch$Jdate=as.POSIXlt(df18ch$date, format="%d_%m_%Y")$yday

firstherbdate$firstherb1[is.finite(firstherbdate$firstherb1) == FALSE] = NA
firstherbdate$firstherb2[is.finite(firstherbdate$firstherb2) == FALSE] = NA

firstherbdate = ddply(df18ch,.(uniqueID), summarise,
  firstherb1 = min(Jdate[is.na(type1herb)==FALSE]),
  phenherb1 = phen[which.min(Jdate[is.na(type1herb)==FALSE])]#is.finite(firstherb1) == T &
  #firstherb2 = min(Jdate[is.na(type2herb)==FALSE]),
  #phenherb2 = phen[which.min(Jdate[is.na(type2herb)==FALSE])]
)

## Make two separate dfs for different herb types
  ## take out any inds that didn't experience that type
#herb1df <- df18ch[df18ch$type1herb]

df18h1 = df18ch %>% mutate (anyherb1 = ifelse (type1herb=="0", T, F))

df18h1$anyherb1[is.na(df18h1$anyherb1)] <- FALSE

df18h1 = subset(df18h1, anyherb1!= "TRUE") 
df18h1 = df18h1 %>% select(-"anyherb1")

firstherbdate1 = ddply(df18h1,.(uniqueID, site), summarise,
                      firstherb1 = min(Jdate[type1herb > 0]),
                      phenherb1 = phen[which.min(Jdate[type1herb > 0])]
)

df18h2 = df18ch %>% mutate (anyherb2 = ifelse (type2herb=="0", T, F))

df18h2$anyherb2[is.na(df18h2$anyherb2)] <- FALSE

df18h2 = subset(df18h2, anyherb2!= "TRUE") 
df18h2 = df18h2 %>% select(-"anyherb2")

firstherbdate2 = ddply(df18h2,.(uniqueID, site), summarise,
                       firstherb2 = min(Jdate[type2herb > 0]),
                       phenherb2 = phen[which.min(Jdate[type2herb > 0])]
)

ggplot(data=firstherbdate1) +
  geom_bar(mapping=aes(firstherb1)) +
  facet_wrap(~site)

ggplot(data=firstherbdate1, aes(x=site, y= firstherb1) ) +
  geom_boxplot() +
  ggtitle ("Insect Herbivory") +
  xlab ("Site") +
  ylab ("First Date Observed Insect Herbivory (Julian)")

#Table with phenology

table(firstherbdate1$site, firstherbdate1$phenherb1)

ggplot(data=firstherbdate2) +
  geom_bar(mapping=aes(firstherb2)) +
  facet_wrap(~site)

ggplot(data=firstherbdate2, aes(x=site, y= firstherb2)) +
  geom_boxplot() +
  ggtitle ("Mammalian Herbivory") +
  xlab ("Site") +
  ylab ("First Date Observed Mammalian Herbivory (Julian)")

table(firstherbdate2$site, firstherbdate2$phenherb2)

##With zeros

firstherbdate = ddply(df18ch,.(uniqueID), summarise,
                      firstherb1 = min(Jdate[type1herb > 0]),
                      #phenherb1 = phen[which.min(Jdate[type1herb > 0])]
                      #firstherb2 = min(Jdate[is.na(type2herb)==FALSE]),
                      #phenherb2 = phen[which.min(Jdate[is.na(type2herb)==FALSE])]
)
summary(df18ch)

##

#Changed Inf values in firstherbdate to NAs because these never experienced that type of herbivory

# crude estimate of frequency - just # of individuals in each plot that have experienced herbivory (not correcting for total # of individuals at each site)
herbfreq = ddply(df18ch,.(site, plot, melt_date), summarise,
   freq1 = sum(type1herb),
   freq2 = sum(type2herb))
   #freq3 = sum(type3herb))

## Change to Julian date! ##

herbfreq$Jmelt_date <- as.POSIXlt(herbfreq$melt_date, format="%d_%m_%Y")$yday

# Plotting #
ggplot(data = herbfreq) +
  geom_point(mapping = aes(x=Jmelt_date, y=freq1, group = plot, color= site))

ggplot(herbfreq, aes(Jmelt_date, freq1)) +
  geom_point() +
  geom_smooth()

ggplot(data = herbfreq) +
  geom_point(mapping = aes(x=Jmelt_date, y=freq2, group = plot, color= site))

ggplot(herbfreq, aes(Jmelt_date, freq2)) +
  geom_point() +
  geom_smooth()

ggplot(data = herbfreq) +
  geom_point(mapping = aes(x=Jmelt_date, y=freq3, group = plot, color= site)) +
  
ggplot(herbfreq, aes(Jmelt_date, freq3)) +
  geom_point() +
  geom_smooth()

lm(Jmelt_date ~ freq1, data=herbfreq)
lm(Jmelt_date ~ freq2, data=herbfreq)
lm(Jmelt_date ~ freq3, data=herbfreq)

# better estimate of frequency: proportion of individuals in each plot that experienced each type of herbivory

# need to add a column indicated number of individuals within a plot 

df18ch = df18ch %>% mutate (plottotal = case_when(
  df18ch$site=="LVTR2.5" ~ sum(with(df18ch, site == "LVTR2.5")),
  df18ch$site=="LVTR2" ~ sum(with(df18ch, site == "LVTR2")),
  df18ch$site=="LVTR1" ~ sum(with(df18ch, site == "LVTR1")),
  df18ch$site=="LV3" & df18ch$plot==1.0 ~ sum(with(df18ch, site == "LV3" & plot==1.0)),
  df18ch$site=="LV3" & df18ch$plot==1.5 ~ sum(with(df18ch, site == "LV3" & plot==1.5)),
  df18ch$site=="LV2" & df18ch$plot==1 ~ sum(with(df18ch, site == "LV2" & plot==1)),
  df18ch$site=="LV2" & df18ch$plot==2 ~ sum(with(df18ch, site == "LV2" & plot==2)),
  df18ch$site=="LV1" & df18ch$plot==1 ~ sum(with(df18ch, site == "LV1" & plot==1)),
  df18ch$site=="LV1" & df18ch$plot==2 ~ sum(with(df18ch, site == "LV1" & plot==2))))

herbperc = ddply(df18ch,.(site, plot, melt_date), summarise,
                 perc1 = sum(type1herb)/plottotal,
                 perc2 = sum(type2herb)/plottotal)

## Change to Julian date! ##

herbperc$Jmelt_date <- as.POSIXlt(herbperc$melt_date, format="%d_%m_%Y")$yday

ggplot(data = herbperc) +
  geom_point(mapping = aes(x=Jmelt_date, y=perc1, group = plot, color= site, size=0.1)) +
  theme(text = element_text(size=20))

ggplot(data = herbperc) +
  geom_point(mapping = aes(x=Jmelt_date, y=perc2, group = plot, color= site, size=0.1)) +
  theme(text = element_text(size=20))

ggplot(data = herbperc) +
  geom_point(mapping = aes(x=Jmelt_date, y=perc3, group = plot, color= site))

herbperc1 = lm(Jmelt_date ~ perc1, data=herbperc)


herbperc2 = lm(Jmelt_date ~ perc2, data=herbperc)

summary(herbperc1)
anova(herbperc1)

summary(herbperc2)
anova(herbperc2)





#### Analyzing the effects of snowmelt and herbivory on fecundity (and the effects of their interaction) ####



## Start with df18c dataframe

snoherb = df18c %>% mutate (type1herb = case_when(
  df18c$herb=="0" ~0,
  df18c$herb=="1" ~1,
  df18c$herb=="1.5" ~1,
  df18c$herb=="2" ~0,
  df18c$herb=="3" ~0
))

snoherb$type1herb[is.na(snoherb$type1herb)] <- 0
  
#type 2 herbivory
snoherb = snoherb %>% mutate (type2herb = case_when(
  snoherb$herb=="0" ~0,
  snoherb$herb=="1" ~0,
  snoherb$herb=="1.5" ~1,
  snoherb$herb=="2" ~1,
  snoherb$herb=="3" ~0
))

snoherb$type2herb[is.na(snoherb$type2herb)] <- 0

## add column to indicate total number of plants in plot
snoherb = snoherb %>% mutate (plottotal = case_when(
  snoherb$site=="LVTR2.5" ~ sum(with(snoherb, site == "LVTR2.5")),
  snoherb$site=="LVTR2" ~ sum(with(snoherb, site == "LVTR2")),
  snoherb$site=="LVTR1" ~ sum(with(snoherb, site == "LVTR1")),
  snoherb$site=="LV3" & snoherb$plot==1.0 ~ sum(with(snoherb, site == "LV3" & plot==1.0)),
  snoherb$site=="LV3" & snoherb$plot==1.5 ~ sum(with(snoherb, site == "LV3" & plot==1.5)),
  snoherb$site=="LV2" & snoherb$plot==1 ~ sum(with(snoherb, site == "LV2" & plot==1)),
  snoherb$site=="LV2" & snoherb$plot==2 ~ sum(with(snoherb, site == "LV2" & plot==2)),
  snoherb$site=="LV1" & snoherb$plot==1 ~ sum(with(snoherb, site == "LV1" & plot==1)),
  snoherb$site=="LV1" & snoherb$plot==2 ~ sum(with(snoherb, site == "LV1" & plot==2))))

snoherb$Jmelt_date <- as.POSIXlt(snoherb$melt_date, format="%d_%m_%Y")$yday
snoherb$Jdate <- as.POSIXlt(snoherb$date, format="%d_%m_%Y")$yday


##### HERE'S WHERE IT'S GOING WRONG #####
#group_by()
#df = df %>% 
  #group_by(site, plot, uniqueID) %>%
  #summarize(anyherb=ifelse(sum(herb)>0,
  #group_by(plot, site, uniqueID),
  #mutate(n=n(1), herbp=herb/n)))

#group by site & plot, then get total number of plants w/in the plot
#length of unique vector of plants per plot to get total per plot
#distinct 
#if sum of (group_by uniqueID in herb of each type) is > 0 new var is a 1, if not a zero

herbperc = ddply(df18ch,.(site, plot, melt_date), summarise,
                 perc1 = sum(type1herb)/plottotal,
                 perc2 = sum(type2herb)/plottotal)


### Visualization with Herb inds only ####

ggplot(data = df18ch) +
  geom_point(mapping = aes(x=date, y=herb, group = ind, color = ind)) +
  geom_jitter(mapping = aes(x=date, y=herb, group = ind, color = ind)) +
  facet_wrap(~site)

ggplot(data = df18ch) +
  geom_point(mapping = aes(x=date, y=flrs, group = ind, color=type1herb)) +
  geom_line(mapping = aes(x=date, y=flrs, group=ind)) +
  geom_jitter(mapping = aes(x=date, y=flrs, group = ind, color=type1herb)) +
  ylim(0,60) +
  facet_wrap(~site)

ggplot(data = df18ch) +
  geom_point(mapping = aes(x=date, y=flrs, group = ind, color=type2herb)) +
  geom_line(mapping = aes(x=date, y=flrs, group=ind)) +
  geom_jitter(mapping = aes(x=date, y=flrs, group = ind, color=type2herb)) +
  ylim(0,60) +
  facet_wrap(~site)

ggplot(data = df18ch) +
  geom_point(mapping = aes(x=date, y=buds, group = ind, color=type1herb)) +
  geom_line(mapping = aes(x=date, y=buds, group=ind)) +
  geom_jitter(mapping = aes(x=date, y=buds, group = ind, color=type1herb)) +
  facet_wrap(~site)

#Fiddling with base R plotting
plot(df18ch$date, df18ch$flrs, col  = "purple", cex = 2)
points(df18ch$date, df18ch$fruits, col  = "blue", pch = 2, cex = 0.5)
points(df18ch$date, df18ch$buds, col  = "orange", pch = 0, cex = 1)


