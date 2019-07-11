library(ggplot2)
library(plyr)
library(reshape2)
library(dplyr)

#went into file and turned "1a" into 1, and "2b" into 2
#if had both 1 and 2, wrote 1.5 (these were only in week 7!!!)

herb = read.csv("herb2017data_cleaning_1.28.19.csv")

#plotting crude histograms showing freq of inds with 1a vs 2b type of herb
par(mfrow=c(2,4))
hist(herb$herb.1)
hist(herb$herb.2)
hist(herb$herb.3)
hist(herb$herb.4)
hist(herb$herb.5)
hist(herb$herb.6)
hist(herb$herb.7)
hist(herb$herb.8)

###### Data formatting ######

herb$site=factor(herb$site,levels=c("LV3","LV2","LV1","LVTR1","LVTR2","LVTR2.5")) #tells R how to order the factors for site (did in order of increasing elevation)

herb <- subset(herb, phen.1!= "D") #rewrite phen dataframe as a subset of itself that doesn't include rows for which phen.1 = D (removing plants that were dead from previous year so the bud/flr/fruit counts aren't skewed towards lower numbers, since these plants will have counts of zero throughout the season)

herbm=melt(herb,id.vars=c("uniqueID", "site", "plot", "quad","ind"),measure.vars = c(grep("^flrs",names(herb),value = TRUE),grep("^buds",names(herb),value = TRUE),grep("^fruits",names(herb),value = TRUE), grep("^herb",names(herb),value = TRUE))) #changed data from wide to long format. measure.vars are counted in the value column.

#colnames(phenm)[7] <- c('date')
names(herbm)
herbm$date
head(herbm)
dim(herbm)

herbm$date <- sapply(strsplit(as.character(herbm$variable), split='_', fixed=TRUE), function(x) (x[2])) #somehow I think this turns i.e. "date.1" into "date_06-30-17", but not sure how
herbm$date <- as.Date(herbm$date,"%d.%m.%Y") #tells R to recognize this column as dates, and what format the dates are in to begin with
herbm$var <- sapply(strsplit(as.character(herbm$variable), split='_', fixed=TRUE), function(x) (x[1])) #making a vector of the "_flrs" (or buds, fruits) portion of the "date_flrs" column and splitting by the "_"
herbm$var=as.factor(herbm$var) #now, putting the vector into its own column, called "repro"

herbm$value2=as.numeric(herbm$value)

#Split variable column into four separate columns for each type of variable

herbc = dcast(herbm, uniqueID + site + plot + quad + ind + date ~ var, value.var = 'value2', fun.aggregate=max) #dcast used for data frame output (vs. acast for matrix/vector output)

###### Data visualization #######

#Plot with herb only
herb_plot = ggplot(data=herbc, aes(x=date, y=herb, group=ind, color=ind)) +
  geom_point() +
  geom_jitter() +
  facet_wrap(~site)
herb_plot

### Herb needs to be a factor
### Fix NAs
### Subset with only individuals that were herbivorized

#Plot with flrs only
flrs_plot = 
  ggplot(data=herbc, aes(x=date, y=flrs, group=ind)) +
  geom_line() +
  geom_point(aes(color=herb)) +
  geom_jitter() +
  facet_wrap(~site)
flrs_plot

#Plot with buds only
buds_plot = ggplot(data=herbc, aes(x=date, y=buds, group=ind, color=ind)) +
  geom_point() +
  geom_jitter() +
  facet_wrap(~site)
buds_plot

#Plot with fruits only
fruits_plot = ggplot(data=herbc, aes(x=date, y=fruits, group=ind, color=ind)) +
  geom_point() +
  geom_line() +
  geom_jitter() +
  facet_wrap(~site)
fruits_plot

#Herb and flrs plot
herb_flrs_plot =
  flrs_plot + 
  geom_point(aes(x=date, y=herb, color="herb")) +
  geom_jitter() +
  facet_wrap(~site)
herb_flrs_plot
flrs_plot

#Overlay herbivory data on flrs plot
# %>% is a function that "pipes" the dataset into "mutate" where we modifty the dataset, adding a new variable which is then piped into ggplot2 for plotting

herbc %>% 
  mutate(highlight_flag = ifelse(herb >1.0, T, F)) %>%
  ggplot(data=herbc, aes(x=date, y=herb)) +
  geom_point(aes(color = highlight_flag)) +
  geom_jitter() +
  scale_fill_manual(values=cbPalette) +
  facet_wrap(~site)

herbmut = herbc %>% 
  mutate(highlight_flag = ifelse(herb >1.0, T, F)) 

  
herbmut = ggplot(data=herbmut, aes(x=date, y=herb, group=highlight_flag)) +
  geom_point(aes(color = highlight_flag)) +
  geom_jitter() +
  #scale_fill_manual(values=cbPalette) +
  facet_wrap(~site)

herbmut
herb_flrs_plot

herbmut = ggplot(data=herbmut, aes(x=date, y=herb)) +
  geom_point(aes(color = herb)) +
  #geom_jitter() +
  #scale_fill_manual(values=cbPalette) +
  facet_wrap(~site)

###### Colorblind-friendly palettes ######

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)

###Combining buds and flrs
buds_flrs_plot = flrs_plot + 
  geom_line(data=herbc, aes(x=date, y=buds)) +
  geom_line() +
  geom_point() +
  geom_jitter() +
  facet_wrap(~site)
buds_flrs_plot

### OUTDATED ###

herbm$value=as.numeric(herbm$value)

#Plotting individuals across sites

ggplot(herbm,aes(x=date,y=value, color=variable, group=ind)) +
  geom_point() + #plotting the inds as points
  geom_line() + #plotting the inds as a line graph
  facet_wrap(~site)
#data=phenm, x=date, y=value2

####Subset:
phenm2 <- subset(phenm, repro="flrs", drop=TRUE)
####Jenny subset method:
phenm2 <- droplevels(phenm[phenm$repro == "flrs",])
summary(phenm2)
dim(phenm)
dim(phenm2)
levels(phenm$repro)

ggplot(phenm2,aes(x=date,y=value2, color=ind, group=ind)) +
  geom_point() + #plotting the inds as points
  geom_line() + #plotting the inds as a line graph
  facet_wrap(~site)



