## Cleaning data Ch. 2 Lassen Phenology
## Elena Suglia

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
#library(arm)
library(lme4)
#library(lattice)
#library(vioplot)
#library(lme4)
#library(arm)
#library(bbmle) # Ben Bolker's library of mle functions
#library(MASS)
library(MuMIn)
#library(nlme)
library(tidyverse)
library(stringr)
library(lubridate)
#library(dplyr)

# Creating master data file ----

# see metadata file for file names and meanings
# used herb2017data_preclean_1.28.19.csv as raw file for analysis because it includes notes and a column for herbivory
# IMPORTANT NOTE: ON 7/30/19, ADDED PREVIOUSLY MISSING DATA TO DATASHEET "trip6" AND DATASHEET "d18". Also updated lassen-phen.csv. THIS MEANS ALL PREVIOUS VERSIONS OF THESE SHEETS ARE OUTDATED AND DO NOT INCLUDE THIS DATA.

# read in 2017 and 2018 data files and do a little cleaning before joining
#d17 <- read_csv("d17.csv")
#d17 <- d17 %>% 
#  select(-(site:ind)) # remove site, plot, quad, and ind. Especially remove "ind" column because csv always reverts some numbers to dates incorrectly

# notes about 2017 data file:
# no notes, protocol notes, or herbivory data from trip 8
# emergence date and iteroparous perennial information needs to be looked at more carefully - in this analysis, will ignore

#d18 <- read_csv("d18.csv")
#d18 <- d18 %>% 
#  select(-(site:ind))

# note - trip 3 2018 has a few plants with herb type "3" (pods herbivorized)

# add trips 2, 4, and 6 to 2018 data
#trip2 <- read_csv("2018trip2_cleaned_ES20190218.csv")
# fixing a couple of mistakes in trip 2:
#trip2[79,1] <- "LV1 P2 QNA #19" # was incorrectly written as #91
#trip4 <- read_csv("trip4.csv") # in previous analysis, trip 6 was mislabelled as trip 4
# trip 4 has some herb type 3a: in herb column labelled as 3 or if there was herb 2 and 3, wrote "2,3"
# several in LV1 plot 2 were mislabelled as being in plot 1 - fixed manually in csv file
#trip6 <- read_csv("trip6.csv")
# fixing a couple of mistakes in trip 6:
#trip6[139,5] <- "LV1 P1 QNA #43-2-1"
#trip6[139,4] <- "43-2-1"

#d18 <- d18 %>%
#  full_join(trip2, by = c("uniqueID" = "uniqueID.2")) %>%
#  full_join(trip4, by = c("uniqueID" = "uniqueID.4")) %>%
#  full_join(trip6, by = c("uniqueID" = "uniqueID.6"))

# join datasets into one
#d <- d17 %>%
#  full_join(d18, by = c("uniqueID" = "uniqueID"))

#write.csv(d, "lassen-phen.csv")

# Adding 2019 data ----

setwd("~/Box Sync/Graduate School/PhD Research/Ch. 2) Lassen Snowmelt & Phenology/Data analysis/lassen-phenology/data")

d19 <- read_csv("d19.csv")
d19 <- d19 %>%
  select(-X23, -X123, -X38, -X1)

master <- read_csv("lassen-phen.csv")

d <-
  master %>%
  full_join(d19, by = c("uniqueID" = "uniqueID.19"))

# plants with .18 in uniqueID are those that were marked starting in 2018 at LVTR2.5. This is to avoid confusion because plants 1-25 were marked there (using different colored bird bands) in both 2016 and again in 2018.

# LV2 plot 1.5 is new in 2019, both to indicate that the plants were in a slightly different location (a few meters downslope of plot 1) and to solve the issue of having the same numbered individuals (so the unique ID will be differentiated)

#write.csv(d, "lassen-phen-2019.csv")

# Cleaning ----

#setwd("~/Box Sync/Graduate School/PhD Research/Lassen Snowmelt & Phenology/Data analysis/lassen-phenology/data cleaning")
#d <- read_csv("lassen-phen.csv")

# clean up final data file a bit
d <- d %>%
  select (-X1) %>%
  separate(uniqueID, sep = " ", into = c("site", "plot", "quad", "ind"), remove = FALSE) %>% # deconcatenate uniqueID column into site, plot, quad, individual
  mutate(ind = str_remove_all(ind, "#")) %>% # remove "#" from individual numbers in ind column
  mutate(quad = str_remove_all(quad, "Q")) %>%
  mutate(plot = str_remove_all(plot, "P")) %>%
  filter(phen.1 != "D" | is.na(phen.1)) %>%
  # remove those that died before the first ever survey in 2017
  # need to explicitly include na values or filter will automatically remove them
  filter(phen_25.07.2019 != "D" | is.na(phen_25.07.2019)) %>% # dead, flrd 2018
  filter(phen_25.07.2019 != "X" | is.na(phen_25.07.2019)) %>% # dead, veg 2018
  filter(phen_08.08.2019 != "D" | is.na(phen_08.08.2019)) %>% # dead, flrd2018
  filter(phen_08.08.2019 != "X" | is.na(phen_08.08.2019)) %>% # dead, veg 2018
  filter(phen_28.09.2019 != "V" | is.na(phen_28.09.2019)) %>% # veg all of 2019
  filter(phen_25.07.2019 != "SX" | is.na(phen_25.07.2019)) %>% # SX all of 2019
  filter(phen_25.07.2019 != "LOST" | is.na(phen_25.07.2019)) %>% # SX all of 2019
  # remove individuals that never flowered within each year
  select(-starts_with("notes"), -starts_with("protocol"), -starts_with("emerg"), -starts_with("itero"), -starts_with("date"), -c(site_27.07.2018:ind_27.07.2018)) %>%
  gather(key = "variable", value = "value", starts_with("buds"), starts_with("fruits"), starts_with("flrs"), starts_with("phen"), starts_with("date"), starts_with("longest"), starts_with("height"), starts_with("length"), starts_with("stem"), starts_with("herb"), starts_with("total"), starts_with("canopy"), starts_with("branch"), -site, -plot, -quad, -ind, -site.x, -plot.x, -quad.x, -ind.x, -site.y, -plot.y, -quad.y, -ind.y, na.rm = FALSE) %>% # changed data from wide to long format
  #
  mutate(variable = str_replace_all(variable, "canopy.width.largest.rosette", "canopywidth")) %>%
  mutate(variable = str_replace_all(variable, "canopy.width", "canopywidth")) %>%
  mutate(variable = str_replace_all(variable, "longest.leaf", "longestleaf")) %>%
  mutate(variable = str_replace_all(variable, "total.branch", "totalbranch")) %>%
  mutate(variable = str_replace_all(variable, "stem.diam", "stemdiam")) %>%
  mutate(variable = str_replace_all(variable, "longest.fruit", "longestfruit")) %>%
  mutate(variable = str_replace_all(variable, "branch.damage", "branchdamage")) %>%

  
# format variables and dates into their own tidy columns
# IMPORTANT NOTE: here, I am giving each trip a date that represents all the data points taken on that trip. I use the first day on which I collected during a given trip. Most trips took 2 days, but there is a span of 1-4 days per trip. In future analyses it is probably worth using the actual day on which data was collected. To do so, don't remove columns in above pipeline that start with date. 

  separate(variable, sep = "_", into = c("variable", "date")) %>%
  separate(variable, sep = "\\.", into = c("variable", "trip17")) %>% # need to use two back slashes before period to "escape" notation of "." by itself means "matches any character"
  #
  mutate(trip17 = str_replace_all(trip17, "1", "08/08/2017")) %>%
  mutate(trip17 = str_replace_all(trip17, "^2$", "14/08/2017")) %>% # need to indicate ^ at beginning at $ at end of the expression because if you don't anchor to indicate the beginning and end, it will find every number 2 within other character strings (like in 2017) and replace those as well
  mutate(trip17 = str_replace_all(trip17, "^3$", "21/08/2017")) %>%
  mutate(trip17 = str_replace_all(trip17, "^4$", "28/08/2017")) %>%
  mutate(trip17 = str_replace_all(trip17, "^5$", "01/09/2017")) %>%
  mutate(trip17 = str_replace_all(trip17, "^6$", "11/09/2017")) %>%
  mutate(trip17 = str_replace_all(trip17, "^7$", "18/09/2017")) %>%
  mutate(trip17 = str_replace_all(trip17, "^8$", "30/09/2017")) %>%
  #
  unite(variable, variable, trip17, sep = "_") %>%
  unite(variable_date, variable, date, sep = "_") %>%
  mutate(variable_date = str_remove_all(variable_date, "_NA")) %>%
  separate(variable_date, sep = "_", into = c("variable", "date")) %>%
  mutate(date = str_replace_all(date, "\\.", "/")) %>% # make date formatting across years consistent
  mutate(date = dmy(date)) %>% # tell R to recognize as a date
  mutate(jday = yday(date)) %>% # format date as Julian day
  mutate(year = year(date)) %>%
  as_tibble(d) %>% # reformat data frame as a tibble 
  mutate (meltdate = case_when(
    year == 2018 & site == "LVTR2.5" ~ "NA",
    year == 2018 & site == "LVTR2" ~ "2018-06-01",
    year == 2018 & site == "LVTR1" ~ "2018-06-01",
    year == 2018 & site == "LV1" ~ "2018-06-20",
    year == 2018 & site == "LV2" & plot == 1 ~ "2018-05-29",
    year == 2018 & site == "LV2" & plot == 2 ~ "2018-05-21",
    year == 2018 & site == "LV3" & plot == 1 ~ "2018-05-17",
    year == 2018 & site == "LV3" & plot == 1.5 ~ "2018-05-03",
    # 2019 
    year == 2019 & site == "LVTR2.5" ~ "2019-06-13",
    year == 2019 & site == "LVTR2.25" ~ "2019-06-17",
    year == 2019 & site == "LVTR2" ~ "2019-06-19",
    year == 2019 & site == "LVTR1.75" ~ "2019-06-28",
    year == 2019 & site == "LVTR1.5" ~ "2019-07-05",
    year == 2019 & site == "LVTR1" ~ "2019-06-15",
    year == 2019 & site == "LV1" & plot == 1 ~ "2019-07-30",
    year == 2019 & site == "LV1" & plot == 2 ~ "2019-08-11",
    year == 2019 & site == "LV2" & plot == 1 ~ "2019-07-01",
    year == 2019 & site == "LV2" & plot == 1.5 ~ "2019-07-01",
    year == 2019 & site == "LV2" & plot == 2 ~ "2019-06-26",
    year == 2019 & site == "LV3" & plot == 1 ~ "2019-07-05",
    year == 2019 & site == "LV3" & plot == 1.5 ~ "2019-07-05")) %>% # add a column for snowmelt date%>%
  #
  mutate(meltdate = ymd(meltdate)) %>% # tell R to recognize this column as a date
  mutate(jmeltdate = yday(meltdate)) %>% # convert snowmelt dates to Julian Days
  mutate(elevation = case_when(
    site=="LVTR2.5" ~ 2853,
    site=="LVTR2.25" ~ 2845,
    site=="LVTR2" ~ 2808,
    site=="LVTR1.75" ~ 2773,
    site=="LVTR1.5" ~ 2761,
    site=="LVTR1" ~ 2756,
    site=="LV3" ~ 2353,
    site=="LV2" ~ 2500,
    site=="LV1" ~ 2593
    )
       )

## GET ELEVATIONS FOR ALL NEW SITES

# used dcast to convert to wide format; go back and figure out how to convert to tidyverse later (spread?)

# d1 <- d %>%
# spread(key = variable, value = value)

dm = d # store "melted" version of dataframe (tidy tibble)

# check to make sure everything went smoothly
dnames = unique(d$variable)
ddates = unique(d$date)
dvalues = unique(d$value)

# do I want to have r recognize each row as a certain format?
#d$variable=as.factor(d$variable)
#d$value=as.numeric(d$value)


# Casting to wide format and cleaning individual columns ----

# remove all rows for which ind = NA, "GONE;", or "V"

d1 = as_tibble(dcast(d, uniqueID + site + plot + quad + ind + year + elevation + date + jday + meltdate + jmeltdate ~ variable, value.var = "value", fun.aggregate = max)) %>%
  # get rid of some annoying values in the flrs & fruits columns
    mutate(buds = na_if(buds, "no.plant")) %>% 
    mutate(buds = na_if(buds, "NO.PLANT")) %>%
    mutate(buds = na_if(buds, "?")) %>%  
    mutate(buds = str_remove_all(buds, "\\*")) %>%
    mutate(buds = na_if(buds, "2.5")) %>%
    mutate(buds = na_if(buds, "1.8")) %>%
    mutate(buds = na_if(buds, "1.2")) %>%
    mutate(buds = na_if(buds, "1.5")) %>%
    mutate(buds = na_if(buds, "0.9")) %>%
    mutate(buds = na_if(buds, "0.8")) %>%
    #mutate(buds = na_if(buds, "^0$")) %>%
    mutate(flrs = na_if(flrs, "no.plant")) %>% 
    mutate(flrs = na_if(flrs, "NO.PLANT")) %>%
    #mutate(flrs = na_if(flrs, "^0$")) %>%
    mutate(flrs = na_if(flrs, "?")) %>%
    filter(ind != "V") %>%
    filter(ind != "GONE;") %>%
    mutate(fruits = na_if(fruits, "no.plant")) %>% 
    mutate(fruits = na_if(fruits, "NO.PLANT")) %>%
    #mutate(fruits = na_if(fruits, "^0$")) %>%
    mutate(fruits = na_if(fruits, "?")) %>%
    mutate(fruits = as.double(fruits)) %>%
    mutate(flrs = as.double(flrs)) %>%
    mutate(buds = as.double(buds)) %>%
    mutate(totalbranch = as.double(totalbranch)) %>%
    mutate(totalbranch = na_if(totalbranch, "(2VB)")) %>%
    mutate(totalbranch = na_if(totalbranch, "?")) %>%
    mutate(totalbranch = na_if(totalbranch, "NA1")) %>%
    mutate(totalbranch = na_if(totalbranch, "no.plant")) %>%
    mutate(totalbranch = na_if(totalbranch, "NO.PLANT")) %>%
    mutate(totalbranch = na_if(totalbranch, "see notes")) %>%
    mutate(totalbranch = na_if(totalbranch, "2?")) %>%
    mutate(totalbranch = na_if(totalbranch, "7?")) %>%
    mutate(totalbranch = na_if(totalbranch, "1.2")) %>%
    mutate(totalbranch = na_if(totalbranch, "10.9")) %>%
    mutate(totalbranch = na_if(totalbranch, "1.8")) %>%
    mutate(totalbranch = na_if(totalbranch, "2.1")) %>%
    mutate(totalbranch = na_if(totalbranch, "1.5")) %>%
    mutate(herb = str_replace_all(herb, "1a, 2b", "1.5")) %>%
    mutate(herb = str_replace_all(herb, "2b", "2")) %>%
    mutate(herb = str_replace_all(herb, "1a", "1")) %>%
    mutate(herb = str_replace_all(herb, "qa", "1")) %>%
    mutate(herb = str_replace_all(herb, "2, 3", "2.5")) %>%
    mutate(herb = na_if(herb, "^0$")) %>%
    mutate(herb1 = case_when(
      herb == "1" ~ 1,
      herb == "1.5" ~ 1,
      herb == "2" ~ 0,
      herb == "2.5" ~ 0,
      herb == "3" ~ 0
    )) %>%
    mutate(herb2 = case_when(
      herb == "1" ~ 0,
      herb == "1.5" ~ 1,
      herb == "2" ~ 1,
      herb == "2.5" ~ 1,
      herb == "3" ~ 0
    )) %>%
    mutate(anyherb = case_when(
      herb == "1" ~ 1,
      herb == "1.5" ~ 1,
      herb == "2" ~ 1,
      herb == "2.5" ~ 1,
      herb == "3" ~ 1
    )) %>%
    mutate(herb1 = replace_na(herb1, 0)) %>%
    mutate(herb2 = replace_na(herb2, 0)) %>%
    mutate(anyherb = replace_na(anyherb, 0)) %>% 
    mutate(buds1 = case_when(
      buds == NA & phen == "V" ~ 0,
      buds == NA & phen == "SX" ~ 0,
      buds == NA & phen == "V" ~ 0,
      buds == NA & phen == "V" ~ 0,
      buds == NA & phen == "V" ~ 0,
    ))


d1$site=factor(d1$site,levels=c("LVTR2.5", "LVTR2.25", "LVTR2", "LVTR1.75", "LVTR1.5", "LVTR1", "LV1", "LV2", "LV3"))

#write.csv(d1, "d1.csv") # not sure what's happening here, but when I try to use this master file in other scripts, it doesn't work
