#### Lassen field work 2017-2018 ####

library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)

# Creating master data file ----

# see metadata file for file names and meanings
# used herb2017data_preclean_1.28.19.csv as raw file for analysis because it includes notes and a column for herbivory

# read in 2017 and 2018 data files and do a little cleaning before joining
#d17 <- read_csv("d17.csv")
#d17 <- d17 %>% 
  #select(-(site:ind)) # remove site, plot, quad, and ind. Especially remove "ind" column because csv always reverts some numbers to dates incorrectly

# notes about 2017 data file:
# no notes, protocol notes, or herbivory data from trip 8
# emergence date and iteroparous perennial information needs to be looked at more carefully - in this analysis, will ignore

#d18 <- read_csv("d18.csv")
#d18 <- d18 %>% 
  #select(-(site:ind))

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
  #full_join(trip2, by = c("uniqueID" = "uniqueID.2")) %>%
  #full_join(trip4, by = c("uniqueID" = "uniqueID.4")) %>%
  #full_join(trip6, by = c("uniqueID" = "uniqueID.6"))

# join datasets into one
#d <- d17 %>%
 # full_join(d18, by = c("uniqueID" = "uniqueID"))

#write.csv(d, "lassen-phen.csv")


# Cleaning ----

d <- read_csv("lassen-phen.csv")

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
  select(-starts_with("notes"), -starts_with("protocol"), -starts_with("emerg"), -starts_with("itero"), -starts_with("date"), -c(site_27.07.2018:ind_27.07.2018)) %>%
  gather(key = "variable", value = "value", starts_with("buds"), starts_with("fruits"), starts_with("flrs"), starts_with("phen"), starts_with("date"), starts_with("longest"), starts_with("height"), starts_with("length"), starts_with("stem"), starts_with("herb"), starts_with("total"), starts_with("canopy"), -site, -plot, -quad, -ind, na.rm = FALSE) %>% # changed data from wide to long format
  #
  mutate(variable = str_replace_all(variable, "canopy.width.largest.rosette", "canopywidth")) %>%
  mutate(variable = str_replace_all(variable, "canopy.width", "canopywidth")) %>%
  mutate(variable = str_replace_all(variable, "longest.leaf", "longestleaf")) %>%
  mutate(variable = str_replace_all(variable, "total.branch", "totalbranch")) %>%
  mutate(variable = str_replace_all(variable, "stem.diam", "stemdiam")) %>%
  mutate(variable = str_replace_all(variable, "longest.fruit", "longestfruit")) %>%
  
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
    year == 2018 & site == "LV3" & plot == 1.5 ~ "2018-05-03")) %>% # add a column for snowmelt date
  mutate(meltdate = ymd(meltdate)) %>% # tell R to recognize this column as a date
  mutate(meltdate = yday(meltdate)) # convert snowmelt dates to Julian Days

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
# if the above doesn't solve this issue, remove uniqueID = LV3 P2 QALL GONE; BANK ERODED AWAY #NA

library(reshape2)
d1 = as_tibble(dcast(d, uniqueID + site + plot + quad + ind + year + jday + meltdate ~ variable, value.var = "value", fun.aggregate = max)) %>%
  # get rid of some annoying values in the flrs column
    mutate(flrs = na_if(flrs, "no.plant")) %>% 
    mutate(flrs = na_if(flrs, "NO.PLANT")) %>%
    mutate(flrs = na_if(flrs, "^0$")) %>%
    mutate(flrs = na_if(flrs, "?")) %>%
    filter(ind != "V") %>%
    filter(ind != "GONE;")

# Phenology over time (2017 and 2018; visualizations only) ----

## flowering phenology
# I MISSED FIRST FLOWERING TIME FOR SEVERAL POPS; MAY BE BETTER TO COMPARE USING PEAK FLOWERING TIME
    flr = d1 %>% 
      drop_na(flrs) %>% # to do a summarize, can't have NAs in column
      group_by(site, plot, year, uniqueID) %>% 
      summarise(
      firstflr = min(jday[is.na(flrs) == FALSE]),
      lastflr = max(jday[is.na(flrs) == FALSE])
      # peakflr = jday[which(flrs == max(flrs))] - not working
      )
    
    # graph: first vs last flowering time
    ggplot(data = flr, mapping = aes(firstflr, lastflr, color = site)) +
    #geom_point()
    facet_wrap(~year) +
    geom_jitter()
  
    #graph: first flowering time (histogram)
    ggplot(data = flr, mapping = aes(firstflr, fill = site)) +
    facet_wrap(~year) +
    geom_histogram()
    # is there a better way to visualize this?
    
    #graph: last flowering time (histogram)
    ggplot(data = flr, mapping = aes(lastflr, fill = site)) +
    facet_wrap(~year) +
    geom_histogram()

# Snowmelt ~ phenology (2018 only) ----

## flowering phenology
    flrsnow = d1 %>% 
      drop_na(meltdate) %>%
      drop_na(flrs) %>% # to do a summarize, can't have NAs in column
      group_by(site, plot, meltdate, uniqueID) %>% 
      summarise(
        firstflr = min(jday[is.na(flrs) == FALSE]),
        lastflr = max(jday[is.na(flrs) == FALSE])
        # peakflr = jday[which(flrs == max(flrs))] - not working
        )

    # graph
    ggplot(data = flrsnow, mapping = aes(meltdate, firstflr, color = site)) +
      geom_point()
      #geom_jitter()
    
    ggplot(data = flrsnow, mapping = aes(meltdate, lastflr, color = site)) +
      #geom_point()
      geom_jitter()
    
    # anova
    firstflrlm = lm(meltdate ~ firstflr, data = flrsnow)
    summary(firstflrlm)
    anova(firstflrlm)
    # later snowmelt significantly delays first flowering 

    lastflrlm = lm(meltdate ~ lastflr, data = flrsnow)
    summary(lastflrlm)
    anova(lastflrlm)    
    # later snowmelt significantly delays last flowering, but to a lesser degree than it delays first flowering. Implies there must be some mechanism for plants to "catch up", OR that flowering just has a hard end date beyond which the plant can't produce more (late season drought?)
    
## 




