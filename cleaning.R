#### Lassen field work 2017-2018 ####

#### Cleaning ####

# see metadata file for file names and meanings
# used herb2017data_preclean_1.28.19.csv as raw file for analysis because it includes notes and a column for herbivory

library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)

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

#### Analysis ####

d <- read_csv("lassen-phen.csv")

# clean up final data file a bit
d <- d %>%
  select (-X1) %>%
  separate(uniqueID, sep = " ", into = c("site", "plot", "quad", "ind")) %>% # deconcatenate uniqueID column into site, plot, quad, individual
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
  mutate(trip17 = str_replace_all(trip17, "1", "2017/08/08")) %>%
  mutate(trip17 = str_replace_all(trip17, "^2$", "2017/14/08")) %>% # need to indicate ^ at beginning at $ at end of the expression because if you don't anchor to indicate the beginning and end, it will find every number 2 within other character strings (like in 2017) and replace those as well
  mutate(trip17 = str_replace_all(trip17, "^3$", "2017/21/08")) %>%
  mutate(trip17 = str_replace_all(trip17, "^4$", "2017/28/08")) %>%
  mutate(trip17 = str_replace_all(trip17, "^5$", "2017/01/09")) %>%
  mutate(trip17 = str_replace_all(trip17, "^6$", "2017/11/09")) %>%
  mutate(trip17 = str_replace_all(trip17, "^7$", "2017/18/09")) %>%
  mutate(trip17 = str_replace_all(trip17, "^8$", "2017/30/09")) %>%
  #
  unite(variable, variable, trip17, sep = "_") %>%
  unite(variable_date, variable, date, sep = "_") %>%
  mutate(variable_date = str_remove_all(variable_date, "_NA")) %>%
  separate(variable_date, sep = "_", into = c("variable", "date")) %>%
  mutate(date = str_replace_all(date, "\\.", "/")) %>% # make date formatting across years consistent
  makedate() # change dates to Julian days
  
# may be worth making a function to convert to Julian day
#require(lubridate)
#x = as.Date('2010-06-10')
#yday(x)
 
# check to make sure everything went smoothly
dnames = unique(d$variable)
ddates = unique(d$date)
dvalues = unique(d$value)


herbm$date <- sapply(strsplit(as.character(herbm$variable), split='_', fixed=TRUE), function(x) (x[2])) #somehow I think this turns i.e. "date.1" into "date_06-30-17", but not sure how
herbm$date <- as.Date(herbm$date,"%d.%m.%Y") #tells R to recognize this column as dates, and what format the dates are in to begin with
herbm$var <- sapply(strsplit(as.character(herbm$variable), split='_', fixed=TRUE), function(x) (x[1])) #making a vector of the "_flrs" (or buds, fruits) portion of the "date_flrs" column and splitting by the "_"
herbm$var=as.factor(herbm$var) #now, putting the vector into its own column, called "repro"
herbm$value2=as.numeric(herbm$value)







