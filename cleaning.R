#### Lassen field work 2017-2018 ####

#### Cleaning ####

# see metadata file for file names and meanings
# used herb2017data_preclean_1.28.19.csv as raw file for analysis because it includes notes and a column for herbivory

library(tidyverse)
library(stringr)

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
d <- select(d, -X1) %>%
  separate(uniqueID, sep = " ", into = c("site", "plot", "quad", "ind")) %>% # deconcatenate uniqueID column into site, plot, quad, individual
  mutate(ind = str_remove_all(ind, "#")) %>% # remove "#" from individual numbers in ind column
  mutate(quad = str_remove_all(quad, "Q")) %>%
  mutate(plot = str_remove_all(plot, "P")) %>%
  rename("itero.per.17" = "itero.per?") %>%
  filter(phen.1 != "D" | is.na(phen.1)) %>%
  # remove those that died before the first ever survey in 2017
  # need to explicitly include na values or filter will automatically remove them
  select(-starts_with("notes"), -starts_with("protocol"), -starts_with("emerg"), -starts_with("itero"), -c(site_27.07.2018:ind_27.07.2018)) %>%
  gather(key = "variable", value = "value", starts_with("buds"), starts_with("fruits"), starts_with("flrs"), starts_with("phen"), starts_with("date"), starts_with("longest"), starts_with("height"), starts_with("length"), starts_with("stem"), starts_with("herb"), starts_with("total"), starts_with("canopy"), -site, -plot, -quad, -ind, na.rm = FALSE) # changed data from wide to long format

# format dates correctly

herbm$date <- sapply(strsplit(as.character(herbm$variable), split='_', fixed=TRUE), function(x) (x[2])) #somehow I think this turns i.e. "date.1" into "date_06-30-17", but not sure how
herbm$date <- as.Date(herbm$date,"%d.%m.%Y") #tells R to recognize this column as dates, and what format the dates are in to begin with
herbm$var <- sapply(strsplit(as.character(herbm$variable), split='_', fixed=TRUE), function(x) (x[1])) #making a vector of the "_flrs" (or buds, fruits) portion of the "date_flrs" column and splitting by the "_"
herbm$var=as.factor(herbm$var) #now, putting the vector into its own column, called "repro"

herbm$value2=as.numeric(herbm$value)

