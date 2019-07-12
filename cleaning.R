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

#write.csv(d, "alldatatest.csv")

#### Analysis ####

read_csv()

#separate(uniqueID, sep = " ", into = c("site", "plot", "quad", "ind")) %>% # deconcatenate uniqueID column into site, plot, quad, individual
# mutate(ind = str_remove_all(ind, "#")) %>% # remove "#" from individual numbers in ind column
#mutate(quad = str_remove_all(quad, "Q")) %>%
#mutate(plot = str_remove_all(plot, "P"))