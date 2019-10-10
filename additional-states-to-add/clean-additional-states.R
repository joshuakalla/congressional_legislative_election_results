library(foreign)
library(tidyverse)
library(readxl)


setwd('~/Dropbox/Data/congressional_legislative_election_results/additional-states-to-add//')


# read existing data to match formatting

hd.results = read.csv('../presidential_results_by_lower_chamber.csv')
ssd.results = read.csv('../presidential_results_by_upper_chamber.csv')

# read census file for formatting
hd.census = read.dta("~/Dropbox/Data/ACS 2016 SLDs/2016acs5yr.dta") %>% 
  select(geoid, geoid2, geoname) %>%
  unique()

upper.districts = hd.census[1:1938,]
lower.districts = hd.census[1939:nrow(hd.census),]

# add in additional states to cleaned Daily Kos presidential vote as of March 2018

# DE IN KY MT NE ND PA SD UT WY

# still missing: AL AR MS

## NEBRASKA UNICAM - skip for now


temp = list.files(pattern="*.xlsx")
list2env(
  lapply(setNames(temp, make.names(gsub("*.xlsx$", "", temp))), 
         read_xlsx), envir = .GlobalEnv)


# clean up 
de.hd = delaware.2016.pres.by.hd %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total ) %>% 
  mutate(stateabbrev = rep("DE")) %>% 
  mutate(state = rep("10")) %>% 
  mutate(level = rep("L"))


de.sd = delaware.2016.pres.by.sd %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("DE")) %>% 
  mutate(state = rep("10")) %>% 
  mutate(level = rep("U"))

in.hd = Indiana.2016.pres.by.hd %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total ) %>% 
  mutate(stateabbrev = rep("IN")) %>% 
  mutate(state = rep("18")) %>% 
  mutate(level = rep("L"))


in.sd = Indiana.2016.pres.by.sd %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("IN")) %>% 
  mutate(state = rep("18")) %>% 
  mutate(level = rep("U"))

ky.hd = Kentucky.2016.pres.by.hd %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total ) %>% 
  mutate(stateabbrev = rep("KY")) %>% 
  mutate(state = rep("21")) %>% 
  mutate(level = rep("L"))

ky.sd = Kentucky.2016.pres.by.sd %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("KY")) %>% 
  mutate(state = rep("21")) %>% 
  mutate(level = rep("U"))

### MONTANA NESTING

mt.hd = Montana.2016.pres.by.sdhd %>% 
  select(-SD, -County) %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD!="Grand Total") %>% 
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(state = rep("30")) %>% 
  mutate(stateabbrev = rep("MT")) %>% 
  mutate(level = rep("L"))


mt.sd = Montana.2016.pres.by.sdhd %>% 
  select(-HD, -County) %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD!="Grand Total") %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(state = rep("30")) %>% 
  mutate(stateabbrev = rep("MT")) %>%
  mutate(level = rep("U"))




## ND - same districts both levels, house elects two members
nd.hd = north.dakota.2016.pres.by.ld %>% 
  select(-County) %>% 
  filter(str_detect(LD, "Total")) %>% # keep only rows with district totals
  filter(LD!="Grand Total") %>% 
  mutate(dist = as.numeric(stringr::word(LD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("ND")) %>% 
  mutate(state = rep("38")) %>% 
  mutate(level = rep("L"))


nd.sd = nd.hd %>% 
  mutate(level = rep("U"))


pa.hd = Pennsylvania.2016.pres.by.hd %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total ) %>% 
  mutate(stateabbrev = rep("PA")) %>% 
  mutate(state = rep("42")) %>% 
  mutate(level = rep("L"))

pa.sd = Pennsylvania.2016.pres.by.sd %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("PA")) %>% 
  mutate(state = rep("42")) %>% 
  mutate(level = rep("U"))


## SD nested 

sd.hd = south.dakota.2016.pres.by.sdhd %>% 
  select(-SD) %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = stringr::word(HD)) %>% #preserve the letters 
  mutate(dist = sprintf("%02s", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total ) %>% 
  mutate(stateabbrev = rep("SD")) %>% 
  mutate(state = rep("46")) %>% 
  mutate(level = rep("L"))

sd.sd = south.dakota.2016.pres.by.sdhd %>% 
  select(-HD) %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("SD")) %>% 
  mutate(state = rep("46")) %>% 
  mutate(level = rep("U"))



ut.hd = utah.2016.pres.by.hd %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total ) %>% 
  mutate(stateabbrev = rep("UT")) %>% 
  mutate(state = rep("49")) %>% 
  mutate(level = rep("L"))

ut.sd = utah.2016.pres.by.sd %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("UT")) %>% 
  mutate(state = rep("49")) %>% 
  mutate(level = rep("U"))


# WY nested 

wy.hd = wyoming.2016.pres.by.sdhd %>% 
  select(-SD) %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  filter(HD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total ) %>% 
  mutate(stateabbrev = rep("WY")) %>% 
  mutate(state = rep("56")) %>% 
  mutate(level = rep("L"))

wy.sd = wyoming.2016.pres.by.sdhd %>% 
  select(-HD) %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  filter(SD != "Grand Total") %>% 
  select(-County) %>% 
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total)%>% 
  mutate(stateabbrev = rep("WY")) %>% 
  mutate(state = rep("56")) %>% 
  mutate(level = rep("U"))


####  merge contextual data to match to Kalla version ### 

sld.pstrat = read.dta('~/Dropbox/Data/ACS 2016 SLDs/2016acs5yr.dta', convert.underscore = TRUE)

sld.pstrat = sld.pstrat %>% 
  select(geoid2, geoname) %>% 
  unique() %>% 
  dplyr::rename(geography = geoname) %>% 
  dplyr::rename(id2 = geoid2)



# put all new states in a list
new.states = list(de.hd,
                  de.sd,
                  in.hd,
                  in.sd,
                  ky.hd,
                  ky.sd,
                  mt.hd,
                  mt.sd,
                  nd.hd,
                  nd.sd,
                  pa.hd,
                  pa.sd,
                  sd.hd,
                  sd.sd,
                  ut.hd,
                  ut.sd,
                  wy.hd,
                  wy.sd
                  )

df.names = c("de.hd", 
               "de.sd", 
               "in.hd", 
               "in.sd", 
               "ky.hd", 
               "ky.sd", 
               "mt.hd", 
               "mt.sd", 
               "nd.hd", 
               "nd.sd", 
               "pa.hd", 
               "pa.sd", 
               "sd.hd", 
               "sd.sd", 
               "ut.hd", 
               "ut.sd", 
               "wy.hd", 
               "wy.sd"
)
  

  
# lmap new district code to three digits 
new.states = new.states %>% 
  map2_df(df.names, ~mutate(., dist = sprintf("%03s", dist))) %>% 
  mutate(id2 = paste0(state,'-',dist)) %>% 
  mutate(geoid2 = paste0(state,dist)) # this is just to match to the district names


# name match census geoname to geog

# pull out district names and geoid2 to match 

upper.districts = upper.districts %>% 
  select(geoid2, geoname) %>% 
  dplyr::rename(geography = geoname)

lower.districts = lower.districts %>% 
  select(geoid2, geoname) %>% 
  dplyr::rename(geography = geoname)

# join in district names
new.states.lower = new.states %>% 
  filter(level=="L") %>% 
  left_join(lower.districts, by = 'geoid2') %>% 
  select(-geoid2)

new.states.upper = new.states %>% 
  filter(level=="U") %>% 
  left_join(upper.districts, by = 'geoid2') %>% 
  select(-geoid2)


# join new results to old, match formatting 
results.lower = hd.results %>% 
  mutate(state = as.character(state)) %>% 
  full_join(new.states.lower) %>% 
  select(-dist, -level)

results.upper = ssd.results %>% 
  mutate(state = as.character(state)) %>% 
  full_join(new.states.upper) %>% 
  select(-dist, -level)

# save
write.csv(results.lower, "../presidential_results_by_lower_chamber.csv")
write.csv(results.upper, "../presidential_results_by_upper_chamber.csv")
x
