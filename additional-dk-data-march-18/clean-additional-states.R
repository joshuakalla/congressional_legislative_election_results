library(foreign)
library(tidyverse)
library(janitor)
library(readxl)

rm(list = ls())

setwd('~/Dropbox/Data/congressional_legislative_election_results/additional-dk-data-march-18/')


# read existing data to match formatting

hd.results = read.csv('../presidential_results_by_lower_chamber.csv')
ssd.results = read.csv('../presidential_results_by_upper_chamber.csv')

# read census file for formatting
hd.census = read.dta("~/Dropbox/Data/ACS 2016 SLDs/2016acs5yr.dta") %>% 
  select(geoid, geoid2, geoname) %>%
  unique()

# add in additional states to cleaned Daily Kos presidential vote as of March 2018

# DE IN KY MT NE ND PA SD UT WY

# still missing: AL AR MS

## NEBRASKA UNICAM - skip for now, not sample for NCS 



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
  mutate(state = rep("10"))


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
  mutate(state = rep("10"))

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
  mutate(state = rep("18"))


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
  mutate(state = rep("18"))

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
  mutate(state = rep("21"))

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
  mutate(state = rep("21"))

### MONTANA NESTING

mt.hd = Montana.2016.pres.by.sdhd %>% 
  select(-SD, -County) %>% 
  filter(str_detect(HD, "Total")) %>% # keep only rows with district totals
  mutate(dist = as.numeric(stringr::word(HD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(state = rep("30"))


mt.sd = Montana.2016.pres.by.sdhd %>% 
  select(-HD, -County) %>% 
  filter(str_detect(SD, "Total")) %>% # keep only rows with district totals
  mutate(dist = as.numeric(stringr::word(SD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(state = rep("30"))




## ND - same districts both levels, house elects two members
nd.hd = north.dakota.2016.pres.by.ld %>% 
  select(-County) %>% 
  filter(str_detect(LD, "Total")) %>% # keep only rows with district totals
  mutate(dist = as.numeric(stringr::word(LD))) %>% 
  mutate(dist = sprintf("%02d", dist)) %>% 
  mutate(clinton = round(Clinton)) %>% 
  mutate(trump = round(Trump)) %>% 
  mutate(total = round(Total)) %>% 
  dplyr::rename('trump_percent' ='Trump%') %>% 
  select(dist, clinton, trump, trump_percent, total) %>% 
  mutate(stateabbrev = rep("ND")) %>% 
  mutate(state = rep("38"))


nd.sd = nd.hd


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
  mutate(state = rep("42"))

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
  mutate(state = rep("42"))


## SD nested ## problem with letters in districts 

sd.hd = south.dakota.2016.pres.by.sdhd %>% 
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

geoids = sld.pstrat %>% 
  select(id2)

# 
# 
# 
# 
# geoids = str_split_fixed(geoids,"^.{2}(.{2})(.*)")
# 
# 
# %>% 
#   mutate(id2 = str_split(id2, "^.{2}(.{2})(.*)"))
#          





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
  map2_df(state.names, ~mutate(., dist = sprintf("%03d", as.numeric(dist))))




# name match census geoname to geog





