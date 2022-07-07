# This is a replication of the H part of the H&T Index for National Capital Region

# packages 
library(dplyr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(RPostgreSQL)
library(data.table)
library(ggplot2)
library(viridis)

# your working directory
setwd("~/git/housing_costs_H_and_T_index")

#######################
# COUNTIES in DMV AREA
#######################

# county shapes and geoids
dmv_counties <- list(
  dc = "District of Columbia",
  md = c("Charles", "Frederick", "Montgomery", "Prince George's"),
  va = c(
    "Alexandria", "Arlington", "Fairfax", "Falls Church", "Loudoun", "Manassas",
    "Manassas Park", "Prince William"
  )
)
shapes <- list()
for(state in c("dc", "md", "va")){
  # shapes
  counties <- counties(state)
  ## store subsets to combine later
  counties <- counties[counties$NAME %in% dmv_counties[[state]],]
  shapes[[state]] <- list(
    counties = counties)
}
for(level in names(shapes$dc)){
  counties <- do.call(rbind, lapply(shapes, "[[", level))}

counties_GEOID <- counties$GEOID

###################################
# FETCH ACS VARS
###################################

years <- lst(2014,2015,2016,2017,2018, 2019) 
dmv.bg <- map(
  years,
  ~ get_acs(geography = "block group",
            year = .x,
            variables = c(smoc = "B25088_002",
                          med_rent = "B25064_001",
                          owners = "B25003_002",
                          renters = "B25003_003"
            ),
            state = c("VA", "DC", "MD"),
            survey = "acs5",
            output = "wide",
            geometry = TRUE)
) %>% map2(years, ~ mutate(.x, year = .y))

#
dmv.bg.red <- reduce(dmv.bg, rbind) %>% filter(substr(GEOID, 1, 5) %in% counties_GEOID) %>% 
  transmute(
    GEOID=GEOID,
    NAME = NAME,
    median_mortgage_cost = smocE, 
    median_rent = med_rentE,
    owners = ownersE,
    renters = rentersE,
    smoc_t_owners = smocE * ownersE, # median morgage costs * number of owners
    rent_t_renters = med_rentE * rentersE, # median rent * number of renters
    owners_p_renters = ownersE + rentersE, # total renters + owners
    year = year,
    geometry = geometry
  )

# HOUSING COSTS (Weighted Average)
dmv.bg.red['h_cost'] <- (dmv.bg.red$smoc_t_owners + dmv.bg.red$rent_t_renters) / dmv.bg.red$owners_p_renters
dmv.bg.up <- dmv.bg.red %>% select(GEOID, median_mortgage_cost, median_rent, owners, renters, h_cost, year)

#################
# H COSTS MAP
#################

ggplot() +
  geom_sf(data = dmv.bg.up, aes(geometry=geometry, fill=h_cost), lwd = 0) +
  scale_fill_viridis() + theme_bw() + 
  geom_sf(data= counties, fill = NA, colour = "white", lwd=0.5) + # add county boundaries
  xlab("longitude") + ylab("latitude") + 
  labs(subtitle="North Capital Region", fill = "Housing Costs ($)") +
  theme(plot.subtitle = element_text(hjust = 0.5))
  

