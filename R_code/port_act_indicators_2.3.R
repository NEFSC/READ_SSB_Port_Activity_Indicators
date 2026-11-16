
#### PORT ACTIVITY COMMERCIAL FISHING INDICATOR ####
#this code takes the data from CAMS and PERMIT tables at the port level and calculated activity indicators####


#### PACKAGES ####

library(caret)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(pals)
library(wesanderson)
library(fredr)
library(lubridate)




#### set working directory ####
setwd("C:/Users/robert.murphy/Documents/Fishing_Social_Indicators_2026/READ_SSB_Port_Activity_Indicators/data_folder/internal")


#### DATA WRANGLING ####

#fishing data from Tanya
dat <- read.csv("fishdata_portFishingIndicators_all_11-16-2026.csv")


#check states present
n_state <- data.frame(table(dat$STATE_ABB))

#remove others and counties
dat_clean<-dat[!grepl("OTHER ", dat$PORT_NAME),]

dat_clean<-dat_clean[!grepl("DISTRICT", dat_clean$PORT_NAME),]

dat_clean<-dat_clean[!grepl("\\(COUNTY\\)", dat_clean$PORT_NAME),]

##### inflation #####

# Set your unique Federal Reserve API developer key
ST_API <- readLines("ST_API.txt")

fredr_set_key(ST_API) 

# Fetch annual "Gross Domestic Product: Implicit Price Deflator" (Series: A191RD3A086NBEA)
deflators <- fredr(
  series_id = "A191RD3A086NBEA",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2025-12-31")
)

#pull the columns we need
deflators <- deflators %>%
  mutate(year = year(date))  %>%
  select(year, series_id, value) %>%
  arrange(year, series_id, value)

#set base year
baseval<-deflators %>%
  filter(year=='2025')%>% #change this for the year you want to adjust to
  pull(value)

#calculate adjustment factor for each year
deflators<-deflators %>%
  mutate(adjust=value/baseval)%>%
  select(-value)



#### SUBSET DATA ####




#subset just variables we care about for now, with lobster pounds for NEFMC port sorting
dat_simple <-dat[c("PORT_NAME","STATE_ABB", "CAL_YEAR",
                  "totalvl","totallbs", 
                  "total_dealers_permitted", 
                  "com_permits",
                  "total_dealers_land", 
                  "total_boats_land",
                  "total_trips")]


#rename fish year column 
dat_simple<-dat_simple %>% 
  rename(year=CAL_YEAR)


# Add a new column with a combined id
dat_simple$place_id <- paste(dat_simple$PORT_NAME, dat_simple$STATE_ABB, sep = "_")


# Process inflation adjustments with raw data
dat_inf <- dat_simple %>%
  left_join(deflators, by = "year") %>%
  mutate(totalvl_inf = totalvl/adjust) %>%
  select(-adjust, -series_id)




##### normalize ####

#normalize data according to min-max methods
#make year factor to prevent it from being included in the normalization process
dat_inf$year<-as.character(dat_inf$year)

#0-1 range scale
dat_process <- preProcess(as.data.frame(NE_dat_inf), rangeBounds = c(0,1), method=c("range"))
dat_normalized <- predict(dat_process, as.data.frame(dat_inf))



#calculate average overall
dat_normalized$port_overall_score <-rowMeans(dat_normalized[,c("totallbs","totalvl_inf",
                                                             "total_dealers_permitted","com_permits",
                                                             "total_dealers_land","total_boats_land", "total_trips" )], na.rm=FALSE)

#calculate permit score
dat_normalized$port_permit_score <-rowMeans(dat_normalized[,c("total_dealers_permitted","com_permits")], na.rm=FALSE)


#calculate volume score
dat_normalized$port_volume_score <-rowMeans(dat_normalized[,c("totallbs","totalvl_inf", "total_trips")], na.rm=FALSE)


#calculate transaction score
dat_normalized$port_transaction_score <-rowMeans(dat_normalized[,c("total_dealers_land","total_boats_land")], na.rm=FALSE)


#make shareable dataframe with no confidential stuff
share_dat_2010 <- dat_normalized[dat_normalized$year >= 2010, c("PORT_NAME", "STATE_ABB", "place_id", "year", "port_overall_score", "port_transaction_score", "port_volume_score", "port_permit_score")]
share_dat_all <- dat_normalized[dat_normalized$year >= 2000, c("PORT_NAME", "STATE_ABB", "place_id", "year", "port_overall_score", "port_transaction_score", "port_volume_score", "port_permit_score")]


# Export using a specific file path
write.csv(share_dat, "C:/Users/robert.murphy/Documents/Fishing_Social_Indicators_2026/READ_SSB_Port_Activity_Indicators/data_folder/internal/PCFA_share_11-16-2026.csv", row.names = FALSE)

# Export using a specific file path
write.csv(share_dat_all, "C:/Users/robert.murphy/Documents/Fishing_Social_Indicators_2026/READ_SSB_Port_Activity_Indicators/data_folder/internal/PCFA_share_11-16-2026_all.csv", row.names = FALSE)





#### STOP ####


