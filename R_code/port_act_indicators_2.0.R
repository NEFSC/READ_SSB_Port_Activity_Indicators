
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
dat <- read.csv("fishdata_portFishingIndicators_all.csv")


#check states present
n_state <- data.frame(table(dat$STATE_ABB))

#remove others and counties
dat_clean<-dat[!grepl("OTHER ", dat$PORT_NAME),]

dat_clean<-dat_clean[!grepl("DISTRICT", dat_clean$PORT_NAME),]

dat_clean<-dat_clean[!grepl("\\(COUNTY\\)", dat_clean$PORT_NAME),]


#### NEW ENGLAND ####

#just keep NE states
NE_dat <- dat_clean[(dat_clean$STATE_ABB=='ME' | 
                       dat_clean$STATE_ABB=='MA'| 
                       dat_clean$STATE_ABB=='RI'| 
                       dat_clean$STATE_ABB=='CT'| 
                       dat_clean$STATE_ABB=='NH'), ]



#subset just variables we care about for now, with lobster pounds for NEFMC port sorting
NE_dat <-NE_dat[c("PORT_NAME","STATE_ABB", "CAL_YEAR",
                  "totalvl","totallbs", 
                  "total_dealers_permitted", 
                  "com_permits",
                  "total_dealers_land", 
                  "total_boats_land")]


#rename fish year column 
NE_dat<-NE_dat %>% 
  rename(year=CAL_YEAR)


# Add a new column with a combined id
NE_dat$place_id <- paste(NE_dat$PORT_NAME, NE_dat$STATE_ABB, sep = "_")





##### inflation #####

# Set your unique Federal Reserve API developer key
ST_API <- readLines("ST_API.txt")

fredr_set_key(ST_API) 

# Fetch annual "Gross Domestic Product: Implicit Price Deflator" (Series: A191RD3A086NBEA)
deflators <- fredr(
  series_id = "A191RD3A086NBEA",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-31")
)

#pull the columns we need
deflators <- deflators %>%
  mutate(year = year(date))  %>%
  select(year, series_id, value) %>%
  arrange(year, series_id, value)

#set base year
baseval<-deflators %>%
  filter(year=='2024')%>% #change this for the year you want to adjust to
  pull(value)

#calculate adjustment factor for each year
deflators<-deflators %>%
  mutate(adjust=value/baseval)%>%
  select(-value)


# Process adjustments with raw data
NE_dat_inf <- NE_dat %>%
  left_join(deflators, by = "year") %>%
  mutate(totalvl_inf = totalvl/adjust) %>%
  select(-adjust, -series_id)






##### normalize ####

#normalize data according to min-max methods
#make year factor to prevent it from being included in the normalization process
NE_dat_inf$year<-as.character(NE_dat_inf$year)

#0-1 range scale
NE_process <- preProcess(as.data.frame(NE_dat_inf), rangeBounds = c(0,1), method=c("range"))
NE_normalized <- predict(NE_process, as.data.frame(NE_dat_inf))


NE_dat <-NE_dat[c("PORT_NAME","STATE_ABB", "CAL_YEAR",
                  "totalvl","totallbs", 
                  "total_dealers_permitted", 
                  "com_permits",
                  "total_dealers_land", 
                  "total_boats_land")]

#calculate average overall
NE_normalized$port_overall_score <-rowMeans(NE_normalized[,c("totallbs","totalvl_inf",
                                                             "total_dealers_permitted","com_permits",
                                                             "total_dealers_land","total_boats_land" )], na.rm=FALSE)

#calculate permit score
NE_normalized$port_permit_score <-rowMeans(NE_normalized[,c("total_dealers_permitted","com_permits")], na.rm=FALSE)


#calculate volume score
NE_normalized$port_volume_score <-rowMeans(NE_normalized[,c("totallbs","totalvl_inf")], na.rm=FALSE)


#calculate transaction score
NE_normalized$port_transaction_score <-rowMeans(NE_normalized[,c("total_dealers_land","total_boats_land")], na.rm=FALSE)


#make shareable dataframe with no confidential stuff
share_dat <- NE_normalized [, c("PORT_NAME", "STATE_ABB", "place_id", "year", "port_overall_score", "port_transaction_score", "port_volume_score", "port_permit_score")]


# Export using a specific file path
write.csv(share_dat, "C:/Users/robert.murphy/Documents/Fishing_Social_Indicators_2026/READ_SSB_Port_Activity_Indicators/data_folder/internal/PCFA_share.csv", row.names = FALSE)

#explore some of the data as an option




#first, make df of 2024 only
NE_normalized_2024<- NE_normalized[(NE_normalized$year==2024), ]






#make some histograms to explore data

# Reshape data and plot
NE_normalized %>%
  pivot_longer(
    cols = c(totallbs, totalvl_inf, total_dealers_permitted, com_permits,total_dealers_land,total_boats_land), # Specify the columns to plot
    names_to = "variable", 
    values_to = "value"
  ) %>%
  ggplot(aes(x = variable, y = value, fill = variable)) +
  # Transparent violin shape
  geom_jitter(alpha = 0.2, width = 0.1, color = "black") + # Transparent raw data points
  geom_violin(alpha = 0.9, trim = FALSE) +
  facet_wrap(~ variable, scales = "free") + # Panel view
  theme_minimal() +
  theme(legend.position = "none") # Removes redundant legend


#lets try just one year
NE_normalized_2024 %>%
  pivot_longer(
    cols = c(totallbs, totalvl_inf, total_dealers_permitted, com_permits,total_dealers_land,total_boats_land), # Specify the columns to plot
    names_to = "variable", 
    values_to = "value"
  ) %>%
  ggplot(aes(x = variable, y = value, fill = variable)) +
  # Transparent violin shape
  geom_jitter(alpha = 0.2, width = 0.1, color = "black") + # Transparent raw data points
  geom_violin(alpha = 0.9, trim = FALSE) +
  facet_wrap(~ variable, scales = "free") + # Panel view
  theme_minimal() +
  theme(legend.position = "none") # Removes redundant legend


#### STOP ####


