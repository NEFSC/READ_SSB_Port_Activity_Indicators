
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
dat <- read.csv("fishdata_portFishingIndicators_all_10202026.csv")


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



#### NEW ENGLAND ####

#just keep NE states
NE_dat <- dat_clean[(dat_clean$STATE_ABB=='ME' | 
                       dat_clean$STATE_ABB=='MA'| 
                       dat_clean$STATE_ABB=='RI'| 
                       dat_clean$STATE_ABB=='CT'| 
                       dat_clean$STATE_ABB=='NH'), ]


#df_sorted <- NE_dat[, order(names(NE_dat))]


#subset just variables we care about for now, with lobster pounds for NEFMC port sorting
#NE_dat <-NE_dat[c("PORT_NAME","STATE_ABB", "CAL_YEAR",
      #            "totalvl","totallbs", 
       #           "total_dealers_permitted", 
        #          "com_permits",
         #         "total_dealers_land", 
          #        "total_boats_land",
           #       "total_trips")]


#rename fish year column 
NE_dat<-NE_dat %>% 
  rename(year=CAL_YEAR)


# Add a new column with a combined id
NE_dat$place_id <- paste(NE_dat$PORT_NAME, NE_dat$STATE_ABB, sep = "_")


# Process inflation adjustments with raw data
NE_dat_inf <- NE_dat %>%
  left_join(deflators, by = "year") %>%
  mutate(totalvl_inf = totalvl/adjust,
         asmfc_totalvl_inf = asmfc_totalvl/adjust,
         bluefish_totalvl_inf = bluefish_totalvl/adjust,
         butmacsq_totalvl_inf = butmacsq_totalvl/adjust,
         dogfish_totalvl_inf = dogfish_totalvl/adjust,
         herring_totalvl_inf = herring_totalvl/adjust,
         hms_totalvl_inf = hms_totalvl/adjust,
         lrgmesh_totalvl_inf = lrgmesh_totalvl/adjust,
         monkfish_totalvl_inf = monkfish_totalvl/adjust,
         nofed_totalvl_inf = nofed_totalvl/adjust,
         other_totalvl_inf = other_totalvl/adjust,
         quahog_totalvl_inf = quahog_totalvl/adjust,
         redcrab_totalvl_inf = redcrab_totalvl/adjust,
         salmon_totalvl_inf = salmon_totalvl/adjust,
         scallops_totalvl_inf = scallops_totalvl/adjust,
         sero_totalvl_inf = sero_totalvl/adjust,
         sfscpbsb_totalvl_inf = sfscpbsb_totalvl/adjust,
         skates_totalvl_inf = skates_totalvl/adjust,
         smlmesh_totalvl_inf = smlmesh_totalvl/adjust,
         surfclam_totalvl_inf = surfclam_totalvl/adjust,
         surfclamquahog_totalvl_inf = surfclamquahog_totalvl/adjust) %>%
  select(-adjust, -series_id)

#scallops proportional value
NE_dat_inf$scallops_prop_vl <- (NE_dat_inf$scallops_totalvl_inf / NE_dat_inf$totalvl_inf)




##### normalize NE ####

#normalize data according to min-max methods
#make year factor to prevent it from being included in the normalization process
NE_dat_inf$year<-as.character(NE_dat_inf$year)

#0-1 range scale
NE_process <- preProcess(as.data.frame(NE_dat_inf), rangeBounds = c(0,1), method=c("range"))
NE_normalized <- predict(NE_process, as.data.frame(NE_dat_inf))


#calculate average overall
NE_normalized$port_overall_score <-rowMeans(NE_normalized[,c("totallbs","totalvl_inf",
                                                             "total_dealers_permitted","com_permits",
                                                             "total_dealers_land","total_boats_land", "total_trips" )], na.rm=FALSE)

#calculate permit score
NE_normalized$port_permit_score <-rowMeans(NE_normalized[,c("total_dealers_permitted","com_permits")], na.rm=FALSE)


#calculate volume score
NE_normalized$port_volume_score <-rowMeans(NE_normalized[,c("totallbs","totalvl_inf", "total_trips")], na.rm=FALSE)


#calculate transaction score
NE_normalized$port_transaction_score <-rowMeans(NE_normalized[,c("total_dealers_land","total_boats_land")], na.rm=FALSE)


#### FMP Scores ####
#scallops average overall
NE_normalized$scallops_overall_score <-rowMeans(NE_normalized[,c("scallops_totallbs",
                                                             "scallops_totalvl_inf",
                                                             "scallops_permits",
                                                             "scallops_dealers_land",
                                                             "scallops_boats_land")], na.rm=FALSE)





#### Plot testing ####
#### Scallops ####

#find top ports last year

#first, make df of 2024 only
NE_2024<- NE_normalized[(NE_normalized$year==2024), ]

#find top scallop ports
NE_2024$top_scallop <- ifelse(
  rank(-NE_2024$scallops_overall_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)


#make list of top ports
scallop_ports <- NE_2024$place_id[NE_2024$top_scallop == "top"]


#make new df of full years based on top list
scallop_ports_dat <- NE_normalized[NE_normalized$place_id %in% scallop_ports, ]



scallop_ports_dat$year <- as.numeric(scallop_ports_dat$year)

# Replace 'scale_factor' with an appropriate number (e.g., 100 if scallop_proportion is 0-1 and scores are 0-100)
scale_factor <- 1

scallop_ports_dat  %>%
  ggplot(aes(x=year)) + 
  
  #scallops_overall_score
  geom_path(aes(y = scallops_overall_score, color = "Scallops score"), linewidth = 1, alpha = 0.9) + 
  
  #port_volume_score
  geom_path(aes(y = port_volume_score, color = "Volume score"), linewidth = 1, alpha = 0.5) + 
  
  #port_transaction_score
  geom_path(aes(y = port_transaction_score , color = "Transaction score"), linewidth = 1, alpha = 0.5) +
  
  #port_permit_score
  geom_path(aes(y = port_permit_score, color = "Permit score"), linewidth = 1, alpha = 0.5) +
  
  #overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"),size=2, alpha = 0.9)+
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"),linewidth=0.2)+
  
  # Second axis line for scallop_proportion (scaled to match the primary axis domain)
  geom_path(aes(y = scallops_prop_vl, color = "Scallop proportional value"), linewidth = 1, linetype = "dashed") + 
  
  # Secondary axis transformation
  scale_y_continuous(
    limits = c(0, NA),
    sec.axis = sec_axis(~ . / scale_factor, name = "Scallop proportional value")
  ) + 
  
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 5),
        legend.position = "bottom")+
  scale_x_continuous(limits= c(2010, 2025), breaks = c(2010,2014,2018,2022))+
  scale_y_continuous(limits = c(0, NA))+
  
  # Define the legend title and exact color mapping
  scale_color_manual(
    name = "Score Type",
    values = c(
      "Scallops score" = "pink",
      "Volume score" = "blue",
      "Transaction score" = "green",
      "Permit score" = "orange",
      "Overall Indicator score" = "black",
      "Scallop proportional value" = "purple"))+
  facet_wrap(~place_id, scales="free_y", ncol=3)



#-------------------------------------------------





tiff("scallop_top_ports_unscaled.tiff", units="in", width=11, height=7, res=300)

scallop_ports_dat %>% 
  ggplot(aes(x = year)) + 
  
  # CHANGED: Shading below the scallops_prop_vl line using geom_area
  geom_area(aes(y = scallops_prop_vl, fill = "Scallop proportional value"), alpha = 0.3) +
  
  
  # overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"), size = 2, alpha = 0.9) + 
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"), linewidth = 0.2) + 
  
  # scallops_overall_score
  geom_path(aes(y = scallops_overall_score, color = "Scallops score"), linewidth = 1, alpha = 0.9) + 
  
 
  
  # Secondary axis transformation (Note: You had duplicate scale_y_continuous calls; combined them here)
  scale_y_continuous(
    limits = c(0, NA), 
    sec.axis = sec_axis(~ . / scale_factor, name = "Scallop proportional value")
  ) + 
  
  ylab("Port Commercial Fishing Activity Indicator score") + 
  xlab("Year") + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_line(color = "#8ccde3", size = 0.2, linetype = 2), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    strip.text = element_text(size = 5), 
    legend.position = "bottom"
  ) + 
  scale_x_continuous(limits = c(2010, 2025), breaks = c(2010, 2014, 2018, 2022)) + 
  
  # Define the legend title and exact color mapping for lines
  scale_color_manual(
    name = "Score Type", 
    values = c(
      "Scallops score" = "purple", 
      "Overall Indicator score" = "black"
    )
  ) +
  
  # ADDED: Mapping the purple fill color for the shaded area to match the legend
  scale_fill_manual(
    name = "",
    values = c("Scallop proportional value" = "purple")
  ) +
  
  facet_wrap(~place_id, ncol = 3)

dev.off()




#-----------------------------------



scallop_ports_dat %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=scallops_overall_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     breaks = c(2007, 2010, 2015, 2020,2024))+
  scale_alpha_discrete(range = c(0.15, 0.9)) + 
  scale_color_brewer(palette = "Paired")+ theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)


scallop_ports_dat %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=scallops_prop_vl, color=place_id)) + 
  geom_point(size=3, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     breaks = c(2010, 2015, 2020,2024),
                     limits=c(2010,2027))+
  scale_alpha_discrete(range = c(0.15, 0.9)) + 
  scale_color_brewer(palette = "Paired")+ theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)








#### Mid-Atlantic ####

#I'm going to leave in NC but will need to exclude from plots
MA_dat <- dat_clean[(dat_clean$STATE_ABB=='NY' | 
                       dat_clean$STATE_ABB=='PA'| 
                       dat_clean$STATE_ABB=='NJ'| 
                       dat_clean$STATE_ABB=='DE'| 
                       dat_clean$STATE_ABB=='MD'| 
                       dat_clean$STATE_ABB=='VA'| 
                       dat_clean$STATE_ABB=='NC'), ]


#rename fish year column 
MA_dat<-MA_dat %>% 
  rename(year=CAL_YEAR)


# Add a new column with a combined id
MA_dat$place_id <- paste(MA_dat$PORT_NAME, MA_dat$STATE_ABB, sep = "_")




# Process inflation adjustments with raw data
MA_dat_inf <- MA_dat %>%
  left_join(deflators, by = "year") %>%
  mutate(totalvl_inf = totalvl/adjust,
         asmfc_totalvl_inf = asmfc_totalvl/adjust,
         bluefish_totalvl_inf = bluefish_totalvl/adjust,
         butmacsq_totalvl_inf = butmacsq_totalvl/adjust,
         dogfish_totalvl_inf = dogfish_totalvl/adjust,
         herring_totalvl_inf = herring_totalvl/adjust,
         hms_totalvl_inf = hms_totalvl/adjust,
         lrgmesh_totalvl_inf = lrgmesh_totalvl/adjust,
         monkfish_totalvl_inf = monkfish_totalvl/adjust,
         nofed_totalvl_inf = nofed_totalvl/adjust,
         other_totalvl_inf = other_totalvl/adjust,
         quahog_totalvl_inf = quahog_totalvl/adjust,
         redcrab_totalvl_inf = redcrab_totalvl/adjust,
         salmon_totalvl_inf = salmon_totalvl/adjust,
         scallops_totalvl_inf = scallops_totalvl/adjust,
         sero_totalvl_inf = sero_totalvl/adjust,
         sfscpbsb_totalvl_inf = sfscpbsb_totalvl/adjust,
         skates_totalvl_inf = skates_totalvl/adjust,
         smlmesh_totalvl_inf = smlmesh_totalvl/adjust,
         surfclam_totalvl_inf = surfclam_totalvl/adjust,
         surfclamquahog_totalvl_inf = surfclamquahog_totalvl/adjust) %>%
  select(-adjust, -series_id)

#skates proportional value
MA_dat_inf$skates_prop_vl <- (MA_dat_inf$skates_totalvl_inf / MA_dat_inf$totalvl_inf)

#skates proportional value
MA_dat_inf$sfscpbsb_prop_vl <- (MA_dat_inf$sfscpbsb_totalvl_inf / MA_dat_inf$totalvl_inf)



##### normalize MA ####

#normalize data according to min-max methods
#make year factor to prevent it from being included in the normalization process
MA_dat_inf$year<-as.character(MA_dat_inf$year)

#0-1 range scale
MA_process <- preProcess(as.data.frame(MA_dat_inf), rangeBounds = c(0,1), method=c("range"))
MA_normalized <- predict(MA_process, as.data.frame(MA_dat_inf))



#calculate average overall
MA_normalized$port_overall_score <-rowMeans(MA_normalized[,c("totallbs","totalvl_inf",
                                                             "total_dealers_permitted","com_permits",
                                                             "total_dealers_land","total_boats_land", "total_trips" )], na.rm=FALSE)

#calculate permit score
MA_normalized$port_permit_score <-rowMeans(MA_normalized[,c("total_dealers_permitted","com_permits")], na.rm=FALSE)


#calculate volume score
MA_normalized$port_volume_score <-rowMeans(MA_normalized[,c("totallbs","totalvl_inf", "total_trips")], na.rm=FALSE)


#calculate transaction score
MA_normalized$port_transaction_score <-rowMeans(MA_normalized[,c("total_dealers_land","total_boats_land")], na.rm=FALSE)


#### FMP Scores ####
#skates average overall
MA_normalized$skates_overall_score <-rowMeans(MA_normalized[,c("skates_totallbs",
                                                                 "skates_totalvl_inf",
                                                                 "skates_permits",
                                                                 "skates_dealers_land",
                                                                 "skates_boats_land")], na.rm=FALSE)

#sfscpbsb average overall
MA_normalized$sfscpbsb_overall_score <-rowMeans(MA_normalized[,c("sfscpbsb_totallbs",
                                                               "sfscpbsb_totalvl_inf",
                                                               "sfscpbsb_permits",
                                                               "sfscpbsb_dealers_land",
                                                               "sfscpbsb_boats_land")], na.rm=FALSE)



#### PLot testing ####


#### skates ####

#find top ports last year

#first, make df of 2024 only
MA_2024<- MA_normalized[(MA_normalized$year==2024), ]

#find top skate ports
MA_2024$top_skates <- ifelse(
  rank(-MA_2024$skates_overall_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)


#make list of top ports
skates_ports <- MA_2024$place_id[MA_2024$top_skates == "top"]


#make new df of full years based on top list
skates_ports_dat <- MA_normalized[MA_normalized$place_id %in% skates_ports, ]

skates_ports_dat$year <- as.numeric(skates_ports_dat$year)


#### summer flounder, scup, black sea bass - sfscpbsb ####


#find top skate ports
MA_2024$top_sfscpbsb <- ifelse(
  rank(-MA_2024$sfscpbsb_overall_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)

#make list of top ports
sfscpbsb_ports <- MA_2024$place_id[MA_2024$top_sfscpbsb == "top"]


#make new df of full years based on top list
sfscpbsb_ports_dat <- MA_normalized[MA_normalized$place_id %in% sfscpbsb_ports, ]

sfscpbsb_ports_dat$year <- as.numeric(sfscpbsb_ports_dat$year)







# Replace 'scale_factor' with an appropriate number (e.g., 100 if skate_proportion is 0-1 and scores are 0-100)
scale_factor <- 1



#-------------------------------------------------





tiff("skates_top_ports_unscaled.tiff", units="in", width=11, height=7, res=300)

skates_ports_dat %>% 
  ggplot(aes(x = year)) + 
  
  # CHANGED: Shading below the skates_prop_vl line using geom_area
  geom_area(aes(y = skates_prop_vl, fill = "skates proportional value"), alpha = 0.3) +
  
  
  # overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"), size = 2, alpha = 0.9) + 
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"), linewidth = 0.2) + 
  
  # skates_overall_score
  geom_path(aes(y = skates_overall_score, color = "skates score"), linewidth = 1, alpha = 0.9) + 
  
  
  
  # Secondary axis transformation (Note: You had duplicate scale_y_continuous calls; combined them here)
  scale_y_continuous(
    limits = c(0, NA), 
    sec.axis = sec_axis(~ . / scale_factor, name = "skates proportional value")
  ) + 
  
  ylab("Port Commercial Fishing Activity Indicator score") + 
  xlab("Year") + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_line(color = "#8ccde3", size = 0.2, linetype = 2), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    strip.text = element_text(size = 5), 
    legend.position = "bottom"
  ) + 
  scale_x_continuous(limits = c(2010, 2025), breaks = c(2010, 2014, 2018, 2022)) + 
  
  # Define the legend title and exact color mapping for lines
  scale_color_manual(
    name = "Score Type", 
    values = c(
      "skates score" = "purple", 
      "Overall Indicator score" = "black"
    )
  ) +
  
  # ADDED: Mapping the purple fill color for the shaded area to match the legend
  scale_fill_manual(
    name = "",
    values = c("skates proportional value" = "purple")
  ) +
  
  facet_wrap(~place_id, ncol = 3)

dev.off()




#-----------------------------------


tiff("sfscpbsb_top_ports_unscaled.tiff", units="in", width=11, height=7, res=300)

sfscpbsb_ports_dat %>% 
  ggplot(aes(x = year)) + 
  
  # CHANGED: Shading below the skates_prop_vl line using geom_area
  geom_area(aes(y = sfscpbsb_prop_vl, fill = "summer flounder, scup, black sea bass proportional value"), alpha = 0.3) +
  
  
  # overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"), size = 2, alpha = 0.9) + 
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"), linewidth = 0.2) + 
  
  # skates_overall_score
  geom_path(aes(y = sfscpbsb_overall_score, color = "summer flounder, scup, black sea bass score"), linewidth = 1, alpha = 0.9) + 
  
  
  
  # Secondary axis transformation (Note: You had duplicate scale_y_continuous calls; combined them here)
  scale_y_continuous(
    limits = c(0, NA), 
    sec.axis = sec_axis(~ . / scale_factor, name = "summer flounder, scup, black sea bass proportional value")
  ) + 
  
  ylab("Port Commercial Fishing Activity Indicator score") + 
  xlab("Year") + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_line(color = "#8ccde3", size = 0.2, linetype = 2), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    strip.text = element_text(size = 5), 
    legend.position = "bottom"
  ) + 
  scale_x_continuous(limits = c(2010, 2025), breaks = c(2010, 2014, 2018, 2022)) + 
  
  # Define the legend title and exact color mapping for lines
  scale_color_manual(
    name = "Score Type", 
    values = c(
      "summer flounder, scup, black sea bass score" = "purple", 
      "Overall Indicator score" = "black"
    )
  ) +
  
  # ADDED: Mapping the purple fill color for the shaded area to match the legend
  scale_fill_manual(
    name = "",
    values = c("summer flounder, scup, black sea bass proportional value" = "purple")
  ) +
  
  facet_wrap(~place_id, ncol = 3)

dev.off()








#### STOP ####


