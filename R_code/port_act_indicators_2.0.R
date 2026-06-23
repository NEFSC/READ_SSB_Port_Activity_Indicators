
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


#make columns numeric 
#NE_dat$totalvl <- as.numeric(NE_dat$totalvl)
#NE_dat$totallbs <- as.numeric(NE_dat$totallbs)
#NE_dat$total_dealers_permitted <- as.numeric(NE_dat$total_dealers_permitted)
#NE_dat$com_permits <- as.numeric(NE_dat$com_permits)





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





#find top communities last year

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







#### TOP PORTS ####
#lets just take top 12 for plotting purposes


#overall
NE_normalized_2024$top_overall <- ifelse(
  rank(-NE_normalized_2024$port_overall_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)

#overall
NE_normalized_2024$top_transaction <- ifelse(
  rank(-NE_normalized_2024$port_transaction_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)

#overall
NE_normalized_2024$top_permit <- ifelse(
  rank(-NE_normalized_2024$port_permit_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)

#overall
NE_normalized_2024$top_volume <- ifelse(
  rank(-NE_normalized_2024$port_volume_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)


#make list of top ports
NE_ports_overall <- NE_normalized_2024$place_id[NE_normalized_2024$top_overall == "top"]
NE_ports_transaction <- NE_normalized_2024$place_id[NE_normalized_2024$top_transaction == "top"]
NE_ports_permit <- NE_normalized_2024$place_id[NE_normalized_2024$top_permit == "top"]
NE_ports_volume<- NE_normalized_2024$place_id[NE_normalized_2024$top_volume == "top"]



#make new df of full years based on top lists
top_ports_overall_dat <- NE_normalized[NE_normalized$place_id %in% NE_ports_overall, ]
top_ports_transaction_dat <- NE_normalized[NE_normalized$place_id %in% NE_ports_transaction, ]
top_ports_permit_dat <- NE_normalized[NE_normalized$place_id %in% NE_ports_permit, ]
top_ports_volume_dat <- NE_normalized[NE_normalized$place_id %in% NE_ports_volume, ]








#plot it
top_ports_overall_dat$year <- as.numeric(top_ports_overall_dat$year)
top_ports_transaction_dat$year <- as.numeric(top_ports_transaction_dat$year)
top_ports_permit_dat$year <- as.numeric(top_ports_permit_dat$year)
top_ports_volume_dat$year <- as.numeric(top_ports_volume_dat$year)


#overall
tiff("port_scores_temporal_overall.tiff", units="in", width=7, height=7, res=200)


top_ports_overall_dat  %>%
  mutate(label = if_else(year == max(year) & port_overall_score >0.1, as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_overall_score, color=place_id)) + 
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
                     limits= c(NA, NA), breaks = c(2000,2004, 2008,2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)

dev.off()




#transaction
tiff("port_scores_temporal_transaction.tiff", units="in", width=7, height=7, res=200)


top_ports_transaction_dat  %>%
  mutate(label = if_else(year == max(year) & port_transaction_score >0.15, as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_transaction_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Transaction Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2000,2004,2008,2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)

dev.off()



#permit
tiff("port_scores_temporal_permit.tiff", units="in", width=7, height=7, res=200)

top_ports_permit_dat  %>%
  mutate(label = if_else(year == max(year) & port_permit_score >0.15, as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_permit_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Permit Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2008,2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)

dev.off()





tiff("port_scores_temporal_volume.tiff", units="in", width=7, height=7, res=200)

top_ports_volume_dat  %>%
  mutate(label = if_else(year == max(year) & port_volume_score >0.05, as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_volume_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Volume Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2000,2004,2008,2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)


dev.off()



#dev.off()

#facet
place_means <- top_ports_dat %>%
  group_by(place_id) %>%
  summarize(overall_mean = mean(port_ind_score, na.rm = TRUE))


#tiff("NEFMC_port_scores_facet_full.tiff", units="in", width=5, height=7, res=200)

top_ports_dat  %>%
  ggplot(aes(x=year, y=port_ind_score)) + 
  geom_point(size=2, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 5))+
  scale_x_continuous(limits= c(2007, 2024), breaks = c(2008,2012,2016,2020,2024))+
  scale_y_continuous(limits = c(0, NA))+
  theme(legend.position = "none")+
  facet_wrap(~place_id, scales="free_y", ncol=3)+geom_hline(
    data = place_means, 
    aes(yintercept = overall_mean), 
    color = "red", 
    linetype = "dotted", 
    linewidth = 0.8
  )

#dev.off()




#make a plot with other sub-dimensions
tiff("NE_scores_plot1.tiff", units="in", width=7, height=7, res=200)



top_ports_dat  %>%
  ggplot(aes(x=year)) + 
  
  geom_hline(
    data = place_means, 
    aes(yintercept = overall_mean), 
    color = "red", 
    linetype = "dotted", 
    linewidth = 0.8
  ) +
  
  #port_volume_score
  geom_path(aes(y = port_volume_score, color = "Volume Score"), linewidth = 1, alpha = 0.5) + 
  
  #port_transaction_score
  geom_path(aes(y = port_transaction_score , color = "transaction Score"), linewidth = 1, alpha = 0.5) +
  
  #port_permit_score
  geom_path(aes(y = port_permit_score, color = "permit Score"), linewidth = 1, alpha = 0.5) +
  
  #overall score
  geom_point(aes(y = port_ind_score, color = "Overall Indicator Score"),size=2, alpha = 0.9)+
  geom_path(aes(y = port_ind_score, color = "Overall Indicator Score"),linewidth=0.2)+
  
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
  scale_x_continuous(limits= c(2007, 2024), breaks = c(2008,2012,2016,2020,2024))+
  scale_y_continuous(limits = c(0, NA))+
  
  # Define the legend title and exact color mapping
  scale_color_manual(
    name = "Score Type",
    values = c(
      "Volume Score" = "blue",
      "transaction Score" = "green",
      "permit Score" = "orange",
      "Overall Indicator Score" = "black"))+
  facet_wrap(~place_id, scales="free_y", ncol=3)



dev.off()
#new plot with changing background colors




# Calculate the mean for each facet and generate thresholds
facet_thresholds <- NEFMC_ports_share_dat %>%
  group_by(place_id) %>%
  summarize(
    facet_mean = mean(port_ind_score, na.rm = TRUE),
    upper_10 = facet_mean * 1.10,
    lower_10 = facet_mean * 0.90
  )

# 3. Filter for year 2024 data and determine the background color string
facet_backgrounds <- NEFMC_ports_share_dat %>%
  filter(year == 2024) %>%
  left_join(facet_thresholds, by = "place_id") %>%
  mutate(
    bg_color = case_when(
      port_ind_score > upper_10 ~ "green",  # Soft green if above upper_10
      port_ind_score < lower_10 ~ "red",  # Soft red if below lower_10
      TRUE                      ~ "white"   # White if between upper and lower
    )
  ) %>%
  select(place_id, bg_color)




tiff("NEFMC_port_scores_facet_full_10per.tiff", units="in", width=5, height=7, res=200)

NEFMC_ports_share_dat  %>%
  ggplot(aes(x=year, y=port_ind_score)) + 
  geom_rect(
    data = facet_backgrounds,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = bg_color),
    inherit.aes = FALSE,
    alpha = 0.2 
  ) +
  geom_point(size=2, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  scale_fill_identity() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 5))+
  scale_x_continuous(limits= c(2007, 2024), breaks = c(2008,2012,2016,2020,2024))+
  scale_y_continuous(limits = c(0, NA))+
  theme(legend.position = "none")+
  facet_wrap(~place_id, scales="free_y", ncol=3)+geom_hline(
    data = place_means, 
    aes(yintercept = overall_mean), 
    color = "red", 
    linetype = "dotted", 
    linewidth = 0.8
  )+ # Upper 10% line
  geom_hline(
    data = facet_thresholds, 
    aes(yintercept = upper_10), 
    color = "blue", linetype = "dashed", linewidth = 0.8, alpha=0.5,
  ) +
  
  # Lower 20% line
  geom_hline(
    data = facet_thresholds, 
    aes(yintercept = lower_10), 
    color = "blue", linetype = "dashed", linewidth = 0.8, alpha=0.5,
  )



dev.off()



#Try K-means clustering

NE_dat_k <-NE_normalized_2024[c(
  "totalvl_inf","totallbs", 
  "total_dealers_permitted", 
  "com_permits",
  "total_dealers_land", "total_boats_land")]


set.seed(123)
km.out <- kmeans(NE_dat_k, centers = 3, nstart = 20)
km.out
