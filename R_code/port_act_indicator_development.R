
#### PORT ACTIVITY COMMERCIAL FISHING INDICATOR ####
#this code is for the NEFMC request to inform their risk policy with our indicators####


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
dat <- read.csv("fishdata_avgPriceSTRev_all.csv")


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


#make new columns representing number of dealers landing in port and number of boat landing in port
NE_dat$total_dealers_land <- NE_dat$bluefish_dealers + NE_dat$butmacsq_dealers + NE_dat$dogfish_dealers + NE_dat$herring_dealers + 
  NE_dat$lobster_dealers + NE_dat$lrgmesh_dealers + NE_dat$monkfish_dealers + NE_dat$quahog_dealers + NE_dat$redcrab_dealers + NE_dat$salmon_dealers + 
  NE_dat$scallops_dealers + NE_dat$sfscpbsb_dealers + NE_dat$skates_dealers + NE_dat$smlmesh_dealers + NE_dat$surfclam_dealers + NE_dat$tilefish_dealers

NE_dat$total_boats_land <- NE_dat$bluefish_boats + NE_dat$butmacsq_boats + NE_dat$dogfish_boats + NE_dat$herring_boats + 
  NE_dat$lobster_boats + NE_dat$lrgmesh_boats + NE_dat$monkfish_boats + NE_dat$quahog_boats + NE_dat$redcrab_boats + NE_dat$salmon_boats + 
  NE_dat$scallops_boats + NE_dat$sfscpbsb_boats + NE_dat$skates_boats + NE_dat$smlmesh_boats + NE_dat$surfclam_boats + NE_dat$tilefish_boats



#subset just variables we care about for now, with lobster pounds for NEFMC port sorting
NE_dat <-NE_dat[c("PORT_NAME","STATE_ABB", "CAL_YEAR",
                  "totalvl","totallbs", 
                  "totaldealers", 
                  "com_permits",
                  "total_dealers_land", "total_boats_land")]


#rename fish year column 
NE_dat<-NE_dat %>% 
  rename(year=CAL_YEAR)


# Add a new column with a combined id
NE_dat$place_id <- paste(NE_dat$PORT_NAME, NE_dat$STATE_ABB, sep = "_")


#make columns numeric 
NE_dat$totalvl <- as.numeric(NE_dat$totalvl)
NE_dat$totallbs <- as.numeric(NE_dat$totallbs)
NE_dat$totaldealers <- as.numeric(NE_dat$totaldealers)
NE_dat$com_permits <- as.numeric(NE_dat$com_permits)





##### inflation #####

# Set your unique Federal Reserve API developer key
ST_API <- readLines("ST_API.txt")

fredr_set_key(ST_API) 

# Fetch annual "Gross Domestic Product: Implicit Price Deflator" (Series: A191RD3A086NBEA)
deflators <- fredr(
  series_id = "A191RD3A086NBEA",
  observation_start = as.Date("2007-01-01"),
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


#calculate average overall
NE_normalized$port_ind_score <-rowMeans(NE_normalized[,c("totallbs","totalvl_inf","totaldealers","com_permits","total_dealers_land","total_boats_land" )], na.rm=FALSE)

#port_home
NE_normalized$port_home_score <-rowMeans(NE_normalized[,c("totaldealers","com_permits")], na.rm=FALSE)


#port_work (volume and actors)
NE_normalized$port_work_score <-rowMeans(NE_normalized[,c("totallbs","totalvl_inf","total_dealers_land","total_boats_land")], na.rm=FALSE)


#port_volume
NE_normalized$port_volume_score <-rowMeans(NE_normalized[,c("totallbs","totalvl_inf")], na.rm=FALSE)

#port_actors
NE_normalized$port_actors_score <-rowMeans(NE_normalized[,c("total_dealers_land","total_boats_land")], na.rm=FALSE)


#find top communities last year

#first, make df of 2024 only
NE_normalized_2024<- NE_normalized[(NE_normalized$year==2024), ]


#lets just take top 12 for plotting purposes

NE_normalized_2024 %>%
  ggplot(aes(x=port_home_score, y=port_work_score)) + 
  geom_point(size=3, alpha = 0.9)+
  ylab("Port Work")+
  xlab("Port Home")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        linewidth = 0.2,
                                        linetype = 2))+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
  geom_label_repel(aes(label = ifelse(port_home_score>0.15,as.character(place_id),'')),
                   size=2,
                   force=2.5,
                   box.padding   = 0.5, 
                   point.padding = 0,
                   segment.color = 'grey50',
                   min.segment.length = 0.01,
                   max.overlaps=Inf,
                   label.size = NA,
                   fill = alpha(c("white"),0.1))


#dev.off()


#make some histograms to explore data

# Reshape data and plot
NE_normalized %>%
  pivot_longer(
    cols = c(totallbs, totalvl_inf, totaldealers, com_permits,total_dealers_land,total_boats_land), # Specify the columns to plot
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
    cols = c(totallbs, totalvl_inf, totaldealers, com_permits,total_dealers_land,total_boats_land), # Specify the columns to plot
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








#select ports for NEFMC analysis
#I'm using the csv below to determine the top ports based on not having more than 50% lobster landings and being amonst the top indicator scores
#write.csv(NE_normalized_2024,"NE_normalized_2024.csv", row.names = FALSE)


#Stonington CT is cutoff port with score = 0.048255759
NE_normalized_2024$top<-ifelse(NE_normalized_2024$port_ind_score>0.048 & NE_normalized_2024$lobster_prop<0.5,'top','not')

#make list of top ports
NEFMC_ports <- NE_normalized_2024$place_id[NE_normalized_2024$top == "top"]


#make new df of full years based on NEFMC_ports list
NEFMC_ports_dat <- NE_normalized[NE_normalized$place_id %in% NEFMC_ports, ]


#make shareable dataframe with no confidential stuff
NEFMC_ports_share_dat <- NEFMC_ports_dat [, c("PORT_NAME", "STATE_ABB", "place_id", "year", "port_ind_score")]





#plot it
NEFMC_ports_share_dat$year <- as.numeric(NEFMC_ports_share_dat$year)


#tiff("NEFMC_port_scores_inf_full.tiff", units="in", width=9, height=7, res=200)

NEFMC_ports_share_dat  %>%
  mutate(label = if_else(year == max(year) & port_ind_score >0.1, as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_ind_score, color=place_id)) + 
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
                     limits= c(NA, NA), breaks = c(2008,2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)


#dev.off()

#facet
place_means <- NEFMC_ports_share_dat %>%
  group_by(place_id) %>%
  summarize(overall_mean = mean(port_ind_score, na.rm = TRUE))


#tiff("NEFMC_port_scores_facet_full.tiff", units="in", width=5, height=7, res=200)

NEFMC_ports_share_dat  %>%
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
  "totaldealers", 
  "com_permits",
  "total_dealers_land", "total_boats_land")]


set.seed(123)
km.out <- kmeans(NE_dat_k, centers = 3, nstart = 20)
km.out
