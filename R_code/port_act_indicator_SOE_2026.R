#### PORT ACTIVITY COMMERCIAL FISHING INDICATOR ####
#this take the PCFA indicators and creates plots and help users vizualize trends####


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

dat <- read.csv("PCFA_share2010_11-17-2026.csv")




#find top ports last year



#create a MA and NE datatable
#not including NC
MA_dat <- dat[(dat$STATE_ABB=='NY' | 
                dat$STATE_ABB=='PA'| 
                dat$STATE_ABB=='NJ'| 
                dat$STATE_ABB=='DE'| 
                dat$STATE_ABB=='MD'| 
                dat$STATE_ABB=='VA'), ]


#just keep NE states
NE_dat <- dat[(dat$STATE_ABB=='ME' | 
                 dat$STATE_ABB=='MA'| 
                 dat$STATE_ABB=='RI'| 
                 dat$STATE_ABB=='CT'| 
                 dat$STATE_ABB=='NH'), ]

#make 2025 data
NE_dat_2025<- NE_dat[(NE_dat$year==2025), ]
MA_dat_2025<- MA_dat[(MA_dat$year==2025), ]


#### TOP PORTS ####
#find top ports last year
NE_dat_2025$top_overall <- ifelse(
  rank(-NE_dat_2025$port_overall_score, ties.method = "min") <= 9, 
  "top", 
  "not"
)

MA_dat_2025$top_overall <- ifelse(
  rank(-MA_dat_2025$port_overall_score, ties.method = "min") <= 9,
  "top", 
  "not"
)

#make list of top ports
NE_ports_overall <- NE_dat_2025$place_id[NE_dat_2025$top_overall == "top"]
MA_ports_overall <- MA_dat_2025$place_id[MA_dat_2025$top_overall == "top"]

#make new df of full years based on top list
NE_top_ports_overall_dat <- NE_dat[NE_dat$place_id %in% NE_ports_overall, ]
MA_top_ports_overall_dat <- MA_dat[MA_dat$place_id %in% MA_ports_overall, ]


NE_top_ports_overall_dat$year <- as.numeric(NE_top_ports_overall_dat$year)
MA_top_ports_overall_dat$year <- as.numeric(MA_top_ports_overall_dat$year)



#means
NE_place_means <- NE_top_ports_overall_dat %>%
  group_by(place_id) %>%
  summarize(overall_mean = mean(port_overall_score, na.rm = TRUE))

MA_place_means <- MA_top_ports_overall_dat %>%
  group_by(place_id) %>%
  summarize(overall_mean = mean(port_overall_score, na.rm = TRUE))


#### NE FACET plot####

tiff("NE_top_ports_2026_v2.tiff", units="in", width=8, height=6, res=300)


NE_top_ports_overall_dat  %>%
  ggplot(aes(x=year)) + 
  
  geom_hline(
    data = NE_place_means, 
    aes(yintercept = overall_mean), 
    color = "red", 
    linetype = "dotted", 
    linewidth = 0.8
  ) +
  
  #port_volume_score
  geom_path(aes(y = port_volume_score, color = "Volume score"), linewidth = 1, alpha = 0.5) + 
  
  #port_transaction_score
  geom_path(aes(y = port_transaction_score , color = "Transaction score"), linewidth = 1, alpha = 0.5) +
  
  #port_permit_score
  geom_path(aes(y = port_permit_score, color = "Permit score"), linewidth = 1, alpha = 0.5) +
  
  #overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"),size=2, alpha = 0.9)+
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"),linewidth=0.2)+
  
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
  scale_x_continuous(limits= c(2010, 2025), breaks = c(2010,2014,2018,2022,2025))+
  scale_y_continuous(limits = c(0, NA))+
  
  # Define the legend title and exact color mapping
  scale_color_manual(
    name = "Score Type",
    values = c(
      "Volume score" = "blue",
      "Transaction score" = "green",
      "Permit score" = "orange",
      "Overall Indicator score" = "black"))+
  facet_wrap(~place_id, scales="free_y", ncol=3)



dev.off()


tiff("NE_top_ports_2026_v3.tiff", units="in", width=8, height=6, res=300)


NE_top_ports_overall_dat  %>%
  ggplot(aes(x=year)) + 
  
  geom_hline(
    data = NE_place_means, 
    aes(yintercept = overall_mean), 
    color = "red", 
    linetype = "dotted", 
    linewidth = 0.8
  ) +
  
  #port_volume_score
  geom_path(aes(y = port_volume_score, color = "Volume score"), linewidth = 1, alpha = 0.5) + 
  
  #port_transaction_score
  geom_path(aes(y = port_transaction_score , color = "Transaction score"), linewidth = 1, alpha = 0.5) +
  
  #port_permit_score
  geom_path(aes(y = port_permit_score, color = "Permit score"), linewidth = 1, alpha = 0.5) +
  
  #overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"),size=2, alpha = 0.9)+
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"),linewidth=0.2)+
  
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
  scale_x_continuous(limits= c(2010, 2025), breaks = c(2010,2014,2018,2022,2025))+
  scale_y_continuous(limits = c(0, NA))+
  
  # Define the legend title and exact color mapping
  scale_color_manual(
    name = "Score Type",
    values = c(
      "Volume score" = "blue",
      "Transaction score" = "green",
      "Permit score" = "orange",
      "Overall Indicator score" = "black"))+
  facet_wrap(~place_id, ncol=3)



dev.off()

 ####MA FACET plot####

tiff("MA_top_ports_2026_v2.tiff", units="in", width=8, height=6, res=300)


MA_top_ports_overall_dat  %>%
  ggplot(aes(x=year)) + 
  
  geom_hline(
    data = MA_place_means, 
    aes(yintercept = overall_mean), 
    color = "red", 
    linetype = "dotted", 
    linewidth = 0.8
  ) +
  
  #port_volume_score
  geom_path(aes(y = port_volume_score, color = "Volume score"), linewidth = 1, alpha = 0.5) + 
  
  #port_transaction_score
  geom_path(aes(y = port_transaction_score , color = "Transaction score"), linewidth = 1, alpha = 0.5) +
  
  #port_permit_score
  geom_path(aes(y = port_permit_score, color = "Permit score"), linewidth = 1, alpha = 0.5) +
  
  #overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"),size=2, alpha = 0.9)+
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"),linewidth=0.2)+
  
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
      "Volume score" = "blue",
      "Transaction score" = "green",
      "Permit score" = "orange",
      "Overall Indicator score" = "black"))+
  facet_wrap(~place_id, scales="free_y", ncol=3)



dev.off()

tiff("MA_top_ports_2026_v3.tiff", units="in", width=8, height=6, res=300)


MA_top_ports_overall_dat  %>%
  ggplot(aes(x=year)) + 
  
  geom_hline(
    data = MA_place_means, 
    aes(yintercept = overall_mean), 
    color = "red", 
    linetype = "dotted", 
    linewidth = 0.8
  ) +
  
  #port_volume_score
  geom_path(aes(y = port_volume_score, color = "Volume score"), linewidth = 1, alpha = 0.5) + 
  
  #port_transaction_score
  geom_path(aes(y = port_transaction_score , color = "Transaction score"), linewidth = 1, alpha = 0.5) +
  
  #port_permit_score
  geom_path(aes(y = port_permit_score, color = "Permit score"), linewidth = 1, alpha = 0.5) +
  
  #overall score
  geom_point(aes(y = port_overall_score, color = "Overall Indicator score"),size=2, alpha = 0.9)+
  geom_path(aes(y = port_overall_score, color = "Overall Indicator score"),linewidth=0.2)+
  
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
      "Volume score" = "blue",
      "Transaction score" = "green",
      "Permit score" = "orange",
      "Overall Indicator score" = "black"))+
  facet_wrap(~place_id, ncol=3)



dev.off()


#### NE OVERALL PLOT ####



tiff("NE_port_scores_temporal_overall.tiff", units="in", width=6, height=6, res=200)

NE_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_overall_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.65)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.5,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()



#### MA OVERALL PLOT ####

tiff("MA_port_scores_temporal_overall.tiff", units="in", width=6, height=6, res=200)

MA_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_overall_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.3)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.5,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()





#### NE TRANSACTION PLOT ####


tiff("NE_port_scores_temporal_transaction.tiff", units="in", width=6, height=6, res=200)

NE_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_transaction_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Transaction Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.9)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.5,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()




#### MA TRANSACTION PLOT ####

tiff("MA_port_scores_temporal_transaction.tiff", units="in", width=6, height=6, res=200)

MA_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_transaction_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Transaction Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.35)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.5,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()





#### NE VOLUME PLOT ####


tiff("NE_port_scores_temporal_volume.tiff", units="in", width=6, height=6, res=200)

NE_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_volume_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Volume Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.5,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()


#### MA VOLUME PLOT ####

tiff("MA_port_scores_temporal_volume.tiff", units="in", width=6, height=6, res=200)

MA_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_volume_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Volume Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.7)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.3,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()



#### NE PERMIT PLOT ####


tiff("NE_port_scores_temporal_permit.tiff", units="in", width=6, height=6, res=200)

NE_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_permit_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Permit Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.7)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.5,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()


#### MA PERMIT PLOT ####


tiff("MA_port_scores_temporal_permit.tiff", units="in", width=6, height=6, res=200)

MA_top_ports_overall_dat  %>%
  #Force place_id into an alphabetical factor first
  mutate(place_id = factor(place_id, levels = sort(unique(place_id)))) %>%
  mutate(label = if_else(year == max(year), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=year, y=port_permit_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.7)+
  geom_path(linewidth=0.2)+
  ylab("Permit Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.3)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     limits= c(NA, NA), breaks = c(2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2037),
                   na.rm = TRUE, max.overlaps = Inf,
                   direction = "y",
                   box.padding = 0.5,
                   force = 2,
                   min.segment.length = 0)+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 100, b = 5, l = 5))

dev.off()



#### CENSUS INDICATORS ####

#from Changhua's data, I changed the excel file to csv
census<- read.csv("2024_National_Indicators_for_Northeast_102126.csv")


#keep only columns that we care about for SOE
census_clean <-census[c("geo_name", "state",
                        "personal_disruption_rank", "pop_composition_rank", "poverty_rank",
                        "labor_force_str_rank","housing_characteristics_rank","housing_disrupt_rank",
                        "retiree_migration_rank","urban_sprawl_index_rank")]

#change name of GEONAME
census_clean <- rename(census_clean, Community = geo_name)


#changing numeric factor scores to factors where 1=low,4=high
census_clean[census_clean==1] <- "low"
census_clean[census_clean==2] <- "moderate"
census_clean[census_clean==3] <- "moderate high"
census_clean[census_clean==4] <- "high"
census_clean[census_clean==0] <- NA


#NE states
NE_census <- census_clean[(census_clean$state=='Maine' | 
                             census_clean$state=='Massachusetts'| 
                             census_clean$state=='Rhode Island'| 
                             census_clean$state=='Connecticut'| 
                             census_clean$state=='New Hampshire'), ]


#MA states
MA_census <- census_clean[(census_clean$state=='New York' | 
                             census_clean$state=='Pennsylvania'| 
                             census_clean$state=='New Jersey'| 
                             census_clean$state=='Delaware'| 
                             census_clean$state=='Maryland'| 
                             census_clean$state=='Virginia'| 
                             census_clean$state=='North Carolina'), ]



#Create dataframes for top ports

#New England

#show top communities from fishing data so I can i.d. them in census dat
print(NE_ports_overall)


#find community associated with top ports
NE_census_top <- NE_census[(NE_census$Community=='New Bedford' & NE_census$state == "Massachusetts"| 
                              NE_census$Community=='Gloucester' & NE_census$state == "Massachusetts" |
                              NE_census$Community=='Narragansett/Point Judith' & NE_census$state == "Rhode Island"|
                              NE_census$Community=='Boston' & NE_census$state == "Massachusetts"| 
                              NE_census$Community=='Portland' & NE_census$state == "Maine" | 
                              NE_census$Community=='Chatham'& NE_census$state == "Massachusetts" |  
                              NE_census$Community=='Harpswell/Bailey Island' & NE_census$state == "Maine"|
                              NE_census$Community=='Stonington' & NE_census$state == "Maine"| 
                              NE_census$Community=='Friendship' & NE_census$state == "Maine"), ]


#Mid Atlantic
print(MA_ports_overall)


#find community associated with top ports
MA_census_top <- MA_census[(MA_census$Community=='District 5/Reedville (Northumberland County)' & MA_census$state == "Virginia"| 
                              MA_census$Community=='Cape May' & MA_census$state == "New Jersey"|  
                              MA_census$Community=='Montauk' & MA_census$state == "New York"|
                              MA_census$Community=='Hampton Bays' & MA_census$state == "New York"| 
                              MA_census$Community=='Point Pleasant Beach borough' & MA_census$state == "New Jersey"| 
                              MA_census$Community=='Barnegat Light borough' & MA_census$state == "New Jersey"| 
                              MA_census$Community=='Newport News' & MA_census$state == "Virginia"| 
                              MA_census$Community=='Ocean City' & MA_census$state == "Maryland"|
                              MA_census$Community=='Virginia Beach' & MA_census$state == "Virginia"), ]





#### FOR SOE ####

# NE Figure = NE_top_ports_2026_v2.tiff

# NE Table = NE_census_top



# MA Figure = MA_top_ports_2026_v2.tiff

# MA Table = MA_census_top



##calculate averages for the first 5 years and then the last year
#this info will be used in discussion of results and implications

# Define a function to calculate the required scores
calculate_scores <- function(df) {
  # Sort the dataframe by the 'years' column in ascending order
  df <- df[order(df$year), ]
  
  # Calculate the average score for the first 5 years
  first_5_years_avg <- mean(df$port_overall_score[1:5])
  
  # Get the score for the last year
  last_year_score <- df$port_overall_score[nrow(df)]
  
  # Return a list or a data frame with the results
  return(data.frame(
    first_5_years_avg = first_5_years_avg,
    last_year_score = last_year_score
  ))
}

#list of dfs by port
NElist <- split(NE_top_ports_overall_dat, NE_top_ports_overall_dat$place_id)
MAlist <- split(MA_top_ports_overall_dat, MA_top_ports_overall_dat$place_id)


# Apply the function to each data frame in the list
NEresults_list <- lapply(NElist, calculate_scores)
MAresults_list <- lapply(MAlist, calculate_scores)

# Combine the results into a single data frame if desired
NEchange <- do.call(rbind, NEresults_list)
MAchange <- do.call(rbind, MAresults_list)



NEchange$percent_change <- ((NEchange$last_year_score - NEchange$first_5_years_avg) /NEchange$first_5_years_avg) * 100
NEchange$percent_change<-round(NEchange$percent_change, digits=0)

MAchange$percent_change <- ((MAchange$last_year_score - MAchange$first_5_years_avg) /MAchange$first_5_years_avg) * 100
MAchange$percent_change<-round(MAchange$percent_change, digits=0)

