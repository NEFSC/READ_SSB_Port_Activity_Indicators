
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

#fishing data from Tanya
dat <- read.csv("PCFA_share.csv")



#find top communities last year

#first, make df of 2024 only
dat_2024<- dat[(dat$year==2024), ]


#### TOP PORTS ####
#lets just take top 12 for plotting purposes


#overall
dat_2024$top_overall <- ifelse(
  rank(-dat_2024$port_overall_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)

#overall
dat_2024$top_transaction <- ifelse(
  rank(-dat_2024$port_transaction_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)

#overall
dat_2024$top_permit <- ifelse(
  rank(-dat_2024$port_permit_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)

#overall
dat_2024$top_volume <- ifelse(
  rank(-dat_2024$port_volume_score, ties.method = "min") <= 12, 
  "top", 
  "not"
)


#make list of top ports
NE_ports_overall <- dat_2024$place_id[dat_2024$top_overall == "top"]
NE_ports_transaction <- dat_2024$place_id[dat_2024$top_transaction == "top"]
NE_ports_permit <- dat_2024$place_id[dat_2024$top_permit == "top"]
NE_ports_volume<- dat_2024$place_id[dat_2024$top_volume == "top"]



#make new df of full years based on top lists
top_ports_overall_dat <- dat[dat$place_id %in% NE_ports_overall, ]
top_ports_transaction_dat <- dat[dat$place_id %in% NE_ports_transaction, ]
top_ports_permit_dat <- dat[dat$place_id %in% NE_ports_permit, ]
top_ports_volume_dat <- dat[dat$place_id %in% NE_ports_volume, ]








#plot it
top_ports_overall_dat$year <- as.numeric(top_ports_overall_dat$year)
top_ports_transaction_dat$year <- as.numeric(top_ports_transaction_dat$year)
top_ports_permit_dat$year <- as.numeric(top_ports_permit_dat$year)
top_ports_volume_dat$year <- as.numeric(top_ports_volume_dat$year)


#overall
#tiff("port_scores_temporal_overall.tiff", units="in", width=7, height=7, res=200)


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

#dev.off()




#transaction
#tiff("port_scores_temporal_transaction.tiff", units="in", width=7, height=7, res=200)


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

#dev.off()



#permit
#tiff("port_scores_temporal_permit.tiff", units="in", width=7, height=7, res=200)

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
                     limits= c(NA, NA), breaks = c(2000,2004,2008,2012,2016,2020,2024))+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)

#dev.off()





#tiff("port_scores_temporal_volume.tiff", units="in", width=7, height=7, res=200)

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


#dev.off()



#dev.off()

#facet
place_means <- top_ports_overall_dat %>%
  group_by(place_id) %>%
  summarize(overall_mean = mean(port_overall_score, na.rm = TRUE))


#tiff("NEFMC_port_scores_facet_full.tiff", units="in", width=5, height=7, res=200)

top_ports_overall_dat  %>%
  ggplot(aes(x=year, y=port_overall_score)) + 
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
tiff("NE_scores_plot2.tiff", units="in", width=7, height=7, res=200)


place_means <- top_ports_overall_dat %>%
  group_by(place_id) %>%
  summarize(overall_mean = mean(port_overall_score, na.rm = TRUE))


top_ports_overall_dat  %>%
  ggplot(aes(x=year)) + 
  
  geom_hline(
    data = place_means, 
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
  scale_x_continuous(limits= c(2007, 2024), breaks = c(2008,2012,2016,2020,2024))+
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
#new plot with changing background colors


#try radar plot

# --- 1. Structure the Data for coord_polar ---
 #pivoting long and duplicating the first port's data at the end of each group.

spider_data_native <- top_ports_overall_dat %>%
  # Bring your 4 scores into a single long column
  pivot_longer(
    cols = c(port_volume_score, port_transaction_score, port_permit_score, port_overall_score), 
    names_to = "Score_Type", 
    values_to = "Score_Value"
  ) %>%
  mutate(Score_Type = case_when(
    Score_Type == "port_volume_score" ~ "Volume score",
    Score_Type == "port_transaction_score" ~ "Transaction score",
    Score_Type == "port_permit_score" ~ "Permit score",
    Score_Type == "port_overall_score" ~ "Overall Indicator score"
  ))

# Crucial Trick: Duplicate the first port's entries so the circular line closes perfectly
first_port <- unique(spider_data_native$place_id)[1]

closed_loop_data <- spider_data_native %>%
  bind_rows(
    spider_data_native %>% 
      filter(place_id == first_port) %>% 
      mutate(place_id = factor(place_id, levels = unique(spider_data_native$place_id)))
  )

# --- 2. Build the Plot ---
ggplot(closed_loop_data, aes(x = place_id, y = Score_Value, color = Score_Type, group = Score_Type)) +
  # Draw the lines connecting the ports
  geom_path(linewidth = 0.8, alpha = 0.8) +
  # Add points on the nodes
  geom_point(size = 1.5, alpha = 0.8) +
  
  # Transform the plot into a circular layout (theta = "x" makes the ports wrap around the circle)
  coord_polar(theta = "x") +
  
  # Split into a radar grid per year
  facet_wrap(~year, ncol = 3) +
  
  # Use your exact original color choices
  scale_color_manual(
    name = "Score Type",
    values = c(
      "Volume score" = "blue",
      "Transaction score" = "green",
      "Permit score" = "orange",
      "Overall Indicator score" = "black"
    )
  ) +
  
  # Styling and clean up
  labs(
    title = "Fishing Port Indicators Across Years",
    x = NULL, 
    y = "Indicator Score"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 8, color = "black"), # Port labels on circle edge
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )





##stop

library(gganimate)
library(transformr)



# --- 1. Identify the Strict 2024 Order ---
port_order_2024 <- top_ports_overall_dat %>%
  filter(year == 2024) %>%
  distinct(place_id, port_overall_score) %>%
  arrange(desc(port_overall_score)) %>%
  pull(place_id)

# Create a reference mapping table with explicit numeric coordinates
port_mapping <- data.frame(
  place_id = port_order_2024,
  port_numeric = 1:length(port_order_2024) # 1 to 12
)

# --- 2. Reshape and Map to Numeric Order ---
spider_data_native <- top_ports_overall_dat %>%
  pivot_longer(
    cols = c(port_volume_score, port_transaction_score, port_permit_score, port_overall_score), 
    names_to = "Score_Type", 
    values_to = "Score_Value"
  ) %>%
  mutate(Score_Type = case_when(
    Score_Type == "port_volume_score" ~ "Volume score",
    Score_Type == "port_transaction_score" ~ "Transaction score",
    Score_Type == "port_permit_score" ~ "Permit score",
    Score_Type == "port_overall_score" ~ "Overall Indicator score"
  )) %>%
  # Merge the numeric sequence into the main data
  inner_join(port_mapping, by = "place_id")

# --- 3. Manually Create the Closed Loop (12 connects back to 1) ---
# For every year and score type, we duplicate the Port #1 row and assign it a position of 13
closed_loops <- spider_data_native %>%
  filter(port_numeric == 1) %>%
  mutate(port_numeric = 13)

final_ordered_data <- spider_data_native %>%
  bind_rows(closed_loops) %>%
  # Physical row ordering rule that R cannot misinterpret
  arrange(year, Score_Type, port_numeric)

# --- 4. Build the Plot Using Numeric X-Axis ---
animated_plot <- ggplot(final_ordered_data, aes(x = port_numeric, y = Score_Value, color = Score_Type, group = Score_Type)) +
  geom_path(linewidth = 1, alpha = 0.4) +
  geom_point(size = 3, alpha = 0.8) +
  
  # Force a circular layout on our numeric system
  coord_polar(theta = "x", clip="off") +
  
  # Replace the numbers (1-12) on the outer edge with your actual Port names
  scale_x_continuous(
    breaks = 1:length(port_order_2024),
    labels = port_order_2024
  ) +
  
  scale_color_manual(
    name = "Score Type",
    values = c(
      "Volume score" = "blue",
      "Transaction score" = "green",
      "Permit score" = "orange",
      "Overall Indicator score" = "black"
    )
  ) +
  
  labs(
    title = "Fishing Port Indicators - Year: {current_frame}",
    subtitle = "Ports ordered clockwise by highest 2024 Overall Score",
    x = NULL, 
    y = "Indicator Score"
  ) +
  theme_bw() +
  theme(
    title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 9, color = "black"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(t = 20, r = 40, b = 20, l = 40, unit = "pt"),
    panel.border = element_blank()
  ) +
  transition_manual(year)

# --- 5. Render and Save ---



animate(
  animated_plot, 
  nframes = length(unique(final_ordered_data$year)), 
  fps = 1, 
  res = 150, 
  width = 1200,    # Increased width
  height = 900,
  renderer = gifski_renderer("port_indicators_perfect_loop.gif")
)

anim_save("animation1.gif")


#stop







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



