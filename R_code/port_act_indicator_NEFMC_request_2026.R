
#### PORT ACTIVITY COMMERCIAL FISHING INDICATOR ####
#this code is for the NEFMC request to inform their risk policy with our indicators####


#### PACKAGES ####

library(caret)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)






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

#subset just variables we care about for now, with lobster pounds for NEFMC port sorting
NE_dat <-NE_dat[c("PORT_NAME","STATE_ABB", "totalvl","totallbs", "totaldealers", "com_permits", "CAL_YEAR","lobster_totallbs")]


#rename fish year column 
NE_dat<-NE_dat %>% 
  rename(YEAR=CAL_YEAR)


# Add a new column with a combined id
NE_dat$place_id <- paste(NE_dat$PORT_NAME, NE_dat$STATE_ABB, sep = "_")


#make columns numeric 
NE_dat$totalvl <- as.numeric(NE_dat$totalvl)
NE_dat$totallbs <- as.numeric(NE_dat$totallbs)
NE_dat$totaldealers <- as.numeric(NE_dat$totaldealers)
NE_dat$com_permits <- as.numeric(NE_dat$com_permits)


#add new column for proportion of lobster landings
NE_dat$lobster_prop<- (NE_dat$lobster_totallbs/NE_dat$totallbs)




##### inflation #####

#im saving some of this script as a reference but ill be changing my inflation adjusments to match quinn bernier

#take just columns that need normalizing for inflation
#NE_dat_inf <- NE_dat[, c("YEAR", "place_id", "totalvl")]

# Split data by YEAR so I can adjust separately for each year of data
#NE_dat_split <- split(NE_dat_inf, NE_dat_inf$YEAR)
#cat("Data split into", length(NE_dat_split), "dataframes.\n")


#0-1 range scale
#process07 <- preProcess(as.data.frame(NE_dat_split[[1]]), rangeBounds = c(0,1), method=c("range"))
#dat_norm07 <- predict(process07, as.data.frame(NE_dat_split[[1]]))



# Bind the normalized dataframes back together ---
#NE_dat_inf_adj <- rbind(dat_norm07, dat_norm08, dat_norm09,dat_norm10,dat_norm11,dat_norm12,dat_norm13,
                  #      dat_norm14,dat_norm15,dat_norm16,dat_norm17,dat_norm18,dat_norm19,dat_norm20,
                   #     dat_norm21,dat_norm22,dat_norm23, dat_norm24)



#make new dataframe with these new normalized dollar columns
#NE_merged<-merge(NE_dat, NE_dat_inf_adj, by=c("YEAR","place_id"))

#checking whether there are any duplicates and there are not
#NEdup<-as.data.frame(duplicated(NE_merged[,1:2]))


##### normalize ####

#normalize data according to min-max methods
#make year factor to prevent it from being included in the normalization process
#NE_merged$YEAR<-as.character(NE_merged$YEAR)
NE_dat$YEAR<-as.character(NE_dat$YEAR)

#0-1 range scale
#norm everything except lobster columns
NE_process <- preProcess(as.data.frame(NE_dat[c(-8,-10)]), rangeBounds = c(0,1), method=c("range"))
NE_normalized <- predict(NE_process, as.data.frame(NE_dat))


#calculate average
NE_normalized$fishing_mean_score <-rowMeans(NE_normalized[,c("totallbs","totalvl","totaldealers","com_permits")], na.rm=FALSE)


#find top communities last year

#first, make df of 2024 only
NE_normalized_2024<- NE_normalized[(NE_normalized$YEAR==2024), ]


#make plot with indicator score vs lobster landings
tiff("NE_top_ports_lobsters.tiff", units="in", width=11, height=7, res=300)

NE_normalized_2024 %>%
  ggplot(aes(x=fishing_mean_score, y=lobster_prop)) + 
  geom_point(size=3, alpha = 0.9)+
  ylab("lobster proportion of landings")+
  xlab("Port Commercial Fishing Activity Indicator score")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  geom_label_repel(aes(label = ifelse(fishing_mean_score>0.04,as.character(place_id),'')),
                   size=2,
                   force=2.5,
                   box.padding   = 0.5, 
                   point.padding = 0,
                   segment.color = 'grey50',
                   min.segment.length = 0.01,
                   max.overlaps=Inf,
                   label.size = NA,
                   fill = alpha(c("white"),0.1))
 

dev.off()



#select ports for NEFMC analysis
#Stonington CT is cutoff port with score = 0.04848843
NE_normalized_2024$top<-ifelse(NE_normalized_2024$fishing_mean_score>0.0484 & NE_normalized_2024$lobster_prop<0.5,'top','not')

#make list of top ports
NEFMC_ports <- NE_normalized_2024$place_id[NE_normalized_2024$top == "top"]


#make ne df of full years base on NEFMC_ports list
NEFMC_ports_dat <- NE_normalized[NE_normalized$place_id %in% NEFMC_ports, ]






###old stuff


top_NE<-slice_max(NE_normalized_2024, fishing_mean_score, n = 10, with_ties = TRUE)
top_NE_no_inf<-slice_max(NE_normalized_2024, fishing_mean_score_no_inf, n = 10, with_ties = TRUE)

#make into list
top_NE_list <- as.list(top_NE$place_id)
top_NE_list_no_inf <- as.list(top_NE_no_inf$place_id)

#select those port ids in normalized, full dataframe
NE_normalized_top <- NE_normalized %>%
  filter(place_id %in% top_NE_list)

NE_normalized_top_no_inf <- NE_normalized %>%
  filter(place_id %in% top_NE_list_no_inf )



#### create CSVs for SOE team ####
#first remove unneccesary columns
#then write file

NE_normalized_soe <-NE_normalized[c("PORT_NAME","STATE_ABB","place_id","fishing_mean_score", "YEAR")]
write.csv(NE_normalized_soe,"NE_normalized_soe.csv", row.names = FALSE)

NE_normalized_top_soe <-NE_normalized_top[c("PORT_NAME","STATE_ABB","place_id","fishing_mean_score", "YEAR")]
write.csv(NE_normalized_top_soe,"NE_normalized_top_soe.csv", row.names = FALSE)

NE_normalized_top_soe_no_inf <-NE_normalized_top_no_inf[c("PORT_NAME","STATE_ABB","place_id","fishing_mean_score_no_inf", "YEAR")]


#### PLOT - TOP ####

NE_normalized_top$YEAR <- as.numeric(NE_normalized_top$YEAR)
NE_normalized_top_no_inf $YEAR <- as.numeric(NE_normalized_top_no_inf $YEAR)

#### PLOT COMBINED ####


#NE

tiff("NE_top_ports_1-6-2025.tiff", units="in", width=11, height=7, res=300)

NE_normalized_top %>%
  mutate(label = if_else(YEAR == max(YEAR), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=YEAR, y=fishing_mean_score, color=place_id)) + 
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

dev.off()

NE_normalized_top_no_inf %>%
  mutate(label = if_else(YEAR == max(YEAR), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=YEAR, y=fishing_mean_score_no_inf, color=place_id)) + 
  geom_point(size=3, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score _no_inf")+
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


#MA
tiff("MA_top_ports_1-6-2025.tiff", units="in", width=11, height=7, res=300)

MA_normalized_top %>%
  mutate(label = if_else(YEAR == max(YEAR), as.character(place_id), NA_character_)) %>%
  ggplot(aes(x=YEAR, y=fishing_mean_score, color=place_id)) + 
  geom_point(size=3, alpha = 0.9)+
  geom_path(linewidth=0.2)+
  ylab("Port Commercial Fishing Activity Indicator score")+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "#8ccde3",
                                        size = 0.2,
                                        linetype = 2))+
  scale_y_continuous(limits = c(0, 0.75)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, .6)),
                     breaks = c(2007, 2010, 2015, 2020,2024))+
  scale_alpha_discrete(range = c(0.15, 0.9)) + 
  scale_color_brewer(palette = "Paired")+ theme(legend.position = "none")+
  geom_label_repel(aes(label = label),hjust=0,
                   nudge_x = 1, xlim=c(2025,2035),
                   na.rm = TRUE, max.overlaps = Inf)

dev.off()





####END###

#### CENSUS INDICATORS ####

#from Changhua's data, I changed the excel file to csv
census<- read.csv("2023_National_Indicators_for__Northeast_120525.csv")


#### DATA PREP ####

#keep only columns that we care about for SOE
census_clean <-census[c("GEO_NAME", "STATEABBR",
                        "personal_disruption_rank", "pop_composition_rank", "poverty_rank",
                        "labor_force_str_rank","housing_characteristics_rank","housing_disrupt_rank",
                        "retiree_migration_rank","urban_sprawl_index_rank")]

#change name of GEONAME
census_clean <- rename(census_clean, Community = GEO_NAME)


#changing numeric factor scores to factors where 1=low,4=high
census_clean[census_clean==1] <- "low"
census_clean[census_clean==2] <- "moderate"
census_clean[census_clean==3] <- "moderate high"
census_clean[census_clean==4] <- "high"
census_clean[census_clean==0] <- NA


#NE states
NE_census <- census_clean[(census_clean$STATEABBR=='ME' | 
                             census_clean$STATEABBR=='MA'| 
                             census_clean$STATEABBR=='RI'| 
                             census_clean$STATEABBR=='CT'| 
                             census_clean$STATEABBR=='NH'), ]


#MA states
MA_census <- census_clean[(census_clean$STATEABBR=='NY' | 
                             census_clean$STATEABBR=='PA'| 
                             census_clean$STATEABBR=='NJ'| 
                             census_clean$STATEABBR=='DE'| 
                             census_clean$STATEABBR=='MD'| 
                             census_clean$STATEABBR=='VA'), ]

#Create dataframes for top ports

#New England
#as a reminder
print(top_NE_list)


#find community associated with top ports
NE_census_top <- NE_census[(NE_census$Community=='New Bedford, MA' | 
                              NE_census$Community=='Gloucester, MA'|  
                              NE_census$Community=='Narragansett/Point Judith, RI'|
                              NE_census$Community=='Boston, MA'| 
                              NE_census$Community=='Portland, ME'| 
                              NE_census$Community=='Chatham, MA'| 
                              NE_census$Community=='Harpswell/Bailey Island, ME'| 
                              NE_census$Community=='Stonington, ME'| 
                              NE_census$Community=='Friendship, ME'| 
                              NE_census$Community=='South Kingstown/Kingston/Wakefield-Peacedale, RI'), ]


#drop state for final table
NE_census_top <-NE_census_top[,-c(2)]


#Mid Atlantic
#as a reminder
print(top_MA_list)


#find community associated with top ports
MA_census_top <- MA_census[(MA_census$Community=='Reedville/District 5 (Northumberland County), VA' | 
                              MA_census$Community=='Cape May, NJ'|  
                              MA_census$Community=='Montauk, NY'|
                              MA_census$Community=='Hampton Bays/Shinnecock, NY'| 
                              MA_census$Community=='Point Pleasant Beach, NJ'| 
                              MA_census$Community=='Barnegat Light, NJ'| 
                              MA_census$Community=='Newport News, VA'| 
                              MA_census$Community=='Bronx/City Island, NY'| 
                              MA_census$Community=='Ocean City, MD'| 
                              MA_census$Community=='Brick, NJ'), ]


#drop state for final table
MA_census_top <-MA_census_top[,-c(2)]


#### FOR SOE ####

# NE Figure = NE_top_ports_1-6-2025.tiff

# NE Table = NE_census_top



# MA Figure = MA_top_ports_1-6-2025.tiff

# MA Table = MA_census_top



#tarsila analysis
#use dat


#just keep NE states
newdat <- dat[(dat$CAL_YEAR==2024 | 
                 dat$CAL_YEAR==2023 |
                 dat$CAL_YEAR==2022), ]
#NE vs MA

#just keep NE states
NE_newdat <- newdat[(newdat$STATE_ABB=='ME' | 
                       newdat$STATE_ABB=='MA'| 
                       newdat$STATE_ABB=='RI'| 
                       newdat$STATE_ABB=='CT'| 
                       newdat$STATE_ABB=='NH'), ]

#average by port
NEav <- NE_newdat %>%
  group_by(PORT_NAME,STATE_ABB) %>%
  summarise(across(where(is.numeric), mean), .groups = 'drop', na.rm = TRUE)

#drop non numeric
NEnumeric_df <- NEav[sapply(NEav, is.numeric)]

#sum columns
NEtotals<-as.data.frame(colSums(NEnumeric_df))

#write
write.csv(NEtotals,"NEtotals.csv")



MA_newdat <- newdat[(newdat$STATE_ABB=='NY' | 
                       newdat$STATE_ABB=='PA'| 
                       newdat$STATE_ABB=='NJ'| 
                       newdat$STATE_ABB=='DE'| 
                       newdat$STATE_ABB=='MD'| 
                       newdat$STATE_ABB=='VA'), ]

#average by port
MAav1 <- MA_newdat %>%
  group_by(PORT_NAME,STATE_ABB) %>%
  summarise(across(where(is.numeric), mean), na.rm = TRUE)

#drop non numeric
MAnumeric_df <- MAav[sapply(MAav, is.numeric)]

#sum columns
MAtotals<-as.data.frame(colSums(MAnumeric_df))

#write
write.csv(MAtotals,"MAtotals.csv")


library(dplyr)

df <- tibble(group1 = rep(c("X", "Y"), each = 3),
             group2 = rep(c("A", "B"), 3),
             value = 1:6)

# Grouped by group1 and group2
df %>% group_by(group1, group2)

# Summarise with default (drops group2, keeps group1)
df %>% group_by(group1, group2) %>% summarise(mean_val = mean(value))

# Summarise with .groups = 'drop' (drops both group1 and group2)
df %>% group_by(group1, group2) %>% summarise(mean_val = mean(value), .groups = 'drop')




