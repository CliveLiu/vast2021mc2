library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(tidyr)
library(dplyr)

library(sf)
library(tmap)
library(clock)

library(rgdal)
library(ggforce)

# set the current dir as workpath
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###################################################################
########################## Data Preparation in Tab 1###############
# import data & transform data type & create new feature
loyalty <- read_csv("data/loyalty_data.csv", locale=locale(encoding ="windows-1252"))
cc <- read_csv("data/cc_data.csv", locale=locale(encoding ="windows-1252"))
loyalty$timestamp <- as.Date(loyalty$timestamp, "%m/%d/%Y")
cc$timestamp <- strptime(cc$timestamp, "%m/%d/%Y %H:%M")
# separate features
loyalty$day <- mday(loyalty$timestamp)
cc$date <- as.Date(cc$timestamp, "%m/%d/%Y %H:%M")
cc$day <- mday(cc$date)
cc$hour <- hour(cc$timestamp)
cc$date_d <- as.Date(format(cc$timestamp, "%Y-%m-%d")) # specially for the DT table in tab2-1
cc$time_h_m <- format(cc$timestamp, format = "%H:%M") # specially for the DT table in tab2-1

## heatmap plot
# calculate the frequency data frame of credit and loyalty card usage
cc_freq_day <- tapply(cc$location,cc[,c("location","day")],table) %>% 
  reshape2::melt() %>% 
  rename("frequency"="value")
cc_freq_hour <- tapply(cc$location,cc[,c("location","hour")],table) %>% 
  reshape2::melt() %>% 
  rename("frequency"="value")
cc_amount_day <- tapply(cc$price,cc[,c("location","day")],sum) %>% 
  reshape2::melt() %>% 
  rename("amount"="value")
cc_amount_hour <- tapply(cc$price,cc[,c("location","hour")],sum) %>% 
  reshape2::melt() %>% 
  rename("amount"="value")

loyalty_amount_day <- tapply(loyalty$price,loyalty[,c("location","day")],sum) %>%
  reshape2::melt()
loyalty_freq_day <- tapply(loyalty$location,loyalty[,c("location","day")],table) %>%
  reshape2::melt()

# create a new empty df for "Daily Dealz", for the aesthetic plot in heatmap
new_table = data.frame("location" = c(rep("Daily Dealz", 14)),
                       "day" = c(6:19),
                       "value" = c(rep(NA,14)))
# 
loyalty_amount_day$location <- as.character(loyalty_amount_day$location)
loyalty_freq_day$location <- as.character(loyalty_freq_day$location)
# add the df to the loyalty, reorder the result
loyalty_amount_day_ <- rbind(loyalty_amount_day, new_table) %>% 
  arrange(location,day) %>% 
  rename(amount = value)
loyalty_freq_day_ <- rbind(loyalty_freq_day, new_table) %>% 
  arrange(location,day) %>% 
  rename(frequency = value)

# add day of week feature for plot
day_of_week <- data.frame(day=seq(6,19),wday=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday	", "Saturday", "Sunday"))
cc_freq_day <- left_join(cc_freq_day, day_of_week, by= "day")
cc_amount_day <- left_join(cc_amount_day, day_of_week, by= "day")
loyalty_amount_day_ <- left_join(loyalty_amount_day_, day_of_week, by= "day")
loyalty_freq_day_ <- left_join(loyalty_freq_day_, day_of_week, by= "day")

##########
# frequency in locations <- bar plot ##########
location_count <- full_join(cc, loyalty, by = c("day", "location", "price")) %>% 
  mutate(cardtype = ifelse(is.na(loyaltynum), "credit",
                           ifelse(is.na(last4ccnum), "loyalty", "both"))) %>% 
  count(location, cardtype)

## make a full join <- parallel plot
card_correspond_count <- full_join(cc, loyalty,
                                   by = c("day", "location", "price")) %>% 
  # calculate frequency
  group_by(last4ccnum, loyaltynum) %>%
  dplyr::summarise(count = n()) %>%
  # filter out mismatch
  drop_na()
# convert 'last4ccnum' into string to plot 
card_correspond_count$last4ccnum <- as.character(card_correspond_count$last4ccnum)
# find cards which appear multiple times
repeat_cc <- card_correspond_count %>% 
  group_by(last4ccnum) %>% 
  filter(n_distinct(loyaltynum)>1)
repeat_loyalty <- card_correspond_count %>% 
  group_by(loyaltynum) %>% 
  filter(n_distinct(last4ccnum)>1)
# filter out one-on-one and other pairs
card_correspond_count_one2one <- card_correspond_count %>% 
  filter(!last4ccnum %in% repeat_cc$last4ccnum) %>% 
  filter(!loyaltynum %in% repeat_loyalty$loyaltynum)
card_correspond_count_others <- card_correspond_count %>% 
  filter(last4ccnum %in% repeat_cc$last4ccnum | loyaltynum %in% repeat_loyalty$loyaltynum)
# sank plot 1
card_correspond_count_others_plot <- card_correspond_count_others  %>%
  gather_set_data(1:2) %>%        # <- ggforce helper function
  arrange(x,last4ccnum,desc(loyaltynum))
# sank plot 2
# card_correspond_count_others2 <- cc %>% 
#   full_join(loyalty, by = c("day", "location", "price")) %>% 
#   drop_na() %>% 
#   filter(last4ccnum %in% repeat_cc$last4ccnum | loyaltynum %in% repeat_loyalty$loyaltynum) %>% 
#   dplyr::select(last4ccnum, loyaltynum)

########################
### for the consumption tracking
fjoin <- read_csv("data/credit_loy.csv")
fjoin_cc <- fjoin %>% 
  filter(!cardtype == "loyalty") %>% 
  separate(timestamp, into = c("date","time"), sep=" ",remove = FALSE)


save(cc,cc_freq_day,cc_amount_day,cc_freq_hour,cc_amount_hour,
     loyalty_freq_day_,loyalty_amount_day_,
     location_count,
     card_correspond_count_one2one,card_correspond_count_others_plot,
     fjoin_cc,
     file = "cards_data_clean")

###################################################################
########################## Data Preparation in Tab 2-1 ##############
#Import data
bgmap <- raster("data/Geospatial/MC2-tourist.tif")
gps <- read.csv("data/gps.csv")
car <- read.csv("data/car-assignments.csv")

#change data type
gps$id <- as.factor(gps$id)
car$CarID <- as.factor(car$CarID)
gps$Timestamp <- date_time_parse(gps$Timestamp,zone = "",format = "%m/%d/%Y %H:%M:%S")


#add additional column
car$Name <- paste(car$FirstName,car$LastName)
gps$day <- get_day(gps$Timestamp)
gps$type <- ifelse(gps$id %in% c('101','104','105','106','107'),'Company Truck',
                   'Company Car')

gps$date <- as.Date(format(gps$Timestamp, "%Y-%m-%d"))
gps_sf_raw <- st_as_sf(gps, coords = c("long", "lat"),crs= 4326)


#### for track route ####
gps_path <- gps_sf_raw %>%
  group_by(id, date) %>%
  summarize(m = mean(Timestamp), 
            do_union=FALSE) %>%
  st_cast("LINESTRING")

# Compute time difference of each position
gps_sf <- dplyr::rename(gps_sf_raw, arrival_time = Timestamp)
gps_sf$day_of_week <- wday(gps_sf$arrival_time, label=TRUE)

#compute departure time and time difference
gps_sf <- gps_sf %>%
  group_by(id) %>%
  mutate(departure_time = lead(arrival_time))%>%
  mutate(time_difference = as.numeric(departure_time - arrival_time)) %>% 
  filter(time_difference >= 60)

gps_sf$arrival_time <- format(as.POSIXct(gps_sf$arrival_time), format = "%H:%M:%S")
gps_sf$departure_time <- format(as.POSIXct(gps_sf$departure_time), format = "%H:%M:%S")

#### track car stop ####
## Company Car
gps_car <- gps_sf %>% 
  #join data with car assignment
  inner_join(car[, c("Name", "CarID")], by = c('id' = 'CarID')) %>% 
  #set the order of the table
  dplyr::select(Name,id,date,day_of_week,time_difference, arrival_time,departure_time,geometry) 

## Company Truck
#set the order of the table
gps_truck <- gps_sf %>%
  filter(type == "Company Truck") %>% 
  dplyr::select(id,date,day_of_week,time_difference,arrival_time,departure_time,geometry)

gps_path_simplify5 <- st_simplify(gps_path, dTolerance = 2)

save(gps_path_simplify5, gps_car, gps_truck,
     file = "gps_clean")

###################################################
# p1 <- ggplot(loyalty_freq_day_,aes(x=day,y=location,text=paste("wday:", wday)))+
#   geom_tile(aes(fill=frequency))+
#   scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
#   theme(panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 10))+
#   labs(x="X Jan 2014", title="Daily Consumption Frequency of Loyalty Cards")
# ggplotly(p1)
