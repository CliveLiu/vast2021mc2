library(shiny)
library(shinythemes)
library(raster)
library(tidyverse)
library(clock)
library(sf)
library(tmap)
library(lubridate)
library(leaflet)
library(rgdal)
library(DT)
library("bsplus") # help text



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
gps_sf0 <- st_as_sf(gps, coords = c("long", "lat"),crs= 4326)


#### for track route ####
gps_path <- gps_sf0 %>%
    group_by(id, date) %>%
    summarize(m = mean(Timestamp), 
              do_union=FALSE) %>%
    st_cast("LINESTRING")

# Compute time difference of each position
gps_sf <- dplyr::rename(gps_sf0, arrival_time = Timestamp)

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





# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                tabsetPanel(
                    tabPanel("Spatial Analysis",
                             sidebarLayout(sidebarPanel(#------ add date range input
                                 shinyWidgets::airDatepickerInput("dateselection1",
                                                                  "Select Date:",
                                                                  multiple = FALSE,
                                                                  range = FALSE,
                                                                  value = "2014-01-06",
                                                                  minDate = "2014-01-06",
                                                                  maxDate = "2014-01-19",
                                                                  inline = FALSE,
                                                                  clearButton = TRUE),
                                 
                                 #------ add time difference bar
                                 selectInput(
                                     "difftime",
                                     "Select Time Interval:",
                                     choices = c("1 min" = 60, 
                                                 "5 mins" = 300,
                                                 "30 mins" = 1800,
                                                 "1 hour" = 3600, 
                                                 "2 hours" = 7200, 
                                                 "5 hours" = 18000)
                                 ) %>% 
                                     shinyInput_label_embed(
                                         shiny_iconlink() %>%   #------ add note
                                             bs_embed_tooltip(title = "Time inverval 
                                                      is to filter the car staytime in one position.")
                                     ),
                                 
                                 #######  vehicle type
                                 radioButtons("vehicle_type",
                                              "Select Company Vehicle Type:",
                                              choices = c("Car" = "car",
                                                          "Truck" = "truck"),
                                              inline = TRUE) %>%  #------ add note
                                     shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                             bs_embed_tooltip(title = "One car was assigned to one person,
                                                                       but trucks have no assignments and can only be used for 
                                                                       business purpose. So we seperately analyze them.")
                                     ),
                                 
                                 ####### condition
                                 conditionalPanel(
                                     condition = "input.vehicle_type == 'car'",
                                     selectInput(
                                         "id",
                                         "Select Car ID:",
                                         choices = c(1,2,3,4,5,
                                                     6,7,8,9,10,
                                                     11,12,13,14,15,
                                                     16,17,18,19,20,
                                                     21,22,23,24,25,
                                                     26,27,28,29,30,
                                                     31,32,33,34,35))
                                 ),
                                 
                                 conditionalPanel(
                                     condition = "input.vehicle_type == 'truck'",
                                     selectInput(
                                         "id",
                                         "Select Car ID:",
                                         choices = c(101,104,105,106,107))
                                 ),
                                 
                                 #----- add submit button
                                 actionButton("goButton",
                                              label = "  Show Routes!",
                                              icon = icon("refresh"), 
                                              style="color: #fff; background-color: #2C3E50; border-color: #2e6da4")),
                                 
                                 
                                 mainPanel(tmapOutput("vehicle_map"))
                                 
                             ),
                             # activate tooltips, popovers, and MathJax
                             use_bs_tooltip(),
                             use_bs_popover(),
                             withMathJax()),
                    tabPanel("Tab 3")
                
                )
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #------vehicle_map
    output$vehicle_map <- renderTmap({
        
        input$goButton
        
        isolate(
            if(input$vehicle_type == "car"){
                gps_path_selected_car <- gps_path %>%
                    filter(id == input$id, date == input$dateselection1)
                
                gps_dot_selected_car <- gps_car %>% 
                    filter(date == input$dateselection1 & 
                               time_difference >= as.numeric(input$difftime))
                
                tm_shape(bgmap) +
                    tm_rgb(bgmap, r = 1,g = 2,b = 3,
                           alpha = NA,
                           saturation = 1,
                           interpolate = TRUE,
                           max.value = 255)+
                    tm_shape(gps_path_selected_car) +
                    tm_lines(col= "green3", lwd= 2, scale = 2)+
                    tm_shape(gps_dot_selected_car)+
                    tm_markers()
            }
            else if(input$vehicle_type == "truck"){
                gps_path_selected_truck <- gps_path %>%
                    filter(id == input$id, date == input$dateselection1)
                
                gps_dot_selected_truck <- gps_truck %>%
                    filter(time_difference >= as.numeric(input$difftime)) %>%
                    filter(date == input$dateselection1)
                
                tm_shape(bgmap) +
                    tm_rgb(bgmap, r = 1,g = 2,b = 3,
                           alpha = NA,
                           saturation = 1,
                           interpolate = TRUE,
                           max.value = 255)+
                    tm_shape(gps_path_selected_truck) +
                    tm_lines(col= "green3", lwd= 2, scale = 2)+
                    tm_shape(gps_dot_selected_truck)+
                    tm_markers()
            })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
