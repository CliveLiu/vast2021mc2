library(shinythemes)
library(shiny)
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(tidyr)
library(dplyr)

library(ggplot2)
library(plotly)
library(sf)
library(tmap)
library(clock)
library(ggforce)
library(parsetR)

library(leaflet)
library(rgdal)
library(DT)
library("bsplus") # help text tooltips

library(timevis)
library(ggiraph)
library(wordcloud)

library(visNetwork)
library(ggiraph)
library(igraph)


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
# frequency in locations <- bar plot
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
card_correspond_count_others2 <- cc %>% 
  full_join(loyalty, by = c("day", "location", "price")) %>% 
  drop_na() %>% 
  filter(last4ccnum %in% repeat_cc$last4ccnum | loyaltynum %in% repeat_loyalty$loyaltynum) %>% 
  dplyr::select(last4ccnum, loyaltynum)


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
  filter(time_difference >= 300)

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


###################################################################
########################## Data Preparation in Tab 2-2 ##############

visfull_ds  <- read_csv("data/aspatial/merged_all.csv")
cl_ds <- read_csv("data/aspatial/credit_loy.csv")
GAStech_node<- read_csv("data/aspatial/employeeinfo.csv")


visfull_ds <- visfull_ds  %>% 
  filter(!is.na(start))

visfull_ds$start <- clock::date_time_parse(visfull_ds$start,zone ="",
                                           format = "%d/%m/%y %H:%M")

vis_gp <- visfull_ds%>% dplyr::select(cid,group) %>%
  rename(id=cid, content = group) %>%
  group_by(id,content) %>%
  dplyr::summarise(count=n()) %>%
  ungroup

###################################################################
########################## Data Preparation in Tab 3 ##############

# eEdges <- read_csv("data/aspatial/network_edges.csv")
# eNodes <- read_csv("data/aspatial/employee_nodes.csv")

###################################################################
###################################################################

# Define UI
ui <- navbarPage(
    "2021 VAST Challenge MC2",
    theme = shinytheme("flatly"),
    tabPanel("Introduction",
             icon = icon("list-alt", lib = "font-awesome"),
             h3("Dynamics Temporal and Spatial Visualisation Dashboard"),
             # tags$a(href="https://vast-challenge.github.io/2021/MC2.html", "2021 VAST Challenge MC2"),
             fluidRow(column(6,
                             HTML("<p>This is based on the senario in <a href='https://vast-challenge.github.io/2021/MC2.html'>2021 VAST Challenge MC2</a>: </p>"),
                             p("In the IPO celebration of a natural gas production company, several employees went missing. 
                             The company has not been as successful in demonstrating environmental stewardship. 
                             And a local environmental organization is suspected in the disappearance."),
                             p("The analysis aims to explore the car tracking data of employees, as well as credit card transactions and loyalty car usage data which happened before the disappearance."),
                             p("The application is divided according to the tabs, shown in the right."),
                             HTML("<p>For more information of the project, please refer to our <a href='https://vast2021.netlify.app/'>Group Project Website</a>.</p>")),
                      column(6,
                             img(src="summary.png", align="right", width="80%")))),
    
    
    ##################### Tab1 #######################
    navbarMenu("EDA",
               icon = icon("bar-chart"),
               tabPanel("Heatmap",
                        h3("Heatmap"),
                        fluidRow(
                          column(6,
                                 plotlyOutput("hmap_f", height = "500px")),
                          column(6,
                                 verbatimTextOutput("loyalty_no_hour"),
                                 plotlyOutput("hmap_l", height = "500px", width = "95%"))),
                        fluidRow(
                          column(4),
                          column(2,
                                 style = "background-color:#ECF0F1;",
                                 radioButtons("c1",
                                              "Time Unit:",
                                              choices = c("Day" = "d",
                                                          "Hour" = "h"),
                                              inline = TRUE)),
                          column(2,
                                 style = "background-color:#ECF0F1;",
                                 radioButtons("c2",
                                              "Measures",
                                              choices = c("Frequency" = "freq",
                                                          "Price" = "price"),
                                              inline = TRUE)),
                          column(4)),
                        headerPanel("    ")
                        ),
               
               tabPanel("Bar Plot",
                        h3("Bar Plot"),
                        p("We make a full join between credit card and loyalty card by date, location and price. Most 
                            consumption records have perfect match, while some can't find correspondings.
                          In the figure below, the 'both' refers to matched consumption records, while the 'loyalty'
                           refers to consumption records in loyalty card which can't find a match in credit card records 
                          (the same for 'credit')."),
                        p("The bar plot below show the popularity of commercial locations. 
                            And these less popular locations can be a breakthrough in the analysis later."),
                        br(),
                        plotlyOutput("bar_location", height = "600px", width = "90%"),
                        headerPanel("    ")),
               
               tabPanel("Parallel Set",
                        h3("Check the connection between cards"),
                        p("Most credit/loyalty cards have only one corresponding card, 
                        but some of them correspond to multiple cards."),
                        p("The left table shows all one to one matched pairds, while the paralle
                          set plot on the right shows multiplt matched pairs."),
                        br(),
                        column(4,
                               DT::dataTableOutput("one2one")),
                        column(1),
                        column(6,
                               br(),
                               br(),
                               p("Mutiple Matched Pairs"),
                               # parsetOutput("sanks2"),
                               plotOutput("sanks")),
                        headerPanel("    "))),
    
    
    ##################### Tab2 #######################
    navbarMenu("Spatial Analysis",
               icon = icon("map"),
               
              tabPanel("GPS Tracking",
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
                                                    choices = c("5 mins" = 300,
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
                                                               label = "Show Routes!",
                                                               icon = icon("refresh"), 
                                                               style="color: #fff; background-color: #2C3E50; border-color: #2e6da4")),
                                     
                                     
                                     mainPanel(tmapOutput("vehicle_map"))
                                     
                                     ),
                       # activate tooltips, popovers, and MathJax
                       use_bs_tooltip(),
                       use_bs_popover(),
                       withMathJax()),
              
              ##################### Tab2-2 #######################
              
              tabPanel("Mapping GPS",
                       titlePanel("EDA on Transaction Records with Geospatial Tracking"),
                       sidebarLayout(
                         sidebarPanel(h4("Parameters for Comparison"),
                                      selectInput( "vehidc", 
                                                   "Vehicle ID:", 
                                                   choices = unique(
                                                     visfull_ds$cid[visfull_ds$cid<999])),
                                      checkboxInput(inputId = "filterc",
                                                    label = "Auotmatic Filter for Vehicle ID",
                                                    value = TRUE),
                                      selectInput( "ccardc",
                                                   "Credit Card(last 4 No):",
                                                   choices = unique(
                                                     visfull_ds$group[visfull_ds$group>1000])),
                                      
                                      h4("Zoom Date Range...."),
                                      dateInput("RangeSc",
                                                "Start Date:",
                                                value = "2014-01-06",
                                                min = "2014-01-06",
                                                max = "2014-01-18"),
                                      dateInput("RangeEc", "End Date:",
                                                value = "2014-01-19",
                                                min = "2014-01-07",
                                                max = "2014-01-19"),
                                      
                                      actionButton("zbtn", "Zoom",
                                                   icon("search"),
                                                   style='font-size:100%',
                                                   style="color: #fff;
                                                   
                                                    background-color: #77B5FE;
                                                    border-color: #2e6da4"),
                                      br(),
                                      actionButton("rbtn",
                                                   "Reset Frame",
                                                   icon("refresh"),
                                                   style='font-size:100%',
                                                   style="color: #fff;
                                                    background-color: #87ceeb;
                                                    border-color: #2e6da4"),
                                      br(),
                                      actionButton("rtablec",
                                                   "Show Full Table",
                                                   icon = icon("repeat"),
                                                   style='font-size:100%',
                                                   style="color: #fff;
                                                    background-color: #87ceeb;
                                                    border-color: #2e6da4")),
                         
                         mainPanel(wellPanel(
                                   h4("Comparison Basd on Top 4 visited locations"),
                                   p("Matching for Top 2 locations visited by 
                                           Selected Vehicle ID and Credit Card prior selected for deep 
                                           investigation via TimeViz"),
                                   fluidRow(
                                     column(6,align="center",
                                            h4("Vehicle ID."),
                                            br(),
                                            plotOutput("cloud1c", width = "300px", height="300px")),
                                     column(6,align="center",
                                            h4(" Credit Card."),
                                            br(),
                                            plotOutput("cloud2c", width = "300px", height="300px"))
                                   )),
                                   hr(),
                                   wellPanel(
                                     h4("Timevis Plot for Selected Parameters."),
                                     br(),
                                     timevisOutput("timelinec")),
                                   hr(),
                                   wellPanel(fluidRow(
                                     column(6, align="center",
                                            h4("Unmatched Loyalty Card Transactions for Credit Card"),
                                            br(),
                                            girafeOutput("loyaltyc")), # girafeOutput("loyaltyc") # plotlyOutput("loyaltyc")
                                     column(6, align="center",
                                            h4("Tagging of Vehicle ID - Credit Card"),
                                            br(),
                                            DT::dataTableOutput('mapTablec',
                                                                width = "100%"))
                                   ))
                                 )
                       ))
    ),
    
    
    
    ##################### Tab3 #######################
    navbarMenu("Network Analysis",
               icon = icon("project-diagram", lib = "font-awesome"),
               tabPanel("Network",
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       h4("Parameters for Settings"),
                                       # selectInput("locc", "Location:",
                                       #             choices =  c("All",sort(
                                       #               unique(eEdges$content[!is.na(eEdges$content)]))),
                                       #             selected ='All'),
                                       selectInput("depc", "Department:",
                                                   choices = c("All" = "All", 
                                                               "Engineering"="Engineering",
                                                               "Executive" = "Executive",
                                                               "Facilities" = "Facilities",
                                                               "Info Tech" = "Info Tech",
                                                               "Security"= "Security"),
                                                   selected = "All"),
                                       selectInput("layoutc", "Network Layout:",
                                                   choices = c("Nicely"="layout_nicely",
                                                               "Circle" = "layout_in_circle")
                                       )
                          ),
                          mainPanel(
                            h4("visNetwork Plot"),
                            hr(),
                            visNetworkOutput("visnetwork1",height="1000px")
                          )
                        )),
               tabPanel("Special",
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       h4("Parameters for Settings"),
                                       selectInput("relc", "Visitation to:",
                                                   choices = c("Anemities" = "D",
                                                               "Industrial" = "B",
                                                               "Resident"= "A",
                                                               "Unknown"= "C"),
                                                   selected = "Anemities")
                          ),
                          mainPanel(
                            h4("visNetwork Plot"),
                            hr(),
                            visNetworkOutput("visnetwork2",height="1000px")
                          )
                        )),
               tabPanel("Community")),
    
    
    ##################### footer #######################
    footer=tags$footer("Built by TAN Choo Thye, LIU Yangguang, ZHONG Linli", align = "center",
    style = "position:fixed;
            bottom:0;
            width:100%;
            height:20px;
            /* Height of the footer */
            color: white;
            font-size: x-small;
            padding: 2px;
            background-color: #2C3E50;
            z-index: 1000;")
)




# Define server
server <- function(input, output) {
  
  
  ##################### Tab1 #######################
  output$hmap_f <- renderPlotly({
    if(input$c1=="d" & input$c2=="freq"){
      p1 <- ggplot(cc_freq_day,aes(x=day,y=location))+
        geom_tile(aes(fill=frequency))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_text(size=7),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(title="Daily Consumption Frequency of Credit Cards")
      ggplotly(p1)
    }
    else if(input$c1=="d" & input$c2=="price"){
      p1 <- ggplot(cc_amount_day,aes(x=day,y=location))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_text(size=7),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(title="Daily Consumption Amount of Credit Cards")
      ggplotly(p1)
    }
    else if(input$c1=="h" & input$c2=="freq"){
      p1 <- ggplot(cc_freq_hour,aes(x=hour,y=location))+
        geom_tile(aes(fill=frequency))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_text(size=7),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(title="Hourly Consumption Frequency of Credit Cards")
      ggplotly(p1)
    }
    else if(input$c1=="h" & input$c2=="price"){
      p1 <- ggplot(cc_amount_hour,aes(x=hour,y=location))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_text(size=7),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(title="Hourly Consumption Amount of Credit Cards")
      ggplotly(p1)
    }
  })
  
  
  output$hmap_l <- renderPlotly({
    if(input$c1=="d" & input$c2=="freq"){
      p1 <- ggplot(loyalty_freq_day_,aes(x=day,y=location))+
        geom_tile(aes(fill=frequency))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(title="Daily Consumption Frequency of Loyalty Cards")
      ggplotly(p1)
    }
    else if(input$c1=="d" & input$c2=="price"){
      p1 <- ggplot(loyalty_amount_day_,aes(x=day,y=location))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(title="Daily Consumption Amount of Loyalty Cards")
      ggplotly(p1)
    }
  })
  
  
  output$loyalty_no_hour <- renderPrint(
    if(input$c1=="h"){
      "The timestamp in loyalty card data only contain date, no hour unit."
    }
  )
  
  
  output$bar_location <- renderPlotly({
    plot_ly(location_count, y = ~reorder(location, -n, sum), x = ~n,
            type = "bar", name = ~cardtype, orientation = 'h', 
            color = ~cardtype, colors = c("#57799E","#4FBAA9","#E2C179")) %>%  # c("grey50", "blue", "red"), "Blues"
      layout(# font = list(size=10),
             xaxis = list(title = "No. of Transactions", showgrid=FALSE), 
             yaxis = list(title = "", autorange = "reversed"), 
             font = list(size=10),
             barmode = "stack",
             legend = list(title = list(text = "Card Type"),
                           x = 0.85,
                           y = 0.15,
                           bgcolor = "#F9F4F1")) %>% 
      layout(title = "Frequency of Transactions",
             font = 15)
  })
  
  
  output$one2one <- DT::renderDataTable(
    card_correspond_count_one2one,
    options = list(pageLength = 10, lengthChange = FALSE),
    rownames = FALSE,
    caption = "One to One Matched Pairs"
  )
  
  
  output$sanks <- renderPlot({
    # plot
    ggplot(card_correspond_count_others_plot,
           aes(x = x, id = id, split = y, value = count)) +
      geom_parallel_sets(aes(fill = last4ccnum), alpha = 0.7,
                         axis.width = 0.2, n=100, strength = 0.5) +
      geom_parallel_sets_axes(axis.width = 0.25, fill = "gray95",
                              color = "gray80", size = 0.15) +
      geom_parallel_sets_labels(colour = 'gray35', size = 4.5,
                                angle = 0, fontface="bold") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.x  = element_blank()
      )
  })
  
  
  output$sanks2 <- renderParset({
    parset(card_correspond_count_others2)
  })
  
  
  
  ##################### Tab2-1 #######################
  
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

  
  ##################### Tab2-2 #######################
  top=2
  
  topword <- reactive({visfull_ds %>% 
      group_by(cid,trip) %>%
      dplyr::summarise(content = last(content), loctype=last(loctype),
                       count =n()) %>%
      filter(!(loctype %in% c("Residential", "Office")))%>%
      
      count(cid, content) %>%
      filter(cid %in% c(input$vehidc)) %>% 
      arrange (cid,desc(n))
  })
  
  observeEvent(topword(),{
    if (input$filterc == TRUE){t2 <- cl_ds %>%
      count(ccard,location) %>%
      arrange (ccard, desc(n)) %>%
      group_by(ccard) %>%
      slice (1:top) %>%
      filter(location %in% c(topword()$content[1]))
    
    choices <- unique(t2$ccard)
    updateSelectInput(inputId = "ccardc", choices = choices)
    }
  })
  
  observeEvent(input$filterc,{
    if(input$filterc==FALSE){choices <- unique(
      visfull_ds$group[visfull_ds$group>1000])
    updateSelectInput(inputId = "ccardc", choices = choices)
    }else{t2 <- cl_ds %>%
      count(ccard,location) %>%
      arrange (ccard, desc(n)) %>%
      group_by(ccard) %>%
      slice (1:2) %>%
      filter(location %in% c(topword()$content[1]))
    
    choices <- unique(t2$ccard)
    updateSelectInput(inputId = "ccardc", choices = choices)
    }
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloud1c <- renderPlot({
    wordcloud(words = topword()$content, freq = topword()$n,
              max.words =4,scale = c(1.6, 0.8),
              random.order=FALSE,random.color = FALSE,
              color=brewer.pal(8,"Dark2"),
              rot.per=0)
  })
  
  output$cloud2c <- renderPlot({
    wc2 <- cl_ds%>%
      count(ccard,location) %>%
      filter(ccard %in% c(input$ccardc))
    wordcloud(words = wc2$location,
              freq = wc2$n,
              max.words =4,
              random.order=FALSE,random.color = FALSE,
              color=brewer.pal(8,"Dark2"),
              scale = c(1.6, 0.8),
              rot.per=0)
  })
  
  output$loyaltyc <-  renderGirafe({ # renderGirafe, renderPlotly
    g <- cl_ds %>%
      filter(ccard  %in% c(input$ccardc)) %>%
      filter(cardtype=="loyalty") %>%
      ggplot(aes(x = day,y=location, fill=location)) + 
      geom_tile_interactive(aes(tooltip = location),
                            width=0.5,
                            height=0.5,
                            show.legend = FALSE) + 
      facet_grid(~ccard) +
      scale_x_continuous(breaks=seq(6,19,1)) +
      labs(y="Transacted Location",
           x="Date(X Jan 2014)") + 
      theme_minimal()+ 
      theme(axis.text.x = element_text(size=8),
            axis.text.y = element_text(size=8),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10, vjust=0.8)
      )
    # gg <- ggplotly(g)
    gg <- girafe(ggobj=g)
    gg <- girafe_options(gg,opts_tooltip(opacity = .9,
                                         offx = 20, offy = -10,
                                         use_fill = TRUE,
                                         use_stroke = TRUE,
                                         delay_mouseout = 1000))
    gg
  })
  
  observeEvent(input$RangeSc,{
    updateDateInput(inputId = "RangeEc", min = input$RangeSc)
  })
  
  observeEvent(input$RangeEc,{
    updateDateInput(inputId = "RangSc", max = input$RangeEc)
  })
  
  output$timelinec <- renderTimevis(
    timevis(data = visfull_ds %>%
              filter(cid %in% c(input$vehidc, input$ccardc)),
            groups=vis_gp %>%
              filter(id %in% c(input$vehidc, input$ccardc)), height=450)
  )
  
  observeEvent(input$rbtn,{
    fitWindow("timelinec", list(animation = FALSE))
  })
  
  observeEvent(input$zbtn, {
    setWindow("timelinec", input$RangeSc, input$RangEc)
    updateDateInput(inputId = "RangeEc", min = "2014-01-07")
    updateDateInput(inputId = "RangSc", max = "2014-01-18")
  })
  
  resetdt <- reactiveValues(wFilter = TRUE)
  
  observeEvent(input$vehidc,{
    resetdt$wFilter <- TRUE
  })   
  
  observeEvent(input$rtablec, {
    # 0 will be coerced to TRUE
    # 1+ will be coerced to FALSE
    resetdt$wFilter <- FALSE
  })
  
  output$mapTablec <- DT::renderDataTable({
    if (resetdt$wFilter){ 
      DT::datatable(GAStech_node %>% 
                      dplyr::select(Veh_ID,Name,Department,Credit) %>%
                      filter(Veh_ID %in% c(input$vehidc)),
                    
                    class = 'cell-border stripe',
                    rownames=FALSE,
                    options = list(
                      autoWidth = TRUE,
                      scrollX = TRUE,
                      lengthChange = FALSE,
                      columnDefs = list(list(className = 'dt-center',
                                             targets = "_all"))),
                    editable = list(
                      target = 'column',
                      disable = list(columns = c(1, 2, 4)))
                    
      )
    }
    else{
      DT::datatable(GAStech_node %>% 
                      dplyr::select(Veh_ID, Name, Department, Credit),
                    class = 'cell-border stripe',
                    rownames=FALSE,
                    options = list(
                      autoWidth = TRUE,
                      scrollX = TRUE,
                      columnDefs = list(list(className = 'dt-center',
                                             targets = "_all"))),
                    editable = list(
                      target = 'column',
                      disable = list(columns = c(1, 2, 4)))
      )
    }
  })
  
  
  ##################### Tab3 #######################
  
  # eNodes$title <- eNodes$name
  # 
  # output$visnetwork1 <- renderVisNetwork({
  #   
  #   
  #   eNodes$color.highlight.background <- "white"
  #   eNodes$color.highlight.border <- "black"
  #   
  #   #degree_value <- degree(net,v=V(net)[group=="Place"], mode = "in")
  #   #V(net)[group=="Place"]$size <- degree_value*2
  #   lay=input$layoutc 
  #   
  #   if (input$locc!="All"){ 
  #     if (input$depc !="All"){ eEdges <- eEdges %>%
  #       filter(content %in% c(input$locc)) %>%
  #       filter(department %in% c(input$depc))
  #     } else { eEdges <- eEdges %>%
  #       filter(content %in% c(input$locc))
  #     }
  #   } else { if (input$depc !="All"){ eEdges <- eEdges %>%
  #     filter(department %in% c(input$depc))
  #   }
  #   }
  #   
  #   net <- graph_from_data_frame(eEdges,eNodes, directed = TRUE)
  #   #isolated = which(degree(net)==0)
  #   #net <- delete.vertices(net,isolated)
  #   #net <- delete.vertices(net,V(net)[degree(net)== 0])
  #   
  #   eEdges$width <- eEdges$weight
  #   visIgraph(net,  layout = lay, smooth= TRUE) %>%
  #     visGroups(groupname = "Security", color="tomato",
  #               shape = "square",
  #               shadow = list(enabled = TRUE,size = 10)) %>% 
  #     visGroups(groupname = "Info Tech", color="steelblue", 
  #               shape = "diamond",
  #               shadow = list(enabled = TRUE,size = 10)) %>%
  #     visGroups(groupname = "Engineering", color="lightgreen",
  #               shape ="triangle",
  #               shadow = list(enabled = TRUE,size = 10)) %>% 
  #     visGroups(groupname = "Executive", color= "gold",
  #               shape ="star",size=40,
  #               border = "black",
  #               shadow = list(enabled = TRUE, size=10)) %>%
  #     visOptions(selectedBy = "group",
  #                highlightNearest = TRUE, 
  #                nodesIdSelection = TRUE) %>%
  #     visLegend( width=0.1, position ='right') %>%
  #     visLayout(randomSeed = 123) %>%
  #     visInteraction(hideEdgesOnDrag = TRUE, hover=TRUE) %>%
  #     visPhysics("barnesHut")
  #   
  # })
  # 
  # output$visnetwork2 <- renderVisNetwork({
  #   eNodes$color.highlight.background <- "white"
  #   eNodes$color.highlight.border <- "black"
  #   eEdges$width <- eEdges$weight
  #   lay="layout_nicely"
  #   
  #   if(input$relc == "A") {
  #     eNodes <- eNodes %>%
  #       filter(loctype %in% c("A"))
  #     eEdges<- eEdges %>%
  #       filter(loctype %in% c("Residential"))
  #   }else if(input$relc =="B"){
  #     eEdges<- eEdges %>% 
  #       filter(from >100)
  #   }else if(input$relc=="C"){ 
  #     eNodes <- eNodes %>% filter(loctype %in% c("C","A"))
  #     eEdges<- eEdges %>% 
  #       filter(loctype %in% c("Unknown"))
  #     
  #   }else{ 
  #     eEdges <- eEdges %>% filter(loctype=="Transacted")
  #     
  #   }
  #   net <- graph_from_data_frame(eEdges,eNodes, directed = TRUE)
  #   visIgraph(net,  layout = lay, smooth= TRUE) %>%
  #     visGroups(groupname = "Security", color="tomato",
  #               shape = "square",
  #               shadow = list(enabled = TRUE,size = 10)) %>% 
  #     visGroups(groupname = "Info Tech", color="steelblue", 
  #               shape = "diamond",
  #               shadow = list(enabled = TRUE,size = 10)) %>%
  #     visGroups(groupname = "Engineering", color="lightgreen",
  #               shape ="triangle",
  #               shadow = list(enabled = TRUE,size = 10)) %>% 
  #     visGroups(groupname = "Executive", color= "gold",
  #               shape ="star",size=40,
  #               border = "black",
  #               shadow = list(enabled = TRUE, size=10)) %>%
  #     visOptions(selectedBy = "group",
  #                highlightNearest = TRUE, 
  #                nodesIdSelection = TRUE) %>%
  #     visLegend( width=0.1, position ='right') %>%
  #     visLayout(randomSeed = 123) %>%
  #     visInteraction(hideEdgesOnDrag = TRUE, hover=TRUE) %>%
  #     visPhysics("barnesHut")
  #   
  # })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
