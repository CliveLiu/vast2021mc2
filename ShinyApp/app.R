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
library(tmap)
library(clock)
library(ggforce)
library(leaflet)
library(rgdal)
library(DT)
library("bsplus") # help text tooltips in selection box
library(timevis)
library(ggiraph)
library(wordcloud)
library(visNetwork)
library(igraph)
library(glue)

### data preparation were separated since they cost a lot of memory

load("data/cards_data_clean")
load("data/gps_clean")

bgmap <- raster("data/MC2-tourist.tif")

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

eEdges <- read_csv("data/aspatial/network_edges2.csv")
eNodes <- read_csv("data/aspatial/employee_nodes.csv")

###################################################################
###################################################################

# Define UI
ui <- navbarPage(
    "DiGTvis",
    theme = shinytheme("flatly"),
    tabPanel("Introduction",
             icon = icon("list-alt", lib = "font-awesome"),
             h3("A Dynamic and Interactive GeoTime Visualisation Dashboard"),
             # tags$a(href="https://vast-challenge.github.io/2021/MC2.html", "2021 VAST Challenge MC2"),
             fluidRow(column(6,
                             HTML("<p>This shiny application is based on the senario of <a href='https://vast-challenge.github.io/2021/MC2.html'>2021 VAST Challenge MC2</a>: </p>"),
                             p("In the IPO celebration of a natural gas production company, several employees went missing. 
                             The company has not been as successful in demonstrating environmental stewardship. 
                             And a local environmental organization is suspected in the disappearance."),
                             p("The analysis aims to explore the car tracking data of employees, as well as credit card transactions and loyalty car usage data, which happened before the disappearance."),
                             p("The application is divided according to several tabs, as shown at right."),
                             HTML("<p>For more information about this project, please refer to our <a href='https://vast2021.netlify.app/'>Group Project Website</a>.</p>")),
                      column(6,
                             img(src="summary.png", align="right", width="80%")))),
    
    
    ##################### Tab1 #######################
    navbarMenu("Consumption Analysis",
               icon = icon("bar-chart"),
               tabPanel("Heatmap",
                        titlePanel("Populary Locations"),
                        p("Based on the credit and loyalty cards consumption records, this plot shows popular locations by different time units and measures:"),
                        fluidRow(
                          column(6,
                                 plotlyOutput("hmap_f", height = "500px")),
                          column(6,
                                 verbatimTextOutput("loyalty_no_hour"),
                                 plotlyOutput("hmap_l", height = "500px", width = "95%"))),
                        fluidRow(
                          br(),
                          column(4),
                          column(2,
                                 style = "background-color:#ECF0F1;",
                                 radioButtons("c1",
                                              "Select Time Unit:",
                                              choices = c("Day" = "d",
                                                          "Hour" = "h"),
                                              inline = TRUE)),
                          column(2,
                                 style = "background-color:#ECF0F1;",
                                 radioButtons("c2",
                                              "Select Measures:",
                                              choices = c("Frequency" = "freq",
                                                          "Price" = "price"),
                                              inline = TRUE)),
                          column(4)),
                        br()
                        ),
               
               tabPanel("Bar Plot",
                        titlePanel("Overall Popularity"),
                        p("Make a full join between credit and loyalty cards consumption records by date, location and price: Most consumption records have perfect one to one match, which is shown as 'both' below. The 'loyalty'
                           refers to loyalty card records which can't find a match in credit card records (the same for 'credit')."),
                        p("The bar plot shows the popularity rank of locations. 
                            And these less popular locations could be a sally port in mapping GPS to locations later."),
                        br(),
                        plotlyOutput("bar_location", height = "600px", width = "90%"),
                        br(),
                        br()),
               
               tabPanel("Parallel Sets Plot",
                        titlePanel("Mapping Credit Cards to Loyalty Cards"),
                        p("After matching records, most credit/loyalty cards have only one corresponding loyalty/credit card, 
                        but some of them correspond to multiple cards."),
                        p("The left table shows all one to one matched pairds, while the parallel
                          set plot on the right shows multiplt matched pairs."),
                        br(),
                        column(5,
                               DT::dataTableOutput("one2one")),
                        column(1),
                        column(6,
                               br(),
                               br(),
                               p("Mutiple Matched Pairs"),
                               # parsetOutput("sanks2"),
                               plotOutput("sanks")),
                        headerPanel("")),
               
               tabPanel("Facet Plot",
                        titlePanel("Tracking Consumptions based on Locations or Cards"),
                        p("Display all transaction records made by one credit card or happeded at one chosen location:"),
                        br(),
                        fluidRow(column(6,
                                        selectInput("track_c",
                                                    "Select One Credit Card:",
                                                    choices = sort(unique(cc$last4ccnum)),
                                                    selected = 6899),
                                        br(),
                                        girafeOutput("track_card")),
                                 column(6,
                                        selectInput("track_loc",
                                                    "Select One Location:",
                                                    choices = sort(unique(fjoin_cc$location)),
                                                    selected = "National Refinery")%>%  #------ add note
                                          shinyInput_label_embed(
                                            shiny_iconlink() %>%
                                              bs_embed_tooltip(title = "Those most popular locations contain too much credit cards and will show messy graphs")
                                          ),
                                        br(),
                                        girafeOutput("track_location")),
                                 headerPanel("")
                                 ))
               ),
    
    
    ##################### Tab2 #######################
    navbarMenu("Geo-Temporal Analysis",
               icon = icon("map"),
               
              tabPanel("GPS Tracking",
                       titlePanel("Map Car Stops to Locations"),
                       p("Car stops imply the car owners might stop driving and go to make consumptions."),
                       p("This page helps to compare all car stops and consumption records on the same day. 
                         You can also choose one vehicle to show its stops and routes."),
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
                                                    choices = c("1 mins" = 60,
                                                                "5 mins" = 300,
                                                                "30 mins" = 1800,
                                                                "1 hour" = 3600, 
                                                                "2 hours" = 7200, 
                                                                "5 hours" = 18000),
                                                    selected = 3600
                                                  ) %>% 
                                                    shinyInput_label_embed(
                                                      shiny_iconlink() %>%   #------ add note
                                                        bs_embed_tooltip(title = "To filter out 
                                                                         car stops with short staytime at one position")
                                                    ),
                                                  
                                                  checkboxInput(inputId = "showRoute",
                                                                label = "Focus on one vehicle",
                                                                value = FALSE),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.showRoute",
                                                    
                                                    #  vehicle type
                                                    radioButtons("vehicle_type",
                                                                 "Select Vehicle Type:",
                                                                 choices = c("Car" = "car",
                                                                             "Truck" = "truck"),
                                                                 inline = TRUE) %>%  #------ add note
                                                      shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                          bs_embed_tooltip(title = "One car was assigned to one person, but trucks have no assignment.")
                                                      ),
                                                    
                                                    ####### condition
                                                    conditionalPanel(
                                                      condition = "input.vehicle_type == 'car'",
                                                      selectInput(
                                                        "id_car",
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
                                                        "id_truck",
                                                        "Select Truck ID:",
                                                        choices = c(101,104,105,106,107))%>%  #------ add note
                                                        shinyInput_label_embed(
                                                          shiny_iconlink() %>%
                                                            bs_embed_tooltip(title = "Sometimes there is no GPS data for one trunk on one day. Then the map will be empty.")
                                                        )
                                                    ),
                                                  ),
                                                  
                                                  
                                                  #----- add submit button
                                                  actionButton("goButton",
                                                               label = "Show!",
                                                               icon = icon("refresh"), 
                                                               style="color: #fff; background-color: #2C3E50; border-color: #2e6da4"),
                                                  
                                                  checkboxInput(inputId = "showCredit",
                                                                label = "Show credit card table",
                                                                value = TRUE)
                                                  ),
                                     
                                     mainPanel(
                                       width = 7,
                                       fluidRow(tmapOutput("vehicle_map"),
                                                hr(),
                                                DT::dataTableOutput("cctable"),
                                                headerPanel("")),
                                     )
                                     
                                     ),
                       # activate tooltips, popovers, and MathJax
                       use_bs_tooltip(),
                       use_bs_popover(),
                       withMathJax()),
              
              ##################### Tab2-2 #######################
              
              tabPanel("Owners Mapping",
                       titlePanel("EDA on Transaction Records with Geospatial Tracking"),
                       sidebarLayout(
                         sidebarPanel(h4("Parameters Selection"),
                                      width = 3,
                                      selectInput( "vehidc", 
                                                   "Vehicle ID:", 
                                                   choices = unique(
                                                     visfull_ds$cid[visfull_ds$cid<999])),
                                      checkboxInput(inputId = "filterc",
                                                    label = "Auotmatic Filter for Vehicle ID",
                                                    value = TRUE),
                                      radioButtons("topc", "Choose Number of TOP Words:",
                                                   choices = list("One" = 1,
                                                                  "Two" = 2,
                                                                  "Three" = 3),
                                                   selected = 2),
                                      selectInput( "ccardc",
                                                   "Credit Card (last 4 No):",
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
                                                          background-color: #2d3d4d;
                                                          border-color: #2e6da4"),
                                      br(),
                                      br(),
                                      actionButton("rbtn",
                                                   "Reset Frame",
                                                   icon("refresh"),
                                                   style='font-size:100%',
                                                   style="color: #fff;
                                                          background-color: #597a9c;
                                                          border-color: #2e6da4"),
                                      br(),
                                      br(),
                                      actionButton("rtablec",
                                                   "Show Full Table",
                                                   icon = icon("table"),
                                                   style='font-size:100%',
                                                   style="color: #fff;
                                                          background-color: #597a9c;
                                                          border-color: #2e6da4")),
                         
                         mainPanel(
                                   wellPanel(
                                   h4("Comparison Basd on Top visited locations"),
                                   p("Matching for Top locations visited by 
                                           Selected Vehicle ID and Credit Card prior selected for deep 
                                           investigation via TimeViz"),
                                   fluidRow(
                                     column(6,align="center",
                                            h4("Vehicle ID."),
                                            plotOutput("cloud1c", width = "300px", height="300px")),
                                     column(6,align="center",
                                            h4(" Credit Card."),
                                            plotOutput("cloud2c", width = "300px", height="300px"))
                                   )),
                                   hr(),
                                   wellPanel(
                                     h4("Timevis Plot for Selected Vehicle ID vs Credit Card."),
                                     br(),
                                     timevisOutput("timelinec")),
                                   hr(),
                                   wellPanel(fluidRow(
                                     column(6, align="center",
                                            h4("Unmatched Loyalty Card Transactions for Credit Card."),
                                            br(),
                                            girafeOutput("loyaltyc")), # girafeOutput("loyaltyc") # plotlyOutput("loyaltyc")
                                     column(6, align="center",
                                            h4("Tagging of Vehicle ID - Credit Car.d"),
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
               tabPanel("Graph Network",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       h4("Parameters for Settings"),
                                       selectInput("locc", "Location:",
                                                   choices =  c("All",sort(
                                                     unique(eEdges$content[!is.na(eEdges$content)]))),
                                                   selected ='All'),
                                       selectInput("depc", "Department:",
                                                   choices = c("All" = "All", 
                                                               "Engineering"="Engineering",
                                                               "Executive" = "Executive",
                                                               "Facilities" = "Facilities",
                                                               "Info Tech" = "Info Tech",
                                                               "Security"= "Security"),
                                                   selected = "All"),
                                       selectInput("layout1c", "Network Layout:",
                                                   choices = c("Circle" = "layout_in_circle",
                                                               "Davidson-Harel"= "layout_with_dh",
                                                               "Forced Direct"= "layout_with_fr",
                                                               "Graphopt"="layout_with_graphopt",
                                                               "MultiDimensional Scaling"= "layout_with_mds",
                                                               "Nicely"="layout_nicely",
                                                               "Sphere" = "layout_on_sphere"),
                                                   selected = "layout_nicely")
                                       
                          ),
                          mainPanel(width = 9,
                                    wellPanel(
                            h4("visNetwork Plot"),
                            br(),
                            shinycssloaders::withSpinner(
                              visNetworkOutput("visnetwork1",height="1000px")
                            ))
                          )
                        )),
               tabPanel("Community Detection",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       h4("Parameters for Settings"),
                                       selectInput("relc", "Visitation to:",
                                                   choices = c("Anemities" = "D",
                                                               "Anemities(AM)" = "AM",
                                                               "Anemities(PM)" = "PM",
                                                               "Anemities(NT)" = "NT",
                                                               "Industrial" = "B",
                                                               "Resident"= "A",
                                                               "Unknown"= "C"),
                                                   selected = "Anemities"),
                                       selectInput("layout2c", "Network Layout:",
                                                   choices = c("Davidson-Harel"= "layout_with_dh",
                                                               "Forced Direct"= "layout_with_fr",
                                                               "Graphopt"="layout_with_graphopt",
                                                               "MultiDimensional Scaling"= "layout_with_mds",
                                                               "Nicely"="layout_nicely"),
                                                   selected="layout_nicely"),
                                       selectInput("comc", "Community Detection:",
                                                   choices = c("Leading Egien" = "L",
                                                               "Walktrap"= "W"),
                                                   selected = "F"),
                                       actionButton("reportc",
                                                    "Generate Report",
                                                    icon = icon("calculator"),
                                                    style='font-size:100%',
                                                    style="color: #fff;
                                      background-color:#87ceeb;
                                      border-color: #2e6da4")
                          ),
                          mainPanel(width = 9,
                                    wellPanel(
                            h4("visNetwork Plot"),
                            br(),
                            shinycssloaders::withSpinner(
                              visNetworkOutput("visnetwork2",height="500px"))
                          ),
                          hr(),
                          wellPanel(h4("Community Dendrogram Plot"),
                                    br(),
                                    plotOutput("dendc", height = "500px"),
                                    hr(),
                                    verbatimTextOutput("dendtxc")
                          )
                          )
                        ))
               ),
    
    
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



#####################################################################
#####################################################################

# Define server
server <- function(input, output) {
  
  
  ##################### Tab1 #######################
  output$hmap_f <- renderPlotly({
    if(input$c1=="d" & input$c2=="freq"){
      p1 <- ggplot(cc_freq_day,aes(x=day,y=location,text=paste("wday:", wday)))+
        geom_tile(aes(fill=frequency))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size=7),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(x="X Jan 2014", title="Daily Consumption Frequency of Credit Cards")
      ggplotly(p1)
    }
    else if(input$c1=="d" & input$c2=="price"){
      p1 <- ggplot(cc_amount_day,aes(x=day,y=location,text=paste("wday:", wday)))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size=7),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(x="X Jan 2014", title="Daily Consumption Amount of Credit Cards")
      ggplotly(p1)
    }
    else if(input$c1=="h" & input$c2=="freq"){
      p1 <- ggplot(cc_freq_hour,aes(x=hour,y=location))+
        geom_tile(aes(fill=frequency))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size=7),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(x="Hour", title="Hourly Consumption Frequency of Credit Cards")
      ggplotly(p1)
    }
    else if(input$c1=="h" & input$c2=="price"){
      p1 <- ggplot(cc_amount_hour,aes(x=hour,y=location))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size=7),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(x="Hour", title="Hourly Consumption Amount of Credit Cards")
      ggplotly(p1)
    }
  })
  
  
  output$hmap_l <- renderPlotly({
    if(input$c1=="d" & input$c2=="freq"){
      p1 <- ggplot(loyalty_freq_day_,aes(x=day,y=location,text=paste("wday:", wday)))+
        geom_tile(aes(fill=frequency))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(x="X Jan 2014", title="Daily Consumption Frequency of Loyalty Cards")
      ggplotly(p1)
    }
    else if(input$c1=="d" & input$c2=="price"){
      p1 <- ggplot(loyalty_amount_day_,aes(x=day,y=location,text=paste("wday:", wday)))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradient(low = "#deeff7", high = "#0D2330")+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 10))+
        labs(x="X Jan 2014", title="Daily Consumption Amount of Loyalty Cards")
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
             font = list(size=9),
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
  
  
  # output$sanks2 <- renderParset({
  #   parset(card_correspond_count_others2)
  # })
  
  
  ### cnsumption tracking
  output$track_card <- renderGirafe({
    
    g2 <- fjoin_cc %>%
      filter(ccard  == input$track_c) %>%
      arrange(location, hr) %>%
      ggplot(aes(y = sort(location), fill=as.character(ccard))) + 
      geom_bar_interactive(aes(tooltip = glue("Day: {day} \nTime: {time} \nLocation: {location}"),
                               data_id = timestamp), 
                           width=0.4) + 
      facet_grid(cols = vars(day), scales = "free_x",space = "free_x") +
      scale_fill_manual(values=c("#57799E"))+
      scale_x_continuous(breaks=seq(0,3,1)) +
      labs(title = "Transaction for the Chosen Credit Card",
           x="No. of Transaction",
           y="") + 
      theme_classic()+ 
      theme(axis.text = element_text(size=9),
            axis.title = element_text(size=9),
            strip.text.y = element_text(angle = 0), # make ccnum horizontal
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(size=12,hjust = 0.5)
      )
    gg2 <- girafe(ggobj=g2)
    gg2 <- girafe_options(gg2,
                          opts_tooltip(opacity = .8, offx = 20, offy = -10,
                                       # use_fill = TRUE,
                                       use_stroke = TRUE, delay_mouseout = 1000),
                          opts_hover_inv(css = "opacity:0.5"), 
                          opts_hover(css = "fill:#314459;cursor:pointer;"),
                          opts_selection(css = "fill:#18212b;cursor:pointer;", only_shiny = FALSE, selected = "J"))
    gg2
  })
  
  output$track_location <- renderGirafe({
    
    p <- fjoin_cc %>% 
      filter(location == input$track_loc) %>%
      ggplot(aes(x=day, fill=location)) + 
      geom_bar_interactive(aes(tooltip = glue("Day: {day} \nTime: {time} \nCredit Card: {ccard}"),
                               data_id = timestamp), 
                           width=1) +
      scale_fill_manual(values=c("#57799E"))+
      scale_y_continuous(breaks=seq(0,3,1)) +
      scale_x_continuous(breaks=seq(6,19,1)) + 
      labs(title = "Transaction at the Choosen Location",
           y="No. of Transaction",
           x=" X Jan 2014") + 
      facet_grid(row = vars(ccard), scales="free_y", space="free_y")+  
      theme_classic()+ 
      theme(axis.text = element_text(size=9),
            axis.title = element_text(size=9),
            strip.text.y = element_text(angle = 0), # make ccnum horizontal
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(size=12,hjust = 0.5)
      )
    
    gg <- girafe(ggobj=p)
    gg <- girafe_options(gg,
                         opts_tooltip(opacity = .8, offx = 20, offy = -10,
                                      # use_fill = TRUE,
                                      use_stroke = TRUE, delay_mouseout = 1000),
                         opts_hover_inv(css = "opacity:0.5"), 
                         opts_hover(css = "fill:#314459;cursor:pointer;"),
                         opts_selection(css = "fill:#18212b;cursor:pointer;", only_shiny = FALSE, selected = "J"))
    gg
      
  })

  
  
  ##################### Tab2-1 #######################
  
  #------vehicle_map
  output$vehicle_map <- renderTmap({
    
    input$goButton
    
    isolate(
      if(input$showRoute==TRUE){
        if(input$vehicle_type == "car"){
          gps_path_selected_car <- gps_path_simplify5 %>%
            filter(id == input$id_car, date == input$dateselection1)
          
          gps_dot_selected_car <- gps_car %>% 
            filter(id == input$id_car, 
                   date == input$dateselection1,
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
            tm_markers(size = 0.25, clustering = FALSE)
          # m + tm_view(set.view = 13.5)
        }
        
        else if(input$vehicle_type == "truck"){
          gps_path_selected_truck <- gps_path_simplify5 %>%
            filter(id == input$id_truck, date == input$dateselection1)
          
          gps_dot_selected_truck <- gps_truck %>%
            filter(id == input$id_truck,
                   time_difference >= as.numeric(input$difftime),
                   date == input$dateselection1) 
          if(nrow(gps_dot_selected_truck)>0 | nrow(gps_path_selected_truck)> 0){
            tm_shape(bgmap) +
              tm_rgb(bgmap, r = 1,g = 2,b = 3,
                     alpha = NA,
                     saturation = 1,
                     interpolate = TRUE,
                     max.value = 255)+
              tm_shape(gps_path_selected_truck) +
              tm_lines(col= "green3", lwd= 2, scale = 2)+
              tm_shape(gps_dot_selected_truck)+
              tm_markers(size = 0.25, clustering = FALSE)
          }
          else{
            tm_shape(bgmap) +
              tm_rgb(bgmap, r = 1,g = 2,b = 3,
                     alpha = NA,
                     saturation = 1,
                     interpolate = TRUE,
                     max.value = 255)
          }
        }
      }
      else {
        gps_dot_selected_car <- gps_car %>% 
          filter(date == input$dateselection1 & 
                   time_difference >= as.numeric(input$difftime))
        gps_dot_selected_truck <- gps_truck %>%
          filter(time_difference >= as.numeric(input$difftime)) %>%
          filter(date == input$dateselection1)
        
        tm_shape(bgmap) +
          tm_rgb(bgmap, r = 1,g = 2,b = 3,
                 alpha = NA,
                 saturation = 1,
                 interpolate = TRUE,
                 max.value = 255)+
          tm_shape(gps_dot_selected_car)+
          tm_markers(size = 0.25, clustering = FALSE)+
          tm_shape(gps_dot_selected_truck)+
          tm_markers(size = 0.25, clustering = FALSE)
        
        }
        
      )
  })
  
  
  output$cctable = DT::renderDataTable({
    input$goButton
    
    isolate(cc1 <- cc%>%
              select(date_d,time_h_m,location,last4ccnum)%>%
              filter(date_d == input$dateselection1))
    
    
    if(input$showCredit){
      DT::datatable(cc1,rownames = FALSE, 
                    options = list(pageLength = 5),
                    colnames = c("Date","Time","Location","Credit Card"),
                    filter = "top")} # caption = "Credit Card Consumption"
    
  })

  
  ##################### Tab2-2 #######################
  
  topword <- reactive({visfull_ds %>% 
      group_by(cid,trip) %>%
      summarise(content = last(content), loctype=last(loctype),
                count =n()) %>% 
      filter(!(loctype %in% c("Residential", "Office")))%>%
      count(cid, content) %>%
      filter(cid %in% c(input$vehidc)) %>% 
      arrange (cid,desc(n))
  })
  top <- reactive({input$topc})
  
  observeEvent(topword(),{
    if (input$filterc == TRUE){t2 <- cl_ds %>%
      count(ccard,location) %>%
      arrange (ccard, desc(n)) %>%
      group_by(ccard) %>%
      slice (1:top()) %>%
      filter(location %in% c(topword()$content[1]))
    
    choices <- unique(t2$ccard)
    updateSelectInput(inputId = "ccardc", choices = choices)
    }
  })
  
  observeEvent(top(),{
    if (input$filterc == TRUE){t2 <- cl_ds %>%
      count(ccard,location) %>%
      arrange (ccard, desc(n)) %>%
      group_by(ccard) %>%
      slice (1:top()) %>%
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
              max.words =20,min.freq=1,scale = c(1.6, 0.8),
              random.order=FALSE,random.color = FALSE,
              color=brewer.pal(8,"Dark2"),
              rot.per=0.3)
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
  
  output$loyaltyc <-  renderGirafe({
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
    updateDateInput(inputId = "RangeSc", max = input$RangeEc)
  })
  
  output$timelinec <- renderTimevis(
    timevis(data = visfull_ds %>%
              filter(cid %in% c(input$vehidc, input$ccardc)),
            groups=vis_gp %>%
              filter(id %in% c(input$vehidc, input$ccardc)),height=450)
  )
  
  observeEvent(input$rbtn,{
    fitWindow("timelinec", list(animation = FALSE))
  })
  
  observeEvent(input$zbtn, {
    setWindow("timelinec", input$RangeSc, input$RangeEc)
    #updateDateInput(inputId = "RangeEc", min = "2014-01-07")
    #updateDateInput(inputId = "RangeSc", max = "2014-01-18")
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
  
  eNodes$title <- eNodes$name
  eEdges$title <- eEdges$weight
  eEdges$width <- eEdges$weight
  
  output$visnetwork1 <- renderVisNetwork({
    eNodes$color.highlight.background <- "white"
    eNodes$color.highlight.border <- "black"
    
    lay=input$layout1c 
    
    if (input$locc!="All"){ 
      if (input$depc !="All"){ eEdges <- eEdges %>%
        filter(content %in% c(input$locc)) %>%
        filter(department %in% c(input$depc))
      } else { eEdges <- eEdges %>%
        filter(content %in% c(input$locc))
      }
    } else { if (input$depc !="All"){ eEdges <- eEdges %>%
      filter(department %in% c(input$depc))
    }
    }
    
    net <- graph_from_data_frame(eEdges,eNodes, directed = TRUE)
    #isolated = which(degree(net)==0)
    #net <- delete.vertices(net,isolated)
    #net <- delete.vertices(net,V(net)[degree(net)== 0])
    
    
    visIgraph(net,  layout = lay, smooth= TRUE) %>%
      visGroups(groupname = "Security", color="tomato",
                shape = "square",
                shadow = list(enabled = TRUE,size = 10)) %>% 
      visGroups(groupname = "Info Tech", color="steelblue", 
                shape = "diamond",
                shadow = list(enabled = TRUE,size = 10)) %>%
      visGroups(groupname = "Engineering", color="lightgreen",
                shape ="triangle",
                shadow = list(enabled = TRUE,size = 10)) %>% 
      visGroups(groupname = "Executive", color= "gold",
                shape ="star",size=40,
                border = "black",
                shadow = list(enabled = TRUE, size=10)) %>%
      visOptions(selectedBy = "group",
                 highlightNearest = TRUE, 
                 nodesIdSelection = TRUE) %>%
      visLegend( width=0.1, position ='right') %>%
      visLayout(randomSeed = 123) %>%
      visInteraction(hideEdgesOnDrag = FALSE, hover=TRUE) %>%
      visPhysics("barnesHut")
    
  })
  
  observe({
    
    if(input$relc == "A") {
      eNodes <- eNodes %>%
        filter(loctype %in% c("A"))
      eEdges<- eEdges %>%
        filter(loctype %in% c("Residential"))
    }else if(input$relc =="B"){
      eEdges<- eEdges %>% 
        filter(from >100)
    }else if(input$relc=="C"){ 
      eNodes <- eNodes %>% filter(loctype %in% c("C","A"))
      eEdges<- eEdges %>% 
        filter(loctype %in% c("Unknown"))
    }else if(input$relc=="D"){
      eEdges <- eEdges %>% filter(loctype=="Transacted")
    }else{ 
      eEdges <- eEdges %>% filter(period==input$relc)
    }
    
    net1 <- graph_from_data_frame(eEdges,eNodes, directed = TRUE)
    
    output$visnetwork2 <- renderVisNetwork({
      eNodes$color.highlight.background <- "white"
      eNodes$color.highlight.border <- "black"
      
      lay=input$layout2c 
      visIgraph(net1,  layout = lay, smooth= TRUE) %>%
        visGroups(groupname = "Security", color="tomato",
                  shape = "square",
                  shadow = list(enabled = TRUE,size = 10)) %>% 
        visGroups(groupname = "Info Tech", color="steelblue", 
                  shape = "diamond",
                  shadow = list(enabled = TRUE,size = 10)) %>%
        visGroups(groupname = "Engineering", color="lightgreen",
                  shape ="triangle",
                  shadow = list(enabled = TRUE,size = 10)) %>% 
        visGroups(groupname = "Executive", color= "gold",
                  shape ="star",size=40,
                  border = "black",
                  shadow = list(enabled = TRUE, size=10)) %>%
        visOptions(selectedBy = "group",
                   highlightNearest = TRUE, 
                   nodesIdSelection = TRUE) %>%
        visLegend( width=0.1, position ='right') %>%
        visLayout(randomSeed = 123) %>%
        visInteraction(hideEdgesOnDrag = TRUE, hover=TRUE) %>%
        visPhysics("barnesHut")
    })
    
    observeEvent(input$comc,{
      isolated = which(degree(net1)==0)
      net1 <- delete.vertices(net1,isolated) 
      
      if(input$comc=="W"){
        com <-cluster_walktrap(net1)
      } else {
        com <- cluster_leading_eigen(net1)
      }
      
      output$dendc <- renderPlot({
        plot_dendrogram(com, mode="hclust",cex=1)
      })
      
      net1$community <- com$membership
      
      observeEvent(input$reportc,{
        output$dendtxc <- renderPrint({
          m <- modularity(com)
          cat('----------------     MODULARITY SCORE/COMMUNITY(>1)      ----------------\n')
          cat("\n")
          cat('Modularity Score: ')
          paste(m) %>% cat("\n")
          for (i in unique(net1$community) ) {
            # create subgraphs for each community
            subgraph <- induced_subgraph(net1, v = which(net1$community == i))
            
            if(gorder(subgraph) > 1) {
              # get degree
              degree <-  degree(subgraph)
              # get top ten degrees
              all <- names(sort(degree, decreasing = TRUE))
              cat("\n")
              cat("Community ")
              paste(i) %>% cat(":")
              #print(all)
              n=1
              for(j in all){
                if( n%% 6=="1"){
                  cat("\n")
                  paste(j) %>%cat(" ")
                }else{
                  paste(j) %>%cat(" ")
                }
                n <- n+1
              }
              
            }
            cat("\n")
          } 
          
        })
      })
    })
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
