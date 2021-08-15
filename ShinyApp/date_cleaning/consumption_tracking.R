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
library(ggiraph)
library(glue)

# set the current dir as workpath
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


fjoin <- read_csv("data/credit_loy.csv")

fjoin_cc <- fjoin %>% 
  filter(!cardtype == "loyalty") %>% 
  separate(timestamp, into = c("date","time"), sep=" ",remove = FALSE)


library(glue)

fjoin_cc %>%
  filter(ccard  == 6899) %>%
  arrange(ccard, location, timestamp) %>%
  ggplot(aes(y = sort(location), fill=as.character(ccard))) + 
  geom_bar_interactive(aes(tooltip = glue("Day: {day} \nTime: {time} \nLocation: {location}"),
                           data_id = timestamp), 
                       width=0.4) +
  facet_grid(cols = vars(day))


g2 <- fjoin_cc %>%
  filter(ccard  == 6899) %>%
  arrange(location, hr) %>%
  ggplot(aes(y = sort(location), fill=as.character(ccard))) + 
  geom_bar_interactive(aes(tooltip = glue("Day: {day} \nTime: {time} \nLocation: {location}"),
                           data_id = timestamp), 
                       width=0.4) + 
  
  # facet_grid(ccard~day, scales="free_x", space="free_x") +
  facet_grid(cols = vars(day), scales = "free_x",space = "free_x")+
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
gg2 <- girafe_options(gg2, opts_tooltip(opacity = .8,
                                        offx = 20, offy = -10,
                                        # use_fill = TRUE,
                                        use_stroke = TRUE,
                                        delay_mouseout = 1000),
                      opts_hover_inv(css = "opacity:0.5"), 
                      opts_hover(css = "fill:#314459;"))
gg2

########################################################

fjoin_cc %>% 
  filter(location == "Brew Served") %>%
  ggplot(aes(x=day, fill=location)) + 
  geom_bar_interactive(aes(tooltip = glue("Day: {day} \nTime: {time} \nCredit Card: {ccard}"),
                           data_id = timestamp), 
                       width=1) +
  scale_fill_manual(values=c("#57799E"))+
  scale_y_continuous(breaks=seq(0,1,1)) +
  scale_x_continuous(breaks=seq(6,19,1)) + 
  labs(title = "Transaction at the Choosen Location",
       y="No. of Transaction",
       x=" X Jan 2014")

p <- fjoin_cc %>% 
  filter(location == "Brew Served") %>%
  ggplot(aes(x=day, fill=location)) + 
  geom_bar_interactive(aes(tooltip = glue("Day: {day} \nTime: {time} \nCredit Card: {ccard}"),
                           data_id = timestamp), 
                       width=1) +
  scale_fill_manual(values=c("#57799E"))+
  scale_y_continuous(breaks=seq(0,1,1)) +
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
gg <- girafe_options(gg, opts_tooltip(opacity = .8,
                                      offx = 20, offy = -10,
                                      # use_fill = TRUE,
                                      use_stroke = TRUE,
                                      delay_mouseout = 1000),
                     opts_hover_inv(css = "opacity:0.5"), 
                     opts_hover(css = "fill:#314459;"))
gg



##########
dat <- data.frame(
  name = c( "Guy", "Ginette", "David", "Cedric", "Frederic" ),
  gender = c( "Male", "Female", "Male", "Male", "Male" ),
  height = c(169, 160, 171, 172, 171 ) )
p <- ggplot(dat, aes( x = name, y = height, fill = gender,
                      data_id = name ) ) +
  geom_bar_interactive(stat = "identity") +
  scale_fill_manual_interactive(
    values = c(Male = "#0072B2", Female = "#009E73"),
    data_id = c(Female = "Female", Male = "Male"),
    tooltip = c(Male = "Male", Female = "Female")
  )
x <- girafe(ggobj = p)
if( interactive() ) print(x)
