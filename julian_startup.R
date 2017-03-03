library(beepr)
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#creating colour palette dictionary
jbpal <- list(blue = "#678B99", green = "#A3C686", brown = "#997567", tan = "#998E67", red = "#996772")

#jbplot default theme
jbplot <- theme_grey() +
  theme(plot.background = element_blank(),
        
        #axis lines
        axis.line = element_line(colour = "#BCBCBC", size = 1),
        axis.ticks = element_blank(),
        
        #panel
        panel.background = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_line(colour = "light grey", size = 0.2),
        panel.grid.minor = element_blank(),
        
        #text
        axis.text = element_text(colour = "grey"),
        axis.title = element_text(colour = "dark grey"),
        plot.title = element_text(hjust = 0.5, colour = "dark grey", face = "bold")
  )

#set scale colours to default
jbscale <- scale_colour_manual(values = unlist(jbpal,use.names = F))

#create default colours for geoms
update_geom_defaults("point", list(colour = jbpal$green))
update_geom_defaults("line", list(colour = jbpal$blue))
update_geom_defaults("smooth", list(colour = jbpal$blue, fill = jbpal$blue))
update_geom_defaults("hline", list(colour = jbpal$brown))
update_geom_defaults("vline", list(colour = jbpal$brown))
update_geom_defaults("abline", list(colour = jbpal$brown))
update_geom_defaults("ribbon", list(fill = jbpal$blue, colour = NA))
update_geom_defaults("errorbar", list(colour = jbpal$green, alpha = 0.5, width = 0.1))
update_geom_defaults("text", list(colour = jbpal$brown, alpha = 0.15))

#create function to paste strings together
`%+%` <- function(a, b) paste0(a, b)
