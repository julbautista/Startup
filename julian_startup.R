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
jbcol <- scale_colour_manual(values = unlist(jbpal,use.names = F))  
jbfill <- scale_fill_manual(values = unlist(jbpal, use.names = F))


#create default colours for geoms
update_geom_defaults("point", list(colour = jbpal$green))
update_geom_defaults("dotplot", list(colour = jbpal$green, fill = jbpal$green, dotsize = 0.5))
update_geom_defaults("line", list(colour = jbpal$blue))
update_geom_defaults("smooth", list(colour = jbpal$blue, fill = jbpal$blue))
update_geom_defaults("hline", list(colour = jbpal$brown))
update_geom_defaults("vline", list(colour = jbpal$brown))
update_geom_defaults("abline", list(colour = jbpal$brown))
update_geom_defaults("ribbon", list(fill = jbpal$blue, colour = NA))
update_geom_defaults("errorbar", list(colour = jbpal$green, alpha = 0.5, width = 0.1))
update_geom_defaults("errorbarh", list(colour = jbpal$green, alpha = 0.5, width = 0.1))
update_geom_defaults("text", list(colour = jbpal$brown, alpha = 0.15))
update_geom_defaults("density", list(colour = jbpal$blue, alpha = 0.15))

#create function to paste strings together
`%+%` <- function(a, b) paste0(a, b)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
