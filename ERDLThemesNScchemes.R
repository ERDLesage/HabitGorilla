# Elise's ggplot themes 
# elise.r.d.lesage@gmail.com
# Customised ggplot2 themes, color schemes, etc

theme_ERDL_simple <- function() {
  theme(
    #base_size = 20,
    # add border 1)
    panel.border = element_rect(colour = "Black", fill = NA, linetype = 1),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "grey90", linetype = 1, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "grey90", linetype = 1, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "Black"), #, family = "TT Arial", face = "italic"
    axis.title = element_text(colour = "Black"), #, family = "TT Arial"
    axis.ticks = element_line(size=1,colour="Black"),
    
    # axis.text.x=element_text(colour="Black"), 
    # axis.title.x = element_text(colour="Black"),
    # axis.text.y= element_text(colour="Black"), 
    # axis.title.y= element_text(colour="Black"),
    # axis.line.y = element_line(colour="Black"),
    # axis.line.x = element_line(colour="Black"),
    # axis.ticks.y = element_line(size=1,colour="Black"),
    # axis.ticks.x = element_line(size=1,colour="Black"),
    
    # legend at the bottom 6)
    legend.position = "bottom"
  )
}

