library(hrbrthemes)
library(showtext)
showtext_auto(enable = TRUE)
font_add("cnfont", regular = "SourceHanSerifSC-Medium.otf")

cnfont <- "cnfont"
theme_set(
  theme_ipsum(base_family = cnfont, grid = F) + 
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_rect(color = NA, 
                                          fill = "grey90"),
          plot.background = element_rect(color = "grey60", 
                                         fill = "grey90"),
          plot.margin = unit(rep(0, 4), "cm"),
          legend.position = "top",
          legend.key.width = unit(3.2, "lines"),
          legend.key.height = unit(0.8, "lines"),
          plot.title = 
            element_text(hjust = 0.5,
                         margin = margin(t = 24, b = 6)),
          plot.subtitle = 
            element_text(hjust = 0.5,
                         margin = margin(t = 0, b = 0)),
          plot.caption = 
            element_text(hjust = 0.5, color = "#7d92af",
                         margin = margin(t = 0, b = 24)))
)
