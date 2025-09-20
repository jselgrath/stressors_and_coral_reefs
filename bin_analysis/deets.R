library(ggplot2)

# # transparent on right, transparent bg
theme_deets <- function(base_size = 14,
                        legend = c("right", "none", "inside"),
                        legend_pos = c(0.8, 0.25),   # used if legend = "inside"
                        legend_bg = "transparent",
                        legend_border = NA,
                        strip_blank = FALSE) {
  
  th <- theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = NA),
      panel.border     = element_rect(fill = NA),
      axis.title       = element_text(size = rel(1.3), colour = "black", lineheight = 0.8),
      axis.text        = element_text(size = rel(1.2), lineheight = 0.8, colour = "black"),
      axis.title.y     = element_text(vjust = 1, lineheight = 0.8),
      axis.title.x     = element_text(vjust = 0.5),
      legend.title     = element_text(size = rel(1.2)),
      legend.text      = element_text(size = rel(1.1)),
      strip.text       = if (strip_blank) element_blank() else element_text(size = rel(1.3))
    )
  
  # legend options
  if (legend[1] == "none") {
    th <- th + theme(legend.position = "none")
  } else if (legend[1] == "inside") {
    th <- th + theme(
      legend.position   = legend_pos,
      legend.background = element_rect(fill = legend_bg, colour = legend_border)
    )
  } else { # default: "right"
    th <- th + theme(
      legend.position   = "right",
      legend.background = element_rect(fill = legend_bg, colour = legend_border)
    )
  }
  
  th
}


# no background
# theme_deets(legend = "none")

# legend inside, white box & black border
# p + theme_deets(legend = "inside", legend_pos = c(0.8, 0.3),
#                 legend_bg = "white", legend_border = "black")


# legend inside, bottom left
# p + theme_deets(legend = "inside", legend_pos = c(0.2, 0.2))

# remove strip labels
# p + theme_deets(strip_blank = TRUE)
