

library(ggplot2)
library(patchwork)
library(here)


# plot function -----------------------------------------------------------


pie_plotter <- function(proportions, groups = c("Group1", "Group2"), plot_title, colours) {
  df <- data.frame(
    group = groups,
    value = proportions
  )
  
  # convert to factor to preserve the order
  df$group <- factor(df$group, levels = groups)
  
  # create a pie chart
  ggplot(df, aes(x="", y=value, fill=group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = colours) + 
    labs(title = plot_title) +
    theme_void() + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 18)
    )
}



# baseline plots ----------------------------------------------------------


wrap_plots(
  pie_plotter(c(10), "Group1", "People", "#009AC7"),
  pie_plotter(c(10), "Group1", "Stimuli", "#009AC7"),
  pie_plotter(c(10), "Group1", "Contexts", "#009AC7"),
  ncol = 3
)

ggsave(filename = "pie_1.png", path = here("media", "figures"), width = 6, height = 3)


# biased sampling ---------------------------------------------------------


wrap_plots(
  pie_plotter(c(8, 92), c("Group1", "Group2"), "People", c("#00467F", "#009AC7")),
  pie_plotter(c(7, 93), c("Group1", "Group2"), "Stimuli", c("#00467F", "#009AC7")),
  pie_plotter(c(10, 90), c("Group1", "Group2"), "Contexts", c("#00467F", "#009AC7")),
  ncol = 3
)

ggsave(filename = "pie_2.png", path = here("media", "figures"), width = 6, height = 3)


# goal sampling -----------------------------------------------------------


wrap_plots(
  pie_plotter(c(65, 45), c("Group1", "Group2"), "People", c("#00467F", "#009AC7")),
  pie_plotter(c(46, 54), c("Group1", "Group2"), "Stimuli", c("#00467F", "#009AC7")),
  pie_plotter(c(44, 65), c("Group1", "Group2"), "Contexts", c("#00467F", "#009AC7")),
  ncol = 3
)

ggsave(filename = "pie_3.png", path = here("media", "figures"), width = 6, height = 3)






