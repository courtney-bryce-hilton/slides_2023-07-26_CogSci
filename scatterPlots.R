
library(tidyverse)
library(scales)
library(here)

rand_points <- function(n, mean_x, mean_y, sd_x, group_x) {
  out <- 
    tibble(
      x = rnorm(n, mean = mean_x, sd = sd_x),
      y = rnorm(n, mean = mean_y, sd = sd_x),
      group = rep(group_x, n) %>% as.factor
    )
  
  return(out)
}

dat <- rand_points(10000, 0, 0, .8, 1)

# Define center and radius of the circle
center_x <- 0
center_y <- 0
radius <- 0.1

# Create a new variable for whether each point is within the circle
dat <- dat %>%
  mutate(within_circle = ifelse((x - center_x)^2 + (y - center_y)^2 <= radius^2, "Within circle", "Outside circle"))





# Plot
ggplot(dat, aes(x,y)) +
  geom_point(size = 0.8, alpha = 0.5, pch = 16, colour = "#009AC7") + 
  theme_void() + 
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  theme(
    legend.position = "none"
  )

ggsave(filename = "scatterPlot0.png", path = here("media", "figures"), height = 6, width = 6)


# Plot
ggplot(dat, aes(x,y, colour = within_circle)) +
  geom_point(size = 0.8, alpha = 0.5, pch = 16) + 
  scale_colour_manual(values = c("#009AC7", "#fcbf49")) +
  theme_void() + 
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  theme(
    legend.position = "none"
  )

ggsave(filename = "scatterPlot1.png", path = here("media", "figures"), height = 6, width = 6)



# second version ----------------------------------------------------------

radius <- 1.5

# Create a new variable for whether each point is within the circle
dat <- dat %>%
  mutate(within_circle = ifelse((x - center_x)^2 + (y - center_y)^2 <= radius^2, "Within circle", "Outside circle"))

# Plot
ggplot(dat, aes(x,y, colour = within_circle)) +
  geom_point(size = 0.8, alpha = 0.5, pch = 16) + 
  scale_colour_manual(values = c("#009AC7", "#fcbf49")) +
  theme_void() + 
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  theme(
    legend.position = "none"
  )

ggsave(filename = "scatterPlot2.png", path = here("media", "figures"), height = 6, width = 6)






# testing -----------------------------------------------------------------



radius <- 1.5


# Define start and end angle of the wedge in degrees
start_angle <- 75
end_angle <- 105

# Create a new variable for whether each point is within the circle
dat <- dat %>%
  mutate(within_circle = ifelse((x - center_x)^2 + (y - center_y)^2 <= radius^2, "Within circle", "Outside circle"))

# Create a new variable for whether each point is within the wedge
dat <- dat %>%
  mutate(angle = atan2(y - center_y, x - center_x) * (180 / pi),
         within_wedge = ifelse(within_circle == "Within circle" & angle >= start_angle & angle <= end_angle, 
                               "Within wedge", 
                               "Outside wedge"))

ggplot(dat, aes(x,y, colour = interaction(within_circle, within_wedge))) +
  geom_point(size = 0.8, alpha = 0.5, pch = 16) + 
  scale_colour_manual(values = c("#009AC7", "#fcbf49", "#7b2cbf")) +
  theme_void() + 
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  theme(
    legend.position = "none"
  )

ggsave(filename = "scatterPlot3.png", path = here("media", "figures"), height = 6, width = 6)


# Define start and end angle of the wedge in degrees
start_angle <- 85
end_angle <- 95

# Create a new variable for whether each point is within the circle
dat <- dat %>%
  mutate(within_circle = ifelse((x - center_x)^2 + (y - center_y)^2 <= radius^2, "Within circle", "Outside circle"))

# Create a new variable for whether each point is within the wedge
dat <- dat %>%
  mutate(angle = atan2(y - center_y, x - center_x) * (180 / pi),
         within_wedge = ifelse(within_circle == "Within circle" & angle >= start_angle & angle <= end_angle, 
                               "Within wedge", 
                               "Outside wedge"))

ggplot(dat, aes(x,y, colour = interaction(within_circle, within_wedge))) +
  geom_point(size = 0.8, alpha = 0.5, pch = 16) + 
  scale_colour_manual(values = c("#009AC7", "#fcbf49", "#7b2cbf")) +
  theme_void() + 
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  theme(
    legend.position = "none"
  )

ggsave(filename = "scatterPlot_tight.png", path = here("media", "figures"), height = 6, width = 6)


# Define start and end angle of the wedge in degrees
start_angle <- -55
end_angle <- 265

# Create a new variable for whether each point is within the circle
dat <- dat %>%
  mutate(within_circle = ifelse((x - center_x)^2 + (y - center_y)^2 <= radius^2, "Within circle", "Outside circle"))

# Create a new variable for whether each point is within the wedge
dat <- dat %>%
  mutate(angle = atan2(y - center_y, x - center_x) * (180 / pi),
         within_wedge = ifelse(within_circle == "Within circle" & angle >= start_angle & angle <= end_angle, 
                               "Within wedge", 
                               "Outside wedge"))

ggplot(dat, aes(x,y, colour = interaction(within_circle, within_wedge))) +
  geom_point(size = 0.8, alpha = 0.5, pch = 16) + 
  scale_colour_manual(values = c("#009AC7", "#fcbf49", "#7b2cbf")) +
  theme_void() + 
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  theme(
    legend.position = "none"
  )

ggsave(filename = "scatterPlot_wide.png", path = here("media", "figures"), height = 6, width = 6)







# bigger ------------------------------------------------------------------

start_angle <- 5
end_angle <- 175

# Create a new variable for whether each point is within the circle
dat <- dat %>%
  mutate(within_circle = ifelse((x - center_x)^2 + (y - center_y)^2 <= radius^2, "Within circle", "Outside circle"))

# Create a new variable for whether each point is within the wedge
dat <- dat %>%
  mutate(angle = atan2(y - center_y, x - center_x) * (180 / pi),
         within_wedge = ifelse(within_circle == "Within circle" & angle >= start_angle & angle <= end_angle, 
                               "Within wedge", 
                               "Outside wedge"))

ggplot(dat, aes(x,y, colour = interaction(within_circle, within_wedge))) +
  geom_point(size = 0.8, alpha = 0.5, pch = 16) + 
  scale_colour_manual(values = c("#009AC7", "#fcbf49", "#7b2cbf")) +
  theme_void() + 
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  theme(
    legend.position = "none"
  )

ggsave(filename = "scatterPlot4.png", path = here("media", "figures"), height = 6, width = 6)












