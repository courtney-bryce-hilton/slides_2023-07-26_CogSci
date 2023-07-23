

library(tidyverse)
library(here)


# create data -------------------------------------------------------------


dat <- bind_rows(
  tibble(
    study = "Extrinsic motivation",
    medium = "Physical",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(3,1,4),
    tresh = 7
  ),
  tibble(
    study = "Intrinsic motivation",
    medium = "Physical",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(4, 4, 0),
    tresh = 7
  ),
  tibble(
    study = "Extrinsic motivation",
    medium = "Web",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(2,1,3),
    tresh = 4
  ),
  tibble(
    study = "Intrinsic motivation",
    medium = "Web",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(3,3,0),
    tresh = 4
  )
) |> 
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(aes(yintercept = tresh), colour = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(vars(medium), ncol = 2) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank()
  )


# simpler version ---------------------------------------------------------

plotter <- function(values, yintercept) {
  dat <- tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = values,
  ) |>
    mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))
  
  
  ggplot(dat, aes(x = study, y = y, fill = source)) +
    geom_col(width = 0.3, position = position_stack()) +
    geom_hline(yintercept = yintercept, colour = "red", linetype = "dashed", linewidth = 0.7) +
    scale_y_continuous(limits = c(0,12)) +
    scale_fill_viridis_d() +
    labs(
      y = "Motivation",
      x = NULL
    ) +
    theme_classic() + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_blank()
    )
}

plotter(c(1.5,0.2,5.3), 7)
ggsave(filename = "barDemo1.png", path = here("media", "figures"), width = 4, height = 6)

plotter(c(3,4,0), 7)
ggsave(filename = "barDemo2.png", path = here("media", "figures"), width = 4, height = 6)

plotter(c(5,2,0), 7)
ggsave(filename = "barDemo3.png", path = here("media", "figures"), width = 4, height = 6)

plotter(c(2,5,0), 7)
ggsave(filename = "barDemo4.png", path = here("media", "figures"), width = 4, height = 6)


# empty graph -------------------------------------------------------------


dat <- bind_rows(
  tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(1,1,5),
  ),
  tibble(
    study = "blah2",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(0,0,0),
  ),
  tibble(
    study = "blah3",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(0,0,0),
  )
) |>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_manual(values = c("white", "white", "white")) +
  # scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(colour = "white"),
    legend.title = element_text(colour = "white")
  )

ggsave(filename = "barDemo_blank.png", path = here("media", "figures"), width = 5, height = 6)


# first plot --------------------------------------------------------------


dat <- bind_rows(
  tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(1.5,0.2,5.3),
  ),
  tibble(
    study = "blah2",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(0,0,0),
  ),
  tibble(
    study = "blah3",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(0,0,0),
  )
) |>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  # scale_fill_manual(values = c("white", "white", "white")) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_blank()
  )

ggsave(filename = "barDemo1.png", path = here("media", "figures"), width = 5, height = 6)



# second plot -------------------------------------------------------------


dat <- bind_rows(
  tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(1.5,0.2,5.3),
  ),
  tibble(
    study = "blah2",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(1.5,0.2,0),
  ),
  tibble(
    study = "blah3",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(0,0,0),
  )
  
) |>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  # scale_fill_manual(values = c("white", "white", "white")) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_blank()
  )

ggsave(filename = "barDemo2.png", path = here("media", "figures"), width = 5, height = 6)


# third plot --------------------------------------------------------------


dat <- bind_rows(
  tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(1.5,0.2,5.3),
  ),
  tibble(
    study = "blah2",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(1.5,0.2,0),
  ),
  tibble(
    study = "blah3",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(3,4,0),
  )
  
) |>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  # scale_fill_manual(values = c("white", "white", "white")) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_blank()
  )

ggsave(filename = "barDemo3.png", path = here("media", "figures"), width = 5, height = 6)


# threshold variation -----------------------------------------------------

ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 4, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  # scale_fill_manual(values = c("white", "white", "white")) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_blank()
  )

ggsave(filename = "barDemo4.png", path = here("media", "figures"), width = 5, height = 6)


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 9, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  # scale_fill_manual(values = c("white", "white", "white")) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_blank()
  )

ggsave(filename = "barDemo5.png", path = here("media", "figures"), width = 5, height = 6)


# more --------------------------------------------------------------------

dat <- bind_rows(
  tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(3.5,3.5,0),
  ),
  tibble(
    study = "blah2",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(0,0,0),
  )
) |>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  # scale_fill_manual(values = c("white", "white", "white")) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_blank()
  )

ggsave(filename = "barDemo6.png", path = here("media", "figures"), width = 5, height = 6)


dat <- bind_rows(
  tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(3.5,3.5,0),
  ),
  tibble(
    study = "blah2",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(2,5,0),
  )
) |>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  # scale_fill_manual(values = c("white", "white", "white")) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_blank()
  )

ggsave(filename = "barDemo7.png", path = here("media", "figures"), width = 5, height = 6)



# just money --------------------------------------------------------------



dat <- tibble(
    study = "blah",
    source = c('curiosity', 'enjoyment', 'money'),
    y = c(0.5,0.5,6),
  )|>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(filename = "barDemo_money.png", path = here("media", "figures"), width = 4, height = 6)

dat <- tibble(
  study = "blah",
  source = c('curiosity', 'enjoyment', 'money'),
  y = c(0.5,0.5,10),
)|>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(filename = "barDemo_money2.png", path = here("media", "figures"), width = 4, height = 6)




# just curious ------------------------------------------------------------



dat <- tibble(
  study = "blah",
  source = c('curiosity', 'enjoyment', 'money'),
  y = c(5,2,0),
)|>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(filename = "barDemo_curious.png", path = here("media", "figures"), width = 4, height = 6)


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 10, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(filename = "barDemo_curious1.png", path = here("media", "figures"), width = 4, height = 6)


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 3, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(filename = "barDemo_curious2.png", path = here("media", "figures"), width = 4, height = 6)



# just enjoy --------------------------------------------------------------



dat <- tibble(
  study = "blah",
  source = c('curiosity', 'enjoyment', 'money'),
  y = c(2,5,0),
)|>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 7, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(filename = "barDemo_enjoy.png", path = here("media", "figures"), width = 4, height = 6)



# everything --------------------------------------------------------------


dat <- tibble(
  study = "blah",
  source = c('curiosity', 'enjoyment', 'money'),
  y = c(5,5,0),
)|>
  mutate(source = factor(source, levels = c('curiosity', 'enjoyment', 'money')))


ggplot(dat, aes(x = study, y = y, fill = source)) +
  geom_col(width = 0.3, position = position_stack()) +
  geom_hline(yintercept = 3, colour = "red", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(limits = c(0,12)) +
  scale_fill_viridis_d() +
  labs(
    y = "Motivation",
    x = NULL
  ) +
  theme_classic() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(filename = "barDemo_all.png", path = here("media", "figures"), width = 4, height = 6)






