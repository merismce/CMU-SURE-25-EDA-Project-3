library(tidyverse)
theme_set(theme_light())
clark_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/clark_shots.csv")
glimpse(clark_shots)

clark_shots |>
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 0.4)


clark_shots |>
  ggplot(aes(shot_distance)) +
  geom_density() +
  geom_rug(alpha = 0.3)


clark_shots |>
  ggplot(aes(shot_distance)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density()

clark_shots |>
  ggplot(aes(shot_distance)) +
  geom_histogram() +
  geom_density(aes(y = after_stat(count)))

library(cowplot)

clark_shot_dens <- clark_shots |>
  ggplot(aes(x = shot_distance)) + 
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Shot distance (in feet)",
       y = "Number of shot attempts")

clark_shot_ecdf <- clark_shots |>
  ggplot(aes(x = shot_distance)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Shot distance (in feet)",
       y = "Proportion of shot attempts")

# ggsave(filename = "Desktop/test.png",
#        plot = clark_shot_dens,
#        width = 500,
#        height = 500,
#        units = "px")

# library(patchwork)
# clark_shot_dens + clark_shot_ecdf
plot_grid(clark_shot_dens, clark_shot_ecdf)


clark_shot_dens_made <- clark_shots |>
  ggplot(aes(x = shot_distance, 
             color = scoring_play)) + 
  geom_density() +
  geom_rug(alpha = 0.3) +
  labs(x = "Shot distance (in feet)",
       y = "Number of shot attempts")

clark_shot_ecdf_made <- clark_shots |>
  ggplot(aes(x = shot_distance,
             color = scoring_play)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  labs(x = "Shot distance (in feet)",
       y = "Proportion of shot attempts")

library(patchwork)
clark_shot_dens_made + clark_shot_ecdf_made + plot_layout(guides = "collect")



clark_shots |>
  glimpse()

clark_shots |>
  ggplot(aes(shot_x, shot_y)) +
  geom_point(alpha = 0.3) +
  geom_density2d() +
  coord_fixed()
               

clark_shots |>
  ggplot(aes(shot_x, shot_y)) +
  stat_density2d(aes(fill  = after_stat(level)),
                 geom = "polygon") +
                 scale_fill_gradient(low = 
                                       "darkblue",
                                       high = "gold")

clark_shots |>
  ggplot(aes(shot_x, shot_y)) +
  stat_density2d(aes(fill = after_stat(density)),
                 
                 geom = "raster") +
  scale_fill_gradient(low = "darkblue",
                      high = "gold")


               


