library(tidyverse)
library(ggplot2)

bio <- read_csv("Olympic_Athlete_Bio.csv")
results <- read_csv("Weightlifting.csv")

wl <- results %>% 
  left_join(bio, by = "athlete_id")

wl <- wl %>% 
  mutate(medal_score = case_when(
    medal == "Gold" ~ 3,
    medal == "Silver" ~ 2,
    medal == "Bronze" ~ 1,
    TRUE ~ 0
  ))

wl_clean <- wl %>% 
  filter(!is.na(height))

ggplot(wl_clean, aes(x = height, y = medal_score)) +
  geom_jitter(width = 0, height = 0.1, alpha = 0.4) +
  geom_smooth(method="lm") +
  labs(
    title = "Height vs Medal Score in Olympic Weightlifting",
    x = "Height (cm)",
    y = "Medal Score (0 = none, 1 = bronze, 2 = silver, 3 = gold)"
  )


ggplot(wl_clean, aes(x = factor(medal_score), y = height, fill = factor(medal_score))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Height Distribution by Medal Category",
    x = "Medal Score",
    y = "Height (cm)"
  ) +
  guides(fill = "none")


ggplot(wl_clean, aes(x = factor(medal_score), y = height, fill = factor(medal_score))) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(
    title = "Height Distribution by Medal Score",
    x = "Medal Score",
    y = "Height (cm)"
  ) + 
  guides(fill = "none")