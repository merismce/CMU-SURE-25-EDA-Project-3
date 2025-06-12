
library(tidyverse)
hospitals <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/hospitals.csv")
spec(hospitals)
view(hospitals)

hospitals <- hospitals %>%
  mutate(
    Mortality_Score = recode(Rating.Mortality,
                             "Below" = 1,
                             "Same" = 0,
                             "Above" = -1,
                             "Unknown" = NA_real_,
                             .default = NA_real_),
    Readmission_Score = recode(Rating.Readmission,
                               "Below" = 1,
                               "Same" = 0,
                               "Above" = -1,
                               "Unknown" = NA_real_,
                                .default = NA_real_)
  ) %>%
  
  mutate(across(c(Mortality_Score, Readmission_Score), as.numeric))


hospitals_clean <- hospitals %>%
  filter(Rating.Overall != -1,
         !is.na(Mortality_Score),
         !is.na(Readmission_Score))


ggplot(hospitals_clean, aes(x = factor(Rating.Overall), y = Mortality_Score)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Mortality Score vs. Overall Hospital Rating",
       x = "Overall Rating (Stars)", y = "Mortality Score") +
  theme_minimal()

ggplot(hospitals_clean, aes(x = factor(Rating.Overall), y = Readmission_Score)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Readmission Score vs. Overall Hospital Rating",
       x = "Overall Rating (Stars)", y = "Readmission Score") +
  theme_minimal()


# Stacked Bar chart showing propoertion of hospitals for each star rating above 
#or below national average in readmission rates

ggplot(hospitals_clean, aes(x = factor(Rating.Overall), fill = Rating.Readmission)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Readmission Ratings by Overall Hospital Rating",
    x = "Overall Hospital Rating (Stars)",
    y = "Percentage of Hospitals",
    fill = "Readmission Rating"
  ) +
  theme_minimal()

#Frequency plot

ggplot(hospitals_clean, aes(x = factor(Rating.Overall), y = Readmission_Score)) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.4, color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(
    title = "Readmission Score Spread by Overall Rating",
    x = "Overall Hospital Rating (Stars)",
    y = "Readmission Score (-1 = Above, 0 = Same, 1 = Below)"
  ) +
  theme_minimal()

#Frequency Plotting for the Number of 5 Star Hospitals in each
#State

five_star <- hospitals %>%
  filter(Rating.Overall == 5)

five_star_by_state <- five_star %>%
  count(Facility.State, name = "num_five_star")

total_by_state <- hospitals %>%
  filter(Rating.Overall != -1) %>%
  count(Facility.State, name = "total_hospitals")

proportion_five_star <- left_join(five_star_by_state, total_by_state, by = "Facility.State") %>%
  mutate(percent_five_star = num_five_star / total_hospitals)

ggplot(proportion_five_star, aes(x = reorder(Facility.State, -percent_five_star), y = percent_five_star)) +
  geom_col(fill = "darkgreen") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of 5-Star Hospitals by State",
    x = "State",
    y = "Percentage of Hospitals Rated 5 Stars"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Frequency Plotting for the Proportion of 4-5 Star Hospitals for
# each of the 3 hospital types such as private, government, public

hospitals_filtered <- hospitals %>%
  filter(Rating.Overall != -1)

hospitals_filtered <- hospitals_filtered %>%
  mutate(High_Rated = if_else(Rating.Overall >= 4, TRUE, FALSE))

proportions_by_type <- hospitals_filtered %>%
  group_by(Facility.Type) %>%
  summarise(
    total = n(),
    high_rated = sum(High_Rated),
    proportion_high = high_rated / total
  ) %>%
  filter(Facility.Type %in% c("Government", "Private", "Proprietary"))


ggplot(proportions_by_type, aes(x = Facility.Type, y = proportion_high, fill = Facility.Type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of 4-5 Star Hospitals by Facility Type",
    x = "Hospital Type",
    y = "Percentage Rated 4 or 5 Stars"
  ) +
  theme_minimal()


 
#Frequency Plotting for the Number of 4-5 Star Hospitals in each
#State
five_star <- hospitals %>%
  filter(Rating.Overall >= 4)

five_star_by_state <- five_star %>%
  count(Facility.State, name = "num_five_star")

total_by_state <- hospitals %>%
  filter(Rating.Overall != -1) %>%
  count(Facility.State, name = "total_hospitals")

view(five_star_by_state)

proportion_five_star <- left_join(five_star_by_state, total_by_state, by = "Facility.State") %>%
  mutate(percent_five_star = num_five_star / total_hospitals)

ggplot(proportion_five_star, aes(x = reorder(Facility.State, -percent_five_star), y = percent_five_star)) +
  geom_col(fill = "darkgreen") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Hospitals Rated Above 4 Stars by State",
    x = "State",
    y = "Percentage of Hospitals Rated 5 Stars"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Hospital Ratings Chi Square test of Independence
hospitals_filtered <- hospitals %>% filter(Rating.Overall != -1)
hospitals_filtered <- hospitals_filtered %>%
  mutate(High_Rated = Rating.Overall >= 4)

table_type_rating <- table(hospitals_filtered$Facility.Type, hospitals_filtered$High_Rated)
chisq.test(table_type_rating)



# Summarize proportions
state_prop <- hospitals_bin %>%
  group_by(Facility.State) %>%
  summarise(
    n = n(),
    high = sum(High_Rated),
    prop = high / n
  ) %>%
  mutate(
    se = sqrt(prop * (1 - prop) / n),
    lb = prop - 1.96 * se,
    ub = prop + 1.96 * se
  )

# Plot
ggplot(state_prop, aes(x = reorder(Facility.State, prop), y = prop)) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of 4–5 Star Hospitals by State (±95% CI)",
    x = "State",
    y = "Proportion of High-Rated Hospitals"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wilson Confidence Intervals

# install.packages("binom")
library(binom)

state_prop <- hospitals_bin %>%
  group_by(Facility.State) %>%
  summarise(
    successes = sum(High_Rated),
    trials = n(),
    .groups = "drop"
  )
  
  state_prop <- state_prop %>%
  rowwise() %>%
  mutate(ci = list(binom.confint(successes, trials, methods = "wilson") %>%
                     slice(1))) %>%
  unnest_wider(ci, names_sep = ".") %>%
  ungroup()


ggplot(state_prop, aes(x = reorder(Facility.State, ci.mean), y = ci.mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of 4–5 Star Hospitals by State (Wilson 95% CI)",
    x = "State",
    y = "Proportion of High-Rated Hospitals"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



  








