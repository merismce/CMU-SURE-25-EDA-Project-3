library(tidyverse)
hospitals <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/hospitals.csv")
spec(hospitals)
view(hospitals)

cluster_data <- hospitals  %>%
  select(
    Rating.Overall,
    `Procedure.Heart Attack.Cost`,
    `Procedure.Heart Failure.Cost`,
    Procedure.Pneumonia.Cost,
    `Procedure.Hip Knee.Cost`
    
  ) %>%
  filter(if_all(everything(), ~ !is.na(.)))

cluster_data_scaled <- scale(cluster_data)

set.seed(123)

kmeans_result <- kmeans(cluster_data_scaled, centers = 3)

clustered <- as.data.frame(cluster_data_scaled)
clustered$Cluster <- as.factor(kmeans_result$cluster)

library(factoextra)

fviz_cluster(kmeans_result,
             data = cluster_data_scaled,
             geom = "point", 
             ellpise.type = "norm",
             palette = "jco",
             main = "K-means Clustering of Hospitals") +
  labs(
    x = "Principal Component 1 (PC1)",
    y = "Principal Component 2 (PC2)"
  )

#PCA data 

pca_result <- prcomp(cluster_data_scaled)
summary(pca_result)

pca_result$rotation


# State Level Analysis

state_summary <- hospitals %>%
  filter(Rating.Overall != -1) %>%
  mutate(
    High_Rated = Rating.Overall >= 4,
    Mortality_Score = recode(Rating.Mortality, "Below" = 1, "Same" = 0, "Above" = -1, "Unknown" = NA_real_, .default = NA_real_),
    Readmission_Score = recode(Rating.Readmission, "Below" = 1, "Same" = 0, "Above" = -1, "Unknown" = NA_real_, .default = NA_real_)
  ) %>%
  group_by(Facility.State) %>%
  summarise(
    avg_rating = mean(Rating.Overall),
    prop_high = mean(High_Rated),
    avg_cost_heart = mean(`Procedure.Heart Attack.Cost`, na.rm = TRUE),
    avg_score_mortality = mean(Mortality_Score, na.rm = TRUE),
    avg_score_readmission = mean(Readmission_Score, na.rm = TRUE),
    hospital_count = n()
  )

ggplot(state_summary, aes(x = reorder(Facility.State, prop_high), y = prop_high)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of 4–5 Star Hospitals by State", x = "State", y = "% High-Rated Hospitals") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

state_scaled <- state_summary %>%
  select(-Facility.State) %>%
  scale()

set.seed(1)
kmeans_result <- kmeans(state_scaled, centers = 3)
state_summary$Cluster <- factor(kmeans_result$cluster)

ggplot(state_summary, aes(x = avg_cost_heart, y = avg_rating, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "State Clusters Based on Cost and Ratings", x = "Avg Heart Attack Cost", y = "Avg Hospital Rating")


# Hierarchical Clustering for Heart Attack and Heart Failure

theme_set(theme_light())

hospitals_heart_std <- hospitals %>%
  select(name = Facility.Name,
         heart_attack = "Procedure.Heart Attack.Cost",
         heart_failure = "Procedure.Heart Failure.Cost") %>%
  drop_na() %>%
         mutate(
           std_attack = scale(heart_attack)[,1],
           std_failure = scale(heart_failure)[,1]
         )

dist_mat <- hospitals_heart_std %>%
  select(std_attack, std_failure) %>%
  dist()

hc <- hclust(dist_mat, method = "complete")

hospitals_heart_std <- hospitals_heart_std %>%
  mutate(cluster = factor(cutree(hc, k = 3)))

ggplot(hospitals_heart_std, aes(x = std_attack, y = std_failure, color = cluster)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Clusters of Hospitals by Procedure Costs",
       x = "Standardized Heart Attack Cost",
       y = "Standardized Heart Failure Cost") +
  ggthemes::scale_color_colorblind()


# Mosaic Plots for Hierarchical Clustering

hospitals_clustered <- hospitals_heart_std %>%
  left_join(
    hospitals %>%
      select(Facility.Name, 
             heart_attack_quality = `Procedure.Heart Attack.Quality`, 
             heart_failure_quality = `Procedure.Heart Failure.Quality`),
    by = c("name" = "Facility.Name")
  )

table(hospitals_clustered$cluster, hospitals_clustered$heart_attack_quality)




library(ggmosaic)

ggplot(data = hospitals_clustered) +
  geom_mosaic(aes(x = product(cluster), fill = heart_attack_quality), na.rm = TRUE) +
  labs(title = "Mosaic Plot: Cluster vs. Heart Attack Quality",
       x = "Cluster", y = "Proportion",
       fill = "Heart Attack Quality") +
  theme_minimal()

ggplot(data = hospitals_clustered) +
  geom_mosaic(aes(x = product(cluster), fill = heart_failure_quality), na.rm = TRUE) +
  labs(title = "Mosaic Plot: Cluster vs. Heart Failure Quality",
       x = "Cluster", y = "Proportion",
       fill = "Heart Failure Quality") +
  theme_minimal()


# Hierarchical Clustering for Pneumonia Costs and 

theme_set(theme_light())

hospitals_heart_std <- hospitals %>%
  select(name = Facility.Name,
         heart_attack = "Procedure.Heart Attack.Cost",
         heart_failure = "Procedure.Heart Failure.Cost") %>%
  drop_na() %>%
  mutate(
    std_attack = scale(heart_attack)[,1],
    std_failure = scale(heart_failure)[,1]
  )

dist_mat <- hospitals_heart_std %>%
  select(std_attack, std_failure) %>%
  dist()

hc <- hclust(dist_mat, method = "complete")

hospitals_heart_std <- hospitals_heart_std %>%
  mutate(cluster = factor(cutree(hc, k = 3)))

ggplot(hospitals_heart_std, aes(x = std_attack, y = std_failure, color = cluster)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Clusters of Hospitals by Procedure Costs",
       x = "Standardized Heart Attack Cost",
       y = "Standardized Heart Failure Cost") +
  ggthemes::scale_color_colorblind()


# Mosaic Plots for Hierarchical Clustering

hospitals_clustered <- hospitals_heart_std %>%
  left_join(
    hospitals %>%
      select(Facility.Name, 
             heart_attack_quality = `Procedure.Heart Attack.Quality`, 
             heart_failure_quality = `Procedure.Heart Failure.Quality`),
    by = c("name" = "Facility.Name")
  )

table(hospitals_clustered$cluster, hospitals_clustered$heart_attack_quality)




library(ggmosaic)

ggplot(data = hospitals_clustered) +
  geom_mosaic(aes(x = product(cluster), fill = heart_attack_quality), na.rm = TRUE) +
  labs(title = "Mosaic Plot: Cluster vs. Heart Attack Quality",
       x = "Cluster", y = "Proportion",
       fill = "Heart Attack Quality") +
  theme_minimal()

ggplot(data = hospitals_clustered) +
  geom_mosaic(aes(x = product(cluster), fill = heart_failure_quality), na.rm = TRUE) +
  labs(title = "Mosaic Plot: Cluster vs. Heart Failure Quality",
       x = "Cluster", y = "Proportion",
       fill = "Heart Failure Quality") +
  theme_minimal()





   
       
       
       
       
       
       


