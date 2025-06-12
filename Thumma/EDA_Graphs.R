library(tidyverse)
library(dplyr)
library(GGally)
library(ggcorrplot)
library(factoextra)
library(dslabs)
library(ggdendro)


# Questions we’re exploring
# Geographical and socioeconomic and rating 
# Hospital type and mortality. 


hospitals <- read_csv('https://raw.githubusercontent.com/36-SURE/2025/main/data/hospitals.csv')

# converts the different word ratings into numbers
hospitals_num <- hospitals |>
  mutate(Rating.Overall = as.numeric(Rating.Overall)) |>
  mutate(across(Rating.Mortality:Rating.Imaging, ~ recode(.x, 
                                                          "Above" = 1,
                                                          "Below" = -1,
                                                          "Same" = 0,
                                                          "None" = NA_real_)))



# a correlation matrix to see which ratings are correlated with each other and 
# more importantly see what ratings are correlated with the overall rating 
hospitals_num |>
  select(Rating.Overall, Rating.Mortality:Rating.Imaging) |>
  mutate(across(everything(), as.numeric)) |>
  cor(use = "pairwise.complete.obs") |>
  ggcorrplot(lab = TRUE,
             title = "Correlation Matrix of Hospital Ratings")



# Average Overall Rating by State
hospitals_num |>
  filter(Rating.Overall != -1) |>
  group_by(Facility.State) |>
  summarise(Average.Rating = mean(Rating.Overall, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(Facility.State, Average.Rating), y = Average.Rating)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(title = "Average Hospital Rating by State", 
       x = "State", 
       y = "Average Overall Rating")

# Overall Rating by Hospital Type
hospitals_num |>
  filter(Rating.Overall != -1) |>
  ggplot(aes(x = Facility.Type, y = Rating.Overall)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Overall Ratings by Hospital Type", 
       x = "Hospital Type", 
       y = "Overall Rating") +
  coord_flip()


# Average Mortality Score by Hospital Type
hospitals_num |>
  group_by(Facility.Type) |>
  summarise(Average.Mortality = mean(Rating.Mortality, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(Facility.Type, Average.Mortality), y = Average.Mortality)) +
  geom_col(fill = "lightgreen") +
  labs(title = "Average Mortality Score by Hospital Type", 
       x = "Hospital Type", 
       y = "Avg Mortality (1=Above, -1=Below)") +
  coord_flip()

#count of each facility type
hospitals_num |>
  group_by(Facility.Type) |>
  summarise(n = n())


###############
##### PCA #####
###############

hospitals_feat <- hospitals_num |>
  select(where(is.numeric)) |>
  drop_na()


hospitals_pca <- prcomp(hospitals_feat, scale. = TRUE)
summary(hospitals_pca)
  

hospitals_pca$rotation
hospital_pc_matrix <- hospitals_pca$x


hospitals_num |>
  filter(if_all(where(is.numeric), ~ !is.na(.))) |>
  mutate(pc1 = hospital_pc_matrix[, 1],
         pc2 = hospital_pc_matrix[, 2]) |>
  ggplot(aes(pc1, pc2)) +
  geom_point(alpha = 0.3) +
  labs(title = "PCA Plot of Hospital Ratings and Costs",
       x = "Principal Component 1",
       y = "Principal Component 2")


hospitals_pca |>
  fviz_pca_biplot(labe = "var",
                  alpha.ind = 0.25,
                  alpha.var = 0.75,
                  labelsize = 5,
                  col.var = "blue",
                  repel = T)

# scree of elbow plot
hospitals_pca |> 
  fviz_eig(addlabels = TRUE) +
  geom_hline( yintercept = 100 * (1 / ncol(hospitals_pca$x)), 
              linetype = "dashed", color = "darkred")


#################
#### k-means ####
#################

pca_for_cluster <- hospital_pc_matrix[, 1:6]  # First 6 principal components

hospital_kmeans <- kmeans(pca_for_cluster, algorithm = "Lloyd", centers = 5, nstart = 30)


hospitals_clustered <- hospitals_num |>
  filter(if_all(where(is.numeric), ~ !is.na(.))) |>
  mutate(cluster = factor(hospital_kmeans$cluster),
         pc1 = hospital_pc_matrix[, 1],
         pc2 = hospital_pc_matrix[, 2])


hospitals_clustered |>
  ggplot(aes(pc1, pc2, color = cluster)) +
  geom_point(alpha = 0.4, size = 2) +
  labs(title = "K-Means Clustering of Hospitals (Using PC1–PC6 for Clustering)") +
  ggthemes::scale_color_colorblind()



hospitals_clustered |>
  group_by(cluster) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = T))) |>
  pivot_longer(-cluster, names_to = "feature", values_to = "mean_value") |>
  ggplot(aes(factor(cluster), mean_value, fill = factor(cluster))) +
  geom_col(show.legend = F) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(title = "Average Numeric Feature Values by Cluster",
       x = "Cluster", y = "Average Value")

hospitals_clustered |>
  ggplot(aes(x = Facility.Type, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Breakdown by Hospital Type",
       x = "Hospital Type", y = "Proportion",
       fill = "Cluster") +
  theme_minimal()

hospitals_clustered |>
  ggplot(aes(Facility.State, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Breakdown by State",
       x = "State", y = "Proportion",
       fill = "Cluster") +
  theme_minimal() +
  coord_flip()



#################################
#### hierarchical clustering ####
#################################

# pca_for_cluster <- hospital_pc_matrix[, 1:6] 
# 
# dist_matrix <- dist(pca_for_cluster)
# 
# hospital_complete <- dist_matrix |>
#   hclust(method = "complete")
# 
# hospitals_num |>
#   filter(if_all(where(is.numeric), ~ !is.na(.))) |>
#   mutate(cluster = factor(cutree(hospital_complete, k = 5)),
#          pc1 = hospital_pc_matrix[, 1],
#          pc2 = hospital_pc_matrix[, 2]) |>
#   ggplot(aes(pc1, pc2, color = cluster)) +
#   geom_point(alpha = 0.4, size = 2) +
#   ggthemes::scale_color_colorblind() +
#   theme(legend.position = "bottom")
# 
# hospital_complete |> 
#   ggdendrogram(labels = FALSE, 
#                leaf_labels = FALSE,
#                theme_dendro = FALSE) +  
#   labs(y = "Dissimilarity between clusters") +
#   theme(axis.text.x = element_blank())
# 
# hospitals_num |> 
#   mutate(cluster = as.factor(cutree(hospital_complete, h = 10))) |>
#   ggplot(aes(x = pc1, y = pc2,
#              color = cluster)) +
#   geom_point(size = 2, alpha = 0.6) + 
#   ggthemes::scale_color_colorblind()


