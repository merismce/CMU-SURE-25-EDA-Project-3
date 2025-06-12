#New File for the EDA Project 3 (Hospital Ratings)

# interested topics / questions 
# is there a correlation between rating a mortality outcomes



install.packages("tidyverse")
library(tidyverse)
hospitals <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/hospitals.csv")
View(hospitals)
typeof(hospitals)


### Basic understanding of the data ####
hospitals |> 
  group_by(Facility.State)|> 
  count(Facility.State)


hospitals |> 
  select(Facility.Type, Facility.State) |> 
  table() 

hospitals |> 
  group_by(Facility.State, Facility.Type)


hospitals |> 
  select(Facility.Type, Rating.Overall, Facility.State) |> 
  group_by(Facility.State)


hospitals |> 
  select(Facility.Type, Rating.Overall, Facility.State) |> 
  group_by(Facility.State)

##################################################################################################


### Inserting supplemental data to help understand the data better ####
### THIS IS INCOME DATA 2024 CENSUS DATA: DOES NOT INCLUDE PR ####
library(readr)
library(dplyr)

average_income <- read_csv("McElveen/AverageIncomeData_US_PR_2019_2023.csv")
View(average_income)


names(average_income)

### ENSURING THAT THE JOIN CAN HAPPEN ON THE STATE KEY ###

state_lookup <- data.frame(
  State = c(state.name, "District of Columbia", "Puerto Rico"),
  Abbreviation = c(state.abb, "DC", "PR"),
  stringsAsFactors = FALSE
)


average_income <- average_income |> 
  filter(State != "United States") |>
  left_join(state_lookup, by = "State")

View(average_income)
.
merged_data <- hospitals |> 
  left_join(average_income, by = c("Facility.State" = "Abbreviation.y"))


merged_data |> filter(Facility.State %in% c("DC", "PR"))


merged_data <- merged_data[, !(names(merged_data) %in% c("State.y", "Annual Income.y", "Abbreviation.x.x", "Abbreviation.x", "Abbreviation.y.y", "Abbreviation.x.x.x", "Abbreviation.y.y.y"))]


View(merged_data)

########################################################################################################################



#looking at the facility type and state 
merged_data |> 
  group_by(Facility.Type, Facility.State)


#have not removed NA values 
merged_data |> 
  count(Facility.Type)|>
  arrange(n) #most hospitals are privately or government owned 

View(merged_data)

#in each state, how many hospitals of each safety rating
merged_data |> 
  group_by(Facility.State) |> 
  count(merged_data$Rating.Safety)
  

#in each state, how many hospitals of each overall rating
merged_data |> 
  group_by(Facility.State) |> 
  count(merged_data$Rating.Overall)



### looking at proportions of safety rating and facility types ###
prop.table(table(merged_data$Rating.Safety)) #most dont have a safety rating
prop.table(table(merged_data$Facility.Type)) #most are privately owened 





# Filter out rows with -1 (no rating)
merged_data_no_rating_removed <- subset(merged_data, Rating.Overall != -1)



### SCATTERPLOT OF THE ANNUAL WAGE AND OVERALL RATING ###
### ATTEMPTING TO DETERMINE IF THERE IS A RELATIONSHIP BETWEEN OVERALL RATING AND THE AMOUNT OF MONEY THE AVG. PERSON IN EACH STATE EARNS ###
options(scipen = 5)  #to std notation (it was scientific before, making it hard to read wages on X-axis)


### the points on this plot are really close together, can't interpret very well ###
plot(merged_data_no_rating_removed$`Annual Income`,
     merged_data_no_rating_removed$Rating.Overall,
     main = "Hospital Ratings vs. Average Income",
     xlab = "Average Income", ylab = "Hospital Rating",
     pch = 19, col = "blue")

### jitter is spacing the points out a bit, but still hard to see ###
plot(jitter(merged_data_no_rating_removed$`Annual Income`),
     jitter(merged_data_no_rating_removed$Rating.Overall),
     main = "Hospital Ratings vs. Average Income",
     xlab = "Average Income (USD)", ylab = "Hospital Rating",
     pch = 19, col = rgb(0, 0, 1, 0.5))

### GOOD Reference -- shows each rating, and average wage that the hospital w. that rating ###
### able to see that the incomes for 4 & 5 star rated hospitals are concentrated within in a certain wage range ###
boxplot(`Annual Income` ~ Rating.Overall,
        data = merged_data_no_rating_removed,
        main = "Average Income by Hospital Rating",
        xlab = "Hospital Rating", ylab = "Average Income (USD)",
        col = "lightblue")


### making a bar plot of each state and their income 
ggplot(merged_data_no_rating_removed, aes(x = Facility.State, y = `Annual Income`))+
  geom_bar(stat = "identity")+
  coord_flip()
  
  






### plotting by state + income + rating  ### 
#allows us to see what ratings of hospitals does each state has access to based on on income
#EX. KS is low-income, but it has access to highly rated hospitals 
### UHH DON'T USE THIS ####
ggplot(merged_data_no_rating_removed, aes(x = factor(Rating.Overall),
                                          y = `Annual Income`,
                                          fill = factor(Rating.Overall))) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ Facility.State, scales = "free_y") +  # Replace 'State' with your actual column name for states
  labs(title = "Average Income by Hospital Rating Across States",
       x = "Hospital Rating",
       y = "Average Income (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")





#install.packages('dyplr')
library(dplyr)

# summary table of all states (not CO, VI, PR)
state_rating_summary <- merged_data_no_rating_removed |>
  group_by(Facility.State) |>
  summarise(
    Total_Hospitals = n(),
    High_Rated_Hospitals = sum(Rating.Overall %in% c(4, 5)),
    Proportion_High_Rated = High_Rated_Hospitals / Total_Hospitals
  ) |>
  arrange(desc(Proportion_High_Rated))  


##LOOKS at proportion of 4/5 star hospitals and state -- good reference graph 
ggplot(state_rating_summary, aes(x = reorder(Facility.State, Proportion_High_Rated), 
                                 y = Proportion_High_Rated)) +
  geom_col(fill = "steelblue") +
  coord_flip()+ 
  labs(title = "Proportion of 4 & 5 Star Hospitals by State",
       x = "State",
       y = "Proportion of Highly Rated Hospitals") +
  theme_minimal()



state_rating_income_summary <- merged_data_no_rating_removed |>
  group_by(Facility.State) |>
  summarise(
    Total_Hospitals = n(),
    High_Rated_Hospitals = sum(Rating.Overall %in% c(4, 5)),
    Proportion_High_Rated = High_Rated_Hospitals / Total_Hospitals,
    Avg_Income = mean(`Annual Income`, na.rm = TRUE)
  ) |>
  arrange(desc(Proportion_High_Rated))






### LOOKS at proportion of 4/5 star hospitals and income -- KEEP THIS FOR REFERENCE ###
### This is a scatterplot and also has a regression line to help visualize the trend
ggplot(state_rating_income_summary, aes(x = Avg_Income, y = Proportion_High_Rated)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Proportion of 4 & 5 Star Hospitals vs. Average Income by State",
       x = "Average Income (USD)",
       y = "Proportion of Highly Rated Hospitals") +
  theme_minimal()




### LOOKING AT MORTALITY AND TYPE OF HOSPITAL ###

#assigning level to the mortality column + made it ordered
hospitals$Rating.Mortality <- factor(
  hospitals$Rating.Mortality,
  levels = c("Below", "Same", "Above", "Unknown"), 
  ordered = TRUE
)
#I am treating unknown as NA -- so it won't have an impact 
hospitals$Rating.Mortality[hospitals$Rating.Mortality == "unknown"] <- NA

hospitals$Rating.Mortality <- factor(
  hospitals$Rating.Mortality,
  levels = c("Below", "Same", "Above"),
  ordered = TRUE
)



### checking that its actually right ###
#str(hospitals$Rating.Mortality)
#levels(hospitals$Rating.Mortality)

### counting the number of each level for each hospital type ###
hospitals |> 
  group_by(Facility.Type) |> 
  count(hospitals$Rating.Mortality)

hospitals |> 
  ggplot(aes(x = Rating.Mortality))+ 
  geom_bar() #majority of hospitals are rated the SAME AS THE NATIONAL AVERAGE
  #alot of them aren't even rated (unknown rating)

xtabs(~ Facility.Type + Rating.Mortality, data = hospitals) 

### plotting the mortality rating based on each hospital type ###
hospitals |> 
  count(Facility.Type, Rating.Mortality) |> 
  ggplot(aes(x = Facility.Type, y=n, 
             fill = Rating.Mortality))+
  geom_col(position = "dodge")

### looks at the mortality rate for each hospital type ###
### MULTIVARIABLE (uses 3: Rating.Mortality, Rating.Safety, Facility.Type) ###
hospitals |> 
  count(Facility.Type, Rating.Mortality, Rating.Safety) |> 
  ggplot(aes(x = Facility.Type, y = n, fill = Rating.Mortality)) +
  geom_col(position = "dodge") +  # or use "stack" for stacked bars
  facet_wrap(~ Rating.Safety) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Facility Type",
    y = "Count",
    fill = "Mortality Rating",
    title = "Hospital Facility Types by Mortality and Safety Ratings"
  )


### clustering ?? or at least trying to ###
hospitals_features <- hospitals |> 
  filter(Facility.Type == "Private") |> 
  select(Rating.Mortality, Rating.Safety, Rating.Overall) |> 
  drop_na() |> 
  mutate(across(everything(), ~as.numeric(as.character(.))))

View(hospitals_features)
std_hospitals_features <- hospitals_features |> 
  scale(center = TRUE, scale = TRUE)

kmeans_std_hospitals <- std_hospitals_features |> 
  kmeans(algorithm = "Lloyd", centers = 5, nstart=10)


### Another attempt at clustering using categorical + numeric data ###
### This uses a method called "gower" ###
#install.packages('cluster')
library(cluster)


hospital_features_2 <- merged_data_no_rating_removed |> 
  select(Facility.State, Facility.Type, Rating.Overall, `Annual Income`) |> 
  mutate(
    Facility.State = as.factor(Facility.State),
    Facility.Type = as.factor(Facility.Type)
  ) #had to change type bcuz daisy() wouldnt work 

gower_dist <- daisy(hospital_features_2, metric = "gower")
pam_fit <- pam(gower_dist, k = 5)
hospital_features_2$Cluster <- pam_fit$clustering
table(hospital_features_2$Cluster)





hospital_features_2 |> 
  group_by(Cluster) |>
  summarise(
    Avg_Rating = mean(Rating.Overall),
    Avg_Income = mean(`Annual Income`),
    Most_Common_State = names(sort(table(Facility.State), decreasing = TRUE))[1],
    Most_Common_Type = names(sort(table(Facility.Type), decreasing = TRUE))[1],
    Count = n()
  )

view(merged_data_no_rating_removed)


#### Second Attempt at clustering (i think its working)

hospital_features_2 <- merged_data_no_rating_removed |> 
  select(Facility.State, Facility.Type, Rating.Overall, `Annual Income`) |> 
  mutate(
    Facility.State = as.factor(Facility.State),
    Facility.Type = as.factor(Facility.Type)
  ) 

gower_dist <- daisy(hospital_features_2, metric = "gower")
pam_fit <- pam(gower_dist, k = 5)
hospital_features_2$Cluster <- pam_fit$clustering
summary(hospital_features_2)


hospital_features_2 |> 
  select(Facility.State, Facility.Type, Rating.Overall, `Annual Income`, Cluster) |> 
  arrange(Cluster)
  
#View(hospital_features_2)

### Tibble of the cluster ###
### able to see the average rating, top states, top facility types, and average income for each cluster ###  
hospital_features_2 |>
  group_by(Cluster) |>
  summarise(
    Count = n(),
    Avg_Income = mean(`Annual Income`, na.rm = TRUE),
    Avg_Rating = mean(Rating.Overall, na.rm = TRUE),
    Top_States = paste0(names(sort(table(Facility.State), decreasing = TRUE)[1:2]), collapse = ", "),
    Top_Types = paste0(names(sort(table(Facility.Type), decreasing = TRUE)[1:2]), collapse = ", ")
  )


### plotting the clusters. honestly, this isn't really making sense to me ###
ggplot(hospital_features_2, aes(x = `Annual Income`, 
                                y = Rating.Overall, 
                                color = as.factor(Cluster))) +
  geom_jitter(width = 0, height = 0.2, alpha = 0.6) +
  labs(title = "Income vs. Hospital Rating by Cluster",
       x = "Average Income (USD)", y = "Hospital Rating", color = "Cluster") +
  theme_minimal()

### same thing as before, but different color ### 
hospital_features_2 |> 
  ggplot(aes(x = `Annual Income`, y = Rating.Overall, 
             color = as.factor(Cluster)))+
  geom_jitter(width = 0, height = 0.2, alpha = 0.5)+
  ggthemes::scale_color_colorblind()+
  theme(legend.position = "bottom")
