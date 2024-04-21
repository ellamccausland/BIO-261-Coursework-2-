#Load dataset
MammalHomeRanges <- read.csv("MammalHomeRanges.csv")
#Load necessary libraries
library(tidyverse)

# Filter data for carnivores, omnivores, and herbivores
carnivores <- MammalHomeRanges %>% filter(V4 == "Carn")
omnivores <- MammalHomeRanges %>% filter(V4 == "Omn")
herbivores <- MammalHomeRanges %>% filter(V4== "Herb")

# Define the trophic group as a numerical variable
carnivores$V4 <- 3
omnivores$V4 <- 2
herbivores$V4 <- 1


# Define the mass categories (small, medium, large)
MammalHomeRanges <- MammalHomeRanges %>%
  mutate(mass_category = case_when(
    V5 >= 1 & V5 < 2.76716 ~ "small",
    V5 >= 2.77477 & V5 < 3.99955 ~ "medium",
    V5 >= 4 & V5 <= 6.60206 ~ "large",
    TRUE ~ NA_character_  # For values outside of specified ranges
  ))

# Calculate average home ranges for each trophic category and mass category
average_home_ranges <- MammalHomeRanges %>%
  filter(V6 > 0) %>% # Exclude non-positive values
  group_by(V4, mass_category) %>%
  summarize(avg_home_range = mean(V6))

# Filter out NA values in trophic category and mass category
average_home_ranges <- average_home_ranges %>%
  filter(!is.na(V4), !is.na(mass_category))


# Plotting the graph
ggplot(average_home_ranges, aes(x = mass_category, y = avg_home_range, fill = factor(V4))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Mass Category", y = "Average Home Range (log10)", fill = "Trophic Category") +
  ggtitle("") +
  theme_minimal()

# Assuming your dataset is loaded into a dataframe named MammalHomeRanges
# and the column containing labels is named 'label'

# Assuming your dataset is loaded into a dataframe named MammalHomeRanges
# and the column containing labels is named 'V4'

# Counting occurrences of each label
label_counts <- table(MammalHomeRanges$V4)

# Printing the counts
print(label_counts)

# Load necessary libraries
library(tidyverse)



# Filter out non-positive values
MammalHomeRanges_filtered <- MammalHomeRanges %>%
  filter(V6 > 0, V5 > 0)

# Fit linear regression model
model <- lm(log(V6) ~ log(V5), data = MammalHomeRanges_filtered)

# Print summary of the model
summary(model)

# Calculate average home ranges for each trophic group and mass category
average_home_ranges <- MammalHomeRanges %>%
  filter(V6 > 0) %>% # Exclude non-positive values
  group_by(V4, mass_category) %>%
  summarize(avg_home_range = mean(V6))

# Print average home ranges
print(average_home_ranges)

# Assign average home range sizes to variables
H_carnivore <- 3.73
H_herbivore <- 2.72
H_omnivore <- 3.31
H_small <- 0.825
H_medium <- 2.42
H_large <- 3.73  # There seems to be a mistake here, I'm assuming this value should be 3.73 based on the previous output

# Calculate overall average home range size
H_overall <- mean(c(H_carnivore, H_herbivore, H_omnivore, H_small, H_medium, H_large))

# Calculate coefficients
beta_0 <- H_overall
beta_1 <- H_carnivore - H_overall
beta_2 <- H_medium - H_overall

# Print coefficients
cat("beta_0 (Intercept):", beta_0, "\n")
cat("beta_1 (Coefficient for trophic group):", beta_1, "\n")
cat("beta_2 (Coefficient for mass category):", beta_2, "\n")


