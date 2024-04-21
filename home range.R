#Load necessary libraries
library(tidyverse)

# Filter data for carnivores, omnivores, and herbivores
carnivores <- MammalHomeRanges %>% filter(V4 == "Carn")
omnivores <- MammalHomeRanges %>% filter(V4 == "Omn")
herbivores <- MammalHomeRanges %>% filter(V4== "Herb")

# Define the trophic group as a numerical variable
# You can assign numeric values to each trophic group (e.g., 1 for herbivores, 2 for omnivores, and 3 for carnivores)
# Adjust this according to your dataset
carnivores$V4 <- 3
omnivores$V4 <- 2
herbivores$V4 <- 1


# Define cutoff points for small, medium, and large masses
small_cutoff <- quantile(MammalHomeRanges$V5, probs = 0.33)
medium_cutoff <- quantile(MammalHomeRanges$V5, probs = 0.67)

# Create a function to categorize masses
categorize_mass <- function(mass) {
  if (mass <= small_cutoff) {
    return("Small")
  } else if (mass <= medium_cutoff) {
    return("Medium")
  } else {
    return("Large")
  }
}

# Apply the function to categorize masses
MammalHomeRanges <- MammalHomeRanges %>%
  mutate(MassCategory = sapply(V5, categorize_mass))
# Calculate average home range for each trophic group
trophic_group_means <- MammalHomeRanges %>%
  group_by(V4) %>%
  summarise(AvgHomeRange = mean(V6))

# Calculate average home range for each size group
size_group_means <- MammalHomeRanges %>%
  group_by(MassCategory) %>%
  summarise(AvgHomeRange = mean(V6))

# Plotting
par(mfrow = c(1, 2))  # Create a 1x2 layout for plots

# Bar plot for trophic groups
barplot(trophic_group_means$AvgHomeRange, 
        names.arg = c("Herbivore", "Omnivore", "Carnivore"),
        main = "Average Home Range by Trophic Group",
        ylab = "Average Home Range (log10)",
        col = "lightblue",
        ylim = range(0, max(trophic_group_means$AvgHomeRange) * 1.2))

# Bar plot for size groups
barplot(size_group_means$AvgHomeRange, 
        names.arg = c("Small", "Medium", "Large"),
        main = "Average Home Range by Size Group",
        ylab = "Average Home Range (log10)",
        col = "lightgreen",
        ylim = range(0, max(size_group_means$AvgHomeRange) * 1.2))





