# Load required libraries
rm(list=ls())
library(dplyr)
library(ggplot2)

# Load the dataset
data_path <- "starbucks.csv"
starbucks_df <- read.csv(data_path)

# Extract size information for the visualization of Calories & Size
starbucks_df$Size <- sub(".*((Short|Tall|Grande|Venti)).*", "\\1", starbucks_df$Beverage_prep)

# Visualize Calories by Beverage Category
ggplot(starbucks_df, aes(x = Calories, y = Beverage_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Calories by Beverage Category", x = "Calories", y = "Beverage Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize Calories by Drink Size
ggplot(starbucks_df, aes(x = Size, y = Calories)) +
  geom_boxplot(aes(fill = Size)) +
  labs(title = "Calories by Drink Size", x = "Size", y = "Calories") +
  scale_fill_brewer(palette = "Set1")

# Visualize Calories vs. Total Fat
ggplot(starbucks_df, aes(x = `Total Fat (g)`, y = Calories, color = Beverage_category)) +
  geom_point() +
  labs(title = "Calories vs. Total Fat", x = "Total Fat (g)", y = "Calories") +
  scale_color_brewer(name = "Beverage Category", palette = "Set1")

# Visualize Calories by Beverage Prep for the top 10 preps
top_10_preps <- head(sort(table(starbucks_df$Beverage_prep), decreasing = TRUE), 10)
filtered_df <- starbucks_df[starbucks_df$Beverage_prep %in% names(top_10_preps), ]
ggplot(filtered_df, aes(x = Calories, y = Beverage_prep)) +
  geom_bar(stat = "identity") +
  labs(title = "Calories by Beverage Prep (Top 10 Preps)", x = "Calories", y = "Beverage Prep")

# Categorize beverages into calorie groups
starbucks_df$`Calorie Group` <- cut(starbucks_df$Calories, breaks = c(-1, 100, 300, Inf),
                                    labels = c("Low (<100)", "Medium (100-300)", "High (>300)"))

# Count the number of beverages in each calorie group
calorie_group_counts <- sort(table(starbucks_df$`Calorie Group`))
print(calorie_group_counts)
