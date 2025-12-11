# ========================================================
# FINAL PROJECT SCRIPT: POPULAR MOVIES ANALYSIS
# Research Question: Correlation between IMDb Ratings & Metascore
# ========================================================

# --- 1. SETUP & LIBRARIES ---
# Check if ggplot2 is installed, install if not, then load it
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# --- 2. DATA IMPORT & CLEANING ---
# Load the dataset (Make sure PopularMovies.csv is in your Files pane)
raw_data <- read.csv("PopularMovies.csv")

# Select only the columns needed for the Research Question
# This prevents deleting rows that have missing data in irrelevant columns
analysis_data <- raw_data[, c("Ratings", "Metascore")]

# Remove rows with missing values (NA) to ensure accurate statistics
cleaned_df <- na.omit(analysis_data)

# Print confirmation to the Console
print(paste("Original Rows:", nrow(raw_data)))
print(paste("Cleaned Rows used for analysis:", nrow(cleaned_df)))

# --- 3. STATISTICAL ANALYSIS ---
# Test: Pearson Correlation Coefficient
# This checks if there is a linear relationship between the two variables
stat_test <- cor.test(cleaned_df$Ratings, cleaned_df$Metascore)

# PRINT RESULTS TO CONSOLE
print("------------------------------------------------")
print("STATISTICAL TEST RESULTS (Copy these for your report):")
print(stat_test)
print(paste("Correlation Coefficient (r):", round(stat_test$estimate, 3)))
print(paste("P-Value:", format.pval(stat_test$p.value)))
print("------------------------------------------------")

# --- 4. VISUALISATION 1: SCATTER PLOT (Main Evidence) ---
# Shows the relationship between User Ratings and Critic Metascore
plot1 <- ggplot(cleaned_df, aes(x=Ratings, y=Metascore)) +
  geom_point(color="#2c3e50", alpha=0.7, size=2) +  # Dark blue points
  geom_smooth(method="lm", color="#e74c3c", size=1, se=FALSE) + # Red trend line
  labs(title = "Relationship between IMDb Ratings and Metascore",
       subtitle = "Analysis of Top 250 Movies",
       x = "IMDb User Rating (0-10)",
       y = "Critic Metascore (0-100)",
       caption = "Figure 1: Scatter plot comparing audience vs. professional critic scores.") +
  theme_minimal()

# Save the plot as a PNG image
ggsave("Figure1_ScatterPlot.png", plot=plot1, width=6, height=4)
print(plot1)

# --- 5. VISUALISATION 2: HISTOGRAM (Supplementary Graphic) ---
# Shows the distribution of User Ratings
# NOTE: Uses 'after_stat(density)' to avoid warnings
plot2 <- ggplot(cleaned_df, aes(x=Ratings)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=0.1, fill="#3498db", color="white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(cleaned_df$Ratings), sd = sd(cleaned_df$Ratings)), 
                color = "#e74c3c", size = 1) +
  labs(title = "Distribution of IMDb User Ratings",
       x = "IMDb Rating",
       y = "Density",
       caption = "Figure 2: Histogram showing the concentration of high ratings.") +
  theme_minimal()

# Save the plot as a PNG image
ggsave("Figure2_Histogram.png", plot=plot2, width=6, height=4)
print(plot2)