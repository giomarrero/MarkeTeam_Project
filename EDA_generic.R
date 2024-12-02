set.seed(2024)
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

# Load the dataset (replace with your actual dataset)
df <- clean_generic_model

# View the structure and summary of the dataset
str(df)
summary(df)
# Histogram for each numeric variable
numeric_vars <- c("MAX_GIFT", "AVG_GIFT", "TOTAL_GIFT", "AMOUNT_GIFTED_LAST_YEAR",
                  "DISTINCT_APPEAL_COUNT", "AVG_DAYS_ACROSS_APPEALCODES", 
                  "RESPONSE_RATE", "TOTAL_AMOUNT_GIFTED_PRO", "DONATION_COUNT",
                  "AGE")

for (var in numeric_vars) {
  p <- ggplot(df, aes_string(x = var)) +
    geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  
  # Print the plot
  print(p)
}

# Filter out data where DONATION_COUNT is above 100
filtered_df_DC <- df %>% 
  filter(DONATION_COUNT <= 100)

# Re-plot the histogram for the filtered data
ggplot(filtered_df_DC, aes(x = DONATION_COUNT)) + 
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of DONATION_COUNT (<= 100)", x = "DONATION_COUNT", y = "Frequency") +
  theme_minimal()

ggplot(filtered_df_DC, aes(y = DONATION_COUNT)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot ofDONATION_COUNT (<= 100)") +
  theme_minimal()

# Filter out data where total amount gifted pro is above 10000
filtered_df_TAG <- df %>% 
  filter(TOTAL_AMOUNT_GIFTED_PRO <= 1000)

# Re-plot the histogram for the filtered data
ggplot(filtered_df_TAG, aes(x = TOTAL_AMOUNT_GIFTED_PRO)) + 
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of TOTAL_AMOUNT_GIFTED_PRO (<= 1000)", x = "TOTAL_AMOUNT_GIFTED_PRO", y = "Frequency") +
  theme_minimal()

ggplot(filtered_df_TAG, aes(y = DONATION_COUNT)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot ofTOTAL_AMOUNT_GIFTED_PRO (<= 1000)") +
  theme_minimal()
# Filter out data where amount gifted last year is above 1000
filtered_df_AGLY <- df %>% 
  filter(AMOUNT_GIFTED_LAST_YEAR <= 1000)

# Re-plot the histogram for the filtered data
ggplot(filtered_df_AGLY, aes(x = AMOUNT_GIFTED_LAST_YEAR)) + 
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of AMOUNT_GIFTED_LAST_YEAR (<= 1,000)", x = "AMOUNT_GIFTED_LAST_YEAR", y = "Frequency") +
  theme_minimal()

ggplot(filtered_df_AGLY, aes(y = AMOUNT_GIFTED_LAST_YEAR)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of AMOUNT_GIFTED_LAST_YEAR (<= 1,000)") +
  theme_minimal()

# Filter out data where total is above 1000
filtered_df_TG <- df %>% 
  filter(TOTAL_GIFT <= 1000)

# Re-plot the histogram for the filtered data
ggplot(filtered_df_TG, aes(x = TOTAL_GIFT)) + 
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of TOTAL_GIFT (<= 1000)", x = "TOTAL_GIFT", y = "Frequency") +
  theme_minimal()

ggplot(filtered_df_TG, aes(y = TOTAL_GIFT)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of TOTAL_GIFT (<= 1000)") +
  theme_minimal()

# Filter out data where avg gift is above 1000
filtered_df_AG <- df %>% 
  filter(AVG_GIFT <= 1000)

# Re-plot the histogram for the filtered data
ggplot(filtered_df_AG, aes(x = AVG_GIFT)) + 
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of AVG_GIFT (<= 1000)", x = "AVG_GIFT", y = "Frequency") +
  theme_minimal()

ggplot(filtered_df_AG, aes(y = AVG_GIFT)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of  AVG_GIFT (<= 1000)") +
  theme_minimal()

# Filter out data where max gift is above 1000
filtered_df_MG <- df %>% 
  filter(MAX_GIFT <= 1000)

# Re-plot the histogram for the filtered data
ggplot(filtered_df_MG, aes(x = MAX_GIFT)) + 
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of MAX_GIFT (<= 1000)", x = "MAX_GIFT", y = "Frequency") +
  theme_minimal()

ggplot(filtered_df_MG, aes(y = MAX_GIFT)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of MAX_GIFT (<= 1000)") +
  theme_minimal()

# Boxplot to detect outliers for each numeric variable
for (var in numeric_vars) {
  p <- ggplot(df, aes_string(y = var)) +
    geom_boxplot(fill = "orange") +
    labs(title = paste("Boxplot of", var), y = var) +
    theme_minimal() 
  print(p)
}

# Bar plot for each categorical variable
categorical_vars <- c("SUS_FLAG" ,"COUPLE","FEMALE","MALE", "MOST_COMMON_GIFT_CHANNEL_PRO",
                      "ONE_TIME_FLAG","AGE_UNDER50" ,"AGE_50_59","AGE_60_69",                  
                      "AGE_70PLUS","AGE_NA" ,"outlier_flag","TOP_5_SUS_PERCENT_FLAG"  )

for (var in categorical_vars) {
  p <- ggplot(df, aes_string(x = var)) +
    geom_bar(fill = "lightgreen") +
    labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
    theme_minimal() +
    scale_y_continuous(labels = comma)
  print(p)
}


# Calculate and plot correlation matrix
cor_matrix <- cor(df[, numeric_vars], use = "complete.obs")
melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

