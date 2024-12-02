# Set seed for reproducibility
set.seed(123)
library(readr)
#read data
Vet_data_flag_outliers_flagged <- read_csv("Vet_data_flag_outliers_flagged.csv", 
                                           col_types = cols(X = col_skip(), TOP_5_COUNT_FLAG = col_skip(), 
                                                            TOP_5_FLAG = col_skip()))
df <- Vet_data_flag_outliers_flagged

#standardize flags to 0/1
df <- df %>%
  mutate(across(where(is.logical), as.integer)) %>%     # Convert TRUE/FALSE to 1/0
  mutate(across(where(~ all(. %in% c("Yes", "No"))), ~ ifelse(. == "Yes", 1, 0)))

#remove account id ,org and member fields
df <- df[,-c(2,10,20)]

#create sustainers/non_sustainers pools
sustainer_pool <- subset(df, SUS_FLAG == 1)
non_sustainer_pool <- subset(df, SUS_FLAG == 0)

describe(sustainer_pool)
#handle NAs in sus
# Calculate the mean of the 'age' column, excluding NA values
mean_age_sus <- mean(sustainer_pool$AGE, na.rm = TRUE)

# Replace NA values in the 'age' column with the calculated mean
sustainer_pool$AGE[is.na(sustainer_pool$AGE)] <- mean_age_sus
sustainer_pool<- sustainer_pool %>%
  mutate(
    COUPLE = ifelse(is.na(COUPLE), 0, COUPLE),
    MALE = ifelse(is.na(MALE), 0, MALE),
    FEMALE = ifelse(is.na(FEMALE), 0, FEMALE),
    STATE = ifelse(is.na(STATE), "no_state", STATE),
    DISTINCT_APPEAL_COUNT = ifelse(is.na(DISTINCT_APPEAL_COUNT), 0, DISTINCT_APPEAL_COUNT),
    AVG_DAYS_ACROSS_APPEALCODES = ifelse(is.na(AVG_DAYS_ACROSS_APPEALCODES), 0, 
                                         AVG_DAYS_ACROSS_APPEALCODES),
    AVG_DAYS_ACROSS_APPEALCODES = ifelse(AVG_DAYS_ACROSS_APPEALCODES < 0, 0, AVG_DAYS_ACROSS_APPEALCODES),
    RESPONSE_RATE = ifelse(is.na(RESPONSE_RATE), 0, 
                           RESPONSE_RATE),
    TOTAL_AMOUNT_GIFTED_PRO = ifelse(is.na(TOTAL_AMOUNT_GIFTED_PRO), 0, TOTAL_AMOUNT_GIFTED_PRO),
    outlier_flag = ifelse(is.na(outlier_flag), 0, outlier_flag)
  )

#handle NAs in non_sus
# Calculate the mean of the 'age' column, excluding NA values
mean_age_non_sus <- mean(non_sustainer_pool$AGE, na.rm = TRUE)

# Replace NA values in the 'age' column with the calculated mean
non_sustainer_pool$AGE[is.na(non_sustainer_pool$AGE)] <- mean_age_non_sus
describe(non_sustainer_pool)
summary(non_sustainer_pool)
non_sustainer_pool<- non_sustainer_pool %>%
  mutate(
    COUPLE = ifelse(is.na(COUPLE), 0, COUPLE),
    MALE = ifelse(is.na(MALE), 0, MALE),
    FEMALE = ifelse(is.na(FEMALE), 0, FEMALE),
    STATE = ifelse(is.na(STATE), "no_state", STATE),
    DISTINCT_APPEAL_COUNT = ifelse(is.na(DISTINCT_APPEAL_COUNT), 0, DISTINCT_APPEAL_COUNT),
    AVG_DAYS_ACROSS_APPEALCODES = ifelse(is.na(AVG_DAYS_ACROSS_APPEALCODES), 0, 
                                         AVG_DAYS_ACROSS_APPEALCODES),
    AVG_DAYS_ACROSS_APPEALCODES = ifelse(AVG_DAYS_ACROSS_APPEALCODES < 0, 0, AVG_DAYS_ACROSS_APPEALCODES),
    MOST_COMMON_GIFT_CHANNEL_PRO = ifelse(is.na(MOST_COMMON_GIFT_CHANNEL_PRO),"unknown", MOST_COMMON_GIFT_CHANNEL_PRO),
    RESPONSE_RATE = ifelse(is.na(RESPONSE_RATE), 0, 
                           RESPONSE_RATE),
    TOTAL_AMOUNT_GIFTED_PRO = ifelse(is.na(TOTAL_AMOUNT_GIFTED_PRO), 0, TOTAL_AMOUNT_GIFTED_PRO),
    outlier_flag = ifelse(is.na(outlier_flag), 0, outlier_flag)
  )

# Determine how many times to duplicate minority samples to balance the classes
num_majority <- nrow(non_sustainer_pool)
num_minority <- nrow(sustainer_pool)

# Sample from the minority class with replacement to create a balanced dataset
oversampled_minority <- sustainer_pool[sample(1:num_minority, num_majority, replace = TRUE), ]
# Combine the oversampled minority class with the majority class
balanced_data <- rbind(non_sustainer_pool, oversampled_minority)
#export data
write.csv(balanced_data, file = "Dataset2_Vet.csv")
