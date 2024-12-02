library(dplyr)
set.seed(123)
df <- Vet_data_flag_outliers_flagged
#check for NAs
describe(df)
#remove account id ,org and member fields
df <- df[,-c(2,10,20)]
#create sustainers/non_sustainers pools
sustainer_pool <- subset(df, SUS_FLAG == 'Yes')
non_sustainer_pool <- subset(df, SUS_FLAG == 'No')
##
# a <- subset(Vet_data_flag_outliers_flagged, SUS_FLAG == 'Yes')
# non_sustainer_pool <- subset(Vet_data_flag_outliers_flagged, SUS_FLAG == 'No')
# a_per <- (12494/1617516)*100
# b_per <- (1605022/1617516)*100

#create 70% non_Sus/ 30% sus
required_non_sus =round(12494 *.7/.3, 0) 
describe(sustainer_pool)
null_age_sus <- (3135/12494)*100 ##25%
# Calculate the mean of the 'age' column, excluding NA values
mean_age_sus <- mean(sustainer_pool$AGE, na.rm = TRUE)
# Replace NA values in the 'age' column with the calculated mean
sustainer_pool$AGE[is.na(sustainer_pool$AGE)] <- mean_age_sus
non_sus_per = 1605022 / 1617516 *100 ##99%
remove_non_sus = 1605022 - required_non_sus
#non_sus

Na_age_nonsus = length(non_sustainer_pool$AGE[is.na(non_sustainer_pool$AGE)])
remove_non_sus - Na_age_nonsus
describe(non_sustainer_pool)
summary(non_sustainer_pool)
nonsus_filtered <- non_sustainer_pool %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(AVG_DAYS_ACROSS_APPEALCODES)) %>%
  filter(AVG_DAYS_ACROSS_APPEALCODES >= 0) 
  # filter(!is.na(AVG_DAYS_ACROSS_APPEALCODES))
summary(nonsus_filtered)
describe(nonsus_filtered)
total_rows <- nrow(nonsus_filtered)
rows_to_keep <- 119618
rows_selected <- sample(1:total_rows, size = required_non_sus)

# Subset the data frame to keep only the randomly selected rows
df_filtered <- nonsus_filtered[rows_selected, ]

subseted_vet <- rbind(df_filtered, sustainer_pool)
summary(subseted_vet)

#handle missing data
subseted_vet <- subseted_vet %>%
  mutate(
    COUPLE = ifelse(is.na(COUPLE), 0, COUPLE),
    MALE = ifelse(is.na(MALE), 0, MALE),
    FEMALE = ifelse(is.na(FEMALE), 0, FEMALE),
    #AGE = ifelse(is.na(AGE), mean(AGE, na.rm = TRUE), AGE),
    DISTINCT_APPEAL_COUNT = ifelse(is.na(DISTINCT_APPEAL_COUNT), 0, DISTINCT_APPEAL_COUNT),
    AVG_DAYS_ACROSS_APPEALCODES = ifelse(is.na(AVG_DAYS_ACROSS_APPEALCODES), 0, 
                                         AVG_DAYS_ACROSS_APPEALCODES),
    AVG_DAYS_ACROSS_APPEALCODES = ifelse(AVG_DAYS_ACROSS_APPEALCODES < 0, 0, AVG_DAYS_ACROSS_APPEALCODES),
    RESPONSE_RATE = ifelse(is.na(RESPONSE_RATE), 0, 
                           RESPONSE_RATE),
    TOTAL_AMOUNT_GIFTED_PRO = ifelse(is.na(TOTAL_AMOUNT_GIFTED_PRO), 0, TOTAL_AMOUNT_GIFTED_PRO),
    outlier_flag = ifelse(is.na(outlier_flag), 0, outlier_flag)
  )
write.csv(subseted_vet,"Dataset1_Vet.csv")
