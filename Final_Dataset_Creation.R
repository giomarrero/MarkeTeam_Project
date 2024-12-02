library(dplyr)
set.seed(123)
#### generic data
###with outliers
###sus
sus_out_pool <- subset(Generic_data_flag_outliers, SUS_FLAG == 'Yes')
sus_out_count = nrow(sus_out_pool)
require_non_sus_out = round(sus_out_count *0.7 / 0.3,0)
nonsus_out_pool <- subset(Generic_data_flag_outliers, SUS_FLAG == 'No')
summary(nonsus_out_pool)
#reduce non-sus
#remove na age
nonsus_out_filtered <- nonsus_out_pool %>%
  filter(!is.na(AGE)) %>% 
  filter(!is.na(DISTINCT_APPEAL_COUNT)) %>%
  filter(!is.na(AVG_DAYS_ACROSS_APPEALCODES)) %>%
  filter(!is.na(RESPONSE_RATE)) %>%
  filter(!is.na(TOTAL_AMOUNT_GIFTED_PRO)) %>%
  filter(AVG_DAYS_ACROSS_APPEALCODES >= 0)
total_rows <- nrow(nonsus_out_filtered)
rows_selected <- sample(1:total_rows, size = require_non_sus_out)

# Subset the data frame to keep only the randomly selected rows
nonsus_out_selected <- nonsus_out_filtered[rows_selected, ]

#combine sus & non-sus
generic_data_out_reduced <- rbind(sus_out_pool, nonsus_out_selected)
summary(generic_data_out_reduced)
##fill NAs
generic_data_out_reduced <- generic_data_out_reduced %>%
  mutate(COUPLE = replace(COUPLE, is.na(COUPLE),0),
         FEMALE = replace(FEMALE, is.na(FEMALE),0),
         MALE = replace(MALE, is.na(MALE),0),
         ORG = replace(ORG, is.na(ORG),0),
         AGE = ifelse(is.na(AGE), mean(AGE, na.rm = TRUE),AGE),
         DISTINCT_APPEAL_COUNT = replace(DISTINCT_APPEAL_COUNT, is.na(DISTINCT_APPEAL_COUNT),0),
         AVG_DAYS_ACROSS_APPEALCODES = replace(AVG_DAYS_ACROSS_APPEALCODES, is.na(AVG_DAYS_ACROSS_APPEALCODES),0),
         RESPONSE_RATE = replace(RESPONSE_RATE, is.na(RESPONSE_RATE),0),
         TOTAL_AMOUNT_GIFTED_PRO = replace(TOTAL_AMOUNT_GIFTED_PRO, is.na(TOTAL_AMOUNT_GIFTED_PRO),0),
         outlier_flag = replace(outlier_flag, is.na(outlier_flag), FALSE)
         )
summary(generic_data_out_reduced)
# write the data to csv
write.csv(generic_data_out_reduced, "generic_data_out_reduced.csv")

###without outliers
##remove outliers
generic_data_out_remv <- subset(Generic_data_flag_outliers, outlier_flag == FALSE)
###sus
sus_no_out_pool <- subset(generic_data_out_remv, SUS_FLAG == 'Yes')
sus_no_out_count = nrow(sus_no_out_pool)
require_non_sus_no_out = round(sus_no_out_count *0.7 / 0.3,0)
nonsus_no_out_pool <- subset(generic_data_out_remv, SUS_FLAG == 'No')
summary(nonsus_no_out_pool)
#reduce non-sus
#remove na age
nonsus_no_out_filtered <- nonsus_no_out_pool %>%
  filter(!is.na(DISTINCT_APPEAL_COUNT)) %>%
  filter(!is.na(AVG_DAYS_ACROSS_APPEALCODES)) %>%
  filter(!is.na(RESPONSE_RATE)) %>%
  filter(!is.na(TOTAL_AMOUNT_GIFTED_PRO)) %>%
  filter(AVG_DAYS_ACROSS_APPEALCODES >= 0)
total_no_out_rows <- nrow(nonsus_no_out_filtered)
rows_no_out_selected <- sample(1:total_no_out_rows, size = require_non_sus_no_out)

# Subset the data frame to keep only the randomly selected rows
nonsus_no_out_selected <- nonsus_no_out_filtered[rows_no_out_selected, ]

#combine sus & non-sus
generic_data_no_out_reduced <- rbind(sus_no_out_pool, nonsus_no_out_selected)
summary(generic_data_no_out_reduced)

##fill NAs
generic_data_no_out_reduced <- generic_data_no_out_reduced %>%
  mutate(COUPLE = replace(COUPLE, is.na(COUPLE),0),
         ORG = replace(ORG, is.na(ORG),0),
         DISTINCT_APPEAL_COUNT = replace(DISTINCT_APPEAL_COUNT, is.na(DISTINCT_APPEAL_COUNT),0),
         AVG_DAYS_ACROSS_APPEALCODES = replace(AVG_DAYS_ACROSS_APPEALCODES, is.na(AVG_DAYS_ACROSS_APPEALCODES),0),
         RESPONSE_RATE = replace(RESPONSE_RATE, is.na(RESPONSE_RATE),0),
         TOTAL_AMOUNT_GIFTED_PRO = replace(TOTAL_AMOUNT_GIFTED_PRO, is.na(TOTAL_AMOUNT_GIFTED_PRO),0)
  )
summary(generic_data_no_out_reduced)
# write the data to csv
write.csv(generic_data_no_out_reduced, "generic_data_no_out_reduced.csv")
