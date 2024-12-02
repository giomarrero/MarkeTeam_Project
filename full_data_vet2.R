#combine the clean sus & non sus
full_data_vet <- rbind(sustainer_pool,non_sustainer_pool)
describe(full_data_vet)
#clean data
full_data_vet<- full_data_vet %>%
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
#export data
write.csv(full_data_vet, file = "Dataset3_Vet.csv")
