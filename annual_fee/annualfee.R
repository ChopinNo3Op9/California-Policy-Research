x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr',
          'lme4','lmtest', 'scales','gridExtra')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


density_mergefee <- read.csv("D:\\Purdue\\research\\density_mergefee_updated.csv")
density_mergefee$AnnualFee <- ifelse(density_mergefee$EnactmentYear > density_mergefee$RetailerYear, 0, density_mergefee$AnnualFee)
density_mergefee$Application.Initial.Fee <- ifelse(density_mergefee$EnactmentYear > density_mergefee$RetailerYear, 0, density_mergefee$Application.Initial.Fee)
write.csv(density_mergefee, "D:\\Purdue\\research\\density_mergefee2.csv", row.names = FALSE)

summary(density_mergefee[,c(6,14,15,17,19,21,22,29)])
my_data <- na.omit(density_mergefee[,c(14,15,17,19,21,22,29)])


adhoc <- read_excel("D:\\Purdue\\research\\annualfee\\ad_hoc.xlsx")
adhoc$TIME_SERIES <- as.yearqtr(paste(adhoc$YEAR, adhoc$QUARTER, sep = "Q"))
sum_active <- aggregate(ACTIVE_COUNT ~ TIME_SERIES, data = adhoc, FUN = sum)
sum_closed <- aggregate(CLOSED_COUNT ~ TIME_SERIES, data = adhoc, FUN = sum)
sum_combined <- merge(sum_active, sum_closed, by = "TIME_SERIES")
sum_combined <- sum_combined %>%  # percentage change
  mutate(normalized_active = (ACTIVE_COUNT - lag(ACTIVE_COUNT)) / lag(ACTIVE_COUNT))
sum_combined <- sum_combined %>%
  mutate(normalized_closed = (CLOSED_COUNT - lag(CLOSED_COUNT)) / lag(CLOSED_COUNT))

# z normalization
# z_score_normalize <- function(x) {
#   (x - mean(x)) / sd(x)
# }
# sum_combined$normalized_active <- z_score_normalize(sum_combined$ACTIVE_COUNT)
# sum_combined$normalized_closed <- z_score_normalize(sum_combined$CLOSED_COUNT)

group_a <- subset(sum_combined, TIME_SERIES >= "2012 Q1" & TIME_SERIES <= "2016 Q4")
group_b <- subset(sum_combined, TIME_SERIES >= "2017 Q1" & TIME_SERIES <= "2021 Q4")
t.test(group_a$normalized_active, group_b$normalized_active)
t.test(group_a$normalized_closed, group_b$normalized_closed)


ggplot(sum_combined, aes(x = TIME_SERIES, y = ACTIVE_COUNT)) +
  geom_line() +
  geom_point() +
  labs(x = "TIME_SERIES", y = "Active Count") +
  theme_minimal()
ggplot(sum_combined, aes(x = TIME_SERIES, y = CLOSED_COUNT)) +
  geom_line() +
  geom_point() +
  labs(x = "TIME_SERIES", y = "Closed Count") +
  theme_minimal()

ggplot(sum_combined, aes(x = TIME_SERIES, y = normalized_active)) +
  geom_line() +
  geom_point() +
  labs(x = "TIME_SERIES", y = "normalized_active") +
  theme_minimal()
ggplot(sum_combined, aes(x = TIME_SERIES, y = normalized_closed)) +
  geom_line() +
  geom_point() +
  labs(x = "TIME_SERIES", y = "normalized_closed") +
  theme_minimal()


