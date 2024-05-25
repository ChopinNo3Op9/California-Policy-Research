x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr',
          'lme4','lmtest', 'scales','gridExtra')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})

# path <- 'D:\\Purdue\\research\\lmer\\'
# density_mergefee <- read.csv(paste0(path, "density_mergefee_lmer_mile.csv"), skip = 0)
# density_mergefee <- read_excel(paste0(path, "density_mergefee_lmer.xlsx"))
# density_mergefee <- subset(density_mergefee, !is.na(AnnualFee) & AnnualFee != 0)
# unique(density_mergefee$AnnualFee)
# unique(density_mergefee$citylicense)
# unique(density_mergefee$EnactmentDate)
# 
# # density_mergefee$RetailerCount_density <- density_mergefee$RetailerCount_density * 1000
# density_mergefee$year <- as.factor(ifelse(density_mergefee$RetailerYear >= 2016, 1, 0)) # year (before 2016: 0, after 2016: 1)
# density_mergefee$City <- as.factor(density_mergefee$City)
# # density_mergefee$citylicense <- ifelse(density_mergefee$citylicense %in% c(0, 1), "weak",
# #                                        ifelse(density_mergefee$citylicense %in% c(2, 3), "moderate", "strong"))
# density_mergefee$citylicense <- as.factor(density_mergefee$citylicense)
# density_mergefee$citylicense <- relevel(density_mergefee$citylicense, ref = "weak")
# density_mergefee$pharmacyban <- as.numeric(density_mergefee$pharmacyban)
# density_mergefee$pharmacyban <- ifelse(density_mergefee$pharmacyban %in% c(2, 3, 4), 0, density_mergefee$pharmacyban)
# density_mergefee$pharmacyban <- as.factor(density_mergefee$pharmacyban)
# # density_mergefee <- density_mergefee[density_mergefee$AnnualFee != 0 & !is.na(density_mergefee$AnnualFee), ]
# 
# density_mergefee$EnactmentDate <- as.POSIXct(density_mergefee$EnactmentDate, tz = "UTC")
# density_mergefee$EnactmentDate <- as.Date(density_mergefee$EnactmentDate)
# density_mergefee$EnactmentDate <- as.character(density_mergefee$EnactmentDate)
# density_mergefee$EnactmentDate[is.na(density_mergefee$EnactmentDate)] <- ""
# non_empty_dates <- density_mergefee$EnactmentDate != ""
# date_split <- strsplit(density_mergefee$EnactmentDate[non_empty_dates], "/")
# year <- sapply(date_split, function(x) as.numeric(x[2]))
# # year <- ifelse(year > 80, year + 1900, year + 2000)
# density_mergefee$EnactmentYear <- "NA"
# density_mergefee$EnactmentYear[non_empty_dates] <- as.character(year)
# density_mergefee$EnactmentYear <- as.integer(density_mergefee$EnactmentYear)
# density_mergefee$AnnualFee <- ifelse(density_mergefee$EnactmentYear > density_mergefee$RetailerYear, 0, density_mergefee$AnnualFee)
# density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)


# density_mergefee <- read_excel("density_mergefee_lmer.xlsx")
density_mergefee <- read.csv("density_mergefee_lmer_mile.csv")
for (col in colnames(density_mergefee)) {
  if (!col %in% c("City", "County", "citylicense")) {
    # Convert to numeric, handling factors and characters appropriately
    if (is.factor(density_mergefee[[col]]) || is.character(density_mergefee[[col]])) {
      density_mergefee[[col]] <- as.numeric(as.character(density_mergefee[[col]]))
    } else {
      density_mergefee[[col]] <- as.numeric(density_mergefee[[col]])
    }
  }
}

sapply(density_mergefee, class)
density_mergefee <- subset(density_mergefee, !is.na(AnnualFee) & AnnualFee != 0)
unique(density_mergefee$AnnualFee)
unique(density_mergefee$citylicense)
unique(density_mergefee$EnactmentDate)

# density_mergefee$RetailerCount_density <- density_mergefee$RetailerCount_density * 1000
density_mergefee$year <- as.factor(ifelse(density_mergefee$RetailerYear >= 2017, 1, 0)) # year (before 2016: 0, after 2016: 1)
density_mergefee <- subset(density_mergefee, year == "0")
density_mergefee$City <- as.factor(density_mergefee$City)
# density_mergefee$citylicense <- ifelse(density_mergefee$citylicense %in% c(0, 1), "weak",
#                                        ifelse(density_mergefee$citylicense %in% c(2, 3), "moderate", "strong"))
density_mergefee$citylicense <- as.factor(density_mergefee$citylicense)
density_mergefee$citylicense <- relevel(density_mergefee$citylicense, ref = "weak")
density_mergefee$pharmacyban <- as.numeric(density_mergefee$pharmacyban)
density_mergefee$pharmacyban <- ifelse(density_mergefee$pharmacyban %in% c(2, 3, 4), 0, density_mergefee$pharmacyban)
density_mergefee$pharmacyban <- as.factor(density_mergefee$pharmacyban)
# density_mergefee <- density_mergefee[density_mergefee$AnnualFee != 0 & !is.na(density_mergefee$AnnualFee), ]

density_mergefee$EnactmentDate <- as.POSIXct(density_mergefee$EnactmentDate, tz = "UTC")
density_mergefee$EnactmentDate <- as.Date(density_mergefee$EnactmentDate)
density_mergefee$EnactmentDate <- as.character(density_mergefee$EnactmentDate)
density_mergefee$EnactmentDate[is.na(density_mergefee$EnactmentDate)] <- ""
non_empty_dates <- density_mergefee$EnactmentDate != ""
date_split <- strsplit(density_mergefee$EnactmentDate[non_empty_dates], "-")
year <- sapply(date_split, function(x) as.numeric(x[1]))
# year <- ifelse(year > 80, year + 1900, year + 2000)
density_mergefee$EnactmentYear <- "NA"
density_mergefee$EnactmentYear[non_empty_dates] <- as.character(year)
density_mergefee$EnactmentYear <- as.integer(density_mergefee$EnactmentYear)
density_mergefee$AnnualFee <- ifelse(density_mergefee$EnactmentYear > density_mergefee$RetailerYear, 0, density_mergefee$AnnualFee)
density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)
density_mergefee$RetailerCount_density <- as.numeric(density_mergefee$RetailerCount_density)

summary(density_mergefee[,c(6,14,15,17,19,21,22,29)])

my_data <- na.omit(density_mergefee[,c(14,15,17,19,21,22,29)])

##########################################################
result <- aggregate(Count ~ RetailerYear, data = density_mergefee, FUN = function(x) c(Sum = sum(x), Average = mean(x)))
# Create a line chart for the sum values
sum_plot <- ggplot(result, aes(x = RetailerYear)) +
  geom_line(aes(y = result$Count[, 1], color = "Sum"), size = 1.5) +
  geom_point(aes(y = result$Count[, 1], color = "Sum"), size = 3) +
  labs(title = "Sum of Count by RetailerYear",
       x = "RetailerYear", y = "Count") +
  scale_color_manual(values = c(Sum = "#FF6F00")) +
  theme_minimal()

# Create a line chart for the average values
avg_plot <- ggplot(result, aes(x = RetailerYear)) +
  geom_line(aes(y = result$Count[, 2], color = "Average"), size = 1.5) +
  geom_point(aes(y = result$Count[, 2], color = "Average"), size = 3) +
  labs(title = "Average of Count by RetailerYear",
       x = "RetailerYear", y = "Count") +
  scale_color_manual(values = c(Average = "#2979FF")) +
  theme_minimal()

# Arrange the two plots side by side
gridExtra::grid.arrange(sum_plot, avg_plot, ncol = 2)

##########################################################

# Create a bar chart for the sum values
sum_plot <- ggplot(result, aes(x = RetailerYear, y = result$Count[, 1])) +
  geom_bar(stat = "identity", fill = "#FF6F00") +
  labs(title = "Sum of Count by RetailerYear",
       x = "RetailerYear", y = "Count")

# Create a bar chart for the average values
avg_plot <- ggplot(result, aes(x = RetailerYear, y = result$Count[, 2])) +
  geom_bar(stat = "identity", fill = "#2979FF") +
  labs(title = "Average of Count by RetailerYear",
       x = "RetailerYear", y = "Count")

# Arrange the two bar charts side by side
gridExtra::grid.arrange(sum_plot, avg_plot, ncol = 2)

##########################################################
counties <- unique(density_mergefee$County)
graphs <- list()
for (county in counties) {
  county_data <- subset(density_mergefee, County == county)
  avg_density <- county_data %>%
    group_by(RetailerYear) %>%
    summarize(avg_density = mean(RetailerCount_density))
  
  p <- ggplot(avg_density, aes(x = RetailerYear, y = avg_density)) +
    geom_line() +
    labs(title = county, x = "Retailer Year", y = "Average Density") +
    theme_minimal()
  graphs[[county]] <- p
}
print(graphs)

# arrange graphs in grid and add title
grid.arrange(grobs = graphs, ncol = 4, 
             top = textGrob("Average Retailer Count Density by County and Retailer Year", gp = gpar(fontsize = 16)))

#########################################################
density_mergefee %>%
  group_by(RetailerYear, citylicense) %>%
  summarize(num_cities = n()) %>%
  print(n = Inf)

density_mergefee %>%
  group_by(citylicense, RetailerYear) %>%
  summarize(num_cities = n()) %>%
  print(n = Inf)


# # set 2016 as the reference
# density_mergefee$RetailerYear <- density_mergefee$RetailerYear - 2016
# density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)
# # density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)
# 
# did_main <- lmer(RetailerCount_density*1000 ~  relevel(RetailerYear, ref = "0") + citylicense + pharmacyban + Total_RacialMinority +
#                    Bachelor_higher_1824 + Bachelor_higher_25 + 
#                    Percent.below.poverty.level + (1 | City) +
#                    year*citylicense + year*pharmacyban + RetailerYear*citylicense + RetailerYear*pharmacyban
#                    , data = density_mergefee)
# summary(did_main)
# 
# did_main <- lmer(RetailerCount_density*1000 ~  relevel(RetailerYear, ref = "2016") + citylicense + pharmacyban + Total_RacialMinority +
#                    Bachelor_higher_1824 + Bachelor_higher_25 + 
#                    Percent.below.poverty.level + (1 | City) +
#                    year*citylicense + year*pharmacyban + RetailerYear*citylicense + RetailerYear*pharmacyban
#                  , data = density_mergefee)
# summary(did_main)
# 
# did_main <- lmer(RetailerCount_density*1000 ~  RetailerYear + citylicense + pharmacyban + Total_RacialMinority +
#                    Bachelor_higher_1824 + Bachelor_higher_25 + 
#                    Percent.below.poverty.level + (1 | City) +
#                    year*citylicense + year*pharmacyban + RetailerYear*citylicense + RetailerYear*pharmacyban
#                  , data = density_mergefee)
# summary(did_main)
# 
# # set 2017 as the reference
# density_mergefee$RetailerYear <- density_mergefee$RetailerYear - 2017
# density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)
# # no need to as a factor if make subtraction
# # density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)
# 
# did_main <- lmer(RetailerCount_density*1000 ~ relevel(RetailerYear, ref = "0") + citylicense + pharmacyban + Total_RacialMinority +
#                    Bachelor_higher_1824 + Bachelor_higher_25 + 
#                    Percent.below.poverty.level + (1 | City) +
#                    year*citylicense + year*pharmacyban + RetailerYear*citylicense + RetailerYear*pharmacyban
#                  , data = density_mergefee)
# summary(did_main)


# A higher REML criterion indicates a better fit, with less unexplained variance.
# More negative values mean a worse fit, while less negative values mean a better fit
# The lmer() function was designed to handle non-normalized data
# Random effects are used to model variation between clusters or groups. 
# They account for the non-independence of observations within groups.
# The random effect (1 | City) specifies a random intercept for City But it will not have a coefficient value.


# wald test follows t distribution under the null hypothesis that the coefficient is zero.
# adjusted Wald test: ratio of a coefficient estimate and its standard error, joint F-test statistic
# whether you can reject the null hypothesis that all predictors are jointly zero
# lm() in R performs t-tests to test for the significance of individual predictors. 
# It does not perform an adjusted Wald test to test for the joint significance of all predictors.
coefs <- summary(did_main)$coefficients[,1]
se <- summary(did_main)$coefficients[,2]
n <- nrow(density_mergefee)
k <- length(coefs)
adj_se <- se * sqrt(n/(n - k))
test_stat <- coefs / adj_se
pvalue <- pf(test_stat, df1 = k, df2 = n - k, lower.tail = FALSE)
pvalue


coef_table <- function(did_main){
  # Extract the fixed effects coefficients and their standard errors
  coef <- summary(did_main)$coefficients[, 1:2]
  coef <- round(coef, 2)
  
  # t-values and the corresponding degrees of freedom
  tval <- round(coef[, 1] / coef[, 2], 2) # Round to 2 decimal places
  
  # p-values   
  df <- nrow(density_mergefee) - length(coef[,1]) - 1 
  pval <- round(2 * pt(abs(tval), df = df, lower.tail = FALSE), 4) # Round to 4 decimal places
  
  # Add asterisks to indicate significance level
  stars <- ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "")))
  
  # Calculate the confidence intervals 
  alpha <- 0.05
  se <- coef[, 2]  
  zval <- qt(1 - alpha/2, df = df)  # quantile function qt() from the t-distribution
  ci_lo <- round(coef[, 1] - zval * se, 2) # Round to 2 decimal places
  ci_hi <- round(coef[, 1] + zval * se, 2) # Round to 2 decimal places
  
  # Add to coefficient table
  coef <- cbind(coef, tval, pval, stars, ci_lo, ci_hi)
  colnames(coef) <- c("Estimate", "Std. Error", "t-value", "p-value", "Significance", "Lower CI", "Upper CI")
  
  print(coef)
}


coef_table <- function(did_main){
  # Extract the fixed effects coefficients and their standard errors
  coef <- summary(did_main)$coefficients[, 1:2]
  coef <- round(coef, 2)
  tval <- round(coef[, 1] / coef[, 2], 2)
  df <- nrow(density_mergefee) - length(coef[,1]) - 1 
  pval <- round(2 * pt(abs(tval), df = df, lower.tail = FALSE), 4)
  stars <- ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "")))
  # Calculate the confidence intervals 
  alpha <- 0.05
  se <- coef[, 2]  
  zval <- qt(1 - alpha/2, df = df)  # quantile function qt() from the t-distribution
  ci_lo <- round(coef[, 1] - zval * se, 2) # Round to 2 decimal places
  ci_hi <- round(coef[, 1] + zval * se, 2) # Round to 2 decimal places
  pval_ci <- paste0("(", ci_lo, "-", ci_hi, ")", stars, " ")
  coef <- cbind(coef[, 1], pval_ci)
  colnames(coef) <- c("Estimate", "CI and Significance")
  print(coef)
}

# The t (or z) values indicate how many standard deviations the coefficient is from 0.
# |t| > 2 indicates the coefficient is likely significant
# |t| > 3 indicates it is more strongly significant
did_main <- lmer(RetailerCount_density ~ year + citylicense + pharmacyban + Total_RacialMinority +
                   Bachelor_higher_1824 + Bachelor_higher_25 + 
                   Percent.below.poverty.level + (1 | City) +
                   year*citylicense + year*pharmacyban, data = density_mergefee)
summary(did_main)
coef_table(did_main)
write.csv(coef_table(did_main), file = paste0(path, "main__.csv"))

# likelihood ratio test
# model with only intercept, 
# baseline model that assumes that the response variable has a constant value across all observations
null_model <- lm(RetailerCount_density ~ 1, data = density_mergefee) 
lrtest <- lrtest(null_model, did_main)
# The test results indicate that Model 2 provides a significantly better fit to the data than Model 1
# Model 2 has 9 additional degrees of freedom compared to Model 1.
lrtest



did_ethnicity <- lmer(RetailerCount_density ~ year + citylicense + pharmacyban + Total_RacialMinority +
                   Bachelor_higher_1824 + Bachelor_higher_25 + 
                   Percent.below.poverty.level + (1 | City) +
                     year*citylicense + year*pharmacyban + year*Total_RacialMinority +
                   citylicense*Total_RacialMinority + pharmacyban*Total_RacialMinority + 
                     year*citylicense*Total_RacialMinority, data = density_mergefee)
summary(did_ethnicity)
coef_table(did_ethnicity)
write.csv(coef_table(did_ethnicity), file = paste0(path, "ethnicity__.csv"))


did_poverty <- lmer(RetailerCount_density ~ year + citylicense + pharmacyban + Total_RacialMinority +
                        Bachelor_higher_1824 + Bachelor_higher_25 + 
                        Percent.below.poverty.level + (1 | City) +
                       year*citylicense + year*pharmacyban +
                        year*Percent.below.poverty.level +
                        citylicense*Percent.below.poverty.level + pharmacyban*Percent.below.poverty.level + 
                      year*Percent.below.poverty.level*citylicense + year*Percent.below.poverty.level*pharmacyban, data = density_mergefee)
summary(did_poverty)
coef_table(did_poverty)
write.csv(coef_table(did_poverty), file = paste0(path, "poverty__.csv"))


did_education <- lmer(RetailerCount_density ~ year + citylicense + pharmacyban + Total_RacialMinority +
                      Bachelor_higher_1824 + Bachelor_higher_25 + 
                      Percent.below.poverty.level + (1 | City) +
                        year*citylicense + year*pharmacyban +
                      year*Bachelor_higher_1824 + year*Bachelor_higher_25 + 
                      citylicense*Bachelor_higher_1824 + pharmacyban*Bachelor_higher_1824 + 
                      citylicense*Bachelor_higher_25 + pharmacyban*Bachelor_higher_25 + 
                        year*Bachelor_higher_1824*citylicense + year*Bachelor_higher_1824*pharmacyban +
                        year*Bachelor_higher_25*citylicense + year*Bachelor_higher_25*pharmacyban, data = density_mergefee)
summary(did_education)
coef_table(did_education)
write.csv(coef_table(did_education), file = paste0(path, "education__.csv"))





###############################

did_main <- lmer(RetailerCount_density ~ citylicense + pharmacyban + 
                 Hispanic.or.Latino + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + 
                 Native.Hawaiian.and.Other.Pacific.Islander + Some.Other.Race + Two.or.More.Races + 
                 Bachelor_higher_1824 + Bachelor_higher_25 + 
                 Percent.below.poverty.level + 
                 (1 | City), data = density_mergefee)
summary(did_main)


did_ethnicity <- lm(RetailerCount_density ~ citylicense + pharmacyban + 
                 Hispanic.or.Latino + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + 
                 Native.Hawaiian.and.Other.Pacific.Islander + Some.Other.Race + Two.or.More.Races + 
                 Bachelor_higher_1824 + Bachelor_higher_25 + 
                 Percent.below.poverty.level + 
                   citylicense*Hispanic.or.Latino + citylicense*Black.or.African.American + 
                   citylicense*American.Indian.and.Alaska.Native + citylicense*Asian + 
                   citylicense*Native.Hawaiian.and.Other.Pacific.Islander + citylicense*Some.Other.Race + 
                   citylicense*Two.or.More.Races + 
                   pharmacyban*Hispanic.or.Latino + pharmacyban*Black.or.African.American + 
                   pharmacyban*American.Indian.and.Alaska.Native + pharmacyban*Asian + 
                   pharmacyban*Native.Hawaiian.and.Other.Pacific.Islander + pharmacyban*Some.Other.Race + 
                   pharmacyban*Two.or.More.Races + 
                   citylicense*pharmacyban*Hispanic.or.Latino + citylicense*pharmacyban*Black.or.African.American + 
                   citylicense*pharmacyban*American.Indian.and.Alaska.Native + citylicense*pharmacyban*Asian + 
                   citylicense*pharmacyban*Native.Hawaiian.and.Other.Pacific.Islander + citylicense*pharmacyban*Some.Other.Race + 
                   citylicense*pharmacyban*Two.or.More.Races, data = density_mergefee)
summary(did_ethnicity)
did_poverty <- lm(RetailerCount_density ~ citylicense + pharmacyban + 
                 Hispanic.or.Latino + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + 
                 Native.Hawaiian.and.Other.Pacific.Islander + Some.Other.Race + Two.or.More.Races + 
                 Bachelor_higher_1824 + Bachelor_higher_25 + 
                 Percent.below.poverty.level +
                   citylicense*Percent.below.poverty.level + pharmacyban*Percent.below.poverty.level + 
                   citylicense*pharmacyban*Percent.below.poverty.level, data = density_mergefee)
summary(did_poverty)
did_education <- lm(RetailerCount_density ~ citylicense + pharmacyban + 
                 Hispanic.or.Latino + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + 
                 Native.Hawaiian.and.Other.Pacific.Islander + Some.Other.Race + Two.or.More.Races + 
                 Bachelor_higher_1824 + Bachelor_higher_25 + 
                 Percent.below.poverty.level + 
                   citylicense*Bachelor_higher_1824 + citylicense*Bachelor_higher_25 + 
                   pharmacyban*Bachelor_higher_1824 + pharmacyban*Bachelor_higher_25 + 
                   citylicense*pharmacyban*Bachelor_higher_1824 + citylicense*pharmacyban*Bachelor_higher_25 + 
                   citylicense*Bachelor_higher_1824*Bachelor_higher_25 + pharmacyban*Bachelor_higher_1824*Bachelor_higher_25 +
                   citylicense*pharmacyban*Bachelor_higher_1824*Bachelor_higher_25, data = density_mergefee)
summary(did_education)



