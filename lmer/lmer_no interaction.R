x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','sandwich','writexl','stringr','lmtest','margins','effects','MuMIn',
          'lme4','lmtest', 'scales','gridExtra')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})

path <- 'D:\\Purdue\\research\\lmer\\'

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


density_mergefee <- read_excel(paste0(path, "density_mergefee_lmer.xlsx"))
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
                   Percent.below.poverty.level + (1 | City), data = density_mergefee)
summary(did_main)

summary_main <- summary(did_main)
fe_summary <- summary_main$coefficients
fe_df <- as.data.frame(fe_summary)
write.csv(fe_df, "FixedEffectsSummary.csv", row.names = TRUE)

coef_table(did_main)
write.csv(coef_table(did_main), file = paste0(path, "main_nointeraction__.csv"))
# Perform the Adjusted Wald test
adjusted_wald_test <- coeftest(did_main, vcov. = vcovHC(did_main))
print(adjusted_wald_test)

wald_test <- anova(did_main)
print(wald_test)

model <- glmer(RetailerCount_density ~ year + citylicense + pharmacyban + Total_RacialMinority +
                 Bachelor_higher_1824 + Bachelor_higher_25 + 
                 Percent.below.poverty.level + (1 | City), 
               data = density_mergefee, family = quasibinomial())
ame <- margins(model)
summary(ame)

# plot(allEffects(did_main))
effects <- allEffects(did_main)
plot(effects, select = 2)

# Calculate R^2 for mixed models
r.squaredGLMM(did_main)

plot(effect("year:citylicense", did_main, given=list(City="SpecificCity")))


# # List of main effects and their interactions
# effects_list <- c("year", "citylicense", "pharmacyban", "Total_RacialMinority",
#                   "year:citylicense", "year:pharmacyban")
# # Plot effects for each variable and interaction
# for (var in effects_list) {
#   effect_plot <- effect(var, did_main)
#   plot(effect_plot, main=paste("Effect of", var))
# }

# likelihood ratio test
# model with only intercept,
# baseline model that assumes that the response variable has a constant value across all observations
# Fit a null mixed-effects model with the same random effects structure
null_model <- lm(RetailerCount_density ~ 1, data = density_mergefee) 
lrtest <- lrtest(null_model, did_main)
# The test results indicate that Model 2 provides a significantly better fit to the data than Model 1
# Model 2 has 9 additional degrees of freedom compared to Model 1.
lrtest




