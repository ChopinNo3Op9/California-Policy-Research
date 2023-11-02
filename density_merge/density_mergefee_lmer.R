x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr',
          'lme4','lmtest', 'scales','gridExtra')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})

density_mergefee <- read.csv("D:\\Purdue\\research\\density_mergefee.csv")
density_mergefee$RetailerCount_density <- density_mergefee$RetailerCount_density*1000
density_mergefee$year <- as.factor(ifelse(density_mergefee$RetailerYear >= 2016, 1, 0)) # year (before 2016: 0, after 2016: 1)
density_mergefee$City <- as.factor(density_mergefee$City)
density_mergefee$citylicense <- ifelse(density_mergefee$citylicense %in% c(0, 1), "weak",
                                       ifelse(density_mergefee$citylicense %in% c(2, 3), "moderate", "strong"))
density_mergefee$citylicense <- as.factor(density_mergefee$citylicense)
density_mergefee$citylicense <- relevel(density_mergefee$citylicense, ref = "weak")
density_mergefee$pharmacyban <- as.numeric(density_mergefee$pharmacyban)
density_mergefee$pharmacyban <- ifelse(density_mergefee$pharmacyban %in% c(2, 3, 4), 0, density_mergefee$pharmacyban)
density_mergefee$pharmacyban <- as.factor(density_mergefee$pharmacyban)
# density_mergefee <- density_mergefee[density_mergefee$AnnualFee != 0 & !is.na(density_mergefee$AnnualFee), ]
density_mergefee$EnactmentDate[is.na(density_mergefee$EnactmentDate)] <- ""
non_empty_dates <- density_mergefee$EnactmentDate != ""
date_split <- strsplit(density_mergefee$EnactmentDate[non_empty_dates], "/")
year <- sapply(date_split, function(x) as.numeric(x[2]))
# year <- ifelse(year > 80, year + 1900, year + 2000)
density_mergefee$EnactmentYear <- "NA"
density_mergefee$EnactmentYear[non_empty_dates] <- as.character(year)
density_mergefee$EnactmentYear <- as.integer(density_mergefee$EnactmentYear)
density_mergefee$AnnualFee <- ifelse(density_mergefee$EnactmentYear > density_mergefee$RetailerYear, 0, density_mergefee$AnnualFee)
density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)
write.csv(density_mergefee, "D:\\Purdue\\research\\density_mergefee_lmer.csv", row.names = FALSE)


