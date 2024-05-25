x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr',
          'lme4','lmtest', 'scales','gridExtra', 'did')
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


density_mergefee <- read_excel( "..//lmer//density_mergefee_lmer.xlsx")
# density_mergefee <- read.csv("..//lmer//density_mergefee_lmer_mile.csv")
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
# density_mergefee$year <- as.factor(ifelse(density_mergefee$RetailerYear >= 2017, 1, 0)) # year (before 2016: 0, after 2016: 1)
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
# density_mergefee$RetailerYear <- as.factor(density_mergefee$RetailerYear)
density_mergefee$RetailerCount_density <- as.numeric(density_mergefee$RetailerCount_density)

summary(density_mergefee[,c(6,14,15,17,19,21,22,29)])

# data <- na.omit(density_mergefee[,c(14,15,17,19,21,22,29)])

# Processing the dataframe
density_mergefee <- density_mergefee %>%
  mutate(flavored = ifelse(is.na(flavored), 0, flavored)) %>%  # Replace NA with 0 in 'flavored'
  group_by(City) %>%
  mutate(
    first_flavor = cumsum(flavored == 1),  # Cumulative sum to identify the first occurrence of 'flavored' being 1
    first_flavor_year = ifelse(first_flavor == 1 & flavored == 1, RetailerYear, NA_real_)  # Capture 'RetailerYear' at first 'flavored' == 1
  ) %>%
  mutate(
    FlavorYear = coalesce(first_flavor_year[match(1, first_flavor)], RetailerYear)  # Propagate the first non-NA 'first_flavor_year'
  )

test_a <- subset(density_mergefee, City == "Albany")
test_b <- subset(density_mergefee, City == "Berkeley")
test_c <- subset(density_mergefee, City == "San Francisco")

table(density_mergefee$flavored[density_mergefee$City == "Albany"])
table(density_mergefee$flavored[density_mergefee$City == "Berkeley"])
table(density_mergefee$flavored[density_mergefee$City == "San Francisco"])

# estimate group-time average treatment effects without covariates
mw.attgt <- att_gt(yname = "RetailerCount_density",
                   gname = "FlavorYear",
                   idname = "City",
                   tname = "RetailerYear",
                   xformla = ~1,
                   data = density_mergefee,
)



