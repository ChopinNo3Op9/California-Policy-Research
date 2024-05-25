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

data <- na.omit(density_mergefee[,c(14,15,17,19,21,22,29)])

# Subset the data for the year 2015
# df_2015 <- filter(density_mergefee, RetailerYear == 2015)
# df_2021 <- filter(density_mergefee, RetailerYear == 2021)

# Normalize the data
# columns_to_scale <- c("Count", "RetailerYear", "RetailerCount_density", "PopulationYear",
#                       "Black.or.African.American", "American.Indian.and.Alaska.Native", "Asian",
#                       "Native.Hawaiian.and.Other.Pacific.Islander", "Some.Other.Race", "Two.or.More.Races",
#                       "Total_RacialMinority", "Percent.below.poverty.level", "Highschool_higher_1824",
#                       "Bachelor_higher_1824", "Highschool_higher_25", "Bachelor_higher_25",
#                       "smokefree", "citylicense", "pharmacyban", "emerging_license", "retailrestrict",
#                       "flavored", "EnactmentDate", "AnnualFee", "Application.Initial.Fee",
#                       "year", "EnactmentYear")
columns_to_scale <- c("Count", "RetailerCount_density","Black.or.African.American", "American.Indian.and.Alaska.Native", "Asian",
                      "Native.Hawaiian.and.Other.Pacific.Islander", "Some.Other.Race", "Two.or.More.Races",
                      "Total_RacialMinority", "Percent.below.poverty.level", "Highschool_higher_1824",
                      "Bachelor_higher_1824", "Highschool_higher_25", "Bachelor_higher_25",
                      "smokefree", "citylicense", "pharmacyban", "emerging_license", "retailrestrict",
                      "flavored", "EnactmentDate", "AnnualFee", "Application.Initial.Fee")

# Check for any non-numeric columns and remove them from the scaling list
numeric_columns_to_scale <- columns_to_scale[sapply(density_mergefee[columns_to_scale], is.numeric)]

# Scale the numeric data
density_mergefee_scaled <- as.data.frame(scale(density_mergefee[numeric_columns_to_scale]))

# Assign the correct names to the scaled data frame
names(density_mergefee_scaled) <- numeric_columns_to_scale

# Identifying non-scaled columns (those not in numeric_columns_to_scale)
non_scaled_columns <- setdiff(names(density_mergefee), numeric_columns_to_scale)

# Combine scaled data with non-scaled columns
final_data <- cbind(density_mergefee[non_scaled_columns], density_mergefee_scaled)

# Ensure the city names are correctly included if not already in non_scaled_columns
if (!"City" %in% non_scaled_columns) {
  final_data$City <- density_mergefee$City
}

# View the final combined dataset
head(final_data)

# Compute Euclidean distance matrix
# distance_matrix <- as.matrix(dist(df_scaled[columns_to_scale], method = "euclidean"))
# Now compute the distance matrix using only the numeric scaled data
distance_matrix <- as.matrix(dist(final_data[, -which(names(final_data) == "City")], method = "euclidean"))

# Set the names of the distance matrix using the City column
colnames(distance_matrix) <- final_data$City
rownames(distance_matrix) <- final_data$City

# Iterate over the matrix and set entries to Inf where column and row names match
for (i in 1:nrow(distance_matrix)) {
  for (j in 1:ncol(distance_matrix)) {
    if (rownames(distance_matrix)[i] == colnames(distance_matrix)[j]) {
      distance_matrix[i, j] <- Inf
    }
  }
}

# Display the matrix or a portion of it to check
print(distance_matrix[1:5, 1:5])  # Adjust indices as necessary for visibility

# Convert distance matrix to a more readable format
similarity_matrix <- as.data.frame(distance_matrix)
print(similarity_matrix)

# Optionally, find the closest city for each city
# This is done by setting the diagonal to Inf to ignore self-comparisons
closest_cities <- apply(distance_matrix, 1, function(x) names(x)[which.min(x)])
print(closest_cities)

Berkeley <- final_data %>% filter(City == 'Berkeley')
SanFrancisco <- final_data %>% filter(City == 'San Francisco')

# Combine into a single data frame
combined_data <- bind_rows(Berkeley, SanFrancisco)

# Ensure the data has a 'Year' column
# combined_data$Year <- rep(c(2015, 2015, 2021, 2021), each = nrow(combined_data) / 4)

# Create interaction term for DiD
# combined_data$Treatment <- ifelse(combined_data$City == "Beaumont", 1, 0)
# combined_data$Post <- ifelse(combined_data$Year >= 2016, 1, 0)
combined_data$Treatment <- as.integer(combined_data$City == "Berkeley")
combined_data$Post <- as.integer(combined_data$RetailerYear >= 2017)   # before 2017 is 0, 2017 and after is 1
combined_data$TreatmentPost <- combined_data$Treatment*combined_data$Post

# Run DiD regression model
model <- lm(flavored ~ Treatment + Post + TreatmentPost, data = combined_data)
summary(model)
# The coefficient for ‘TreatmentPost’ is the differences-in-differences estimator.
# The effect is significant at 10% with the treatment having a negative effect.

# Estimating the DID estimator (using the multiplication method, no need to generate the interaction)
model <- lm(flavored ~ Treatment*Post, data = combined_data)
summary(model)

ggplot(combined_data, aes(x = RetailerYear, y = RetailerCount_density, color = City, group = City)) +
  geom_line() +
  geom_point() +
  labs(title = "Pre-Treatment Trends in Retailer Count Density",
       x = "RetailerYear", y = "Retailer Count Density")



