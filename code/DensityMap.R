x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr',
          'sf','gridExtra','choroplethr','choroplethrMaps','gridExtra','ggmap','grid')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


# density_mergefee <- read.csv("D:\\Purdue\\research\\density_mergefee.csv")
density_mergefee <- read.csv("D:\\Purdue\\research\\lmer\\density_mergefee_lmer_mile.csv")
density_mergefee$RetailerCount_density <- density_mergefee$RetailerCount_density*1000
density_mergefee <- density_mergefee[density_mergefee$AnnualFee != 0 & !is.na(density_mergefee$AnnualFee), ]
density_mergefee$year <- as.factor(ifelse(density_mergefee$RetailerYear >= 2016, 1, 0))
density_mergefee$City <- as.factor(density_mergefee$City)
density_mergefee$citylicense <- ifelse(density_mergefee$citylicense %in% c(0, 1), "weak",
                                       ifelse(density_mergefee$citylicense %in% c(2, 3), "moderate", "strong"))
density_mergefee$citylicense <- as.factor(density_mergefee$citylicense)
density_mergefee$citylicense <- relevel(density_mergefee$citylicense, ref = "weak")
density_mergefee$pharmacyban <- as.numeric(density_mergefee$pharmacyban)
density_mergefee$pharmacyban <- ifelse(density_mergefee$pharmacyban %in% c(2, 3, 4), 0, density_mergefee$pharmacyban)
density_mergefee$pharmacyban <- as.factor(density_mergefee$pharmacyban)
density_mergefee$County <- tolower(density_mergefee$County)
data(county.regions)

# Aggregating data in 'county.regions' if appropriate
county.regions <- county.regions %>%
  group_by(county.name) %>%
  summarize(region = first(region))  # or use another suitable aggregation function

# Perform the join after aggregation
# density_mergefee <- left_join(density_mergefee, county.regions, by = c("County" = "county.name"))
# Use the 'relationship' argument in left_join to handle many-to-many joins
density_mergefee <- left_join(density_mergefee, county.regions, by = c("County" = "county.name"), relationship = "many-to-many")



# # all rows from density_mergefee are joined
# density_mergefee <- left_join(density_mergefee,  
#                               # select(county.regions, county.name, region), 
#                               county.regions, 
#                               by = c("County" = "county.name"))


# county_choropleth_by_year <- function(year) {
#   density_mergefee %>%
#     filter(RetailerYear == year) %>%
#     group_by(region) %>%
#     summarize(value = sum(RetailerCount_density)) %>%
#     county_choropleth(title = paste("Retailer Count Density in California, ", year),
#                       legend = "Retailer Count Density",
#                       num_colors = 1,
#                       state_zoom = "california")
# }
# county_choropleth_by_year(2017)

years <- c(2012:2021)
plots <- list()
breaks <- seq(0, 6000, 1000) # Set the breaks for the legend
for (i in 1:length(years)) {
  county_sum <- density_mergefee %>%
    filter(RetailerYear == years[i]) %>%
    group_by(region) %>%
    summarize(value = sum(RetailerCount_density))
  
  plot <- county_choropleth(county_sum, 
                            title = paste("Density in ", years[i]),
                            legend = "Retailer Count Density",
                            num_colors = 1,
                            state_zoom = "california")
  plots[[i]] <- plot
}
grid.arrange(grobs = plots, ncol = 5, 
             top = textGrob("Retailer Count Mile Density in each County in California by Year", gp = gpar(fontsize = 16)))


