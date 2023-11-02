x <- list('magrittr','ggplot2','dplyr','readxl','arrow','plyr','tidyverse','qdap','rdrr','writexl','stringr','scales')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


mergefee <- read.csv("D:\\Purdue\\research\\mergefee.csv")

retailer <- read_excel("D:\\Purdue\\research\\Licensed Tobacco Retailers-CA-Statewide-updated.xlsx")
retailer <- retailer[-(1:4), ]
colnames(retailer) <- retailer[1,]
retailer <- retailer[-1,]
retailer <- retailer[-1,-c(8:19)]
retailer$Zip <- gsub('.{5}$', '', retailer$Zip)
retailer$City <- tolower(retailer$City)
retailer$City <- str_to_title(retailer$City)


# density <- retailer %>%    # gropu and match by zip code
#   group_by(Zip,City) %>%
#   dplyr::summarize(count = n())
#
# d <- merge(x = density, y = mergefee[,c(1:5)], by.x = "Zip", by.y = "bestzip", all.x=TRUE) # total merge with diff year
#
# density$County <- NA
# density$Population <- NA
# density$Density <- NA
# for (i in 1:nrow(density)){
#   if (density[i,]$Zip %in% mergefee$bestzip) {
#     density[i,]$County <- list(unique(mergefee[which(mergefee$bestzip == density[i,]$Zip),]$countyname))
#     if (unique(mergefee[which(mergefee$bestzip == density[i,]$Zip),]$pop) == 1){
#       density[i,]$Population <- unique(mergefee[which(mergefee$bestzip == density[i,]$Zip),]$pop)
#     }
#     else {
#       density[i,]$Population <- sum(unique(mergefee[which(mergefee$bestzip == density[i,]$Zip),]$pop))
#     }
#     density[i,]$Density <- density[i,]$count/density[i,]$Population
#   }
# }


density <- retailer %>%    # group and match by city
  group_by(City) %>%
  dplyr::summarize(count = n())

density$County <- NA
density$Population <- NA
density$Density <- NA
multicounty_index <- c()
for (i in 1:nrow(density)){
  if (any(mergefee$cityname == density[i,]$City)) {  # exactly same
    if (length(unique(mergefee[which(mergefee$cityname == density[i,]$City),]$countyname)) == 1) {
      density[i,]$County <- unique(mergefee[which(mergefee$cityname == density[i,]$City),]$countyname)
    } else {
      density[i,]$County <- unique(mergefee[which(mergefee$cityname == density[i,]$City),]$countyname)[1]
      multicounty_index <- append(multicounty_index, i)
    }
  } else if (any(grepl(density[i,]$City, mergefee$cityname))) { # in the string
    if (length(unique(mergefee[which(grepl(density[i,]$City, mergefee$cityname)),]$countyname)) == 1) {
      density[i,]$County <- unique(mergefee[which(grepl(density[i,]$City, mergefee$cityname)),]$countyname)
    } else {
      density[i,]$County <- unique(mergefee[which(grepl(density[i,]$City, mergefee$cityname)),]$countyname)[1]
      multicounty_index <- append(multicounty_index, i)
    }
  } else {
    density[i,]$County <- NA
  }
  # density[i,]$Density <- density[i,]$count/density[i,]$Population
}
density[multicounty_index, ]$City  # city name with multi county name, record the 1st by default

density$RetailerYear <- 2023
density <- density[, c("City","County","count","Population","Density","RetailerYear")]
colnames(density)[3] <- "Count"
colnames(density)[5] <- "RetailerCount_density"
write.csv(density, "D:\\Purdue\\research\\density.csv", row.names = FALSE)
density <- read.csv("D:\\Purdue\\research\\density.csv")



# pop2010 <- read_excel("D:\\Purdue\\research\\pop2010.xls")
# pop2010 <- pop2010[,c(1,2,3,10,5,6,7,8,9,4)]
# colnames(pop2010)[3:10] <- c("Total","Hispanic or Latino","Black or African American","American Indian and Alaska Native","Asian",
#                             "Native Hawaiian and Other Pacific Islander","Some Other Race","Two or More Races")
# pop2010$Total <- gsub("\\(.*?)", "", pop2010$Total)
# pop2010$`Geographic Area Name` <- gsub(", California", "", pop2010$`Geographic Area Name`)
# pop2010$`Geographic Area Name` <- gsub("city", "", pop2010$`Geographic Area Name`)
# pop2010$`Geographic Area Name` <- gsub("CDP", "", pop2010$`Geographic Area Name`)
# pop2010$`Geographic Area Name` <- gsub("[[:space:]]*$","", pop2010$`Geographic Area Name`)
# pop2010 <- pop2010 %>% mutate_at(c("Total","Hispanic or Latino","Black or African American","American Indian and Alaska Native",
#                                    "Asian","Native Hawaiian and Other Pacific Islander","Some Other Race",
#                                    "Two or More Races"), as.numeric)
# pop2010 <- pop2010[!duplicated(pop2010), ]
# pop2010[,5:10] <- pop2010[,5:10]/100
# pop2010[,5:10] <- lapply(pop2010[,5:10], round, 3)
# pop2010$Year <- 2010
# pop2010$Total_RacialMinority <- apply(pop2010[,5:10], 1, sum)
# write.csv(pop2010, "D:\\Purdue\\research\\pop2010.csv", row.names = FALSE)
pop2010 <- read.csv("D:\\Purdue\\research\\pop2010.csv")


# pop2020 <- read_excel("D:\\Purdue\\research\\pop2020.xls")
# pop2020 <- pop2020[,c(1,2,3,10,5,6,7,8,9,4)]
# colnames(pop2020)[3:10] <- c("Total","Hispanic or Latino","Black or African American","American Indian and Alaska Native","Asian",
#                              "Native Hawaiian and Other Pacific Islander","Some Other Race","Two or More Races")
# pop2020$Total <- gsub("\\(.*?)", "", pop2020$Total)
# pop2020$`Geographic Area Name` <- gsub(", California", "", pop2020$`Geographic Area Name`)
# pop2020$`Geographic Area Name` <- gsub("city", "", pop2020$`Geographic Area Name`)
# pop2020$`Geographic Area Name` <- gsub("CDP", "", pop2020$`Geographic Area Name`)
# pop2020$`Geographic Area Name` <- gsub("[[:space:]]*$","", pop2020$`Geographic Area Name`)
# pop2020 <- pop2020 %>% mutate_at(c("Total","Hispanic or Latino","Black or African American","American Indian and Alaska Native",
#                                    "Asian","Native Hawaiian and Other Pacific Islander","Some Other Race",
#                                    "Two or More Races"), as.numeric)
# pop2020 <- pop2020[!duplicated(pop2020), ]
# pop2020[,5:10] <- pop2020[,5:10]/100
# pop2020$Year <- 2020
# pop2020$Total_RacialMinority <- apply(pop2020[,5:10], 1, sum)
# write.csv(pop2020, "D:\\Purdue\\research\\pop2020.csv", row.names = FALSE)
pop2020 <- read.csv("D:\\Purdue\\research\\pop2020.csv")


density$PopulationYear <- NA
density$Black.or.African.American <- NA
density$American.Indian.and.Alaska.Native <- NA
density$Asian <- NA
density$Native.Hawaiian.and.Other.Pacific.Islander <- NA
density$Some.Other.Race <- NA
density$Two.or.More.Races <- NA
density$Total_RacialMinority <- NA
multicity_index <- c()
for (i in 1:nrow(density)){
  # if (any(grepl(density[i,]$City, pop2010$Geographic.Area.Name))) { # in the string
  #   if (length(pop2010[which(grepl(density[i,]$City, pop2010$Geographic.Area.Name)),]$Geographic.Area.Name) == 1) {
  if (density[i,]$City %in% pop2010$Geographic.Area.Name) {
    density[i,]$Population <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Total[1]
    density[i,]$RetailerCount_density <- density[i,]$Count / density[i,]$Population[1]
    density[i,]$PopulationYear <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Year[1]
    density[i,]$Black.or.African.American <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Black.or.African.American[1]
    density[i,]$American.Indian.and.Alaska.Native <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$American.Indian.and.Alaska.Native[1]
    density[i,]$Asian <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Asian[1]
    density[i,]$Native.Hawaiian.and.Other.Pacific.Islander <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Native.Hawaiian.and.Other.Pacific.Islander[1]
    density[i,]$Some.Other.Race <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Some.Other.Race[1]
    density[i,]$Two.or.More.Races <- pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Two.or.More.Races[1]
    density[i,]$Total_RacialMinority <- percent(pop2010[which(pop2010$Geographic.Area.Name == density[i,]$City),]$Total_RacialMinority)[1]
  } else {
    density[i,]$Population <- NA
    density[i,]$RetailerCount_density <- NA
    density[i,]$PopulationYear <- NA
    density[i,]$Black.or.African.American <- NA
    density[i,]$American.Indian.and.Alaska.Native <- NA
    density[i,]$Asian <- NA
    density[i,]$Native.Hawaiian.and.Other.Pacific.Islander <- NA
    density[i,]$Some.Other.Race <- NA
    density[i,]$Two.or.More.Races <- NA
    density[i,]$Total_RacialMinority <- NA
    
    multicity_index <- append(multicity_index, i)
  }
}
  
density[multicity_index, ]$City  # city name with NA as population info



# poverty2012 <- read.csv("D:\\Purdue\\research\\Povertydata 2012.csv")
# colnames(poverty2012) <- poverty2012[1,]
# poverty2012 <- poverty2012[-1,]
# colnames(poverty2012)[3:4] <- c("Below poverty level","Percent below poverty level")
# poverty2012$`Geographic Area Name` <- gsub(", California", "", poverty2012$`Geographic Area Name`)
# poverty2012$`Geographic Area Name` <- gsub("city", "", poverty2012$`Geographic Area Name`)
# poverty2012$`Geographic Area Name` <- gsub("CDP", "", poverty2012$`Geographic Area Name`)
# poverty2012$`Geographic Area Name` <- gsub("[[:space:]]*$","", poverty2012$`Geographic Area Name`)
# poverty2012 <- poverty2012 %>% mutate_at(c("Below poverty level","Percent below poverty level"), as.numeric)
# poverty2012 <- poverty2012[!duplicated(poverty2012), ]
# poverty2012$`Percent below poverty level` <- paste0(poverty2012$`Percent below poverty level`, "%")
# write.csv(poverty2012, "D:\\Purdue\\research\\poverty2012.csv", row.names = FALSE)
poverty2012 <- read.csv("D:\\Purdue\\research\\poverty2012.csv")

# poverty2017 <- read.csv("D:\\Purdue\\research\\Povertydata 2017.csv")
# colnames(poverty2017) <- poverty2017[1,]
# poverty2017 <- poverty2017[-1,]
# colnames(poverty2017)[3:4] <- c("Below poverty level","Percent below poverty level")
# poverty2017$`Geographic Area Name` <- gsub(", California", "", poverty2017$`Geographic Area Name`)
# poverty2017$`Geographic Area Name` <- gsub("city", "", poverty2017$`Geographic Area Name`)
# poverty2017$`Geographic Area Name` <- gsub("CDP", "", poverty2017$`Geographic Area Name`)
# poverty2017$`Geographic Area Name` <- gsub("[[:space:]]*$","", poverty2017$`Geographic Area Name`)
# poverty2017 <- poverty2017 %>% mutate_at(c("Below poverty level","Percent below poverty level"), as.numeric)
# poverty2017 <- poverty2017[!duplicated(poverty2017), ]
# poverty2017$`Percent below poverty level` <- paste0(poverty2017$`Percent below poverty level`, "%")
# write.csv(poverty2017, "D:\\Purdue\\research\\poverty2017.csv", row.names = FALSE)
poverty2017 <- read.csv("D:\\Purdue\\research\\poverty2017.csv")



density$Percent.below.poverty.level <- NA
multicity_index <- c()
for (i in 1:nrow(density)){
  if (density[i,]$City %in% pop2010$Geographic.Area.Name) {
    density[i,]$Percent.below.poverty.level <- poverty2017[which(poverty2017$Geographic.Area.Name == density[i,]$City),]$Percent.below.poverty.level[1]
  } else {
    density[i,]$Percent.below.poverty.level <- NA
    multicity_index <- append(multicity_index, i)
  }
}
density[multicity_index, ]$City  # city name with NA as population info



# education2012 <- read.csv("D:\\Purdue\\research\\Educationdata 2012.csv")
# colnames(education2012) <- education2012[1,]
# education2012 <- education2012[-1,]
# colnames(education2012)[3:12] <- c("Population 18 to 24 years","High school graduate (includes equivalency)",
#                                    "Some college or associate's degree","Bachelor's degree or higher",
#                                    "Population 25 years and over","High school graduate (includes equivalency)",
#                                    "Some college, no degree","Associate's degree","Bachelor's degree",
#                                    "Graduate or professional degree")
# education2012$`Geographic Area Name` <- gsub(", California", "", education2012$`Geographic Area Name`)
# education2012$`Geographic Area Name` <- gsub("city", "", education2012$`Geographic Area Name`)
# education2012$`Geographic Area Name` <- gsub("CDP", "", education2012$`Geographic Area Name`)
# education2012$`Geographic Area Name` <- gsub("[[:space:]]*$","", education2012$`Geographic Area Name`)
# education2012[education2012 == "-"] <- NA
# education2012[3:12] <- sapply(education2012[3:12],as.numeric)
# education2012 <- education2012[!duplicated(education2012), ]
# education2012[,c(4:6,8:12)] <- apply(education2012[,c(4:6,8:12)], 2, function(x) x/100)
# education2012$Highschool_higher_1824 <- percent(education2012[,4]+education2012[,5]+education2012[,6])
# education2012$Bachelor_higher_1824 <- percent(education2012[,6])
# education2012$Highschool_higher_25 <- percent(education2012[,8]+education2012[,9]+education2012[,10]+education2012[,11]+education2012[,12])
# education2012$Bachelor_higher_25 <- percent(education2012[,11]+education2012[,12])
# write.csv(education2012, "D:\\Purdue\\research\\education2012.csv", row.names = FALSE)
education2012 <- read.csv("D:\\Purdue\\research\\education2012.csv")


# education2017 <- read.csv("D:\\Purdue\\research\\Educationdata 2017.csv")
# colnames(education2017) <- education2017[1,]
# education2017 <- education2017[-1,]
# colnames(education2017)[3:12] <- c("Population 18 to 24 years","High school graduate (includes equivalency)",
#                                    "Some college or associate's degree","Bachelor's degree or higher",
#                                    "Population 25 years and over","High school graduate (includes equivalency)",
#                                    "Some college, no degree","Associate's degree","Bachelor's degree",
#                                    "Graduate or professional degree")
# education2017$`Geographic Area Name` <- gsub(", California", "", education2017$`Geographic Area Name`)
# education2017$`Geographic Area Name` <- gsub("city", "", education2017$`Geographic Area Name`)
# education2017$`Geographic Area Name` <- gsub("CDP", "", education2017$`Geographic Area Name`)
# education2017$`Geographic Area Name` <- gsub("[[:space:]]*$","", education2017$`Geographic Area Name`)
# education2017[education2017 == "-"] <- NA
# education2017[3:12] <- sapply(education2017[3:12],as.numeric)
# education2017 <- education2017[!duplicated(education2017), ]
# education2017[,c(4:6,8:12)] <- apply(education2017[,c(4:6,8:12)], 2, function(x) x/100)
# education2017$Highschool_higher_1824 <- percent(education2017[,4]+education2017[,5]+education2017[,6])
# education2017$Bachelor_higher_1824 <- percent(education2017[,6])
# education2017$Highschool_higher_25 <- percent(education2017[,8]+education2017[,9]+education2017[,10]+education2017[,11]+education2017[,12])
# education2017$Bachelor_higher_25 <- percent(education2017[,11]+education2017[,12])
# write.csv(education2017, "D:\\Purdue\\research\\education2017.csv", row.names = FALSE)
education2017 <- read.csv("D:\\Purdue\\research\\education2017.csv")



density$Highschool_higher_1824 <- NA
density$Bachelor_higher_1824 <- NA
density$Highschool_higher_25 <- NA
density$Bachelor_higher_25 <- NA
multicity_index <- c()
for (i in 1:nrow(density)){
  if (density[i,]$City %in% pop2010$Geographic.Area.Name) {
    density[i,]$Highschool_higher_1824 <- education2017[which(density[i,]$City == education2017$Geographic.Area.Name),]$Highschool_higher_1824[1]
    density[i,]$Bachelor_higher_1824 <- education2017[which(density[i,]$City == education2017$Geographic.Area.Name),]$Bachelor_higher_1824[1]
    density[i,]$Highschool_higher_25 <- education2017[which(density[i,]$City == education2017$Geographic.Area.Name),]$Highschool_higher_25[1]
    density[i,]$Bachelor_higher_25 <- education2017[which(density[i,]$City == education2017$Geographic.Area.Name),]$Bachelor_higher_25[1]
  } else {
    density[i,]$Highschool_higher_1824 <- NA
    density[i,]$Bachelor_higher_1824 <- NA
    density[i,]$Highschool_higher_25 <- NA
    density[i,]$Bachelor_higher_25 <- NA
    multicity_index <- append(multicity_index, i)
  }
}
density[multicity_index, ]$City  # city name with NA as population info


write.csv(density, "D:\\Purdue\\research\\density.csv", row.names = FALSE)
density <- read.csv("D:\\Purdue\\research\\density.csv")

