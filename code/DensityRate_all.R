x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','writexl','stringr','scales','progress')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


pop2010 <- read.csv("D:\\Purdue\\research\\pop2010.csv")
pop2020 <- read.csv("D:\\Purdue\\research\\pop2020.csv")
poverty2012 <- read.csv("D:\\Purdue\\research\\poverty2012.csv")
poverty2017 <- read.csv("D:\\Purdue\\research\\poverty2017.csv")
education2012 <- read.csv("D:\\Purdue\\research\\education2012.csv")
education2017 <- read.csv("D:\\Purdue\\research\\education2017.csv")

mergefee <- read.csv("D:\\Purdue\\research\\mergefee.csv")
mergefee$bestzip <- as.character(mergefee$bestzip)
mergefee <- mergefee %>%
  filter(!grepl("CDP", cityname))
adhoc <- read_excel("D:\\Purdue\\research\\AD HOC 102 - PRA Request - Cigarette Licenses 1q11 - 1q23 - MASTER.xlsx")
adhoc <- adhoc[adhoc$QUARTER == 4,]
adhoc <- adhoc[adhoc$YEAR != 2011,-c(2,4:7)]
sum(adhoc[adhoc$YEAR == 2012,]$Final_count)
d <- merge(x = adhoc, y = unique(mergefee[, c(1:3)]), by.x = "ZIP", by.y = "bestzip")
# d <- adhoc %>% inner_join(mergefee, by = c("ZIP" = "bestzip"))
d <- d[!duplicated(d),]
sum(d[d$cityname == 'Laguna Hills' & d$YEAR == 2012,]$Final_count)
sum(d[d$YEAR == 2012,]$Final_count)
den <- d %>%
  dplyr::group_by(cityname, countyname, YEAR) %>%
  dplyr::summarize(Count = sum(Final_count))
den <- den[,c(1,2,4,3)]
colnames(den) <- c("City","County","Count","RetailerYear")
sum(den[den$RetailerYear == 2012,]$Count)
sum(den[den$RetailerYear == 2016,]$Count)
# den$CDP <- grepl("CDP", den$City)
# den$City <- gsub(" CDP, CA", "", den$City)
density_1216 <- den[den$RetailerYear %in% c(2012:2016),]
density_1719 <- den[den$RetailerYear %in% c(2017:2019),]
density_2022 <- den[den$RetailerYear %in% c(2020:2022),]
density_20 <- den[den$RetailerYear == 2020,]
# density_22 <- den[den$RetailerYear == 2022,]



# add population, poverty, education to density_1216, density_1719, density_2022
percentage_merge <- function(density_year, pop_year, poverty_year, education_year, filename) {
  density_year$Population <- NA
  density_year$RetailerCount_density <- NA
  density_year$PopulationYear <- NA
  density_year$Hispanic.or.Latino <- NA
  density_year$Black.or.African.American <- NA
  density_year$American.Indian.and.Alaska.Native <- NA
  density_year$Asian <- NA
  density_year$Native.Hawaiian.and.Other.Pacific.Islander <- NA
  density_year$Some.Other.Race <- NA
  density_year$Two.or.More.Races <- NA
  density_year$Total_RacialMinority <- NA
  multicity_index <- c()
  for (i in 1:nrow(density_year)){
    if (density_year[i,]$City %in% pop_year$Geographic.Area.Name) {
      density_year[i,]$Population <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Total[1]
      density_year[i,]$RetailerCount_density <- density_year[i,]$Count / density_year[i,]$Population[1]
      density_year[i,]$PopulationYear <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Year[1]
      density_year[i,]$Hispanic.or.Latino <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Hispanic.or.Latino[1]
      density_year[i,]$Black.or.African.American <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Black.or.African.American[1]
      density_year[i,]$American.Indian.and.Alaska.Native <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$American.Indian.and.Alaska.Native[1]
      density_year[i,]$Asian <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Asian[1]
      density_year[i,]$Native.Hawaiian.and.Other.Pacific.Islander <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Native.Hawaiian.and.Other.Pacific.Islander[1]
      density_year[i,]$Some.Other.Race <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Some.Other.Race[1]
      density_year[i,]$Two.or.More.Races <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Two.or.More.Races[1]
      density_year[i,]$Total_RacialMinority <- pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Total_RacialMinority[1]
      # density_year[i,]$Total_RacialMinority <- percent(pop_year[which(pop_year$Geographic.Area.Name == density_year[i,]$City),]$Total_RacialMinority)[1]
    } else {
      density_year[i,]$Population <- NA
      density_year[i,]$RetailerCount_density <- NA
      density_year[i,]$PopulationYear <- NA
      density_year[i,]$Hispanic.or.Latino <- NA
      density_year[i,]$Black.or.African.American <- NA
      density_year[i,]$American.Indian.and.Alaska.Native <- NA
      density_year[i,]$Asian <- NA
      density_year[i,]$Native.Hawaiian.and.Other.Pacific.Islander <- NA
      density_year[i,]$Some.Other.Race <- NA
      density_year[i,]$Two.or.More.Races <- NA
      density_year[i,]$Total_RacialMinority <- NA
      
      multicity_index <- append(multicity_index, i)
    }
  }
  density_year[multicity_index, ]$City
  
  
  density_year$Percent.below.poverty.level <- NA
  multicity_index <- c()
  for (i in 1:nrow(density_year)){
    if (density_year[i,]$City %in% pop_year$Geographic.Area.Name) {
      density_year[i,]$Percent.below.poverty.level <- poverty_year[which(poverty_year$Geographic.Area.Name == density_year[i,]$City),]$Percent.below.poverty.level[1]
    } else {
      density_year[i,]$Percent.below.poverty.level <- NA
      multicity_index <- append(multicity_index, i)
    }
  }
  density_year[multicity_index, ]$City
  
  
  density_year$Highschool_higher_1824 <- NA
  density_year$Bachelor_higher_1824 <- NA
  density_year$Highschool_higher_25 <- NA
  density_year$Bachelor_higher_25 <- NA
  multicity_index <- c()
  for (i in 1:nrow(density_year)){
    if (density_year[i,]$City %in% pop_year$Geographic.Area.Name) {
      density_year[i,]$Highschool_higher_1824 <- education_year[which(density_year[i,]$City == education_year$Geographic.Area.Name),]$Highschool_higher_1824[1]
      density_year[i,]$Bachelor_higher_1824 <- education_year[which(density_year[i,]$City == education_year$Geographic.Area.Name),]$Bachelor_higher_1824[1]
      density_year[i,]$Highschool_higher_25 <- education_year[which(density_year[i,]$City == education_year$Geographic.Area.Name),]$Highschool_higher_25[1]
      density_year[i,]$Bachelor_higher_25 <- education_year[which(density_year[i,]$City == education_year$Geographic.Area.Name),]$Bachelor_higher_25[1]
    } else {
      density_year[i,]$Highschool_higher_1824 <- NA
      density_year[i,]$Bachelor_higher_1824 <- NA
      density_year[i,]$Highschool_higher_25 <- NA
      density_year[i,]$Bachelor_higher_25 <- NA
      multicity_index <- append(multicity_index, i)
    }
  }
  density_year[multicity_index, ]$City
  
  # deparse(substitute(density_year))
  write.csv(density_year, paste0("D:\\Purdue\\research\\", filename, ".csv"), row.names = FALSE)
  filename <- read.csv(paste0("D:\\Purdue\\research\\", filename, ".csv"))
  return(density_year)
}
percentage_merge(density_1216, pop2010, poverty2012, education2012, "density_1216")
percentage_merge(density_1719, pop2010, poverty2017, education2017, "density_1719")
percentage_merge(density_2022, pop2020, poverty2017, education2017, "density_2022")
density_1216 <- read.csv(paste0("D:\\Purdue\\research\\", "density_1216", ".csv"))
density_1719 <- read.csv(paste0("D:\\Purdue\\research\\", "density_1719", ".csv"))
density_2022 <- read.csv(paste0("D:\\Purdue\\research\\", "density_2022", ".csv"))


# CDP last step: sum avg
# density_year$Black.or.African.American <- as.numeric(sub("%", "", density_year$Black.or.African.American, fixed=TRUE))/100
unincorporated_merge<- function(density_year, filename) {
  density_year[,16:21] <- apply(density_year[,16:21],2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  density_year <- na.omit(density_year)  # renive CDP with NA so that mean and sum of other will be calculated
  unincorporated <- density_year[density_year$CDP == TRUE,] %>% group_by(County, RetailerYear, PopulationYear) %>% 
    summarize(Count = sum(Count), Population = sum(Population), 
              Hispanic.or.Latino = mean(Hispanic.or.Latino),
              American.Indian.and.Alaska.Native = mean(American.Indian.and.Alaska.Native),
              Black.or.African.American = mean(Black.or.African.American),
              American.Indian.and.Alaska.Native = mean(American.Indian.and.Alaska.Native),
              Asian = mean(Asian), 
              Native.Hawaiian.and.Other.Pacific.Islander = mean(Native.Hawaiian.and.Other.Pacific.Islander),
              Some.Other.Race = mean(Some.Other.Race), 
              Two.or.More.Races = mean(Two.or.More.Races),
              Percent.below.poverty.level = mean(Percent.below.poverty.level), 
              Highschool_higher_1824 = mean(Highschool_higher_1824),
              Bachelor_higher_1824 = mean(Bachelor_higher_1824), Highschool_higher_25 = mean(Highschool_higher_25),
              Bachelor_higher_25 = mean(Bachelor_higher_25))
  unincorporated$CDP <- TRUE
  unincorporated$RetailerCount_density <- unincorporated$Count / unincorporated$Population
  unincorporated$Total_RacialMinority <- apply(unincorporated[,10:15],1, sum)
  unincorporated$City <- paste(unincorporated$County, "Unincorporated")
  unincorporated <- unincorporated[,c(21,1,4,2,5,19,3,6,8,7,9,10,11,12,20,13,14,15,16,17,18)]
  density_year <- rbind(density_year[density_year$CDP == FALSE,], unincorporated)
  write.csv(density_year, paste0("D:\\Purdue\\research\\", filename, ".csv"), row.names = FALSE)
  filename <- read.csv(paste0("D:\\Purdue\\research\\", filename, ".csv"))
  return(density_year)
}
density_1216 <- unincorporated_merge(density_1216, "density_1216unincorporated")
density_1719 <- unincorporated_merge(density_1719, "density_1719unincorporated")
density_2022 <- unincorporated_merge(density_2022, "density_2022unincorporated")

sum(density_2022[density_2022$CDP == TRUE & density_2022$County == 'Alameda' & 
                   density_2022$RetailerYear == 2021 & density_2022$PopulationYear == 2020,]$Count)



# combine all
density_1222 <- do.call("rbind", list(density_1216, density_1719, density_2022))
density_1222 <- density_1222[,-c(5,8)]
sum(density_1222[density_1222$RetailerYear == 2012,]$Count)
sum(density_1222[density_1222$RetailerYear == 2016,]$Count)
write.csv(density_1222, "D:\\Purdue\\research\\density_1222.csv", row.names = FALSE)
density_1222 <- read.csv("D:\\Purdue\\research\\density_1222.csv")

# to find the missing city in 2019: Amador City
density_1222_2019 <- density_1222[density_1222$RetailerYear == 2019,1]
density_1222_2020 <- density_1222[density_1222$RetailerYear == 2020,1]
for (i in 1:length(unique(density_1222_2019))) {
  if (! density_1222_2020[i] %in% density_1222_2019) {
    print(density_1222_2020[i])
  }
}


