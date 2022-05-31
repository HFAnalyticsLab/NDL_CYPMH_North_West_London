library(tidyverse)
library(readxl)
library(data.table)

read_in_denom_data <- function(Path,year,Skip=5){
  
  allps <- paste0("Mid-",year," Persons")
  males <- paste0("Mid-",year," Males")
  females <- paste0("Mid-",year," Females")

  d_all <- read_excel(path = Path, sheet = allps, skip = Skip)
  d_males <- read_excel(path = Path, sheet = males, skip = Skip)
  d_females <- read_excel(path = Path, sheet = females, skip = Skip)
  
  output <- list(
    all = d_all,
    males = d_males,
    females = d_females
  )
  return(output)
}

data_2016 <- read_in_denom_data(Path = "denominators/data/SAPE20DT5-mid-2016-ccg-syoa-estimates-formatted.xls","2016")
data_2017 <- read_in_denom_data(Path = "denominators/data/SAPE20DT5-mid-2017-ccg-syoa-estimates-formatted.xls","2017")
data_2018 <- read_in_denom_data(Path = "denominators/data/SAPE21DT5-mid-2018_on_2019-ccg-syoa-estimates-formatted_corrected.xlsx","2018")
data_2019 <- read_in_denom_data(Path = "denominators/data/SAPE22DT6a-mid-2019-ccg-2020-estimates-unformatted.xlsx","2019")
data_2020 <- read_in_denom_data(Path = "denominators/data/sape23dt6amid2020ccg2021estimatesunformatted.xlsx","2020")

ccg_codes <- c(
  "E38000020" #Brent
  ,"E38000031" #Central London
  ,"E38000048" #Ealing
  ,"E38000070" #H&F
  ,"E38000074" #Harrow
  ,"E38000082" #Hillingdon
  ,"E38000084" #Hounslow
  ,"E38000202" #West London
  ,"E38000256" #NWL
)

summarise_data <- function(data, year,ccgs = ccg_codes){
  
  data <- data.frame(data)
  data <- subset(data, data[,1] %in% ccgs)
  
  ctk <- paste0("X",0:25)
  data <- data[,ctk]
  data <- data.frame(counts = apply(data,2,sum))
  data$year = year
  data$age = as.numeric(str_remove_all(pattern = "X",string = rownames(data)))
  
  return(data)
  
}

data_all <- bind_rows(
  summarise_data(data_2016$all,2016),
  summarise_data(data_2017$all,2017),
  summarise_data(data_2018$all,2018),
  summarise_data(data_2019$all,2019),
  summarise_data(data_2020$all,2020)
)

data_males <- bind_rows(
  summarise_data(data_2016$males,2016),
  summarise_data(data_2017$males,2017),
  summarise_data(data_2018$males,2018),
  summarise_data(data_2019$males,2019),
  summarise_data(data_2020$males,2020)
)

data_female <- bind_rows(
  summarise_data(data_2016$females,2016),
  summarise_data(data_2017$females,2017),
  summarise_data(data_2018$females,2018),
  summarise_data(data_2019$females,2019),
  summarise_data(data_2020$females,2020)
)

data_all <- data_all %>%
  mutate(age_group = case_when(
    age %in% 0:4 ~ "0-4",
    age %in% 5:11 ~ "5-11",
    age %in% 12:17 ~ "12-17",
    age %in% 18:21 ~ "18-21",
    age %in% 22:25 ~ "22-25")) %>%
  group_by(age_group,year) %>%
  summarise(counts = sum(counts))

data_males <- data_males %>% group_by(year) %>% summarise(counts = sum(counts)) %>% mutate(Gender = "Male")
data_females <- data_female %>% group_by(year) %>% summarise(counts = sum(counts)) %>% mutate(Gender = "Female")

fwrite(data_all,"denominators/denominator_ages.csv")
fwrite(data_males,"denominators/denominator_males.csv")
fwrite(data_females,"denominators/denominator_females.csv")

denom_gen <- bind_rows(data_males, data_females)

#Edit new tables

agetab <-  read_excel("dataV2/DataRepository.xlsx",sheet = "o1setge") 

agetab <-  agetab %>% select(Setting,Gender, year,n) %>% 
  left_join(denom_gen, by = c("year","Gender")) %>%
  mutate(prop = as.numeric(n)/counts)

fwrite(agetab,"denominators/o1setagegen.csv")

agetab <-  read_excel("dataV2/DataRepository.xlsx",sheet = "o1setage",col_types = c("guess","text","guess","guess","guess","guess")) 

agetab <-  agetab %>% select(Setting,AgeGroup, year,n) %>% 
  left_join(data_all, by = c("year","AgeGroup" = "age_group")) %>%
  mutate(prop = as.numeric(n)/counts)

fwrite(agetab,"denominators/o1setage.csv")




