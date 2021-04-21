library(readr)
library(dplyr)
library(tidyr)

#80+
Cumulative_vaccination_percent_among_80_population <- read_csv("machine_readable_outputs/figure_csvs/Cumulative  vaccination percent among 80+ population by ethnicity 6 groups_tpp.csv")
#70-79
Cumulative_vaccination_percent_among_70_79_population <- read_csv("machine_readable_outputs/figure_csvs/Cumulative  vaccination percent among 70-79 population by ethnicity 6 groups_tpp.csv")
#65-69
Cumulative_vaccination_percent_among_65_69_population<- read_csv("machine_readable_outputs/figure_csvs/Cumulative  vaccination percent among 65-69 population by ethnicity 6 groups_tpp.csv")
#60-64
Cumulative_vaccination_percent_among_60_64_population <- read_csv("machine_readable_outputs/figure_csvs/Cumulative  vaccination percent among 60-64 population by ethnicity 6 groups_tpp.csv")
#55-59
Cumulative_vaccination_percent_among_55_59_population <- read_csv("machine_readable_outputs/figure_csvs/Cumulative  vaccination percent among 55-59 population by ethnicity 6 groups_tpp.csv")
#50-54
Cumulative_vaccination_percent_among_50_54_population <- read_csv("machine_readable_outputs/figure_csvs/Cumulative  vaccination percent among 50-54 population by ethnicity 6 groups_tpp.csv")

#latest date - 2021-04-14
population_80 <-Cumulative_vaccination_percent_among_80_population %>% filter(covid_vacc_date == "2021-04-14") %>% mutate(Age_group ="80+")
population_70_79 <-Cumulative_vaccination_percent_among_70_79_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="70-79")
population_65_69 <-Cumulative_vaccination_percent_among_65_69_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="65-69")
population_60_64 <-Cumulative_vaccination_percent_among_60_64_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="60-64")
population_55_59 <-Cumulative_vaccination_percent_among_55_59_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="55-59")
population_50_54 <- Cumulative_vaccination_percent_among_50_54_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="50-54")

#Combine data
all_data <- rbind(population_80, population_70_79,population_65_69,population_60_64,population_55_59, population_50_54 )
