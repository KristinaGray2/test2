#R packages
library(readr)
library(dplyr)
library(tidyr)
library(scales)

#Read in the data
#Uploaded to this repo from here https://github.com/opensafely/nhs-covid-vaccination-coverage/tree/master/released-outputs/machine_readable_outputs
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

#Filter by latest date - 2021-04-14 and add variable for age
population_80 <-Cumulative_vaccination_percent_among_80_population %>% filter(covid_vacc_date == "2021-04-14") %>% mutate(Age_group ="80+")
population_70_79 <-Cumulative_vaccination_percent_among_70_79_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="70-79")
population_65_69 <-Cumulative_vaccination_percent_among_65_69_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="65-69")
population_60_64 <-Cumulative_vaccination_percent_among_60_64_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="60-64")
population_55_59 <-Cumulative_vaccination_percent_among_55_59_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="55-59")
population_50_54 <- Cumulative_vaccination_percent_among_50_54_population %>% filter(covid_vacc_date == "2021-04-14")%>% mutate(Age_group ="50-54")

#Combine data
all_data <- rbind(population_80, population_70_79,population_65_69,population_60_64,population_55_59, population_50_54 )

#Percentage by ethnicity
all_data$black_percent <- all_data$Black / all_data$Black_total
all_data$mixed_percent <- all_data$Mixed/ all_data$Mixed_total
all_data$other_percent <- all_data$Other/ all_data$Other_total
all_data$south_asian_percent <- all_data$`South Asian` / all_data$`South Asian_total`
all_data$unknown_percent <- all_data$Unknown / all_data$Unknown_total
all_data$white_percent <- all_data$White / all_data$White_total

#Total population by age (sum of ethnicity)
all_data$total_pop <- all_data$Black_total + all_data$Mixed_total + all_data$Other_total + all_data$`South Asian_total` + all_data$Unknown_total + all_data$White_total

#Sum total population
total <- sum(all_data$total_pop)
print(total)

#Total population - percent
all_data$total_pop_percent <- all_data$total_pop / total

##############################################################
#Weights
all_data$black_weighted <- (all_data$black_percent * all_data$total_pop_percent)
all_data$mixed_weighted <- (all_data$mixed_percent * all_data$total_pop_percent)
all_data$other_weighted <- (all_data$other_percent * all_data$total_pop_percent)
all_data$south_asian_weighted <- (all_data$south_asian_percent * all_data$total_pop_percent)
all_data$unknown_weighted <- (all_data$unknown_percent * all_data$total_pop_percent)
all_data$white_weighted <- (all_data$white_percent * all_data$total_pop_percent)

#Sum of total_pop_percent
total_percent <- sum(all_data$total_pop_percent)
print(total_percent)

#Final weighted percentages
weights$black_final <- sum(all_data$black_weighted) / total_percent
print(black_final)
mixed_final <- sum(all_data$mixed_weighted) /total_percent
print(mixed_final)
other_final <- sum(all_data$other_weighted) / total_percent
print(other_final)
south_asian_final <- sum(all_data$south_asian_weighted) / total_percent
print(south_asian_final)
unknown_final <- sum(all_data$unknown_weighted) / total_percent
print(unknown_final)
white_final <- sum(all_data$white_weighted) / total_percent
print(white_final)

#Final data frame
final <- data.frame(percent(black_final), percent(mixed_final), percent(other_final), percent(south_asian_final), percent(unknown_final), percent(white_final))
print(final)
