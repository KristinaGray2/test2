#R packages
library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(bbplot2)
library(ggplot2)
library(tinifyr)

reith_style()
all_reith_underneath()

##############################################################
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


###########14th April###########################################
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
black_final <- sum(all_data$black_weighted) / total_percent
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
Percent <- c(black_final, mixed_final, other_final, south_asian_final, unknown_final, white_final)
Ethnicity <- c("Black", "Mixed", "Other", "South Asian", "Unknown", "White")

final <- data.frame(Ethnicity, Percent)

###############################################################
#No weights
per_black <- sum(all_data$Black) / sum(all_data$Black_total)
print(per_black)
per_mixed <- sum(all_data$Mixed) / sum(all_data$Mixed_total)
print(per_mixed)
per_other <- sum(all_data$Other) / sum(all_data$Other_total)
print(per_other)
per_south_asian <- sum(all_data$`South Asian`) / sum(all_data$`South Asian_total`)
print(per_south_asian)
per_unknown <- sum(all_data$Unknown) / sum(all_data$Unknown_total)
print(per_unknown)
per_white <- sum(all_data$White) / sum(all_data$White_total)
print(per_white)

#Combine into data frame
Percent <- c(per_black, per_mixed, per_other, per_south_asian, per_unknown, per_white)
Ethnicity <- c("Black", "Mixed", "Other", "South Asian", "Unknown", "White")

final_no_weights <- data.frame(Ethnicity, Percent) %>% filter(!(Ethnicity == "Unknown"))

#################17th March#####################################
#Filter by latest date - 2021-04-14 and add variable for age
population_80_2 <-Cumulative_vaccination_percent_among_80_population %>% filter(covid_vacc_date == "2021-03-17") %>% mutate(Age_group ="80+")
population_70_79_2 <-Cumulative_vaccination_percent_among_70_79_population %>% filter(covid_vacc_date == "2021-03-17") %>% mutate(Age_group ="70-79")
population_65_69_2 <-Cumulative_vaccination_percent_among_65_69_population %>% filter(covid_vacc_date == "2021-03-17") %>% mutate(Age_group ="65-69")
population_60_64_2 <-Cumulative_vaccination_percent_among_60_64_population %>% filter(covid_vacc_date == "2021-03-17") %>% mutate(Age_group ="60-64")
population_55_59_2 <-Cumulative_vaccination_percent_among_55_59_population %>% filter(covid_vacc_date == "2021-03-17") %>% mutate(Age_group ="55-59")
population_50_54_2 <- Cumulative_vaccination_percent_among_50_54_population %>% filter(covid_vacc_date == "2021-03-17") %>% mutate(Age_group ="50-54")

#Combine data
all_data_2 <- rbind(population_80_2, population_70_79_2,population_65_69_2, population_60_64_2,population_55_59_2, population_50_54_2 )

#Percentage by ethnicity
all_data_2$black_percent <- all_data_2$Black / all_data_2$Black_total
all_data_2$mixed_percent <- all_data_2$Mixed/ all_data_2$Mixed_total
all_data_2$other_percent <- all_data_2$Other/ all_data_2$Other_total
all_data_2$south_asian_percent <- all_data_2$`South Asian` / all_data_2$`South Asian_total`
all_data_2$unknown_percent <- all_data_2$Unknown / all_data_2$Unknown_total
all_data_2$white_percent <- all_data_2$White / all_data_2$White_total

#Total population by age (sum of ethnicity)
all_data_2$total_pop <- all_data_2$Black_total + all_data_2$Mixed_total + all_data_2$Other_total + all_data_2$`South Asian_total` + all_data_2$Unknown_total + all_data_2$White_total

#Sum total population
total_2 <- sum(all_data_2$total_pop)
print(total_2)

#Total population - percent
all_data_2$total_pop_percent <- all_data_2$total_pop / total_2

##############################################################
#Weights
all_data_2$black_weighted <- (all_data_2$black_percent * all_data_2$total_pop_percent)
all_data_2$mixed_weighted <- (all_data_2$mixed_percent * all_data_2$total_pop_percent)
all_data_2$other_weighted <- (all_data_2$other_percent * all_data_2$total_pop_percent)
all_data_2$south_asian_weighted <- (all_data_2$south_asian_percent * all_data_2$total_pop_percent)
all_data_2$unknown_weighted <- (all_data_2$unknown_percent * all_data_2$total_pop_percent)
all_data_2$white_weighted <- (all_data_2$white_percent * all_data_2$total_pop_percent)

#Sum of total_pop_percent
total_percent_2 <- sum(all_data_2$total_pop_percent)
print(total_percent_2)

#Final weighted percentages
black_final_2 <- sum(all_data_2$black_weighted) / total_percent_2
print(black_final_2)
mixed_final_2 <- sum(all_data_2$mixed_weighted) /total_percent_2
print(mixed_final_2)
other_final_2 <- sum(all_data_2$other_weighted) / total_percent_2
print(other_final_2)
south_asian_final_2 <- sum(all_data_2$south_asian_weighted) / total_percent_2
print(south_asian_final_2)
unknown_final_2 <- sum(all_data_2$unknown_weighted) / total_percent_2
print(unknown_final_2)
white_final_2 <- sum(all_data_2$white_weighted) / total_percent_2
print(white_final_2)

#Final data frame
final_2 <- data.frame(percent(black_final_2), percent(mixed_final_2), percent(other_final_2), percent(south_asian_final_2), percent(unknown_final_2), percent(white_final_2))
print(final_2)

###############################################################
#No weights
per_black_2 <- sum(all_data_2$Black) / sum(all_data_2$Black_total)
print(per_black_2)
per_mixed_2 <- sum(all_data_2$Mixed) / sum(all_data_2$Mixed_total)
print(per_mixed_2)
per_other_2 <- sum(all_data_2$Other) / sum(all_data_2$Other_total)
print(per_other_2)
per_south_asian_2 <- sum(all_data_2$`South Asian`) / sum(all_data_2$`South Asian_total`)
print(per_south_asian_2)
per_unknown_2 <- sum(all_data_2$Unknown) / sum(all_data_2$Unknown_total)
print(per_unknown_2)
per_white_2 <- sum(all_data_2$White) / sum(all_data_2$White_total)
print(per_white_2)

#Combine into data frame
final_no_weights_2 <- data.frame(percent(per_black_2), percent(per_mixed_2), percent(per_other_2), percent(per_south_asian_2), percent(per_unknown_2), percent(per_white_2))

###############################################################
#Graphs

#All those over 50
#No weights
vaccine_ethnicity <- ggplot(final_no_weights,
                             aes(x = Ethnicity,
                                 y = Percent
                             )) +
  geom_col(fill = '#D1700E') +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent) +
  bbc_style() +
  reith_style() +
  labs(title="Take-up lower among ethnic minorities",
       subtitle = "Percentage vaccinated over 50-years-old") +
  #facet_wrap(~age_grp, ncol = 3, nrow = 2) +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
  geom_label(
    aes(
      x = Ethnicity,
      y = Percent,
      label = paste0(format(round(Percent*100)),"%")),
    hjust = 1,
    vjust = 0.5,
    colour = '#ffffff',
    fill = NA,
    label.size = NA,
    size = 7,
    fontface = "bold")

vaccine_ethnicity

vaccinated_date <- "14 Apr"

finalise_plot(
  vaccine_ethnicity,
  source = paste0('Source: OpenSAFELY analysis of NHS data in England. Data to ', vaccinated_date),
  tinify_file = T,
  height_pixels = 640,
  save_filepath = paste0(
    "~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/output/vaccine_ethnicity_Kristina-nc.png"
  )
)

##############################################################
#55-59 year olds on 14th April
#Data
#No weights
all_data_age <- all_data %>% filter(Age_group %in% c("60-64", "65-69"))

per_black_age <- sum(all_data_age$Black) / sum(all_data_age$Black_total)
per_mixed_age <- sum(all_data_age$Mixed) / sum(all_data_age$Mixed_total)
per_other_age <- sum(all_data_age$Other) / sum(all_data_age$Other_total)
per_south_asian_age <- sum(all_data_age$`South Asian`) / sum(all_data_age$`South Asian_total`)
per_unknown_age <- sum(all_data_age$Unknown) / sum(all_data_age$Unknown_total)
per_white_age <- sum(all_data_age$White) / sum(all_data_age$White_total)


#Combine into data frame
Percent <- c(per_black_age, per_mixed_age, per_other_age, per_south_asian_age, per_unknown_age, per_white_age)
Ethnicity <- c("Black", "Mixed", "Other", "South\nAsian", "Unknown", "White")

final_no_weights_age <- data.frame(Ethnicity, Percent) %>% filter(!(Ethnicity == "Unknown"))


vaccine_ethnicity_age <- ggplot(final_no_weights_age,
                            aes(x = Ethnicity,
                                y = Percent
                            )) +
  geom_col(fill = '#D1700E') +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent) +
  bbc_style() +
  reith_style() +
  labs(title="14 April",
       subtitle = "Percentage vaccinated 60-69") +
  #facet_wrap(~age_grp, ncol = 3, nrow = 2) +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
  geom_label(
    aes(
      x = Ethnicity,
      y = Percent,
      label = paste0(format(round(Percent*100)),"%")),
    hjust = 1,
    vjust = 0.5,
    colour = '#ffffff',
    fill = NA,
    label.size = NA,
    size = 7,
    fontface = "bold")

vaccine_ethnicity_age

vaccinated_date <- "14 Apr"

finalise_plot(
  vaccine_ethnicity_age,
  source = paste0('Source: OpenSAFELY analysis of NHS data in England. Data to ', vaccinated_date),
  tinify_file = F,
  height_pixels = 640,
  save_filepath = paste0(
    "~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/output/vaccine_ethnicity_age_14_Apr-nc.png"
  )
)

##########################################################
#55-59 year olds on 17th March
#Data
#No weights
all_data_age_2 <- all_data_2 %>% filter(Age_group %in% c("60-64", "65-69"))

per_black_age_2 <- sum(all_data_age_2$Black) / sum(all_data_age_2$Black_total)
per_mixed_age_2 <- sum(all_data_age_2$Mixed) / sum(all_data_age_2$Mixed_total)
per_other_age_2 <- sum(all_data_age_2$Other) / sum(all_data_age_2$Other_total)
per_south_asian_age_2 <- sum(all_data_age_2$`South Asian`) / sum(all_data_age_2$`South Asian_total`)
per_unknown_age_2 <- sum(all_data_age_2$Unknown) / sum(all_data_age_2$Unknown_total)
per_white_age_2 <- sum(all_data_age_2$White) / sum(all_data_age_2$White_total)


#Combine into data frame
Percent <- c(per_black_age_2, per_mixed_age_2, per_other_age_2, per_south_asian_age_2, per_unknown_age_2, per_white_age_2)
Ethnicity <- c("Black", "Mixed", "Other", "South\nAsian", "Unknown", "White")

final_no_weights_age_2 <- data.frame(Ethnicity, Percent) %>% filter(Ethnicity %in% c("White", "Black"))
final_no_weights_age_2$date <- "17 Mar - 30% gap"

Percent <- c(per_black_age, per_mixed_age, per_other_age, per_south_asian_age, per_unknown_age, per_white_age)
Ethnicity <- c("Black", "Mixed", "Other", "South\nAsian", "Unknown", "White")

final_no_weights_age <- data.frame(Ethnicity, Percent) %>% filter(Ethnicity %in% c("White", "Black"))
final_no_weights_age$date <- "14 Apr - 27% gap"

for_chart <- rbind(final_no_weights_age_2, final_no_weights_age) %>% mutate(date = factor(date, levels = c("17 Mar - 30% gap", "14 Apr - 27% gap")))




vaccine_ethnicity_age_2 <- ggplot(for_chart,
                                aes(x = Ethnicity,
                                    y = Percent
                                )) +
  geom_col(fill = '#D1700E') +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  labs(title="Gap narrows slightly",
       subtitle = "Proportion of people vaccinated aged 50-60") +
  facet_wrap(~date, ncol = 2, nrow = 2) +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
  geom_label(
    aes(
      x = Ethnicity,
      y = Percent,
      label = paste0(format(round(Percent*100)),"%")),
    hjust = 1,
    vjust = 0.5,
    colour = '#ffffff',
    fill = NA,
    label.size = NA,
    size = 7,
    fontface = "bold")

vaccine_ethnicity_age_2

vaccinated_date <- "17 Mar"

finalise_plot(
  vaccine_ethnicity_age_2,
  source = paste0('Source: OpenSAFELY analysis of NHS data in England'),
  tinify_file = F,
  height_pixels = 320,
  save_filepath = paste0(
    "~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/output/vaccine_ethnicity_age_50_60.png"
  )
)
