###Criminal Justice System Statistics
#https://www.gov.uk/government/statistics/criminal-justice-system-statistics-quarterly-december-2020

#Load libraries
pacman::p_load(tidyverse, rdrop2, lubridate, purrr, jsonlite, stringr, bbmap,bbplot2,readxl, sf,shadowtext,rgdal,gridExtra, scales, R.utils, googlesheets4, gridExtra,ggpubr, WriteXLS, forcats, janitor, zoo, httr, ggtext, ggrepel, urltools)
library(base)

#############################################################
#All court data - prosecutions and convictions

all_courts <- read_csv("~/Downloads/all_courts_2020.csv")

#Only rape offences and those committed by a person rather than a company
rape_offences <- all_courts %>% filter(grepl("Rape", Offence)) %>% filter(`Person/other` == "01: Person")

rape_offences_year <- rape_offences %>% select(c(Year, `Proceeded against`, Sentenced)) %>% group_by(Year) %>% summarise_all(sum) %>%
  mutate(percent = Sentenced/`Proceeded against`)

sentence_graph <- ggplot(rape_offences_year,
                     aes(x = as.character(Year),
                         y = percent
                     )) +
  geom_col(fill = '#D1700E') +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  labs(title="Less than half of defendants are sentenced",
       subtitle = "Percentage of rape prosecutions that were sentenced") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
  geom_label(
    aes(
      x = as.character(Year),
      y = percent,
      label = paste0(format(round(percent*100)),"%")),
    hjust = 1,
    vjust = 0.5,
    colour = '#ffffff',
    fill = NA,
    label.size = NA,
    size = 7,
    fontface = "bold")

sentence_graph

finalise_plot(
  sentence_graph,
  source = paste0('Source: Criminal Justice System Statistics, Ministry of Justice'),
  tinify_file = F,
  height_pixels = 540,
  save_filepath = paste0(
    "~/Downloads/sentenced_graph.png"
  )
)

#########################################################################
#Sentence outcomes

rape_outcomes <- rape_offences %>%  mutate(Other == `Absolute Discharge` + `Conditional Discharge` +
                                             `Fine` + `Compensation (primary disposal)` +`Total Otherwise Dealt With`) %>%
  select(c(Year,`Total Community Sentence`,`Suspended Sentence`, `Total Immediate Custody`, `Other`)) %>%
  group_by(Year) %>% summarise_all(sum) %>% pivot_longer(!(Year))

rape_outcomes_plot <- ggplot(rape_outcomes,
                         aes(x = as.character(Year),
                             y = value,
                             fill = name,
                         )) +
  geom_bar(position="stack", stat = "identity") +
  #coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  #scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  scale_fill_manual(values = rev(bbc_pal('main', 4))) +
  labs(title="Majority of defendants are sent to custody",
       subtitle = "Sentence outcome for rape offences") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        #axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm'))
  # geom_label(
  #   aes(
  #     x = as.character(Year),
  #     y = percent,
  #     label = paste0(format(round(percent*100)),"%")),
  #   hjust = 1,
  #   vjust = 0.5,
  #   colour = '#ffffff',
  #   fill = NA,
  #   label.size = NA,
  #   size = 7,
  #   fontface = "bold")

rape_outcomes_plot

finalise_plot(
  rape_outcomes_plot,
  source = paste0('Source: Criminal Justice System Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 1000,
  save_filepath = paste0(
    "~/Downloads/sentenced_outcomes_graph.png"
  )
)

#####################################
#Custody by length
custody_length <- rape_offences %>% select(c(Year, starts_with("Custody")))%>% pivot_longer(!(Year)) %>%
  mutate(custody = case_when(name %in% c( "Custody - Up to and including 1 month", "Custody - Over 1 month and up to and including 2 months" ,
                              "Custody - Over 2 months and up to and including 3 months", "Custody - More than 3 months and under 6 months" ,
                              "Custody - 6 months", "Custody - More than 6 months and up to 9 months", "Custody - More than 9 months and under 12 months" ,
                              "Custody - 12 months", "Custody - More than 12 months and up to 18 months", "Custody - More than 18 months and up to 2 years") ~ "Less than 2\nyears",
                             name %in% c("Custody - More than 2 years and up to 3 years", "Custody - More than 3 years and under 4 years",  "Custody - 4 years" ) ~ "Between 2 and 4\nyears",
                             name %in% c("Custody - More than 4 years and up to 5 years" , "Custody - More than 5 years and up to 6 years" ,"Custody - More than 6 years and up to 7 years",
                                         "Custody - More than 7 years and up to 8 years", "Custody - More than 8 years and up to 9 years","Custody - More than 9 years and up to 10 years") ~ "Between 4 and 10\nyears",
                             name %in% c("Custody - Life", "Custody - Indeterminate Sentence", "Custody - More than 15 years and less than life") ~ "More than 15\nyears",
                             name %in% c("Custody - More than 10 years and up to 15 years") ~ "Between 10 and 15\nyears",

                   TRUE ~ name
 )) %>% filter(Year == 2019) %>% group_by(custody) %>% summarise(value=sum(value))

custody_values <- factor(custody_length$custody, level =c('Less than 2\nyears', 'Between 2 and 4\nyears', 'Between 4 and 10\nyears', 'Between 10 and 15\nyears', 'More than 15\nyears' ))


custody_length_plot <- ggplot(custody_length,
                             aes(x = custody_values,
                                 y = value,
                             )) +
  geom_col(fill = '#D1700E') +
  #coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  #scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  #scale_fill_manual(values = rev(bbc_pal('main', 14))) +
  labs(title="High proportion sentenced to over 4 years",
       subtitle = "Length of custodial sentences for rape in 2019") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        #axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_text(angle=90, hjust=1),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm'))
# geom_label(
#   aes(
#     x = as.character(Year),
#     y = percent,
#     label = paste0(format(round(percent*100)),"%")),
#   hjust = 1,
#   vjust = 0.5,
#   colour = '#ffffff',
#   fill = NA,
#   label.size = NA,
#   size = 7,
#   fontface = "bold")

custody_length_plot

finalise_plot(
  custody_length_plot,
  source = paste0('Source: Criminal Justice System Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 1200,
  save_filepath = paste0(
    "~/Downloads/custody_length_graph.png"
  )
)

##################################################################
###Areas based on where the offence is dealt with rather than where the offence was committed - add as a footnote?

#By police force area - prosecutions and convictions
pfa <- read_csv("~/Downloads/courts-by-pfa-2020.csv")

pa_rape <- pfa %>% filter(grepl("Rape", Offence)) %>% filter(`Type of Defendant` == "01: Person")

prosecutions <- pa_rape %>% filter(`Court Type` == "02: Magistrates Court") %>% group_by(`Police Force Area`, `Year of Appearance`) %>% summarise(prosecutions=n())
convictions <- pa_rape %>% filter(`Convicted/Not Convicted` == "01: Convicted") %>% group_by(`Police Force Area`, `Year of Appearance`) %>%
  summarise(convictions =n())

#Only do the last 5 years
conviction_ratio <- left_join(prosecutions,convictions, by =c("Police Force Area", "Year of Appearance")) %>%
  mutate(convictions = replace_na(convictions, 0)) %>% filter(`Year of Appearance` > "2015") %>%
  select(!(`Year of Appearance`)) %>% summarise_all(sum) %>% mutate(percent = convictions/prosecutions) %>% arrange(percent)

conviction_ratio <-conviction_ratio[order(conviction_ratio$percent),]

ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")

conviction_rate_plot <- ggplot(conviction_ratio,
                              aes(x = reorder(`Police Force Area`, percent),
                                  y = percent,
                              )) +
  #geom_col(fill = '#D1700E') +
  geom_bar(stat = "identity", fill = '#D1700E') +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  #scale_fill_manual(values = rev(bbc_pal('main', 14))) +
  labs(title="Conviction rate between 30-67% by Police Force Area",
       subtitle = "Proportion of convictions for rape prosecutions in 2016 - 2020") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
geom_label(
  aes(
    x = `Police Force Area`,
    y = percent,
    label = paste0(format(round(percent*100)),"%")),
  hjust = 1,
  vjust = 0.5,
  colour = '#ffffff',
  fill = NA,
  label.size = NA,
  size = 7,
  fontface = "bold")

conviction_rate_plot

finalise_plot(
  conviction_rate_plot,
  source = paste0('Source: Criminal Justice System Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 1200,
  height_pixels = 3000,
  save_filepath = paste0(
    "~/Downloads/conviction_by_PFA.png"
  )
)


#####################################################
#Remanded by police prior to appearing at court

remands_police <- read_csv("~/Downloads/remands_magistrates_2020.csv")

remands_police_rape <- remands_police %>% filter(grepl("Rape", Offence)) %>%
  group_by(`Year of Appearance`, `Remand status with Police`) %>% summarise(count = sum(Count))

remands_police_rape_per <- remands_police_rape  %>% mutate(percent = count/sum(count))

remands_police_rape_per_plot <- ggplot(remands_police_rape_per,
                                   aes(x = `Year of Appearance`,
                                       y = percent,
                                       group = `Remand status with Police`,
                                       fill = `Remand status with Police`,
                                   )) +
  #geom_col(fill = '#D1700E') +
  geom_bar(position = "dodge", stat = "identity") +
  #coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  scale_fill_manual(values = rev(bbc_pal('main', 3))) +
  labs(title="Not remanded after charge has increased since 2017",
       subtitle = "Defendants’ remand status with Police prior to appearing at magistrates' court") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm'))
# geom_label(
#   aes(
#     x = `Police Force Area`,
#     y = percent,
#     label = paste0(format(round(percent*100)),"%")),
#   hjust = 1,
#   vjust = 0.5,
#   colour = '#ffffff',
#   fill = NA,
#   label.size = NA,
#   size = 7,
#   fontface = "bold")

remands_police_rape_per_plot

finalise_plot(
  remands_police_rape_per_plot,
  source = paste0('Source: Criminal Justice System Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 900,
  save_filepath = paste0(
    "~/Downloads/remands_police.png"
  )
)

##########################################################
#Remands at the Crown Court

remands_cc <- read_csv("~/Downloads/remands_CC_2020.csv")

remands_cc_rape <- remands_cc %>% filter(grepl("Rape", Offence)) %>% filter(category == "01: Person") %>%
  group_by(`Year of Appearance`, `Remand status at the Crown Court`) %>% summarise(count = sum(count))

remands_cc_rape_per <- remands_cc_rape  %>% mutate(percent = count/sum(count))

remands_cc_rape_per_plot <- ggplot(remands_cc_rape_per,
                               aes(x = `Year of Appearance`,
                                   y = percent,
                                   group = `Remand status at the Crown Court`,
                                   fill = `Remand status at the Crown Court`,
                               )) +
  #geom_col(fill = '#D1700E') +
  geom_bar(position = "dodge", stat = "identity") +
  #coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  scale_fill_manual(values = rev(bbc_pal('main', 3))) +
  labs(title="Custody increased from 2019",
       subtitle = "Remanded status for defendants tried or sentenced at the Crown Court") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm'))
  # geom_label(
  #   aes(
  #     x = `Police Force Area`,
  #     y = percent,
  #     label = paste0(format(round(percent*100)),"%")),
  #   hjust = 1,
  #   vjust = 0.5,
  #   colour = '#ffffff',
  #   fill = NA,
  #   label.size = NA,
  #   size = 7,
  #   fontface = "bold")

remands_cc_rape_per_plot

finalise_plot(
  remands_cc_rape_per_plot,
  source = paste0('Source: Criminal Justice System Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 1000,
  save_filepath = paste0(
    "~/Downloads/remands_cc.png"
  )
)
