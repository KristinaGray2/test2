##Criminal court statistics

#Latest quarterly publication https://www.gov.uk/government/statistics/criminal-court-statistics-quarterly-october-to-december-2020
#Latest annual publication https://www.gov.uk/government/statistics/criminal-court-statistics-quarterly-january-to-march-2020
#CSVs are downloaded from the transparency file zip folders
#The quarterly publication provides data up to 2020 Q4 but the annual publication is required for timeliness by offence type csv

############################################################
#Effectiveness of Crown Court trials
effectiveness <- read_csv("~/Downloads/trial_2020Q4.csv")

#Vacated trials not included in the percentage total
trials_effective <- effectiveness %>% mutate(value = replace_na(value,0)) %>% group_by(year,offence_group, trial_effectiveness) %>%
  summarise(count=sum(value)) %>% filter(year == "2019") %>% filter(!trial_effectiveness == "4. Vacated trial") %>% mutate(percent=count/sum(count)) %>%
  filter(!(offence_group %in% c("13: Unknown", "13: Not known")))

#trials_effective$trial_effectiveness <- factor(trials_effective$trial_effectiveness, level =c('1: Effective trials', '3: Ineffective trials', '2: Cracked trials'))

trials_effective_plot <- ggplot(trials_effective,
                                   aes(x = offence_group,
                                       y = percent,
                                       group = trial_effectiveness,
                                       fill = trial_effectiveness,
                                   )) +
  #geom_col(fill = '#D1700E') +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  scale_fill_manual(values = rev(bbc_pal('main', 3))) +
  labs(title="Sexual offence trials are the most effective",
       subtitle = "Effectiveness of trials at Crown Court by offence") +
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

trials_effective_plot

finalise_plot(
  trials_effective_plot,
  source = paste0('Source: Criminal Justice System Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 1000,
  save_filepath = paste0(
    "~/Downloads/trials_effectivee.png"
  )
)



###By reason
reasons <- effectiveness %>% group_by(reason) %>% summarise(count=n())

#Group some of the reasons for ease
reasons_grouped <- effectiveness %>% mutate(reason_group = case_when(

  reason %in% c("01. Cracked Reason: Acceptable guilty plea(s) entered late, offered for the first time by the defence",
                "02. Cracked Reason: Acceptable guilty plea(s) entered late, previously rejected by the prosecution",
                "03. Cracked Reason: Acceptable guilty plea(s) to alternative new charge, first time offered by defence",
                "04. Cracked Reason: Acceptable guilty plea(s) to alternative new charge, previously rejected by the prosecution",
                "05. Cracked Reason: Defendant bound over - acceptable to prosecution, offered for the first time by the defence",
                "06. Cracked Reason: Defendant bound over - now acceptable to prosecution, previously rejected by the prosecution",
                "12. Cracked Reason: Defendant deceased") ~ "Cracked trial - other",
  reason %in% c("04. Ineffective reason: Prosecution witness absent - police",
                "05. Ineffective reason: Prosecution witness absent - professional/expert",
                "06. Ineffective reason: Prosecution witness absent - other") ~ "Ineffective reason: Prosecution witness absent",
  reason %in% c("11. Ineffective reason: Defendant absent - did not proceed in absence (judicial discretion)",
                "12. Ineffective reason: Defendant absent - unable to proceed as Defendant not notified of place and time of hearing") ~ "Ineffective reason: Defendant absent",
  TRUE ~ reason
))

sex_offences_effectiveness <- reasons_grouped %>% filter(offence_group == "02: Sexual offences")
Only_2019 <- sex_offences_effectiveness %>% filter(year == "2019") %>% mutate(value =replace_na(value,0)) %>% group_by(reason_group) %>% summarise(count=sum(value)) %>%
  arrange(-count)

########################################################
#Guilty plea rate

guilty_plea_rate <- read_csv("~/Downloads/cc_plea_2020Q4.csv") %>% filter(annual_quarterly == "Annual") %>%
  filter(case_type == "0. All for trial") %>% filter(measure == "0: Guilty plea rate (%)")

gpr_england_wales <- guilty_plea_rate %>% filter(region == "England and Wales") %>% filter(year == "2019")

gpr_england_wales_plot <- ggplot(gpr_england_wales,
                                aes(x = offence_group,
                                    y = value,
                                )) +
  geom_col(fill = '#D1700E') +
  #geom_bar(position = "stack", stat = "identity") +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  scale_fill_manual(values = rev(bbc_pal('main', 3))) +
  labs(title="Sexual offences have lowest guilty plea rate",
       subtitle = "Guilty plea rate at Crown Court") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
geom_label(
  aes(
    x = offence_group,
    y = value,
    label = paste0(format(round(value*100)),"%")),
  hjust = 1,
  vjust = 0.5,
  colour = '#ffffff',
  fill = NA,
  label.size = NA,
  size = 7,
  fontface = "bold")

gpr_england_wales_plot

finalise_plot(
  gpr_england_wales_plot,
  source = paste0('Source: Criminal Court Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 800,
  save_filepath = paste0(
    "~/Downloads/guilty_plea.png"
  )
)

##By region

gpr_region <- guilty_plea_rate %>% filter(!(region == "England and Wales")) %>% filter(year == "2019") %>%
  filter(offence_group == "02: Sexual offences")

gpr_region_plot <- ggplot(gpr_region,
                                 aes(x = region,
                                     y = value,
                                 )) +
  geom_col(fill = '#D1700E') +
  #geom_bar(position = "stack", stat = "identity") +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  #scale_fill_manual(values = rev(bbc_pal('main', 7))) +
  labs(title="London has lowest guilty plea rate",
       subtitle = "Guilty plea rate for sexual offences at Crown Court") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
  geom_label(
    aes(
      x = region,
      y = value,
      label = paste0(format(round(value*100)),"%")),
    hjust = 1,
    vjust = 0.5,
    colour = '#ffffff',
    fill = NA,
    label.size = NA,
    size = 7,
    fontface = "bold")

gpr_region_plot

finalise_plot(
  gpr_region_plot,
  source = paste0('Source: Criminal Court Statistics, Ministry of Justice'),
  tinify_file = F,
  #width_pixels = 800,
  save_filepath = paste0(
    "~/Downloads/guilty_plea_region.png"
  )
)

#####################################################
#Average hearing time and average waiting time

hearing_waiting_data <- read_csv("~/Downloads/cc_waiting_hearing_2020Q4.csv") %>% filter(annual_quarterly == "Annual") %>%
  filter(case_type == "02. Trials: total")

hearing_time <- hearing_waiting_data %>% filter(waiting_hearing_times == "2. Hearing times") %>% filter(measure == "3. Median (hours)")
hearing_time_england_wales <- hearing_time %>% filter(region == "England and Wales") %>% filter(year == "2019")

hearing_time_england_wales_plot <- ggplot(hearing_time_england_wales,
                                 aes(x = offence_group,
                                     y = value,
                                 )) +
  geom_col(fill = '#D1700E') +
  #geom_bar(position = "stack", stat = "identity") +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  #scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  scale_fill_manual(values = rev(bbc_pal('main', 3))) +
  labs(title="Sexual offences have highest number of hearings",
       subtitle = "Average hearing time for trial cases at Crown Court") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
  geom_label(
    aes(
      x = offence_group,
      y = value,
      label = paste0(format(round(value, digits = 1)))),
    hjust = 1,
    vjust = 0.5,
    colour = '#ffffff',
    fill = NA,
    label.size = NA,
    size = 7,
    fontface = "bold")

hearing_time_england_wales_plot

finalise_plot(
  hearing_time_england_wales_plot,
  source = paste0('Source: Criminal Court Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 800,
  save_filepath = paste0(
    "~/Downloads/hearing_time.png"
  )
)

#Waiting time
waiting_time <- hearing_waiting_data %>% filter(waiting_hearing_times == "1. Waiting times") %>% filter(measure == "3. Median (weeks)")
waiting_time_england_wales <- waiting_time %>% filter(region == "England and Wales") %>% filter(year == "2019")

waiting_time_england_wales_plot <- ggplot(waiting_time_england_wales,
                                          aes(x = offence_group,
                                              y = value,
                                          )) +
  geom_col(fill = '#D1700E') +
  #geom_bar(position = "stack", stat = "identity") +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  #scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  bbc_style() +
  reith_style() +
  scale_fill_manual(values = rev(bbc_pal('main', 3))) +
  labs(title="Sexual offences have longest waiting time",
       subtitle = "Average waiting time for trial cases at Crown Court") +
  theme(strip.text = element_text(margin = margin(b= 0.5, unit = 'cm')),
        axis.line.x = element_blank(),
        #axis.ticks.x =element_blank(),
        #axis.text.x = element_blank(),
        panel.spacing.x = unit(0.5, 'cm'),
        panel.grid = element_blank(),
        plot.margin = margin(l = 0.2, r = 1.2, unit = 'cm')) +
  geom_label(
    aes(
      x = offence_group,
      y = value,
      label = paste0(format(round(value, digits = 1)))),
    hjust = 1,
    vjust = 0.5,
    colour = '#ffffff',
    fill = NA,
    label.size = NA,
    size = 7,
    fontface = "bold")

waiting_time_england_wales_plot

finalise_plot(
  waiting_time_england_wales_plot,
  source = paste0('Source: Criminal Court Statistics, Ministry of Justice'),
  tinify_file = F,
  width_pixels = 800,
  save_filepath = paste0(
    "~/Downloads/waiting_time.png"
  )
)
