####Option 1
#4 groups rather than 6 - no more 5-year age gap

all_data_grouped <- all_data %>% mutate(Age_group2 =

  case_when( Age_group == "65-69" ~ "60-69",
             Age_group == "60-64" ~ "60-69",
             Age_group == "50-54" ~ "50-59",
             Age_group == "55-59" ~ "50-59",
             TRUE ~ Age_group


))

all_data_pivot <-pivot_longer(all_data_grouped, Black:White_total, names_to ="name", values_to= "value") %>% select(!(Age_group))
all_data_pivot2 <- aggregate(all_data_pivot$value, by=list(Age_group2 = all_data_pivot$Age_group2, name = all_data_pivot$name, covid_vacc_date = all_data_pivot$covid_vacc_date), FUN=sum)
all_data_grps <- pivot_wider(all_data_pivot2, names_from = name, values_from = x)


#Percentage by ethnicity
all_data_grps$black_percent <- all_data_grps$Black / all_data_grps$Black_total
all_data_grps$mixed_percent <- all_data_grps$Mixed/ all_data_grps$Mixed_total
all_data_grps$other_percent <- all_data_grps$Other/ all_data_grps$Other_total
all_data_grps$south_asian_percent <- all_data_grps$`South Asian` / all_data_grps$`South Asian_total`
all_data_grps$unknown_percent <- all_data_grps$Unknown / all_data_grps$Unknown_total
all_data_grps$white_percent <- all_data_grps$White / all_data_grps$White_total

#Remove unknown ethnicity
final_no_weights_grouped <- all_data_grps %>% select(Age_group2, black_percent, mixed_percent, south_asian_percent, white_percent, other_percent)
final_no_weights_grouped_pivot <- pivot_longer(final_no_weights_grouped, cols=black_percent:other_percent, names_to = "Ethnicity", values_to = "Percent")

#Charts
first_plot <- ggplot(final_no_weights_grouped_pivot,
                                  aes(x = Ethnicity,
                                      y = Percent
                                  )) +
  geom_col(fill = '#D1700E') +
  coord_flip()+
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels=scales::percent, limits =c(0,1)) +
  scale_x_discrete(labels = c("White", "South Asian", "Other", "Mixed", "Black")) +
  bbc_style() +
  reith_style() +
  labs(title="Take-up lower among ethnic minorities",
       subtitle = "Percentage vaccinated in each age group") +
  facet_wrap(~Age_group2, ncol = 2, nrow = 2) +
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

first_plot

finalise_plot(
  first_plot,
  source = paste0('Source: OpenSAFELY analysis of NHS data in England up to 14 Apr'),
  tinify_file = F,
  height_pixels = 540,
  save_filepath = paste0(
    "~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/output/vaccine_ethnicity_10_year_gap.png"
  )
)

#######
#Second plot

