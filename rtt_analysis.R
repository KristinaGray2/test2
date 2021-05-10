
#IN THE LATEST MONTH

# 1) which 10 trusts had the highest number of patients waiting 52+
# 2) which trusts had the highest proportion?
# 3) how many trusts had 10% of their patients waiting 52+
# 4) what was the median average wait for England?

#read in the data 
rtt_months_52 <- read.csv("~/Dropbox (BBC)/Visual Journalism/Data/2021/vjdata.30699.nhscovidyear/derived/rtt_timeseries_jan20_feb21.csv", 
                          stringsAsFactors = F)

rtt_months_52$date <- as.Date(rtt_months_52$date)

#select the version of the data you are working with 
rtt_latest <- rtt_months_52 %>% 
  filter(date == as.Date("2021-02-01"))

#write.csv(rtt_latest, "~/Downloads/feb_21_with_more_specialists.csv", row.names = F)

# 1) Which 10 trusts had the highest number of patients waiting 52+
top_10_52wks <- rtt_latest %>% slice_max(over_52, n = 10)
print(top_10_52wks)


# 2) Which trusts had the highest proportion?
top_10_52pc <- rtt_latest %>% slice_max(over_52pc, n = 10) 
print(top_10_52pc)


# 3) how many trusts had 10% of their patients waiting 52+
#over 10pc
{over_10pc_52wks <- rtt_latest %>% 
  filter(over_52pc >= 10) 

number_over_10 <- count(over_10pc_52wks)

#over 15
number_over_15 <- rtt_latest %>%
  filter(over_52pc >= 15) %>% 
  count(over_52pc) %>% 
  colSums()

number_over_15 <- number_over_15[[2]]

#over 20
number_over_20 <- rtt_latest %>%
  filter(over_52pc >= 20) %>% 
  count(over_52pc) %>% 
  colSums()

number_over_20 <- number_over_20[[2]]
}

#extract the month as a word
month <- month.abb[month(rtt_latest$date[1])]
year <- year(rtt_latest$date[1])

writeLines(paste0('In ', month,' ', year, ', ', number_over_10, ' NHS trusts had more than 10% of their patients waiting over a year\n',
      'In ', month,' ',year, ', ', number_over_15, ' NHS trusts had more than 15% of their patients waiting over a year\n',
      'In ', month,' ', year, ', ', number_over_20, ' NHS trusts had more than 20% of their patients waiting over a year'))


rtt_latest %>% filter(!is.na(over_52pc))

########

#Bring in the 12 month version 

rtt_data_52 <- rtt_months_52

#how many trusts had 0% in March 2020?
march <- rtt_data_52 %>% filter(date == as.Date("2020-03-01")) %>% 
  group_by(over_52pc) %>% 
  summarise(n())

#how many trusts had 0% in Feb 2021?
feb_21 <- rtt_data_52 %>% filter(date == as.Date("2021-02-01")) %>% 
  group_by(over_52pc) %>% 
  summarise(n())






# 
# 
# p1 <- ggplot()+
#   geom_point(data = rtt_data_52,
#              aes(x= date, 
#                  y= within_18pc
#                  #y = over_52pc
#                  #colour = Region
#              ),
#              colour = '#1380A1',
#              alpha = 0.4)+
#   scale_y_continuous(limits = c(0,100))+
#   reith_style()+
#   labs(title = "Percentage of patients being treated\nwithin 18 weeks\n")
# 
# p1
# 
# finalise_plot(plot_name = p1,
#               source = paste0('NHS England'),
#               save_filepath = "~/Downloads/rtt_trusts_under18pc-nc.png"
# )



p2 <- ggplot()+
  geom_point(data = rtt_data_52 %>% 
               filter(date >= as.Date("2020-03-01")),
             aes(x= date,
                 y = over_52pc
                 ),
             colour = '#1380A1',
             alpha = 0.4)+
  scale_y_continuous(limits = c(0,30),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0))+
  scale_x_date(limits = c(as.Date("2020-02-01"), 
                          as.Date("2021-02-01")),
    expand = c(0.05, 0.05))+
  reith_style()+
  labs(title = "How year-long waits started to build",
       subtitle = "Percentage of patients waiting more than a year for\nroutine treatment to start - each dot is one NHS trust\n")+
  theme(plot.margin = margin(t = 0.3, b = 0.3, r = 1, l = 0.3, unit = "cm"))+
  coord_cartesian(clip = 'off')

p2

p2_labels <- p2+
  geom_label(aes( x= as.Date("2020-02-01", "%Y-%m-%d"
                             ),
                  y = 11,
                  label = "Start of pandemic:\nall trusts\nbelow 1%"),
             hjust = 0,
             vjust = 0.5,
             colour = "#616161",
             fill = "white",
             label.size = NA,
             size = 6,
             lineheight = 0.9)+
  geom_segment(aes(x = as.Date("2020-03-01"), 
                   y = 7, 
                   xend = as.Date("2020-03-01"), yend = 3), 
               colour = "#222222", 
               size=0.5,
               arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes( x= as.Date("2021-01-01", "%Y-%m-%d")-10,
                  y = 28,
                  label = "Feb 2021:\n35 trusts\nover 10%"),
             hjust = 0,
             vjust = 0.5,
             colour = "#616161",
             fill = "white",
             label.size = NA,
             size = 6,
             lineheight = 0.9)+
  geom_segment(aes(x = as.Date("2021-02-01"), 
                   y = 24, 
                   xend = as.Date("2021-02-01"), yend = 22
                   ), 
               colour = "#222222", 
               size=0.5,
               arrow = arrow(length = unit(0.03, "npc")))
  # geom_curve(aes(x = as.Date("2021-01-10"), y = 23, 
  #                xend = as.Date("2021-02-01"), yend =21 ), 
  #            colour = "#222222", 
  #            size=0.5, 
  #            curvature = -0.2,
  #            arrow = arrow(length = unit(0.03, "npc")))
  

p2_labels

finalise_plot(plot_name = p2_labels,
              source = paste0('NHS England'),
              footnote = "Percentage is of all patients who are waiting at the end of each month",
              save_filepath = "~/Downloads/rtt_trusts_over52pc_labels-nc.png"
)




  p3 <- ggplot()+
  geom_point(data = rtt_data_52,
             aes(x= date, 
                 #y= within_18pc
                 y = over_52
                 #colour = Region
             ),
             colour = '#1380A1',
             alpha = 0.4)+
  #scale_y_continuous(limits = c(0,30))+
  reith_style()+
  labs(title = "Number of patients waiting more than a year\n")

p3

finalise_plot(plot_name = p3,
              source = paste0('NHS England'),
              save_filepath = "~/Downloads/rtt_trusts_over52_number-nc.png"
)

p4 <- ggplot()+
  geom_point(data = rtt_data_52,
             aes(x= date, 
                 y= within_18
                 #y = over_52pc
                 #colour = Region
             ),
             colour = '#1380A1',
             alpha = 0.4)+
  #scale_y_continuous(limits = c(0,100))+
  reith_style()+
  labs(title = "Number of patients being treated\nwithin 18 weeks\n")

p4

finalise_plot(plot_name = p4,
              source = paste0('NHS England'),
              save_filepath = "~/Downloads/rtt_trusts_under18_number-nc.png"
)



# #find highest by 52+ wait
# top_10_52weeks <- rtt_data_52 %>% slice_max(over_52, n = 10) %>% 
#   select(1,3,5,6:10)
# 
# write.csv(top_10_52weeks, "output/top_52plus_by_number.csv", row.names = F)
# 
# #find highest percentage by 52+ wait LATEST MONTH
# top_10_52weeks_pc_latest <- rtt_data_52 %>% 
#   filter(!is.na(date)) %>% 
#   filter(date == max(date)) %>%
#   slice_max(over_52pc, n = 10) %>% 
#   select(1,3,5,6:10)
# 
# write.csv(top_10_52weeks_pc_latest, "output/top_52plus_by_percentage_latest.csv", row.names = F)
# 
# 
# #highest number within 18 weeks
# top_10_within_18 <- rtt_data_52 %>% slice_max(within_18, n = 10) %>% 
#   select(1,3,5,6:10)
# 
# write.csv(top_10_within_18, "output/top_within_18_number.csv", row.names = F)
# 
# #highest percentage within 18 weeks
# top_10_within_18_pc <- rtt_data_52 %>% slice_max(within_18pc, n = 10) %>% 
#   select(1,3,5,6:10)
# 
# write.csv(top_10_within_18_pc, "output/top_within_18_percentage.csv", row.names = F)
# 
# #lowest number within 18 weeks
# bottom_10_within_18 <- rtt_data_52 %>% slice_min(within_18, n = 10) %>% 
#   select(1,3,5,6:10)
# 
# write.csv(bottom_10_within_18, "output/lowest_within_18_number.csv", row.names = F)
# 
# #lowest percentage within 18 weeks
# bottom_10_within_18_pc <- rtt_data_52 %>% slice_min(within_18pc, n = 10) %>% 
#   select(1,3,5,6:10)
# 
# write.csv(bottom_10_within_18_pc, "output/lowest_within_18_percentage.csv", row.names = F)
