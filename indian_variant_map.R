
pacman::p_load('dplyr','tidyverse' ,'bbplot2', 'tidyr', 'rgdal', 'ggplot2', 'readxl', 'png', 'grid', 'ggpubr','RColorBrewer', 'classInt', 'rmapshaper', 'bbmap')
library(sf)


la_cases <- read.table(file = '~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/source/sanger/lineages_by_ltla_and_week.tsv', sep = '\t', header = TRUE)

la_latest_cases_all <- la_cases %>%
  filter(WeekEndDate == "2021-05-08") %>%
  group_by(LTLA) %>%
  summarise(latest_all = sum(Count))

la_latest_cases_non_ind <- la_cases %>%
  filter(WeekEndDate == "2021-05-08",
         Lineage != "B.1.617.2") %>%
  group_by(LTLA) %>%
  summarise(latest_non_ind = sum(Count))


la_names <- read.csv('~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/reference/new_pop_data.csv', stringsAsFactors = F)

#select only the indian variant and collapse to cumulative

# indian_la <- la_cases %>%
#   filter(Lineage == "B.1.617.2") %>%
#   group_by(LTLA) %>%
#   summarise(Cumulative = sum(Count)) %>%
#   left_join(la_names, by = c('LTLA' = 'gssid')) %>%
#   mutate(per_100k = (Cumulative/(population/100000)))


indian_la_latest <- la_cases %>%
  filter(Lineage == "B.1.617.2",
         WeekEndDate == "2021-05-08") %>%
  group_by(LTLA) %>%
  summarise(latest = sum(Count)) %>%
  right_join(la_latest_cases_all) %>%
  left_join(la_latest_cases_non_ind) %>%
  left_join(la_names, by = c('LTLA' = 'gssid')) %>%
  mutate(per_100k = (latest/(population/100000)),
         all_per_100k = (latest_all/(population/100000)),
         non_per_100k = (latest_non_ind / (population/100000)))


# Read in shapefiles
sf.la.prepared <- read_sf("~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/reference/Local_Authority_Districts__May_2020__UK_BUC-shp/Local_Authority_Districts__May_2020__UK_BUC.shp") %>%
  select("areaCode"=LAD20CD,"areaName"=LAD20NM,geometry)

plot(sf.la.prepared)

#
# test_map <- sf.la.prepared %>%
#   select(areaCode,areaName,geometry) %>%
#   left_join(indian_la, by = c('areaCode' = 'LTLA')) %>%
#   filter(grepl("E", areaCode)) %>%
#   mutate(Cumulative = case_when(is.na(Cumulative) ~ 0L,
#                                 T ~ Cumulative))
#
#
# test_map$brks <- cut(test_map$Cumulative,
#                      breaks=c(-1,0,9,24,49,99, 199, 399, 999,1000 ),
#                      labels=(c("Not found",'1-9',
#                                '10-24', '25-49', '50-99','100-199', '200-399', '400-999', '1000+'))
#                      )
#
# test_map$brks <- as.character(test_map$brks)
# test_map$brks[is.na(test_map$brks)] <- "No data"
#
# test_map$brks <- factor(test_map$brks,levels=(c("Not found",'1-9',
#                                                 '10-24', '25-49', '50-99','100-199', '200-399', '400-999', '1000+')))
#
# test_map_simple <- test_map %>%
#   select()
#
# all_reith_underneath()
#
#
# test_map.p <- ggplot() +
#   geom_sf(data = test_map,
#           aes(fill = brks),
#           show.legend = T,
#           colour = 'white',
#           size = 0.2) +
#   reith_style()+
#   scale_fill_manual(values = c("#BABABA",
#                                "#CFE1E5",
#                                "#A1CCD9",
#                                "#4BA7BC",
#                                "#1380A1",
#                                "#FFA31E",
#                                "#cc6404",
#                                "#990000",
#                                "#5B0101"),
#                     drop = FALSE,
#                     na.translate=FALSE) +
#   ## Bounding box of map is
#   ##      xmin         ymin         xmax         ymax
#   ## -116.1928    7054.1001  655653.8500 1218618.0700
#   scale_y_continuous(expand = c(0, 0), limits = c(40000, 800000)) +
#   scale_x_continuous(expand = c(0, 0), limits = c(200000, 655653)+40000) +
#   theme(legend.title=element_blank(),
#         legend.position = "left",
#         legend.justification = "top",
#         axis.text = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line.x = element_blank(),
#         plot.margin=margin(l=3)) +
#   labs(title="Indian variant cases detected DRAFT",
#        subtitle="Sanger data local authority DRAFT") +
#   coord_sf(clip="off") +
#   guides(fill = guide_legend(
#     ncol = 1,
#     byrow = TRUE,
#     reverse = F
#   ))
#
# test_map.p
#
# finalise_plot(plot_name = test_map.p,
#               footnote = 'England only',
#               source = 'Source: Wellcome Sanger Institute, data to 8 May 2021',
#               save_filepath = "~/Downloads/sanger_totals-nc.png",
#               width_pixels = 640,
#               height_pixels = 650
# )

####### CASE RATES ######

rate_map <- sf.la.prepared %>%
  select(areaCode,areaName,geometry) %>%
  left_join(indian_la_latest,
            by = c('areaCode' = 'LTLA')) %>%
  filter(grepl("E", areaCode)) %>%
  mutate(per_100k = case_when((is.na(per_100k) & !is.na(latest_all)) ~ 0,
                                T ~ per_100k))


rate_map$brks <- cut(rate_map$per_100k,
                     breaks=c(-1,0, 0.1,10,25,50,100, 200
                              # , 399, 999,1000
                              ),
                     labels=(c("No data",'Not found', 'Fewer than 10', '10-24', '25-49', '50-99','100-199'
                               #, '200-399', '400-999', '1000+'
                               ))
                     )

rate_map$brks <- as.character(rate_map$brks)
rate_map$brks[is.na(rate_map$brks)] <- "No data"

rate_map <- rate_map %>%
  mutate(brks = case_when(per_100k == 0 ~ "Not found",
                          T ~ brks))

rate_map$brks <- factor(rate_map$brks,levels=(c("No data",'Not found','Fewer than 10', '10-24', '25-49', '50-99','100-199'
                                                #, '200-399', '400-999', '1000+'
                                                )))


all_reith_underneath()


rate_map.p <- ggplot() +
  geom_sf(data = rate_map,
          aes(fill = brks),
          show.legend = T,
          colour = '777777',
          size = 0.2) +
  reith_style()+
  scale_fill_manual(values = c(
                               "#BABABA",
                               'white',
                               "#CFE1E5",
                               "#A1CCD9",
                               "#4BA7BC",
                               "#1380A1",
                               "#FFA31E"
                               # ,"#cc6404",
                               # "#990000",
                               # "#5B0101"
                               ),
                    drop = FALSE,
                    na.translate=FALSE) +
  ## Bounding box of map is
  ##      xmin         ymin         xmax         ymax
  ## -116.1928    7054.1001  655653.8500 1218618.0700
  scale_y_continuous(expand = c(0, 0), limits = c(20000, 700000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(200000, 655653)+40000) +
  theme(legend.title=element_blank(),
        legend.position = "left",
        legend.justification = "top",
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.margin=margin(l=3)) +
  coord_sf(clip="off") +
  labs(             title="Indian variant found in a few hotspots\nDRAFT",
                    subtitle="Cases of B.1.617.2 found* per 100,000 people in English\nlocal authorities, week ending 8 May 2021")+
  guides(fill = guide_legend(
    ncol = 1,
    byrow = TRUE,
    reverse = F
  ))

rate_map.p

finalise_plot(plot_name = rate_map.p,
              footnote = '*Not all cases of coronavirus are tested for the B.1.617.2 variant',
              source = 'Source: BBC analysis of Wellcome Sanger Institute data 2-8 May',
              save_filepath = "~/Downloads/sanger_rates_latest_DRAFT-nc.png",
              width_pixels = 640,
              height_pixels = 650
)


