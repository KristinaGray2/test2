#Load libraries
pacman::p_load(tidyverse, rdrop2, lubridate, purrr, jsonlite, stringr, bbmap,bbplot2,readxl, sf,shadowtext,rgdal,gridExtra, scales, R.utils, googlesheets4, gridExtra,ggpubr, WriteXLS, forcats, janitor, zoo, httr, ggtext, ggrepel, urltools)
library(base)
library(vroom)


#Stats on NHS stop smoking services in England
#Raw numbers per LA
#Pregnant women

#Table 3.7 - Pregnant women setting a quit date by LA
#This gives you the number of pregnant women accessing smoking services
date <- "Apr-Dec-2020"
fn_date <- paste0('~/Downloads/stop-smoking-serv-',date,'.xlsx')
url <- "https://files.digital.nhs.uk/70/F41C72/stat-stop-smok-serv-eng-q3-2021-tab.xlsx"
download.file(url, fn_date)

accessed_services_2020 <- read_excel(fn_date, sheet= "3.7",range= "A6:I170")
accessed_services_2020_V2 <- accessed_services_2020 %>% filter(!(is.na(`LA name`))) %>% select(!(`...2`))
accessed_services_2020_V2[accessed_services_2020_V2 == ":"] <- "0"

number_accessing_2020 <- accessed_services_2020_V2 %>% group_by(`ONS code`, `LA name`, `Region name`) %>% summarise(accessed_count=`Setting a quit date`)

#But you want to know the percentage % of pregnant women
#So need to match against the number of pregnant women who are smoking

#Smoking at time of booking (SATOD)
#Maternity services monthly statistics
#SmokingStatusGroupBooking - The mother's self-reported smoking status at the Booking Appointment

download <- function(url){
  folder <- '~/Downloads/maternity_stats'
  filename <- basename(url)
  file <- file.path(folder,filename)
  download.file(url, file)

  smoking_status <- read_csv(file)

}

download(url= "https://files.digital.nhs.uk/FE/92D6D1/msds-dec2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/D3/DA21D5/msds-nov2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/6C/77F3AA/msds-oct2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/7D/94EBDB/msds-sep2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/7C/1CE676/msds-aug2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/4D/C4E5FA/msds-jul2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/FE/096EFA/msds-jun2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/AB/09DE17/msds-may2020-exp-data.csv")
download(url = "https://files.digital.nhs.uk/EF/E17B45/msds-apr2020-exp-data.csv")

files <- fs::dir_ls(path = '~/Downloads/maternity_stats', glob = '*.csv')

maternity_raw <- vroom(files) %>% filter(Dimension == "SmokingStatusGroupBooking") %>% filter(Org_Level == "Local Authority of Residence")

maternity_raw_2 <- maternity_raw %>% filter(Measure == "Smoker") %>% filter(!(Org_Name == "LOCAL AUTHORITY UNKNOWN"))

test <- maternity_raw_2 %>% group_by(ReportingPeriodEndDate) %>% summarise(count=n())

smokers_2020 <- maternity_raw_2 %>% group_by(Org_Code) %>% summarise(Count_smokers = sum(Final_value))

##Join
Joined <- left_join(number_accessing_2020, smokers_2020, by = c("ONS code" = "Org_Code"))
