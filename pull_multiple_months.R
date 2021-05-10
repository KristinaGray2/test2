
#set up the list of data source urls

rtt_url_list <- c('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Incomplete-Provider-Jan20-XLS-8374K-22427.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/04/Incomplete-Provider-Feb20-XLS-8388K-20005.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/05/Incomplete-Provider-Mar20-XLS-8346K-73640.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/06/Incomplete-Provider-Apr20-XLS-8174K-19537.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/07/Incomplete-Provider-May20-XLS-8116K-43296.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/08/Incomplete-Provider-Jun20-XLS-8097K-84459.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Incomplete-Provider-Jul20-XLS-8164K-59000.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Incomplete-Provider-Aug20-XLS-8230K-v2.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Incomplete-Provider-Sep20-XLS-8295K-v2.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Incomplete-Provider-Oct20-XLS-8269K-v2.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/Incomplete-Provider-Nov20-XLS-8287K-26885.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/Incomplete-Provider-Dec20-XLS-8346K-v2.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/Incomplete-Provider-Jan21-XLS-8402K-v2.xls',
                  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Incomplete-Provider-Feb21-XLS-8343K-25692.xls')

#give them a date id - !!this is not foolproof as it's generated not read from the data!! a better
#way would be to to extract if from the url
rtt_dates <- seq(as.Date("2020-01-01"), as.Date("2021-02-01"), by = "month")

#set up a destination folder, using the date ID
rtt_destinations <- paste0("~/Downloads/rtt_monthly", rtt_dates, ".xls")


#download the files one by one, looping through the list. The rtt_destinations column is to make sure we
#know which month is which 

for(i in seq_along(rtt_url_list)){
  download.file(rtt_url_list[i], rtt_destinations[i], mode="wb")
}

# read in the latest month individually, purely to get the data structure
rtt_latest_url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Incomplete-Provider-Feb21-XLS-8343K-25692.xls"

#relevant month in format "2021-02-01"
rtt_month <- "2021-02-01"
rtt_filepath <- paste0('~/Dropbox (BBC)/Visual Journalism/Data/2021/vjdata.30699.nhscovidyear/source/rtt_monthly', rtt_month, '.xls') 

# download it
download.file(rtt_latest_url, rtt_filepath)

#read it in 
rtt_latest_month_df <- read_xls(rtt_filepath, skip = 13)%>%
  filter(`Treatment Function` == 'Total') %>% 
  clean_names() %>% 
  #give it a date column too
  mutate(date = as.Date(rtt_month))


#temporary df and a cumulative one, to read in each month. Uses the same data structure as the single month
#but with zero rows

rtt_full <- rtt_latest_month_df[0,]
rtt_months_df <- rtt_latest_month_df[0,]

#then loop through the downloaded files and read them in, building them all up into the rtt_months_df
#the final column "date" is still the destination url and will be swapped to date further down the line
for (i in seq_along(rtt_destinations)){
  rtt_months_df <- read_xls(rtt_destinations[i], skip = 13)%>%
    filter(`Treatment Function` == 'Total') %>%
    clean_names() %>%
    mutate(date = rtt_destinations[i])%>%
    rbind(rtt_full, rtt_months_df)
}
#   





