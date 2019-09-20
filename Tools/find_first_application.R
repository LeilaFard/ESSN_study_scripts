#TODO 


source(paste(tools, 'format_data.R', sep='/'))
source(paste(tools, 'matching_tools.R', sep=''))

data_ <- full_preprocessed_dataset(year, month)
ids <- data.frame(data_[which(is.na(data_$months_since_application)), 'assistance_no'])

#### On itère sur les dates suivantes
dates_list = list(c(2017, 6), c(2017, 7), c(2017, 8), c(2017, 9), c(2017, 10), c(2017, 11), 
                  c(2017, 12), c(2018, 1), c(2018, 2), c(2018, 3), c(2018, 4))

for (i in 1:length(dates_list)){
  date = dates_list[[length(dates_list)-1]]
  
  print(paste('T =', paste(date[1], date[2], sep='/')))
  
  he <- read_household_data(date0[1], date0[2], 'eligible')
  hi <- read_household_data(date0[1], date0[2], 'ineligible')
  
  d <- rbind(he$assistance_no, hi$assistance_no)
  
  l <- ids[which(!(ids$assistance_no%in%d)), 'assistance_no']
  data_[which(data_$assistance_no%in%l), 'months_since_application'] = i
}
