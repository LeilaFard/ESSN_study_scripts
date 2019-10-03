#data_ <- full_preprocessed_dataset(year, month)
res <- setNames(data.frame(data_[which(is.na(data_$months_since_application)), 'assistance_no']), c('assistance_no'))
res[['months_since_application']] = NA

#### On itère sur les dates suivantes
dates_list = list( c(2016, 12), c(2017, 1), c(2017, 2), c(2017, 3), c(2017, 4), c(2017, 5), c(2017, 6), 
                   c(2017, 7), c(2017, 8), c(2017, 9), c(2017, 10), c(2017, 11),  c(2017, 12), 
                   c(2018, 1), c(2018, 2), c(2018, 3), c(2018, 4))
 
for (i in 1:(length(dates_list)-1)){
  k=length(dates_list)-i
  date = dates_list[[k]]
  
  print(paste('T =', paste(date[1], date[2], sep='/')))
  
  he <- read_household_data(date[1], date[2], 'eligible')
  hi <- read_household_data(date[1], date[2], 'ineligible')
  
  d <- rbind(he$assistance_no, hi$assistance_no)
  
  l <- res[which(!(res$assistance_no%in%d)), 'assistance_no']
  data_[which((res$assistance_no%in%l)&(is.na(res$months_since_application))), 'months_since_application'] = i
}


rm(d, k, l)

