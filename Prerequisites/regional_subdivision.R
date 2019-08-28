library(XML)

url <- 'http://www.statoids.com/utr.html'
page <- htmlParse(url)
table_regions <- readHTMLTable(page['//table'][[2]])
table_regions <- table_regions[1:81,c('ISO', 'Reg')] # 'Province' (deleted - used as verification)

table_regions[['national_code']] = as.integer(table_regions$ISO)
table_regions[which(table_regions$national_code > 77), 'national_code'] = sapply(table_regions[which(table_regions$national_code > 77), 'national_code'], function(x)(return(x-1)))
table_regions[['ISO']] <- NULL

regions <- read.csv(paste(data_path, 'Master Data/Provinces.csv', sep='/'), sep=',')
regions = merge(table_regions, regions, by='national_code', all=TRUE)

rm(table_regions, url, page)

regions <- setNames(regions ,c('national_code', 'Reg', 'province_id', 'names'))
  
saveRDS(regions, file = './Data/Processed/regions.rds')