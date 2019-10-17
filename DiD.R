remove(list = ls())

path <- dirname(rstudioapi::getSourceEditorContext()$path)

source(paste(path, 'settings.R', sep='/'))

year=2018
month=4
source(paste(tools, 'matching_preprocess.R', sep=''))
nchildren_own <- function(year, month, data){
  data[which(data$age_w > 18), 'nc_own_1'] = data[which(data$age_w > 18), 'num_children']
  a = data[which(data$age_w <= 18), 'assistance_no']
  ii <- read_individual_data(year, month, 'ineligible')
  enf <- setNames(data.frame(table(ii[which((ii$assistance_no%in%a)&(ii$age<6)), 'assistance_no'])), c('assistance_no', 'nc_own_2'))
  data <- merge(data, enf, by='assistance_no', all.x=TRUE)
  data[['num_children_own']] <- rowSums(data[,c('nc_own_1', 'nc_own_2')], na.rm=TRUE)
  data$nc_own_1 <- NULL
  data$nc_own_2 <- NULL
  
  return(data)
}

preprocessed_DiD_data <- function(year, month){
  d <- nchildren_own(year, month, merge(matching_data_preprocess(year, month), unique_f_ids(year, month), by='assistance_no', all.y=TRUE))
  d[['year']] = year
  return(d)
}

DiD_data_2018 <- rbind(preprocessed_DiD_data(2017,6), preprocessed_DiD_data(2018,6))

DiD_data_2018$time = (DiD_data_2018$year == 2017)
DiD_data_2018$did = DiD_data_2018$time * DiD_data_2018$eligible


didreg2018 = lm(num_children_own ~ eligible + time + did + age_w + married + nat_country +Reg, data = DiD_data_2018)

out.tex = xtable(summary(didreg2018))
print(out.tex, type='latex', file=paste(outputs_matching, '/didreg2018.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

DiD_data_2019 <- rbind(preprocessed_DiD_data(2017,6), preprocessed_DiD_data(2019,1))

DiD_data_2019$time = (DiD_data_2019$year == 2017)
DiD_data_2019$did = DiD_data_2019$time * DiD_data_2019$eligible


didreg2019 = lm(num_children_own ~ eligible + time + did + age_w + married + nat_country + Reg, data = DiD_data_2019)
out.tex = xtable(summary(didreg2019))
print(out.tex, type='latex', file=paste(outputs_matching, '/didreg2019.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)
