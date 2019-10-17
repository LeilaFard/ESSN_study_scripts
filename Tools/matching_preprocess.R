library(stats)
library(ggplot2)
library(xtable)
#library(MatchItSE) 

source(paste(tools, 'format_data.R', sep='/'))
source(paste(tools, 'matching_tools.R', sep=''))


print(paste(month, year, sep='/'))

#outputs_matching = paste('./Outputs/PsMatch', year, month, sep='/')
outputs_matching = paste('./Outputs/PsMatch_test', year, month, sep='/')
dir.create(outputs_matching, recursive=TRUE)


unique_f <- function(year, month, status){
  df_i <- read_individual_data(year, month, status)
  
  df_iff<- df_i[which(df_i$gender=='female' & df_i$age>=15 & df_i$age<49),]
  iffs <- df_iff[which(duplicated2(df_iff$assistance_no)==FALSE),]
  iffs$married <- (iffs$marital_status == 'married')
  iffs <-  setNames(data.frame(iffs$assistance_no, iffs$age, iffs$married), c('assistance_no', 'age_w', 'married'))
  
  return(iffs)
}

unique_f_ids <- function(year, month){
  fff_e <- unique_f(year, month, 'eligible')
  fff_i <- unique_f(year, month, 'ineligible')
  return(rbind(fff_i, fff_e))
}




#data_ <- data[sample(nrow(data), (nrow(data)/10)),]

births_y1 <- function(data){
  ii <- read_individual_data(year+1, month, 'ineligible')
  ie <- read_individual_data(year+1, month, 'eligible')
  births_y1 <- rbind(ii[which(ii$assistance_no %in% data$assistance_no), c('assistance_no', 'age')],
                     ie[which(ie$assistance_no %in% data$assistance_no), c('assistance_no', 'age')])
  rm(ii, ie)
  
  births_y1 <- births_y1[which(births_y1$age == 0),]
  births_y1[['birth_y1']] <- 1
  
  births_y1[['age']] <- NULL
  births_y1 <- births_y1 %>% dplyr::group_by(assistance_no) %>%
               dplyr::summarise(birth_y1 = sum(birth_y1))
  
  data <- merge(data, births_y1[,c('assistance_no', 'birth_y1')], by='assistance_no', all.x=TRUE)
  data[which(is.na(data$birth_y1)), 'birth_y1'] <- 0 
  
  return(data)
}

full_preprocessed_dataset <- function(year, month){
  data_unique_f_y0 <- merge(matching_data_preprocess(year, month), unique_f_ids(year, month), by='assistance_no', all.y=TRUE)
  print(paste('Unique_f_y0: _ ineligible:', str(nrow(data_unique_f_y0[which(data_unique_f_y0$eligible==0),])))) 
  print(paste('Unique_f_y0: _ eligible:',  str(nrow(data_unique_f_y0[which(data_unique_f_y0$eligible==1),]))))
  data_unique_f_y1 <-  merge(matching_data_preprocess(year+1, month), unique_f_ids(year+1, month), by='assistance_no', all.y=TRUE)
  data <- data_unique_f_y0[which(data_unique_f_y0$assistance_no %in% data_unique_f_y1$assistance_no),]
  print(paste('data: _ ineligible:', str(nrow(data[which(data$eligible==0),])))) 
  print(paste('data: _ eligible:',  str(nrow(data[which(data$eligible==1),]))))
  data <- births_y1(data)
  return(data)
}




