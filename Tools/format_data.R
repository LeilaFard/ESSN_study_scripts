library(plyr)
library(lubridate)

# settings.R - if not already defined, run the following lines:
#path <- dirname(rstudioapi::getSourceEditorContext()$path)
#source(paste(path, 'settings.R', sep='/'))

### LOAD BIRTH AND DEATH
get_births <- function(year, month){
  return(readRDS(paste(processed_data_path, 'births_', year, '_', month, '.RDS', sep='')))
}

get_deaths <- function(year, month){
  return(read.csv(paste(processed_data_path, 'deaths_', year, '_', month, '.csv', sep='')))
}

#### ADD DATABASES TO MAIN DATAFRAME
add_births <- function(year, month, data){
  births <- get_births(year, month)
  return(merge(x=data, y=births, by='assistance_no', all.x=TRUE))
}

add_deaths <- function(year, month, data){
  deaths <- get_deaths(year, month)
  return(merge(x=data, y=deaths, by='assistance_no', all.x=TRUE))
}


##### HOUSEHOLD DATA

### Dummify acceptance criteria in databases
acceptance_criteria <- function(df){
  for (i in 1:6){
    df[[paste('AC', i, sep='_')]] = sapply(df$acceptance_criteria, function(x){return(grepl(paste(i, '. ', sep=''), x, fixed=TRUE))})
  }
  return(df)
}

read_household_data <- function(year, month, dataframe, add_births=FALSE, add_deaths=FALSE){
  dataframe_name = paste(dataframe, 'households', sep=' ')
  df <- read.csv(paste(data_path, dataframe_name, '/', year, '/', dataframe_name, ' ', year, '-', month, '.csv', sep=''), sep=',')
  df <- acceptance_criteria(df)
  
  df[['application_date']] <- lubridate::ymd_hms(df$application_date)
  df[['application_date']] <- lubridate::round_date(df$application_date, unit='day')
  
  if (add_births==TRUE){
    df <- add_births(year, month, df)
  }
  if (add_deaths==TRUE){
    df <- add_deaths(year, month, df)
  }
  return(df)
}

##### INDIVIDUAL DATA
read_individual_data <- function(year, month, dataframe){
  dataframe_name = paste(dataframe, 'individuals', sep=' ')
  df <- read.csv(paste(data_path, dataframe_name, '/', year, '/', dataframe_name, ' ', year, '-', month, '.csv', sep=''), sep=',')
  df[['gender']] = factor(((df$gender_column=='Erkek')*1), labels=c('female', 'male'))
  #Marital status column: on prend les 2 premieres lettres pour eviter les bugs puis on recode  (Bekar / Bosanmis / Dul / Evli)
  df[['marital_status']] <- factor(unclass(as.factor(substr(df$marital_status_column, 1, 2))), 
                                   labels = c('single', 'divorced', 'widow', 'married'))
  df[['gender_column']] <- NULL
  df[['marital_status_column']] <- NULL
  
  
  df <- plyr::rename(df, c('age'='age_decl'))
  df$date <- lubridate::ymd(paste(year, month, '01', sep='/'))
  df$date_of_birth <- lubridate::ymd_hms(df$date_of_birth)
  df$date_of_birth <- lubridate::round_date(df$date_of_birth,unit='day')
  
  df$age <- lubridate::interval(df$date_of_birth, df$date) %/% years(1)
  return(df)
}


#df <- plyr::rename(df, c('age'='age_decl'))
#df$age <- age2(year, month, df) 
