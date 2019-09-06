library(dplyr)
library(plyr)
library(lubridate)


######### I - FROM INDIVIDUAL DATABASES 

# 1 - Nationality of household head

add_nationality <- function(df){
  nationalities <- read.csv(paste(data_path, 'Master Data/Countries.csv', sep='/'), sep=',')
  nationalities[['nat_country']] = 'Other'
  nationalities[which(nationalities$code=='AF'), 'nat_country'] = 'Afghanistan'
  nationalities[which(nationalities$code=='IQ'), 'nat_country'] = 'Iraq'
#  nationalities[which(nationalities$code=='IR'), 'nat_country'] = 'Iran'
  nationalities[which(nationalities$code=='SY'), 'nat_country'] = 'Syria'
  nationalities <- setNames(nationalities,c('nationality_id', 'name', 'code', 'mernis_code', 'active', 'nat_country'))
  df <- merge(x=df, y=nationalities[,c('nationality_id', 'nat_country')], all.x=TRUE)
  return(df)
}

nat_hh_head <- function(df_i, df_h){
  nat <- df_i[which(df_i$main_applicant_flag==1), c('assistance_no', 'nationality_id')]
  df_h <- add_nationality(merge(x=df_h, y=nat, by='assistance_no', all.x=TRUE))
  return(df_h)
}

# 2 - Number of hh members by age groups
# TODO : see pb of ages up to 221
get_age_groups <- function(df){
  df[['age_group']] <- cut(df$age, c(0, 10, 18, 45, 60, 300), right=FALSE, labels=FALSE)
  df[['AG_1']] <- (df$age_group==1)
  df[['AG_2']] <- (df$age_group==2)
  df[['AG_3']] <- (df$age_group==3)
  df[['AG_4']] <- (df$age_group==4)
  df[['AG_5']] <- (df$age_group==5)
  df[['age_group']] = NULL
  age_groups <- setNames(aggregate(df[, c('AG_1', 'AG_2', 'AG_3', 'AG_4', 'AG_5')], 
                                   by = list(df$assistance_no), FUN = sum),
                         c('assistance_no', 'AG_1', 'AG_2', 'AG_3', 'AG_4', 'AG_5'))
  return(age_groups)
}

age_groups_hh <- function(df_i, df_h){
  df_h <- merge(x=df_h, y=get_age_groups(df_i), by='assistance_no', all.x=TRUE)
  return(df_h)
}


# 3 - Gender and age of respondent

age_and_gender_hh_head <- function(df_i, df_h){
  df_i[['age_group']] <- cut(df_i$age, c(0, 10, 18, 45, 60, 300), right=FALSE, labels=FALSE)
  gender_and_age <- plyr::rename(df_i[which(df_i$main_applicant_flag==1), c('assistance_no', 'gender', 'age_group')],
                                 c('gender'='gender_main_resp', 'age_group'='age_group_main_resp'))
  df_h <- merge(x=df_h, y=gender_and_age, by='assistance_no', all.x=TRUE)
  return(df_h)
}



# 4 - Global function

get_individual_data_informations <- function(df_i, df_h){
  return(age_and_gender_hh_head(df_i, age_groups_hh(df_i, nat_hh_head(df_i, df_h))))
}


######### II - FROM HOUSEHOLD DATABASE

# 1 - Months since application

months_since_application <- function(df_h){
  df_h$date <- lubridate::ymd(paste(year, month, '01', sep='/'))
  df_h$months_since_application <- lubridate::interval(df_h$application_date, df_h$date) %/% months(1)
  df_h$date = NULL
  return(df_h)
}


# 2 - Region
add_regions <- function(df_h){
  regions = readRDS('./Data/Processed/regions.rds')
  df_h <- merge(df_h, regions,  by='province_id', all.x=TRUE)
  return(df_h)
}


# 3 - Number of children
num_children <- function(df_h){
  df_h[['num_children']] = df_h$num_male_children + df_h$num_female_children
  return(df_h)
}


# 4 - Global function

get_household_data_information <- function(df_h){
  return(months_since_application(add_regions(num_children(df_h))))
}


######## III - PREPROCESS FUNCTION

harmonize_data <- function(df_i, df_h){
  return(get_household_data_information(get_individual_data_informations(df_i, df_h)))
}


######## IV - FULL DATABASE

matching_data_preprocess <- function(year, month){
  ii <- read_individual_data(year, month, 'ineligible')
  hi <- read_household_data(year, month, 'ineligible')
  ie <- read_individual_data(year, month, 'eligible')
  he <- read_household_data(year, month, 'eligible')
  
  he <- harmonize_data(ie, he)
  hi <- harmonize_data(ii, hi)
  
  he[['eligible']] = 1
  hi[['eligible']] = 0
  vars <- c('assistance_no',
            'eligible',
            'AC_1', 'AC_2', 'AC_3', 'AC_4', 'AC_5', 'AC_6',
            'Reg', 
            'num_children',
            'months_since_application', 
            'nat_country',
            'AG_1', 'AG_2', 'AG_3', 'AG_4', 'AG_5',
            'age_group_main_resp'
            #'gender_main_resp'
            )
  
  return(rbind(he[, vars], hi[,vars]))
}
