library(dplyr)
library(plyr)
library(lubridate)



#GENERIC 
# TODO ne plus l'avoir en dupliqué

# Fonction qui permet de reperer les doublons
duplicated2 <- function(x){ 
  if (sum(dup <- duplicated(x))==0) 
    return(dup) 
  if (class(x) %in% c('data.frame', 'matrix')) 
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
}

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


# 4 - Gender and age of respondent

age_and_gender_hh_head <- function(df_i, df_h){
  df_i[['age_group']] <- cut(df_i$age, c(0, 10, 18, 45, 60, 300), right=FALSE, labels=FALSE)
  gender_and_age <- plyr::rename(df_i[which(df_i$main_applicant_flag==1), c('assistance_no', 'gender', 'age_group')],
                                 c('gender'='gender_main_resp', 'age_group'='age_group_main_resp'))
  df_h <- merge(x=df_h, y=gender_and_age, by='assistance_no', all.x=TRUE)
  return(df_h)
}

# 5 - Intervalle Intergenesique
intervalle_intergenesique <- function(df_i, df_h){
  
  ### BASE DES INDIVIDUS: ON SE RESTREINT AUX FAMILLES NUCLEAIRES
  #df <- read_individual_data(year, month, dataframe)
  #isolement des menages ou il y a une seule femme en age de procreer
  df_ff <- df_i[which(df_i$gender=='female' & df_i$age>=15 & df_i$age<45),]
  df_ffs <- df_ff[which(duplicated2(df_ff$assistance_no)==FALSE),]
  #on met en reserve les identifants menages ou il n' y a qu une seule femme en age fecond
  no_hh <- plyr::rename(data.frame(df_ffs$assistance_no), c('df_ffs.assistance_no'='assistance_no'))
  
  ### Databases
  df_children <- merge(df_i, no_hh, by='assistance_no', all=FALSE)
  df_children <- df_children[which((df_children$age < 15)|((df_children$age < 18)&(df_children$marital_status=='single'))),]
  
  ### Rank children
  df_children <- dplyr::group_by(df_children, assistance_no)
  df_children <- dplyr::mutate(df_children, child_rank = order(order(age, decreasing=TRUE)))
  
  ### Date of birth
  df_children$date_of_birth <- lubridate::ymd(df_children$date_of_birth)
  
  ### Keep those born during program
  born_during_program <- df_children[which(df_children$date_of_birth >= ymd('2016-12-01')),]
  
  birth_2 <- plyr::rename(born_during_program[which(born_during_program$child_rank==2),][c('assistance_no', 'date_of_birth')],
                          c('date_of_birth'='birth_child2'))
  birth_1_2 <- merge(plyr::rename(df_children[which(df_children$child_rank==1),][c('assistance_no', 'date_of_birth')], c('date_of_birth'='birth_child1')), 
                     birth_2, by='assistance_no', all=FALSE)
  birth_1_2$int1 <- interval(ymd(birth_1_2$birth_child1), ymd(birth_1_2$birth_child2)) %/% months(1)
  
  df_h <- merge(x=df_h, y=birth_1_2[,c('assistance_no', 'int1')], by='assistance_no', all.x=TRUE)
  
  birth_3 <- plyr::rename(born_during_program[which(born_during_program$child_rank==3),][c('assistance_no', 'date_of_birth')],
                          c('date_of_birth'='birth_child3'))
  birth_2_3 <- merge(plyr::rename(df_children[which(df_children$child_rank==2),][c('assistance_no', 'date_of_birth')], c('date_of_birth'='birth_child2')), 
                     birth_3, by='assistance_no', all=FALSE)
  birth_2_3$int2 <- interval(ymd(birth_2_3$birth_child2), ymd(birth_2_3$birth_child3)) %/% months(1)
  
  df_h <- merge(x=df_h, y=birth_2_3[,c('assistance_no', 'int2')], by='assistance_no', all.x=TRUE)
  
  birth_4 <- plyr::rename(born_during_program[which(born_during_program$child_rank==4),][c('assistance_no', 'date_of_birth')],
                          c('date_of_birth'='birth_child4'))
  birth_3_4 <- merge(plyr::rename(df_children[which(df_children$child_rank==3),][c('assistance_no', 'date_of_birth')], c('date_of_birth'='birth_child3')), 
                     birth_4, by='assistance_no', all=FALSE)
  birth_3_4$int3 <- interval(ymd(birth_3_4$birth_child3), ymd(birth_3_4$birth_child4)) %/% months(1)
  df_h <- merge(x=df_h, y=birth_3_4[,c('assistance_no', 'int3')], by='assistance_no', all.x=TRUE)
  
  return(df_h)
}


# 5 - Global function

get_individual_data_informations <- function(df_i, df_h){
  return(intervalle_intergenesique(df_i, age_and_gender_hh_head(df_i, age_groups_hh(df_i, nat_hh_head(df_i, df_h)))))
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
            'AG_1', 'AG_2', 'AG_3', 'AG_4', 'AG_5'
            #'int1', 'int2', 'int3',
            #'birth_child1', 'birth_child2', 'birth_child3'
            #'age_group_main_resp'
            #'gender_main_resp'
  )
  
  return(rbind(he[, vars], hi[,vars]))
}
