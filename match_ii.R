library(MatchIt)
library(Zelig)
library(lubridate)

# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
# https://gking.harvard.edu/matchit

remove(list = ls())

path <- dirname(rstudioapi::getSourceEditorContext()$path)

source(paste(path, 'settings.R', sep='/'))


year = 2019
month = 4

source(paste(tools, 'matching_preprocess.R', sep=''))


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
            'int1', 'int2', 'int3'
            #'birth_child1', 'birth_child2', 'birth_child3'
            #'age_group_main_resp'
            #'gender_main_resp'
  )
  
  return(rbind(he[, vars], hi[,vars]))
}

full_preprocessed_dataset <- function(year, month){
  return(merge(matching_data_preprocess(year, month), unique_f_ids(year, month), by='assistance_no', all.y=TRUE))
}


source(paste(tools, 'find_first_application.R', sep=''))

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


data_ <- full_preprocessed_dataset(year, month)
data_[['ineligible']] <- 1-data_$eligible
data_ <- nchildren_own(year, month, data_)

data_[['month']] = month
data_[['year']] = year

dates = list(c(3,2019), c(2,2019), c(1,2019), c(12,2018), c(11, 2018), c(10, 2018), c(9,2018), c(8,2018), c(7,2018), c(6,2018), c(5, 2018), c(4,2018))
for (d in dates){
  month = d[[1]]
  year = d[[2]]
  data <- full_preprocessed_dataset(year, month)
  data[['ineligible']] <- 1-data$eligible
  data <- nchildren_own(year, month, data)
  data <- data[which(!data$assistance_no%in%data_$assistance_no),]
  
  data[['month']] = month
  data[['year']] = year
  
  data_ <- rbind(data_, data)
}

for (int in list('int1', 'int2', 'int3')){
  data_[which(data_[[int]]>168), int] = NA
  data_[which(data_[[int]]<10), int] = NA
}


data_i1 <- data_[which(!is.na(data_$int1)),]
data_i1[['int2']] <- NULL
data_i1[['int3']] <- NULL
data_i1 <- data_i1[complete.cases(data_i1),]
data_i1 <- plyr::rename(data_i1, c('int1'='int'))

  
data_i2 <- data_[which(!is.na(data_$int1)),]
data_i2[['int1']] <- NULL
data_i2[['int3']] <- NULL
data_i2 <- data_i2[complete.cases(data_i2),]
data_i2 <- plyr::rename(data_i2, c('int2'='int'))


data_i3 <- data_[which(!is.na(data_$int3)),]
data_i3[['int1']] <- NULL
data_i3[['int2']] <- NULL
data_i3 <- data_i3[complete.cases(data_i3),]
data_i3 <- plyr::rename(data_i3, c('int3'='int'))



desc_stats_covariates <- function(data){
  
  todesc =  data[, c('eligible', 'AC_1', 'months_since_application', 'nat_country', 'Reg', 'AG_1', 'AG_2', 'AG_3', 'AG_4', 'AG_5')]
  
  todesc$Afghanistan =ifelse(todesc$nat_country=='Afghanistan', 1, 0)
  todesc$Iraq =ifelse(todesc$nat_country=='Iraq', 1, 0)
  todesc$Other =ifelse(todesc$nat_country=='Other', 1, 0)
  todesc$Syria =ifelse(todesc$nat_country=='Syria', 1, 0)
  todesc$nat_country <- NULL
  
  todesc$AG  =ifelse(todesc$Reg=='AG', 1, 0)
  todesc$BS =ifelse(todesc$Reg=='BS', 1, 0)
  todesc$CA =ifelse(todesc$Reg=='CA', 1, 0)
  todesc$EA =ifelse(todesc$Reg=='EA', 1, 0)
  todesc$MD =ifelse(todesc$Reg=='MD', 1, 0)
  todesc$MM =ifelse(todesc$Reg=='MM', 1, 0)
  todesc$SE =ifelse(todesc$Reg=='SE', 1, 0)
  todesc$Reg <- NULL
  
  todesc_C <- todesc[which(todesc$eligible==0),]
  todesc_C$eligible <- NULL 
  todesc_T <- todesc[which(todesc$eligible==1),]
  todesc_T$eligible <- NULL
  
  rm(todesc)
  
  L <- colnames(todesc_T)
  des = data.frame(matrix(ncol=5, nrow=length(L)))
  colnames(des) <- c('var', 'mean_c', 'sd_c', 'mean_t', 'sd_t')
  des$var=L
  
  for (var in L){
    des[which(des$var==var), 'mean_c'] = round(mean(todesc_C[[var]]), digits=3)
    des[which(des$var==var), 'sd_c'] = round(sd(todesc_C[[var]]), digits=3)
    des[which(des$var==var), 'mean_t'] = round(mean(todesc_T[[var]]), digits=3)
    des[which(des$var==var), 'sd_t'] = round(sd(todesc_C[[var]]), digits=3)
  }
  
  des$delta = (des$mean_t-des$mean_c)/sqrt(des$sd_t^2+des$sd_c^2)
  
  return(des)
  
}


des <- desc_stats_covariates(data_i1)
out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_cov_data_i1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

des <- desc_stats_covariates(data_i2)
out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_cov_data_i2.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

des <- desc_stats_covariates(data_i2)
out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_cov_data_i3.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

## Increase ratio -> bias/variance to compennsate replacement
mod_match_2NN_int1 <- matchit(eligible ~ AC_1 + months_since_application + nat_country + Reg +
                              AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                              method = 'nearest', data = data_i1, discard='both', reestimate=TRUE, 
                              replace=TRUE, ratio=2)

mod_match_2NN_int2 <- matchit(eligible ~ AC_1 +months_since_application + nat_country + Reg +
                                AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                              method = 'nearest', data = data_i2, discard='both', reestimate=TRUE, 
                              replace=TRUE, ratio=2)

mod_match_2NN_int3 <- matchit(eligible ~ AC_1 + months_since_application + nat_country + Reg +
                                AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                              method = 'nearest', data = data_i3, discard='both', reestimate=TRUE, 
                              replace=TRUE, ratio=2)




difference_in_means <- function(match_outputs){
  match.data=match.data(match_outputs)
  matches <- data.frame(match_outputs$match.matrix)
  
  group0<-match(row.names(matches),row.names((match.data)))
  group1<-match(matches$X1,row.names(match.data))
  group2<-match(matches$X2,row.names(match.data))
  yT<-match.data$int[group0]
  yC<-(match.data$int[group1]+match.data$birth_y1[group2])*(1/2)
  matched.cases<-cbind(matches,yT,yC)
  return(t.test(matched.cases$yT,matched.cases$yC,paired= TRUE))
}

reg_no_covariates <- function(match_outputs, name){
  match.data = match.data(match_outputs)
  lm_treat1 <- lm(int ~ eligible, data = match.data)
  out.tex = xtable(lm_treat1)
  print(out.tex, type='latex', file=paste(outputs_matching, '/', name, '_nocov.tex', sep=''), compress = FALSE) 
  rm(out.tex)
  return(summary(lm_treat1))
}

reg_with_covariates <- function(match_outputs, name){
  match.data = match.data(match_outputs)
  lm_treat2 <- lm(int ~ eligible + married + age_w + nat_country, data = match.data)
  out.tex = xtable(lm_treat2)
  print(out.tex, type='latex', file=paste(outputs_matching, '/', name, '_cov.tex', sep=''), compress = FALSE) 
  rm(out.tex)
  return(summary(lm_treat2))
}


des <- desc_stats_covariates(match.data(mod_match_2NN_int1))

out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_i1_T1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_int1, type='hist')

#difference_in_means(mod_match_2NN_int1)
reg_no_covariates(mod_match_2NN_int1, '2NN_int1_T1')
reg_with_covariates(mod_match_2NN_int1, '2NN_int1_T1')



des <- desc_stats_covariates(match.data(mod_match_2NN_int2))
out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_i2_T1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_int2, type='hist')

#difference_in_means(mod_match_2NN_int2)
reg_no_covariates(mod_match_2NN_int2, '2NN_int2_T1')
reg_with_covariates(mod_match_2NN_int2, '2NN_int2_T1')



des <- desc_stats_covariates(match.data(mod_match_2NN_int3))
out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_i3_T1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_int3, type='hist')

#difference_in_means(mod_match_2NN_int3)
reg_no_covariates(mod_match_2NN_int3, '2NN_int3_T1')
reg_with_covariates(mod_match_2NN_int3, '2NN_int3_T1')


### TO

## Increase ratio -> bias/variance to compennsate replacement
mod_match_2NN_int1_0 <- matchit(ineligible ~ AC_1 + months_since_application + nat_country + Reg +
                                AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                              method = 'nearest', data = data_i1, discard='both', reestimate=TRUE, 
                              replace=TRUE, ratio=2)

mod_match_2NN_int2_0 <- matchit(ineligible ~ AC_1 +months_since_application + nat_country + Reg +
                                AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                              method = 'nearest', data = data_i2, discard='both', reestimate=TRUE, 
                              replace=TRUE, ratio=2)

mod_match_2NN_int3_0 <- matchit(ineligible ~ AC_1 + months_since_application + nat_country + Reg +
                                AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                              method = 'nearest', data = data_i3, discard='both', reestimate=TRUE, 
                              replace=TRUE, ratio=2)


des <- desc_stats_covariates(match.data(mod_match_2NN_int1_0))

out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_i1_T0.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_int1_0, type='hist')

#difference_in_means(mod_match_2NN_int1)
reg_no_covariates(mod_match_2NN_int1_0, '2NN_int1_T0')
reg_with_covariates(mod_match_2NN_int1_0, '2NN_int1_T0')



des <- desc_stats_covariates(match.data(mod_match_2NN_int2_0))
out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_i2_T0.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_int2_0, type='hist')

#difference_in_means(mod_match_2NN_int2)
reg_no_covariates(mod_match_2NN_int2, '2NN_int2_T0')
reg_with_covariates(mod_match_2NN_int2, '2NN_int2_T0')



des <- desc_stats_covariates(match.data(mod_match_2NN_int3_0))
out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_i3_T0.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_int3_0, type='hist')

#difference_in_means(mod_match_2NN_int3)
reg_no_covariates(mod_match_2NN_int3_0, '2NN_int3_T0')
reg_with_covariates(mod_match_2NN_int3_0, '2NN_int3_T0')



