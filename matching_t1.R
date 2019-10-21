library(MatchIt)
library(Zelig)

# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
# https://gking.harvard.edu/matchit

remove(list = ls())

path <- dirname(rstudioapi::getSourceEditorContext()$path)

source(paste(path, 'settings.R', sep='/'))


year = 2018
month = 4

source(paste(tools, 'matching_preprocess.R', sep=''))

data_ <- full_preprocessed_dataset(year, month)

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

data_[['ineligible']] <- 1-data_$eligible
data_ <- nchildren_own(year, month, data_)


data_ <- data_[complete.cases(data_),]
print(paste('data (complete cases): _ ineligible:', str(nrow(data_[which(data_$eligible==0),])))) 
print(paste('data (complete cases): _ eligible:',  str(nrow(data_[which(data_$eligible==1),]))))


source(paste(tools, 'plots_discarded_comp.R', sep='/'))

# STATS ON COVARIATES
desc_stats_covariates <- function(data){
  todesc =  data[, c('eligible', 'AC_1', 'AC_6', 'months_since_application', 'nat_country', 'Reg', 'AG_1', 'AG_2', 'AG_3', 'AG_4', 'AG_5')]
  
  todesc$Afghanistan = ifelse(todesc$nat_country=='Afghanistan', 1, 0)
  todesc$Iraq = ifelse(todesc$nat_country=='Iraq', 1, 0)
  todesc$Other = ifelse(todesc$nat_country=='Other', 1, 0)
  todesc$Syria = ifelse(todesc$nat_country=='Syria', 1, 0)
  todesc$nat_country <- NULL
  
  todesc$AG = ifelse(todesc$Reg=='AG', 1, 0)
  todesc$BS = ifelse(todesc$Reg=='BS', 1, 0)
  todesc$CA = ifelse(todesc$Reg=='CA', 1, 0)
  todesc$EA = ifelse(todesc$Reg=='EA', 1, 0)
  todesc$MD = ifelse(todesc$Reg=='MD', 1, 0)
  todesc$MM = ifelse(todesc$Reg=='MM', 1, 0)
  todesc$SE = ifelse(todesc$Reg=='SE', 1, 0)
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

des <- desc_stats_covariates(data_)

out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_datatomatch.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)


########## First regressions

# data_['ch0'] = data_$num_children==0
data_['ch1'] = data_$num_children==1
data_['ch2'] = data_$num_children==2
data_['ch3'] = data_$num_children==3
data_['ch4'] = data_$num_children==4
data_['ch5'] = data_$num_children==5
data_['chsup'] = data_$num_children>5



#FROM : https://arxiv.org/pdf/1707.05835.pdf
#Matching or weighting based on the subgroup-fitted propensity scores is expected to lead to
#better balance of covariates within each subgroup and thus smaller biases in causal estimates.
#However, due to the smaller sample sizes of the subgroups, the ensuing causal estimates may
#have larger variances.


#### Naive regressions 

print(mean(data_[which(data_$eligible==1), 'birth_y1']))
print(mean(data_[which(data_$eligible==0), 'birth_y1']))
print(mean(data_[which(data_$eligible==1), 'birth_y1'])-mean(data_[which(data_$eligible==0), 'birth_y1']))

lm_naive <- lm(birth_y1 ~ eligible, data = data_)
out.tex = xtable(lm_naive)
print(out.tex, type='latex', file=paste(outputs_matching, '/lmnaive_nocov.tex', sep=''), compress = FALSE) 
rm(out.tex)
summary(lm_naive)

lm_naive <- lm(birth_y1 ~ eligible + married + ch1 + ch2 + ch3 + ch4 + ch5 + chsup + age_w + nat_country, data = data_)
out.tex = xtable(lm_naive)
print(out.tex, type='latex', file=paste(outputs_matching, '/lmnaive_cov.tex', sep=''), compress = FALSE) 
rm(out.tex)
summary(lm_naive)

### LOGIT

m_ps <- glm(eligible ~ AC_1 + AC_6 + 
              months_since_application +  nat_country + Reg +
              AG_1 + AG_2 + AG_3 + AG_4 + AG_5, 
            #+ age_group_main_resp,
            family=binomial(link='logit'), data = data_)
out.tex = xtable(m_ps)
print(out.tex, type='latex', file=paste(outputs_matching, 'logit_full_df.tex', sep='/'), compress = FALSE) 
rm(out.tex)
prs_df <- data.frame(pr_score = predict(m_ps, type = 'response'),
                     eligible = m_ps$model$eligible)
labs <- paste('Eligibility status:', c('Eligible', 'Ineligible'))
plot <- prs_df %>%
  mutate(eligible = ifelse(eligible == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = 'white') +
  facet_wrap(~eligible) +
  xlab('Probability of being eligible') + 
  theme_light()
ggsave(filename = paste(outputs_matching, 'ps_score_full_df.png', sep='/'), plot,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')




######### II - MATCHING


difference_in_means <- function(match_outputs){
  match.data=match.data(match_outputs)
  matches <- data.frame(match_outputs$match.matrix)
  
  group0<-match(row.names(matches),row.names((match.data)))
  group1<-match(matches$X1,row.names(match.data))
  group2<-match(matches$X2,row.names(match.data))
  yT<-match.data$birth_y1[group0]
  yC<-(match.data$birth_y1[group1]+match.data$birth_y1[group2])*(1/2)
  matched.cases<-cbind(matches,yT,yC)
  return(t.test(matched.cases$yT,matched.cases$yC,paired= TRUE))
}


# REGRESSIONS WITH AND WITHOUT COVARIATES 
reg_no_covariates <- function(match_outputs, name){
  match.data = match.data(match_outputs)
  lm_treat1 <- lm(birth_y1 ~ eligible, data = match.data)
  out.tex = xtable(lm_treat1)
  print(out.tex, type='latex', file=paste(outputs_matching, '/', name, '_nocov.tex', sep=''), compress = FALSE) 
  rm(out.tex)
  return(summary(lm_treat1))
}

reg_with_covariates <- function(match_outputs, name){
  match.data = match.data(match_outputs)
  lm_treat2 <- lm(birth_y1 ~ eligible + married + ch1 + ch2 + ch3 + ch4 + ch5 + chsup + age_w + nat_country, data = match.data)
  out.tex = xtable(lm_treat2)
  print(out.tex, type='latex', file=paste(outputs_matching, '/', name, '_cov.tex', sep=''), compress = FALSE) 
  rm(out.tex)
  return(summary(lm_treat2))
}


## Increase ratio -> bias/variance to compennsate replacement
mod_match_2NN_T1 <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                              AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                            method = 'nearest', data = data_, discard='both', reestimate=TRUE, 
                            replace=TRUE, ratio=2)


plot(mod_match_2NN_T1, type='hist')

des <- desc_stats_covariates(match.data(mod_match_2NN_T1))

out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_matched_2NN_T1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

difference_in_means(mod_match_2NN_T1)

reg_no_covariates(mod_match_2NN_T1, '2NN_T1')

reg_with_covariates(mod_match_2NN_T1, '2NN_T1')



# ESTIMATIONS PAR SOUS GROUPES SELON LE NOMBRE D'ENFANTS

reg_with_covariates2 <- function(match_outputs, name){
  match.data = match.data(match_outputs)
  lm_treat2 <- lm(birth_y1 ~ eligible + married + age_w + nat_country, data = match.data)
  out.tex = xtable(lm_treat2)
  print(out.tex, type='latex', file=paste(outputs_matching, '/', name, '_cov.tex', sep=''), compress = FALSE) 
  rm(out.tex)
  return(summary(lm_treat2))
}

#### T1

data_1ch <- data_[which(data_$num_children==1),]
mod_match_2NN_1ch_T1 <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                                  AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                                method = 'nearest', data = data_1ch, discard='treat', reestimate=TRUE, 
                                replace=TRUE, ratio=2) 

des <- desc_stats_covariates(match.data(mod_match_2NN_1ch_T1))

out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_1ch_T1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_1ch_T1, type='hist')

difference_in_means(mod_match_2NN_1ch_T1)
reg_no_covariates(mod_match_2NN_1ch_T1, '2NN_1ch_T1')
reg_with_covariates2(mod_match_2NN_1ch_T1, '2NN_1ch_T1')


data_2ch <- data_[which(data_$num_children==2),]
mod_match_2NN_2ch_T1 <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                                  AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                                method = 'nearest', data = data_2ch, discard='treat', reestimate=TRUE, 
                                replace=TRUE, ratio=2) 


des <- desc_stats_covariates(match.data(mod_match_2NN_2ch_T1))

out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_2ch_T1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_2ch_T1, type='hist')

difference_in_means(mod_match_2NN_2ch_T1)
reg_no_covariates(mod_match_2NN_2ch_T1, '2NN_2ch_T1')
reg_with_covariates2(mod_match_2NN_2ch_T1, '2NN_2ch_T1')


data_3ch <- data_[which(data_$num_children==3),]
mod_match_2NN_3ch_T1 <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                                  AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                                method = 'nearest', data = data_3ch, discard='treat', reestimate=TRUE, 
                                replace=TRUE, ratio=2) 
des <- desc_stats_covariates(match.data(mod_match_2NN_3ch_T1))

out.tex = xtable(des)
print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_2NN_3ch_T1.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
rm(out.tex)

plot(mod_match_2NN_3ch_T1, type='hist')

difference_in_means(mod_match_2NN_3ch_T1)
reg_no_covariates(mod_match_2NN_3ch_T1, '2NN_3ch_T1')
reg_with_covariates2(mod_match_2NN_3ch_T1, '2NN_3ch_T1')


# BLOCKING
mod_match_strat <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                             AG_1 + AG_2 + AG_3 + AG_4 + AG_5 , distance = 'logit', method = 'subclass', 
                           data = data_, subclass=16, discard='both', reestimate=TRUE) 

plot(mod_match_strat, type='hist')
data.sub <- match.data(mod_match_strat)
# Subclass-specific t-test for given subclass
for(s in 1:max(data.sub$subclass)){
  data_tmp <- data.sub[which(data.sub$subclass == s),]
  tab_desc_tmp <-  desc_stats_covariates(data_tmp)
  out.tex = xtable(tab_desc_tmp)
  print(out.tex, type='latex', file=paste(outputs_matching, '/desc_match_sub_', s, '.tex', sep=''), compress = FALSE, include.rownames=FALSE) 
  rm(out.tex, data_tmp, tab_desc_tmp)
  }
##ATE estimate (combining subclass-specific estimate)
# define N = total number of people 
N <- dim(data.sub)[1]
# Initialize vectors for subclass-specific effects
#(“sub.effect”), variances(“sub.var”), and sample size(“sub.N”) 
sub.effect <- rep(NA, max(data.sub$subclass))
sub.var <- rep(NA, max(data.sub$subclass))
sub.N <- rep(NA, max(data.sub$subclass))
# Run linear regression model within each subclass
for(s in 1:max(data.sub$subclass)){
  tmp <- lm(birth_y1 ~ eligible, data=data.sub, subset=subclass==s)
  sub.effect[s] <- tmp$coef[2]
  sub.var[s] <- summary(tmp)$coef[2,2]^2 
  sub.N[s] <- sum(data.sub$subclass==s) } 
# Calculate overall ATE effect
sum((sub.N/N)*sub.effect)
sqrt(sum((sub.N/N)^2*sub.var))

for(s in 1:max(data.sub$subclass)){
  tmp <- lm(birth_y1 ~ eligible + married + ch1+ ch2 + ch3 + ch4 + ch5 + chsup + age_w + nat_country, data=data.sub, subset=subclass==s)
  sub.effect[s] <- tmp$coef[2]
  sub.var[s] <- summary(tmp)$coef[2,2]^2 
  sub.N[s] <- sum(data.sub$subclass==s) }
# Calculate overall ATE effect
sum((sub.N/N)*sub.effect)
sqrt(sum((sub.N/N)^2*sub.var))


for(s in 1:max(data.sub$subclass)){
  tmp <- t.test(birth_y1~eligible, data=data.sub, subset=subclass==1)
  sub.effect[s] <- tmp$estimate[['mean in group 1']] - tmp$estimate[['mean in group 0']]
  sub.N[s] <- sum(data.sub$subclass==s) }
# Calculate overall ATE effect
sum((sub.N/N)*sub.effect)
