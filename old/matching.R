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


data_ <- data_[complete.cases(data_), ]
source(paste(tools, 'plots_discarded_comp.R', sep='/'))

# TODO: use t-tests within each strata to test if the distribution of X-variables is the same within both groups 

#FROM : https://arxiv.org/pdf/1707.05835.pdf
#Matching or weighting based on the subgroup-fitted propensity scores is expected to lead to
#better balance of covariates within each subgroup and thus smaller biases in causal estimates.
#However, due to the smaller sample sizes of the subgroups, the ensuing causal estimates may
#have larger variances.

######### II - MATCHING


## Increase ratio -> bias/variance to compennsate replacement
mod_match_2NN_att <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                         AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                         method = 'nearest', data = data_, discard='both', reestimate=TRUE, 
                         replace=TRUE, ratio=2) 

mod_match_2NN_tnt <- matchit(ineligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                               AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                             method = 'nearest', data = data_, discard='both', reestimate=TRUE, 
                             replace=TRUE, ratio=2) 


# Post processing
summary(mod_match_2NN_att)
summary(mod_match_2NN_tnt)
#Histogram
plot(mod_match_2NN_att, type='hist')
plot(mod_match_2NN_tnt, type='hist')


treat_effect_diff <- function(match_outputs_2NN){
  match.data=match.data(match_outputs_2NN)
  matches <- data.frame(match_outputs_2NN$match.matrix)
  
  group0<-match(row.names(matches),row.names((match.data)))
  group1<-match(matches$X1,row.names(match.data))
  group2<-match(matches$X2,row.names(match.data))
  yT<-match.data$birth_y1[group0]
  yC<-(match.data$birth_y1[group1]+match.data$birth_y1[group2])*(1/2)
  matched.cases<-cbind(matches,yT,yC)
  return(t.test(matched.cases$yT,matched.cases$yC,paired= TRUE))
}

treat_effect <- function(match_outputs_2NN){
  match.data=match.data(match_outputs_2NN)
  matches <- data.frame(match_outputs_2NN$match.matrix)
  
  group0<-match(row.names(matches),row.names((match.data)))
  group1<-match(matches$X1,row.names(match.data))
  group2<-match(matches$X2,row.names(match.data))
  yT<-match.data$birth_y1[group0]
  yC<-(match.data$birth_y1[group1]+match.data$birth_y1[group2])*(1/2)
  matched.cases<-cbind(matches,yT,yC)
  return(t.test(matched.cases$yT,matched.cases$yC,paired= TRUE))
}

ATT <- treat_effect(mod_match_2NN_att)
TNT <- treat_effect(mod_match_2NN_tnt)

####  REGRESSION


match.data.att=match.data(mod_match_2NN_att)
lm_treat1 <- lm(birth_y1 ~ eligible, data = match.data.att)
out.tex = xtable(lm_treat1)
print(out.tex, type='latex', file=paste(outputs_matching, 'lm_att_1.tex', sep='/'), compress = FALSE) 
rm(out.tex)

lm_treat2 <- lm(birth_y1 ~ eligible + married + num_children_own + age_w + age_w*num_children_own, data = match.data.att)

out.tex = xtable(lm_treat2)
print(out.tex, type='latex', file=paste(outputs_matching, 'lm_att_2.tex', sep='/'), compress = FALSE) 
rm(out.tex)

match.data.tnt=match.data(mod_match_2NN_tnt)
lm_treat1 <- lm(birth_y1 ~ ineligible, data = match.data.tnt)
out.tex = xtable(lm_treat1)
print(out.tex, type='latex', file=paste(outputs_matching, 'lm_tnt_1.tex', sep='/'), compress = FALSE) 
rm(out.tex)
match.data.tnt['ch2'] = match.data.tnt$num_children==2
match.data.tnt['ch3'] = match.data.tnt$num_children==3
lm_treat2 <- lm(birth_y1 ~ ineligible + married + num_children_own + age_w + age_w*num_children_own , data = match.data.tnt)
out.tex = xtable(lm_treat2)
print(out.tex, type='latex', file=paste(outputs_matching, 'lm_tnt_2.tex', sep='/'), compress = FALSE) 
rm(out.tex)

## Stratified
mod_match_strat <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                             AG_1 + AG_2 + AG_3 + AG_4 + AG_5 , distance = 'logit', method = 'subclass', 
                           data = data_, subclass=16, discard='both', reestimate=TRUE) 

match.data.att['ch0'] = match.data.att$num_children==0
match.data.att['ch1'] = match.data.att$num_children==1
match.data.att['ch2'] = match.data.att$num_children==2
match.data.att['ch3'] = match.data.att$num_children==3
match.data.att['ch4'] = match.data.att$num_children==4
match.data.att['ch5'] = match.data.att$num_children==5
match.data.att['chsup'] = match.data.tnt$num_children>5
lmtest <- lm(birth_y1 ~ ineligible + married + num_children_own + age_w + ch0+ch1+ch2+ch3+ch4+ch5+chsup, data = match.data.att)
summary(lmtest)



data_['ch0'] = data_$num_children==0
data_['ch1'] = data_$num_children==1
data_['ch2'] = data_$num_children==2
data_['ch3'] = data_$num_children==3
data_['ch4'] = data_$num_children==4
data_['ch5'] = data_$num_children==5
data_['chsup'] = data_$num_children>5
lmtest <- lm(birth_y1 ~ married+ ch0+ch1+ch3+ch4+ch5+chsup + age_w , data = data_[which(data_$eligible==1),]) #
summary(lmtest)


# Post processing
summary(mod_match_strat)
#Histogram
plot(mod_match_strat, type='hist')

### Number of children

data_1ch <- data_[which(data_$num_children==1),]
mod_match_2NN_1ch <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                             AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                             method = 'nearest', data = data_1ch, discard='treat', reestimate=TRUE, 
                             replace=TRUE, ratio=2) 
plot(mod_match_2NN_1ch, type='hist')

ATT(mod_match_2NN_1ch)


data_2ch <- data_[which(data_$num_children==2),]
mod_match_2NN_2ch <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                               AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                             method = 'nearest', data = data_2ch, discard='treat', reestimate=TRUE, 
                             replace=TRUE, ratio=2) 
plot(mod_match_2NN_2ch, type='hist')
ATT(mod_match_2NN_2ch)

data_3ch <- data_[which(data_$num_children==3),]
mod_match_2NN_3ch <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                               AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                             method = 'nearest', data = data_3ch, discard='treat', reestimate=TRUE, 
                             replace=TRUE, ratio=2) 
plot(mod_match_2NN_3ch, type='hist')

ATT(mod_match_2NN_3ch)





######### I - MODEL SPECIFICATION

# 1 - Model with all the data

m_ps <- glm(eligible ~ AC_1 + AC_6 + 
              months_since_application +  nat_country + Reg +
              AG_1 + AG_2 + AG_3 + AG_4 + AG_5, 
            #+ age_group_main_resp,
            family=binomial(link='logit'), data = data_)
out.tex = xtable(m_ps)
print(out.tex, type='latex', file=paste(outputs_matching, 'logit_full_df.tex', sep='/'), compress = FALSE) 
rm(out.tex)
prs_df <- data.frame(pr_score = predict(m_ps, type = 'response'),
                     eligible = m_ps$model$eligible,
                     AC_1 = m_ps$model$AC_1,
                     AC_6 = m_ps$model$AC_6,
                     months_since_application = m_ps$model$months_since_application,
                     nat_country = m_ps$model$nat_country,
                     Reg = m_ps$model$Reg,
                     AG_1 = m_ps$model$AG_1,
                     AG_2 = m_ps$model$AG_2, 
                     AG_3 = m_ps$model$AG_3,
                     AG_4 = m_ps$model$AG_4,
                     AG_5 = m_ps$model$AG_5
                     #age_group_main_resp = m_ps$model$age_group_main_resp
                     #gender_main_resp = m_ps$model$gender_main_resp
)
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

# 2 - Trimming 
data_trim = prs_df[which(prs_df$pr_score<0.99),]
m_ps_trim <- glm(eligible ~ AC_1 + AC_6 +  months_since_application +  nat_country + Reg + AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,
                 family=binomial(link='logit'), data = data_trim)

out.tex = xtable(m_ps)
print(out.tex, type='latex', file=paste(outputs_matching, 'logit_trimmed_df.tex', sep='/'), compress = FALSE) 
rm(out.tex)
prs_df_trim <- data.frame(pr_score = predict(m_ps_trim, type = 'response'),
                          eligible = m_ps_trim$model$eligible)
plot <- prs_df_trim %>%
  mutate(eligible = ifelse(eligible==1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = 'white') +
  facet_wrap(~eligible) +
  xlab('Probability of being eligible, trimmed dataset') + 
  theme_light()
ggsave(filename = paste(outputs_matching, 'ps_score_trimmed_df.png', sep='/'), plot,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')
