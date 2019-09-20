library(MatchIt)

# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
# https://gking.harvard.edu/matchit

remove(list = ls())

path <- dirname(rstudioapi::getSourceEditorContext()$path)

source(paste(path, 'settings.R', sep='/'))


year = 2018
month = 4

source(paste(tools, 'matching_preprocess.R', sep=''))

data_ <- full_preprocessed_dataset(year, month)

nchildren_own <- function(year, month, data){
  data[which(data$age_w > 18), 'nc_own_1'] = data[which(data$age_w > 18), 'num_children']
  a = data[which(data$age_w <= 18), 'assistance_no']
  ii <- read_individual_data(year, month, 'ineligible')
  enf <- setNames(data.frame(table(ii[which((ii$assistance_no%in%a)&(ii$age<6)), 'assistance_no'])), c('assistance_no', 'nc_own_2'))
  data <- merge(data, enf, by='assistance_no')
  data[['num_children_own']] <- rowSums(data[,c('nc_own_1', 'nc_own_2')], na.rm=TRUE)
  data$nc_own_1 <- NULL
  data$nc_own_2 <- NULL
  
  return(data)
}

source(paste(tools, 'plots_discarded_comp.R', sep='/'))

data_[['ineligible']] <- 1-data_$eligible
data_ <- nchildren_own(year, month, data_)

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
#plot(mod_match_2NN, type='QQ')

treat_effect <- function(match_outputs_2NN){
  m.data_2NN <- match.data(match_outputs_2NN, distance ='pscore')
  m.data_2NN['index'] <- rownames(m.data_2NN)
  
  matched_ids <- data.frame(match_outputs_2NN$match.matrix)
  matched_ids['X0'] <- rownames(matched_ids)
  
  outcomes.0 <- setNames(m.data_2NN[, c('index', 'birth_y1')], c('X0', 'birth_y1.0'))
  outcomes.1 <- setNames(m.data_2NN[, c('index', 'birth_y1')], c('X1', 'birth_y1.1'))
  outcomes.2 <- setNames(m.data_2NN[, c('index', 'birth_y1')], c('X2', 'birth_y1.2'))
  
  matched_data <- merge(matched_ids, outcomes.0, by='X0', all.x=TRUE)
  matched_data <- merge(matched_data, outcomes.1, by='X1', all.x=TRUE)
  matched_data <- merge(matched_data, outcomes.2, by='X2', all.x=TRUE)
  
  matched_data <- matched_data[complete.cases(matched_data), ]
  
  matched_data[['teffect']] <- matched_data$birth_y1.0 - (1/2)*matched_data$birth_y1.1 - (1/2)*matched_data$birth_y1.2
  print(paste('Mean = ', mean(matched_data$teffect)))
  print(paste('Std = ', sd(matched_data$teffect)))
  print(paste('IC = [', paste(quantile(matched_data$teffect, c(0.025, 0.975)), collapse=' ; '), ']', sep=''))
}

treat_effect(mod_match_2NN_att)
treat_effect(mod_match_2NN_tnt)



z.out1 <- zelig(birth_y1 ~ eligible, data = match.data(mod_match_2NN_tnt, "control"), model = "ls")

x.out1 <- setx(z.out1, data = match.data(mod_match_2NN_tnt, "treat"), cond = TRUE)
s.out1 <- sim(z.out1, x = x.out1)


#### ADD REGRESSION


## Stratified
mod_match_strat <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                             AG_1 + AG_2 + AG_3 + AG_4 + AG_5 , distance = 'logit', method = 'subclass', 
                           data = data_, subclass=16, discard='both', reestimate=TRUE) 

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
