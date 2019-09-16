# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
# https://gking.harvard.edu/matchit

remove(list = ls())

path <- dirname(rstudioapi::getSourceEditorContext()$path)

source(paste(path, 'settings.R', sep='/'))

source(paste(tools, 'format_data.R', sep='/'))
source(paste(tools, 'matching_preprocess.R', sep=''))

library(stats)
library(ggplot2)
library(MatchIt)
library(xtable)
#library(MatchItSE) 

year = 2018
month = 4


print(paste(month, year, sep='/'))

#outputs_matching = paste('./Outputs/PsMatch', year, month, sep='/')
outputs_matching = paste('./Outputs/PsMatch_test', year, month, sep='/')
dir.create(outputs_matching, recursive=TRUE)


unique_f <- function(year, month, status){
  df_i <- read_individual_data(year, month, status)
  
  df_iff<- df_i[which(df_i$gender=='female' & df_i$age>=15 & df_i$age<45),]
  iffs <- df_iff[which(duplicated2(df_iff$assistance_no)==FALSE),]
  iffs <-  setNames(data.frame(iffs$assistance_no), c('assistance_no'))
  
  return(iffs)
}

unique_f_ids <- function(year, month){
  fff_e <- unique_f(year, month, 'eligible')
  fff_i <- unique_f(year, month, 'ineligible')
  return(rbind(fff_i, fff_e))
}

data_unique_f_y0 <- merge(matching_data_preprocess(year, month), unique_f_ids(year, month), by='assistance_no', all.y=TRUE)
data_unique_f_y1 <-  merge(matching_data_preprocess(year+1, month), unique_f_ids(year+1, month), by='assistance_no', all.y=TRUE)


data_ <- data_unique_f_y0[which(data_unique_f_y0$assistance_no %in% data_unique_f_y1$assistance_no),]
#data_ <- data[sample(nrow(data), (nrow(data)/10)),]

births_y1 <- function(data){
  ii <- read_individual_data(year+1, month, 'ineligible')
  ie <- read_individual_data(year+1, month, 'eligible')
  births_y1 <- rbind(ii[which(ii$assistance_no %in% data$assistance_no), c('assistance_no', 'age')],
                     ie[which(ie$assistance_no %in% data$assistance_no), c('assistance_no', 'age')])
  rm(ii, ie)
  
  births_y1 <- births_y1[which(births_y1$age == 0),]
  births_y1[['birth_y1']] <- 1
  data <- merge(data, births_y1[,c('assistance_no', 'birth_y1')], by='assistance_no', all=TRUE)
  data[which(is.na(data$birth_y1)), 'birth_y1'] <- 0 
  
  return(data)
}

data_ <- births_y1(data_)
# REMOVE MISSING VALUES
data_ <- data_[complete.cases(data_), ]

# TODO: use t-tests within each strata to test if the distribution of X-variables is the same within both groups 

#FROM : https://arxiv.org/pdf/1707.05835.pdf
#Matching or weighting based on the subgroup-fitted propensity scores is expected to lead to
#better balance of covariates within each subgroup and thus smaller biases in causal estimates.
#However, due to the smaller sample sizes of the subgroups, the ensuing causal estimates may
#have larger variances.

######### II - MATCHING

## Stratified
mod_match_strat <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                           AG_1 + AG_2 + AG_3 + AG_4 + AG_5 , distance = 'logit', method = 'subclass', 
                           data = data_, subclass=16, discard='treat', reestimate=TRUE) 
# Post processing
summary(mod_match_strat)
#Histogram
plot(mod_match_strat, type='hist')
#plot(mod_match_strat, type='QQ')
z_out_strat <- zelig(birth_y1 ~ distance, 
                     data = match.data(mod_match_strat, 'control'), 
                     model = 'ls', by = 'subclass')
x_out_strat <- setx(z_out_strat, data = match.data(mod_match_strat, 'treat'), fn = NULL, cond = TRUE)
s_out_strat <- sim(z_out_strat, x = x_out_strat)
summary(s_out_strat)

## Increase ratio -> bias/variance to compennsate replacement
mod_match_2NN <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                         AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                         method = 'nearest', data = data_, discard='treat', reestimate=TRUE, 
                         replace=TRUE, ratio=2) 

# Post processing
summary(mod_match_2NN)
#Histogram
plot(mod_match_2NN, type='hist')
#plot(mod_match_2NN, type='QQ')
z_out_2NN <- zelig(birth_y1 ~ eligible, data = match.data(mod_match_2NN), 
                   model = 'ls')
x_out_2NN <- setx(z_out_2NN, data = match.data(mod_match_2NN, "control"), cond = TRUE)
s_out_2NN <- sim(z_out_2NN, x = x_out_2NN)

ATE_2NN <- c(s_out_2NN$qi$att.ev, -s_out_2NN$qi$att.ev)
mean(ATE_2NN)
sd(ATE_2NN)
quantile(ATE_2NN, c(0.025, 0.975))









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
