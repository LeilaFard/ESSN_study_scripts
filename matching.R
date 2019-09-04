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

year = 2018
month = 4

outputs_matching = paste('./Outputs/PsMatch', year, month, sep='/')
dir.create(outputs_matching, recursive=TRUE)

data <- matching_data_preprocess(year, month)

data_ <- data
#data_ <- data[sample(nrow(data), (nrow(data)/10)),]


######### I - MODEL SPECIFICATION

# 1 - Model with all the data

m_ps <- glm(eligible ~ AC_1 + AC_6 + 
              months_since_application +  nat_country + Reg +
              AG_1 + AG_2 + AG_3 + AG_4 + AG_5, 
            #+ gender_main_resp,
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
m_ps_trim <- glm(eligible ~ AC_1 + AC_6 + 
                   months_since_application +  nat_country + Reg +
                   AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,
                 #+ age_group_main_resp + gender_main_resp,
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
          theme_bw()
ggsave(filename = paste(outputs_matching, 'ps_score_trimmed_df.png', sep='/'), plot,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')


######### II - MATCHING

## Stratified
mod_match_strat <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                             AG_1 + AG_2 + AG_3 + AG_4 + AG_5 , distance = 'logit',
                           method = 'subclass', data = data_trim, subclass=16) 
# Post processing
summary(mod_match_strat)
#Histogram
plot(mod_match_strat, type='hist')
#plot(mod_match_strat, type='QQ')



## Increase ratio -> bias/variance to compennsate replacement
mod_match_2NN <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                         AG_1 + AG_2 + AG_3 + AG_4 + AG_5 ,  distance = 'logit',
                         method = 'nearest', data = data_trim, replace=TRUE, ratio=2) 
# Post processing
summary(mod_match_2NN)
#Histogram
plot(mod_match_2NN, type='hist')
plot(mod_match_2NN, type='QQ')


# TODO: use t-tests within each strata to test if the distribution of X-variables is the same within both groups 


