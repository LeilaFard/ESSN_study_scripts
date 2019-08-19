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

year = 2018
month = 4

data <- matching_data_preprocess(year, month)

data_ <- data[sample(nrow(data), (nrow(data)/10)),]
  
  
######### I - MODEL SPECIFICATION

# 1 - Model with all the data

m_ps <- glm(eligible ~ AC_1 + AC_6 + 
              months_since_application +  nat_country + Reg +
              AG_1 + AG_2 + AG_3 + AG_4 + AG_5 +
              age_main_resp + gender_main_resp,
            family=binomial(link='logit'), data = data_)
summary(m_ps)
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
                     AG_5 = m_ps$model$AG_5,
                     age_main_resp = m_ps$model$age_main_resp,
                     gender_main_resp = m_ps$model$gender_main_resp
)
labs <- paste('Eligibility status:', c('Eligible', 'Ineligible'))
prs_df %>%
  mutate(eligible = ifelse(eligible == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = 'white') +
  facet_wrap(~eligible) +
  xlab('Probability of being eligible') + 
  theme_bw()

# 2 - Trimming 
data_trim = prs_df[which(prs_df$pr_score<0.99),]
m_ps_trim <- glm(eligible ~ AC_1 + AC_6 + 
                   months_since_application +  nat_country + Reg +
                   AG_1 + AG_2 + AG_3 + AG_4 + AG_5 +
                   age_main_resp + gender_main_resp,
                 family=binomial(link='logit'), data = data_trim)
summary(m_ps_trim)
prs_df_trim <- data.frame(pr_score = predict(m_ps_trim, type = 'response'),
                          eligible = m_ps_trim$model$eligible)
prs_df_trim %>%
  mutate(eligible = ifelse(eligible==1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = 'white') +
  facet_wrap(~eligible) +
  xlab('Probability of being eligible, trimmed dataset') + 
  theme_bw()


######### II - MATCHING

mod_match <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                       AG_1 + AG_2 + AG_3 + AG_4 + AG_5 + age_main_resp + gender_main_resp,
                     method = 'nearest', data = data_trim) #, replace=TRUE


