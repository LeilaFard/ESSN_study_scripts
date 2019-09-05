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

# 2 - Trimming 
data_trim = prs_df[which(prs_df$pr_score<0.99),]


######### II - MATCHING

## Stratified

adequate <- function(data, n){
  
  mod_match_strat <- matchit(eligible ~ AC_1 + AC_6 + months_since_application + nat_country + Reg +
                               AG_1 + AG_2 + AG_3 + AG_4 + AG_5 , distance = 'logit',
                             method = 'subclass', data = data, subclass=n) 
  
  mod_match_data = mod_match_strat$model$data
  qcut = data.frame(mod_match_strat$q.cut)
  
  mod_match_data$strat = mod_match_strat$subclass
  mod_match_data$treat = mod_match_strat$treat
  
  mod_match_data[['log_OR']] = log(mod_match_data$pr_score/(1-mod_match_data$pr_score))
  
  N_c = rep(0,n)
  N_t = rep(0,n)
  L_bar_c = rep(0,n) 
  L_bar_t = rep(0,n)
  S_sq_c = rep(0,n)
  S_sq_t = rep(0,n)
  t = rep(0,n)
  
  N_min_c = rep(0,n)
  N_max_c = rep(0,n)
  N_min_t = rep(0,n)
  N_max_t = rep(0,n)
  
  adequate = rep(0,n)
  
  
  for (j in 1:n){
    
    N_c[j] = sum((1-mod_match_data$treat)*(mod_match_data$strat==j)) 
    N_t[j] = sum((mod_match_data$treat)*(mod_match_data$strat==j)) 
    
    L_bar_c[j] = (1/N_c[j])*sum((1-mod_match_data$treat)*(mod_match_data$strat==j)*(mod_match_data$log_OR))
    L_bar_t[j] = (1/N_t[j])*sum((mod_match_data$treat)*(mod_match_data$strat==j)*(mod_match_data$log_OR))
    
    S_sq_c[j] = (1/(N_c[j]-1))*sum((1-mod_match_data$treat)*(mod_match_data$strat==j)*(mod_match_data$log_OR-L_bar_c[j]))
    S_sq_t[j] = (1/(N_t[j]-1))*sum((mod_match_data$treat)*(mod_match_data$strat==j)*(mod_match_data$log_OR-L_bar_t[j]))
    
    t[j] = (L_bar_t[j] - L_bar_c[j]) / sqrt((S_sq_t[j]^2/N_t[j])+(S_sq_c[j]^2/N_c[j]))
    
    ps_j = mod_match_data$pr_score[which((mod_match_data$pr_score>=qcut[j,1])&(mod_match_data$pr_score<qcut[j+1,1]))]
    med_j = median(ps_j)
    
    N_min_c[j] = sum((1-mod_match_data$treat)*((mod_match_data$pr_score>=qcut[j,1])&(mod_match_data$pr_score<med_j)))
    N_max_c[j] = sum((1-mod_match_data$treat)*((mod_match_data$pr_score>=med_j)&(mod_match_data$pr_score<qcut[j+1,1])))
    
    N_min_t[j] = sum((mod_match_data$treat)*((mod_match_data$pr_score>=qcut[j,1])&(mod_match_data$pr_score<med_j)))
    N_max_t[j] = sum((mod_match_data$treat)*((mod_match_data$pr_score>=med_j)&(mod_match_data$pr_score<qcut[j+1,1])))
    
    
    ## CONDITIONS
    
    if (t[j]<=1.96){
      adequate[j] = 1
    }
    else if (min(N_min_t[j], N_min_c[j])<=3){
      adequate[j] = 1
    }
    else if (min(N_min_c[j]+N_min_t[j], N_min_c[j]+N_min_t[j])<=length(mod_match_strat$model$coefficients)+2){
      adequate[j] = 1
    }
  } 
  
  return(adequate)
}

K= rep(0,59)

for (i in 2:100){
  K[i]= sum(adequate(data_trim, i)) / i 
}

print(data.frame(K))
