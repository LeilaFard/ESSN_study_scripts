##
##      TEST ON BLOCKING 
##      Matching Methods in Practicee: Three examples, Guido W. Imbens, 2014 (IZA DP No. 8049)
##      section 5.3
##


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

######### I - MODEL SPECIFICATION

# 1 - Model with all the data

m_ps <- glm(eligible ~ AC_1 + AC_6 + months_since_application +  nat_country + Reg + AG_1 + AG_2 + AG_3 + AG_4 + AG_5, 
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
                     AG_5 = m_ps$model$AG_5)


# 2 - Trimming 
data_trim = prs_df[which(prs_df$pr_score<0.99),]


######### II -Stratification of data_trim
block_stats <- function(data, j, block_statistics, blocks){
  N_c_j = sum((1-data$treat)*(data$strat==j)) 
  N_t_j = sum((data$treat)*(data$strat==j)) 
  
  L_bar_c_j = (1/N_c_j)*sum((1-data$treat)*(data$strat==j)*(data$log_OR))
  L_bar_t_j = (1/N_t_j)*sum((data$treat)*(data$strat==j)*(data$log_OR))
  
  S_sq_c_j = (1/(N_c_j-1))*sum((1-data$treat)*(data$strat==j)*(data$log_OR-L_bar_c_j))
  S_sq_t_j  = (1/(N_t_j-1))*sum((data$treat)*(data$strat==j)*(data$log_OR-L_bar_t_j))
  
  block_statistics$t[j] = (L_bar_t_j - L_bar_c_j) / sqrt((S_sq_t_j^2/N_t_j)+(S_sq_c_j^2/N_c_j))
  
  ps_j = data$pr_score[which((data$pr_score>=blocks[j])&(data$pr_score<blocks[j+1]))]
  med_j = median(ps_j)
  
  block_statistics$N_min_c[j] = sum((1-data$treat)*((data$pr_score>=blocks[j])&(data$pr_score<med_j)))
  block_statistics$N_max_c[j] = sum((1-data$treat)*((data$pr_score>=med_j)&(data$pr_score<blocks[j+1])))
  
  block_statistics$N_min_t[j] = sum((data$treat)*((data$pr_score>=blocks[j])&(data$pr_score<med_j)))
  block_statistics$N_max_t[j] = sum((data$treat)*((data$pr_score>=med_j)&(data$pr_score<blocks[j+1])))
  
  return(block_statistics)
}  

adequate <- function(n, stats, len_coefs){ #adequate <- function(j, stats, coefficients)
  adequate = rep(0,n)
  for (j in 1:n){
    if (stats$t[j]<=1.96){
      adequate[j] = 1
    }
    else if (min(stats$N_min_t[j], stats$N_min_c[j])<=3){
      adequate[j] = 1
    }
    else if (min(stats$N_min_c[j]+stats$N_min_t[j], stats$N_min_c[j]+stats$N_min_t[j])<=len_coefs+2){
      adequate[j] = 1
    }
  }
  return(adequate)
}

# Init : 2 blocks
blocks = c(0,1)

med = median(data_trim$pr_score)

data_trim$strat = 0
data_trim$strat[which(data_trim$pr_score<med)] = 1
data_trim$strat[which(data_trim$pr_score>=med)] = 2
blocks = sort(append(blocks, med))

data_trim$treat=data_trim$eligible

data_trim[['log_OR']] = log(data_trim$pr_score/(1-data_trim$pr_score))

n=2
statistics <- list('t'=rep(0,n), 'N_min_t'=rep(0,n), 'N_min_c'=rep(0,n), 'N_max_t'=rep(0,n), 'N_max_c'=rep(0,n))

for (j in 1:n){
  statistics <- block_stats(data_trim, j, statistics, blocks)
}

adeq <- adequate(n, stats=statistics, len_coefs=18)

data_trim[['strat2']] = 0

while (0 %in% adeq){
  
  for (i in 1:n){
    
    if (adeq[i]==0){
      df <- data_trim[which(data_trim$strat==i),]
      med = median(df$pr_score)
      df[which(df$pr_score>=med), 'strat2'] = 1
      blocks = sort(append(blocks, med))
      
      data_trim[which(data_trim$strat==i), 'strat2'] <- df[['strat2']]
      
    }
  }
  
  data_trim$strat <- data_trim %>% 
                     group_by(strat, strat2) %>% 
                     group_indices()
  data_trim$strat2 <- 0
  
  n <- length(blocks) - 1
  print(n)
  
  statistics <- list('t'=rep(0,n), 'N_min_t'=rep(0,n), 'N_min_c'=rep(0,n), 'N_max_t'=rep(0,n), 'N_max_c'=rep(0,n))
  
  for (j in 1:n){
    statistics <- block_stats(data_trim, j, statistics, blocks)
  }
  
  adeq <- adequate(n, stats=statistics, len_coefs=18)
  print(adeq)
  
}

#out = 220 classes

