remove(list = ls())
path <- dirname(rstudioapi::getSourceEditorContext()$path)

library(ggplot2)
library(gridExtra)
library(dplyr)
library(plyr)

source(paste(path, 'settings.R', sep='/'))

source(paste(tools, 'format_data.R', sep='/'))

source(paste(tools, 'stats_des_preprocess.R', sep='/'))


year = 2018
month = 4


outputs_graph_path = paste('./Outputs/Plots', year, month, sep='/')
outputs_tables_path = paste('./Outputs/Tables', year, sep='/')

dir.create(outputs_graph_path, recursive=TRUE)
dir.create(outputs_tables_path, recursive=TRUE)

ii <- read_individual_data(year, month, 'ineligible')
hi <- read_household_data(year, month, 'ineligible')
ie <- read_individual_data(year, month, 'eligible')
he <- read_household_data(year, month, 'eligible')



#
#   Eligible and ineligible ppl
###########################################################################################

elig <- data.frame(status=c('eligible', 'ineligible'), 
                   number_idv=c(nrow(ie), nrow(ii)), 
                   number_hh=c(nrow(he), nrow(hi)))

plot <- ggplot(data=elig, aes(x=status, y=number_hh)) + 
        geom_bar(stat='identity', position=position_dodge()) +
        labs(title = paste('Number of eligible and ineligible households, ', paste(year, month, sep='/'),sep=' '), x= '', y = 'Number of households' ) + 
        theme_light()

ggsave(filename = paste(outputs_graph_path, 'intervalle_intergenesique.png', sep='/'), plot,
       width = 7, height = 10, dpi = 300, units = 'in', device='png')

rm(plot)

#
#   Number of children by eligible status
###########################################################################################

children_by_elig <- function(hi, he){
  he[['num_children']] <- he$num_male_children + he$num_female_children
  hi[['num_children']] <- hi$num_male_children + hi$num_female_children
  he[['eligiblility']] = 'eligible'
  hi[['eligiblility']] = 'ineligible'
  hh <- rbind(he[, c('eligiblility', 'num_children')], hi[, c('eligiblility', 'num_children')])
  hh[which(hh$num_children>10), 'num_children'] <- 10
  
  data <- data.frame(table(hh$eligiblility, hh$num_children))

  return(data)
}

data <- children_by_elig(ii, ie)

p <- ggplot(data=data, aes(x=Var2, y=Freq, fill=Var1)) + 
     geom_bar(stat='identity', position=position_dodge()) +
     labs(title = '', x= '', y = 'Frequency' ) + 
     theme_light()

p
ggsave(filename = paste(outputs_graph_path, 'ac_eligibles.png', sep='/'), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')


rm(p)

#
#   Acceptance criteria
###########################################################################################

# Graph AC prevalence

AC_graph <- function(data_hh){
  data <- setNames(data.frame((colSums(data_hh[,c('AC_1', 'AC_2', 'AC_3', 'AC_4', 'AC_5', 'AC_6')]) / nrow(data_hh))*100),
                   c('Percentage'))
  data[['AC']] <- rownames(data)
  data[['mod']] <- 'Yes'
  rownames(data) <- NULL
  for (i in 1:6){
    newline <- data.frame('Percentage'=100-data[i, 'Percentage'], 'AC'=paste('AC', i, sep='_'), 'mod'='No')
    data <- rbind(data, newline)
  }
  p <- ggplot() + geom_bar(aes(y = Percentage, x = AC, fill=mod), data = data, stat='identity') + 
    theme_light() + scale_fill_manual('', values=c('grey','black'), labels=c('No', 'Yes'))
  return(p)
}

ggsave(filename = paste(outputs_graph_path, 'ac_eligibles.png', sep='/'), AC_graph(he),
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

ggsave(filename = paste(outputs_graph_path, 'ac_ineligibles.png', sep='/'), AC_graph(hi),
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

# At least 2 AC
ac <- function(df){
  tot <- df$AC_1 + df$AC_2 + df$AC_3 + df$AC_4 + df$AC_5 + df$AC_6 
  tot <- tot[which(tot>1)]
  return(length(tot)/nrow(df))
}

AC_df = rbind(he[,c('AC_1', 'AC_2', 'AC_3', 'AC_4', 'AC_5', 'AC_6')], hi[,c('AC_1', 'AC_2', 'AC_3', 'AC_4', 'AC_5', 'AC_6')])
col <- lm(AC_5 ~ AC_1 + AC_2 + AC_3 + AC_4 + AC_6, data=AC_df)
#summary(col)
out.tex = xtable(col)
print(out.tex, type='latex', file=paste(outputs_tables_path, 'AC5col.tex', sep='/'), compress = FALSE) 
rm(out.tex)

#ac(he)
#ac(hi)

#
#  FERTILITY (1) :  OWN CHILDREN METHOD
############################################ 

<<<<<<< HEAD
# Calcul du taux de fecondite par age 
fff_e = own_children(ie)
fff_i = own_children(ii)
# Preprocess
Text_e = paste('Total fertility rate of eligible =',round(sum(fff_e$tf, na.rm=TRUE),2))
Text_i = paste('Total fertility rate of ineligible =',round(sum(fff_i$tf, na.rm=TRUE),2))
 fff_e$cat = 'eligible'
fff_i$cat = 'ineligible'
fff = bind_rows(fff_e, fff_i)
# Nettoyage 
rm(fff_e, fff_i)
=======
preprocess_own_ch_graph <- function(df_i, cat){
  # Calcul du taux de fecondite par age 
  fff_ = own_children(df_i)
  fff_$cat = cat
  return(fff_)
}

text_own_ch_graph <- function(df_i, cat){
  fff_ = preprocess_own_ch_graph(df_i, cat)
  return(paste('Total fertility rate of', cat, '=',round(sum(fff_$tf, na.rm=TRUE),2)))
}


>>>>>>> 332892b3e5c2a0b983c560612bdcd652901d2a07
# Graph
plot <- ggplot(bind_rows(preprocess_own_ch_graph(ie, 'eligible'), preprocess_own_ch_graph(ii, 'ineligible'))) + 
        aes(x=age) + geom_point(aes(y=tf, shape=cat)) + scale_shape_manual(values=c(16, 1))+
        labs(title = paste('Fertility rate by age - ', paste(year, month, sep='/'), sep=' '),
        x = 'Age', y = 'Fertility rate',
        caption = paste(text_own_ch_graph(ie, 'eligible'), text_own_ch_graph(ii, 'ineligible'), sep='\n'), shape = 'Group') +
        #scale_x_discrete(breaks=c(15, 20, 25, 30, 35, 40, 45))+
        theme_light() 

ggsave(filename = paste(outputs_graph_path, 'fertility_rate.png', sep='/'), plot,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')

rm(plot)

#
#  INTERVALLE INTERGENESIQUE
##############################


intervalle_intergenesique <- data.frame(enfants=c(2,3,4,2,3,4), 
                                        eligibility=c('eligible', 'eligible', 'eligible', 'ineligible', 'ineligible', 'ineligible'), 
                                        mean=append(intervalle_intergenesique(ie), intervalle_intergenesique(ii)))

intervalle_intergenesique$enfants <- factor(intervalle_intergenesique$enfants, labels=c('2nd child', '3rd child', '4th child'))

plot <- ggplot(data=intervalle_intergenesique, aes(x=enfants, y=mean, fill=eligibility)) + 
        geom_bar(stat='identity', position=position_dodge()) +
        scale_fill_manual('Eligibility', values=c('grey','black'), labels=c('Eligible', 'Ineligible'))+
        labs(title = paste('Interval between two births, ', paste(year, month, sep='/'),sep=' '), x= '', y = 'Interval in months' ) + 
        theme_light()


ggsave(filename = paste(outputs_graph_path, 'intervalle_intergenesique.png', sep='/'), plot,
       width = 7, height = 10, dpi = 300, units = 'in', device='png')

rm(plot)


#
#  AGE PYRAMID
################################ 

age_pyramid <- function(df_i, status){
  #on discetise la variable age en tranches de 10 ans
  df_i$cut.age <- cut(df_i$age,include.lowest = TRUE,right=FALSE,seq(0,100,5))
  #on trace la pyramide
  gg2 <-  ggplot(df_i) +
    aes(x=cut.age,fill=gender) + geom_bar(data = subset(df_i,gender=='male'),aes(y=..count..*(-1))) + # les valeurs deviennent negatives
    geom_bar(data = subset(df_i,gender=='female')) +
    scale_fill_manual('Gender', values=c('grey','black'), labels=c('Female', 'Male'))+
    # Etiquettes pour l'axe des x, a modifier selon vos donnees.
    coord_flip() + 
    labs(title = paste(status,' age structure ', paste(year, month, sep='/'),sep=' ') ,
         subtitle = 'Thousands of individuals',
         x = 'Age', y = 'Frequency' ) + # Titres des axes
    scale_y_continuous(breaks = c(-1.5e+05, -1.25e+05, -1e+05, -7.5e+04, -5e+04, -2.5e04, 0, 2.5e+04, 5e+04, 7.5e04, 1e+05, 1.25e+05, 1.5e+05),
                       labels = c('150', '125', '100', '75', '50', '25', '0', '25', '50', '75', '100', '125', '150')) +
    scale_x_discrete(breaks=c('[0,5)', '[5,10)', '[10,15)', '[15,20)', '[20,25)', '[25,30)', '[30,35)', '[35,40)', '[40,45)','[45,50)',
                              '[50,55)', '[55,60)', '[60,65)', '[65,70)', '[70,75)', '[75,80)', '[80,85)', '[85,90)', '[90,95)', '[95,100]'), 
                     labels=c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', 
                              '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-94', '95-99')) +
    theme_light()  # Theme simple, ideal pour publication
  return(gg2)
}

p1 = plot(age_pyramid(ie, 'Eligibles'))
p2 = plot(age_pyramid(ii, 'Ineligibles'))
p = grid.arrange(p1, p2, nrow = 2)


ggsave(filename = paste(outputs_graph_path, 'age_pyramid.png', sep='/'), p,
       width = 7, height = 10, dpi = 300, units = 'in', device='png')


#
#   Months since application
#
###################################################################

months_since_application <- function(df_h){
  df_h$date <- lubridate::ymd(paste(year, month, '01', sep='/'))
  df_h$months_since_application <- lubridate::interval(df_h$application_date, df_h$date) %/% months(1)
  df_h$date = NULL
  return(df_h)
}

he <- months_since_application(he)
hi <- months_since_application(hi)

d_e <- data.frame(table(he$months_since_application))
d_e[['eligible']] = 'eligible'
d_i <- data.frame(table(hi$months_since_application))
d_i[['eligible']] = 'ineligible'
d  <- rbind(d_e, d_i)
rm(d_e, d_i)

p <- ggplot(data=d, aes(x=Var1, y=Freq, fill=eligible)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  scale_fill_manual('Eligibility', values=c('grey','black'), labels=c('Eligible', 'Ineligible'))+
  labs(title = 'Months since first application by eligible status', x= 'Months since first application', y = 'Number of housholds' ) + 
  theme_light()


ggsave(filename = paste(outputs_graph_path, 'months_since_application.png', sep='/'), p,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')



#
#    AGE AU MARIAGE 
#############################   

# pour calculer les proportions d'age au mariage

marriage_age <- function(df_i){
  # calcul de la proportion de femmes mariees par age
  df_i <- df_i[which(df_i$gender=='female' & df_i$age<40),]
  married <- df_i[which(df_i$marital_status=='married'),]
  d_married<-data.frame(table(married$age))
  d_married <- dplyr::rename(d_married, Num_married = Freq, age=Var1)
  d <- data.frame(table(df_i$age))
  d <- dplyr::rename(d, Num_women = Freq, age=Var1)
  d <-merge(x = d, y = d_married, by = 'age') 
  d$pct_married <- ( d$Num_married / d$Num_women )*100
  d$age <- as.numeric(d$age)
  
  return(d)
}

d_e = marriage_age(ie)
d_i = marriage_age(ii)

# married before age 16
stats_married <- function(d){
  d16 <- d[which(d$age<17),]
  u16 <- sum(d16$Num_married)/sum(d16$Num_women)*100
  # married before age 18
  d18 <- d[which(d$age<19),]
  u18 <- sum(d18$Num_married)/sum(d18$Num_women)*100
  return(c(u16, u18))
}

stats_e = stats_married(d_e)
stats_i = stats_married(d_i)

d_e$cat = 'eligible'
d_i$cat = 'ineligible'
d = bind_rows(d_e, d_i)
rm(d_e, d_i)

# graphique
plot <- ggplot(d) + aes(x=age, shape=cat) +
  geom_point(aes(y=pct_married)) +
  labs(title = paste('Proportion of married females', paste(year, month, sep='/'), sep=' '),
       x = "Age", y = "%",
       caption = paste('            % married under 18         % married under 16 ',
                       paste('Eligible                            ',  
                             round(stats_e[2],2),'%                                     ',  
                             round(stats_e[1],2), '%            ', sep=''),
                       paste('Ineligible                      ',  
                             round(stats_i[2],2),'%                                   ',  
                             round(stats_i[1],2), ' %           ', sep=''),
                       
                       sep='\n'),
       shape = 'Group') +
  scale_shape_manual(values=c(16, 1))+
  theme_light()  # Th?me simple, id?al pour publication

ggsave(filename = paste(outputs_graph_path, 'marriage_age.png', sep='/'), plot,
       width = 8, height = 5, dpi = 300, units = "in", device='png')

##### Eligible families by province

source(paste(tools, 'map.R', sep=''))

data_e <- he %>%
  dplyr::group_by(province_id) %>%
  dplyr::summarise(eligible_hh = n())

data_i <- hi %>%
  dplyr::group_by(province_id) %>%
  dplyr::summarise(ineligible_hh = n())

data_ <- merge(data_e, data_i, by='province_id')
data_[['proportion_eligibles']] = data_$eligible_hh / (data_$eligible_hh + data_$ineligible_hh)
data_[['number_registered']] = data_$eligible_hh + data_$ineligible_hh

df <- data_frame(id=rownames(TUR@data), province_id=TUR@data$ID_1) %>%
      left_join(data_, by='province_id')

TUR_fixed <- fortify(TUR)

final_map <- left_join(TUR_fixed, df, by = 'id')

p <-  ggplot(final_map) +
      geom_polygon( aes(x = long, y = lat, group = group, fill = proportion_eligibles), color = 'grey') +
      coord_map() + theme_void() + 
      labs(title = 'Proportion of eligible households households by province') +
      scale_fill_distiller(name = 'Number of hh', palette = 'Spectral', limits = c(0,0.80), na.value = 'grey') +
      theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste(outputs_graph_path, 'map_proportion_eligibles.png', sep='/'), p,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')

p <-  ggplot(final_map) +
      geom_polygon( aes(x = long, y = lat, group = group, fill = number_registered), color = 'grey') +
      coord_map() + theme_void() + 
      labs(title = 'Number of registered households by province') +
      scale_fill_distiller(name = 'Number of hh', palette = 'Spectral', limits = c(0,62000), na.value = 'grey') +
      theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste(outputs_graph_path, 'map_number_registered.png', sep='/'), p,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')


data_ <- rbind(he[, c('province_id', 'num_male_children', 'num_female_children')], hi[, c('province_id', 'num_male_children', 'num_female_children')]) %>%
     dplyr::group_by(province_id) %>%
     dplyr::summarise(num_children = mean(num_male_children + num_female_children))

data_$province_id = as.integer(data_$province_id)

df <- data_frame(id=rownames(TUR@data), province_id=TUR@data$ID_1) %>%
      left_join(data_, by='province_id')

TUR_fixed <- fortify(TUR)

final_map <- left_join(TUR_fixed, df, by = 'id')

p <-  ggplot(final_map) +
  geom_polygon( aes(x = long, y = lat, group = group, fill = num_children), color = 'grey') +
  coord_map() + theme_void() + 
  labs(title = 'Average number of children in a household, by province') +
  scale_fill_distiller(name = 'Number of hh', palette = 'Spectral', limits = c(0,4), na.value = 'grey') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste(outputs_graph_path, 'map_children_by_hh.png', sep='/'), p,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')

# More on maps
#https://web.stanford.edu/~kjytay/courses/stats32-aut2018/Session%207/Session_7_Code.html
  

#  
#  Origines ethniques
###################################################################

nat_data <- function(df){
  df <- add_nationality(df)
  nat <- data.frame(table(df$nat_country))
  nat[['Pct']] = 100 * (nat$Freq / sum(nat$Freq) )
  return(nat)
  }

df_e = nat_data(ie)
df_i = nat_data(ii)
df_e[['eligible']] = TRUE
df_i[['eligible']] = FALSE
df = rbind(df_e,df_i)


p <- ggplot() +
     geom_bar(aes(y = Pct, x = eligible, fill = Var1), data = df,
                 stat='identity')+
     scale_fill_manual(values = c( '#D16103',  '#52854C', '#4E84C4', '#C4961A')) 


ggsave(filename = paste(outputs_graph_path, 'nat.png', sep='/'), p,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')

#  
#  Origines ethniques et  nombre d'enfants
#
###################################################################
# TODO: reprendre


children_by_nat <- function(hi, he, ii, ie){
  he <- nat_hh_head(ie, he)
  he[['num_children']] <- he$num_male_children + he$num_female_children
  hi <- nat_hh_head(ii, hi)
  hi[['num_children']] <- hi$num_male_children + hi$num_female_children
  hh <- rbind(he[,c('nat_country', 'num_children')], hi[,c('nat_country', 'num_children')])
  hh[which(hh$num_children>10), 'num_children'] <- 10
  
  data <- data.frame(table(hh$nat_country, hh$num_children))
  tot_nat <- data.frame(table(hh$nat_country))
  
  for (nat in as.list(levels(data$Var1))){
    data[which(data$Var1==nat), 'Pct'] = 100 * (data[which(data$Var1==nat),'Freq'] / tot_nat[which(tot_nat$Var1==nat),'Freq'])
  }

  return(data)
}

data <- children_by_nat(hi, he, ii, ie)

p <- ggplot(data=data, aes(x=Var2, y=Pct, fill=Var1)) + 
      geom_bar(stat='identity', position=position_dodge()) +
     scale_fill_manual('Nationality', values=c( '#D16103',  '#52854C', '#4E84C4', '#C4961A'))+
      labs(title = '', x= '', y = 'Percentage' ) + 
      theme_light()

p



#  OLD
#  FERTILITY (2) : ADDITIONAL CHILDREN DURING A GIVEN TIME PERIOD
###################################################################
## TODO REPRENDRE LES NAISSANCES
#births <- readRDS(paste('./Data/Processed/births_', year, '_', month,'.rds', sep=''))
#births <- births[, c('assistance_no', paste('births', year, month, sep='_'))]
#births <- plyr::rename(births, c(paste('births', year, month, sep='_') = 'birth'))
#he <- merge(he, births, by='assistance_no', all.x=TRUE)
#hi <- merge(hi, births, by='assistance_no', all.x=TRUE)
#births_y <- data.frame(count(births[[paste('births', year, month, sep='_')]]))
#n_births <- sum(births_y$x*births_y$freq)

#iff_i <- ii[which(ii$gender=='female' & ii$age>=15 & ii$age<45),]
#nrow(iff_i)

