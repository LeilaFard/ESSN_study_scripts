remove(list = ls())
path <- dirname(rstudioapi::getSourceEditorContext()$path)

library(dplyr)
library(ggplot2)
library(readstata13)

source(paste(path, 'settings.R', sep='/'))


SHFS2009 <- dplyr::rename(read.dta13(paste(data_path, 'SHFS2009/SFHS2009(1).dta', sep='/')), age=XH105A, gender=H103)
SHFS2009 <- SHFS2009 %>% dplyr::mutate(hh_no = group_indices(., cluster, hhnum))

#le nom de la variable
str(SHFS2009$gov)
freq(SHFS2009$gov)

df_i = SHFS2009[which((SHFS2009$gov=='Lateqia')|
                      (SHFS2009$gov=='Edlb')|
                      (SHFS2009$gov=='Raqa')|
                      (SHFS2009$gov=='Halab')|
                      (SHFS2009$gov=='Hasaka')), c('age', 'gender', 'hh_no')]

duplicated2 <- function(x){ 
  if (sum(dup <- duplicated(x))==0) 
    return(dup) 
  if (class(x) %in% c('data.frame', 'matrix')) 
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
}

own_children_SHFS <- function(df_i){
  #isolement des menages ou il y a une seule femme en age de procreer
  df_iff<- df_i[which(df_i$gender=='Female' & df_i$age>=15 & df_i$age<45),]
  #on conserve uniquement les menages ou il y a une seule femme en age fecond
  iffs <- df_iff[which(duplicated2(df_iff$hh_no)==FALSE),]
  #on met en reserve les identifants menages ou il n' y a qu une seule femme en age fecond
  iffs <-  data.frame(iffs$hh_no)
  
  #on retourne dans la base individus pour isoler toutes les personnes qui vivent dans un menage
  #où il  y a une seule femme feconde
  o <- merge(df_i, iffs, by.x='hh_no', by.y='iffs.hh_no', all=FALSE)
  
  #on fait une distribution par age des femmes en age fecond
  off <- o[which(o$gender=='Female' & o$age>=15 & o$age<45),]
  f <- data.frame(table(off$age))
  
  #on veut isoler les menages ou il y a des nouveaux nes
  onn <- o[which(o$age==0),]
  #on met en reserve les identifants menages ou il y a des nouveaux nes
  onns <-  data.frame(onn$hh_no)
  #on retourne dans la base individus pour faire la distribution par age des femmes vivant
  #dans les menages ou il y a des nouveaux nes
  onnss <- merge(off,onns, by.x='hh_no', by.y='onn.hh_no', all=FALSE)
  onnsss <- onnss[which(onnss$gender=='Female' & onnss$age>=15 & onnss$age<45),]
  
  ff <- data.frame(table(onnsss$age))
  #on regroupe le nb de femmes en age fecond et le nb de nouveaux nes par age
  fff <- merge(f,ff,by='Var1', all=TRUE)
  #on calcule les taux de fecondite par age
  fff$tf <- fff$Freq.y/fff$Freq.x
  
  fff = data.frame(fff$Var1, fff$tf)
  fff = dplyr::rename(fff, age=fff.Var1, tf=fff.tf)
  
  return(fff)
}

SHFS_oc <- own_children_SHFS(df_i)
SHFS_oc[['Data']] = 'SHFS 2009'


source(paste(tools, 'format_data.R', sep='/'))
source(paste(tools, 'stats_des_preprocess.R', sep='/'))

year = 2018
month = 4

add_nationality <- function(df){
  nationalities <- read.csv(paste(data_path, 'Master Data/Countries.csv', sep='/'), sep=',')
  nationalities[['nat_country']] = 'Other'
  nationalities[which(nationalities$code=='AF'), 'nat_country'] = 'Afghanistan'
  nationalities[which(nationalities$code=='IQ'), 'nat_country'] = 'Iraq'
  #  nationalities[which(nationalities$code=='IR'), 'nat_country'] = 'Iran'
  nationalities[which(nationalities$code=='SY'), 'nat_country'] = 'Syria'
  nationalities <- setNames(nationalities,c('nationality_id', 'name', 'code', 'mernis_code', 'active', 'nat_country'))
  df <- merge(x=df, y=nationalities[,c('nationality_id', 'nat_country')], all.x=TRUE)
  return(df)
}

ii <- add_nationality(read_individual_data(year, month, 'ineligible'))
ie <- add_nationality(read_individual_data(year, month, 'eligible'))

df_i <- rbind(ii[which(ii$nat_country == 'Syria'), c('assistance_no', 'age', 'gender')], ie[which(ii$nat_country == 'Syria'), c('assistance_no', 'age', 'gender')])
ESSN_oc <- own_children(df_i)
ESSN_oc[['Data']] <- 'ESSN 2018'



# Graph
plot <- ggplot(rbind(ESSN_oc, SHFS_oc)) + 
  aes(x=age) + geom_point(aes(y=tf, shape=Data)) + scale_shape_manual(values=c(16, 1))+
  labs(title = 'Fertility rate by age', x = 'Age', y = 'Fertility rate', shape = 'Data',
       caption = paste(paste('Total fertility rate 2009 =',round(sum(SHFS_oc$tf, na.rm=TRUE),2)), 
                       paste('Total fertility rate 2018 =',round(sum(ESSN_oc$tf, na.rm=TRUE),2)), sep='\n'), 
      shape = 'Group') +
  #scale_x_discrete(breaks=c(15, 20, 25, 30, 35, 40, 45))+
  theme_light() 

outputs_graph_path = paste('C:/Users/lfardeau/Documents/ESSN/Outputs/Plots', year, month, sep='/')
ggsave(filename = paste(outputs_graph_path, 'fertility_rate_comp_SHFS.png', sep='/'), plot,
       width = 7, height = 5, dpi = 300, units = 'in', device='png')

rm(plot)
                          