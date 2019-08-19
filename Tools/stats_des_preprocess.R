#### STATS DES TOOLS
library(lubridate)
library(plyr)

### OWN CHILDREN METHOD

#recherches exploratoires pour estimer la fecondite a partir de l'own children method
#nb : le principe est de calculer des taux par age de nombre
# d'enfants corésidents de moins d'un an par femme seule en
# age de faire des enfants (on reduit a 15-44 ans revolus)
# en ne retenant les menages ou il n'y a qu'une seule femme
# en age fecond pour ne pas mal apparier un nouveau ne.
# La somme des taux par age donne donc une sorte
# d'Indicateur Conjoncturel de coresidence avec des nouveaux nes
# c'est donc le nb de fois qu'une femme coresiderait avec des enfants de moins d'1 an
# au cours de sa vie feconde (15-44 ans se justifie ici par le fait que la fecondite
# est quasiment nulle a 44 ans) si elle connaissait tout au long de sa vie feconde
# les conditions de fecondite de la periode d'observation

#assistance_no = identifiant menage

# Fonction qui permet de reperer les doublons
duplicated2 <- function(x){ 
  if (sum(dup <- duplicated(x))==0) 
    return(dup) 
  if (class(x) %in% c('data.frame', 'matrix')) 
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
}

own_children <- function(df_i){
  #isolement des menages ou il y a une seule femme en age de procreer
  df_iff<- df_i[which(df_i$gender=='female' & df_i$age>=15 & df_i$age<45),]
  #on conserve uniquement les menages ou il y a une seule femme en age fecond
  iffs <- df_iff[which(duplicated2(df_iff$assistance_no)==FALSE),]
  #on met en reserve les identifants menages ou il n' y a qu une seule femme en age fecond
  iffs <-  data.frame(iffs$assistance_no)
  
  #on retourne dans la base individus pour isoler toutes les personnes qui vivent dans un menage
  #où il  y a une seule femme feconde
  o <- merge(df_i, iffs, by.x='assistance_no', by.y='iffs.assistance_no', all=FALSE)
  
  #on fait une distribution par age des femmes en age fecond
  off <- o[which(o$gender=='female' & o$age>=15 & o$age<45),]
  f <- data.frame(table(off$age))
  
  #on veut isoler les menages ou il y a des nouveaux nes
  onn <- o[which(o$age==0),]
  #on met en reserve les identifants menages ou il y a des nouveaux nes
  onns <-  data.frame(onn$assistance_no)
  #on retourne dans la base individus pour faire la distribution par age des femmes vivant
  #dans les menages ou il y a des nouveaux nes
  onnss <- merge(off,onns, by.x='assistance_no', by.y='onn.assistance_no', all=FALSE)
  onnsss <- onnss[which(onnss$gender=='female' & onnss$age>=15 & onnss$age<45),]
  
  ff <- data.frame(table(onnsss$age))
  #on regroupe le nb de femmes en age fecond et le nb de nouveaux nes par age
  fff <- merge(f,ff,by='Var1', all=TRUE)
  #on calcule les taux de fecondite par age
  fff$tf <- fff$Freq.y/fff$Freq.x
  
  fff = data.frame(fff$Var1, fff$tf)
  fff = dplyr::rename(fff, age=fff.Var1, tf=fff.tf)
  
  return(fff)
}


###### INTERVALLE INTERGENESIQUE
intervalle_intergenesique <- function(df){
  
  ### BASE DES INDIVIDUS: ON SE RESTREINT AUX FAMILLES NUCLEAIRES
  #df <- read_individual_data(year, month, dataframe)
  #isolement des menages ou il y a une seule femme en age de procreer
  df_ff<- df[which(df$gender=='female' & df$age>=15 & df$age<45),]
  df_ffs <- df_ff[which(duplicated2(df_ff$assistance_no)==FALSE),]
  #on met en reserve les identifants menages ou il n' y a qu une seule femme en age fecond
  no_hh <-  plyr::rename(data.frame(df_ffs$assistance_no), c('df_ffs.assistance_no'='assistance_no'))
  
  ### Databases
  df_children <- merge(df, no_hh, by='assistance_no', all=FALSE)
  df_children <- df_children[which((df_children$age < 15)|((df_children$age < 18)&(df_children$marital_status=='single'))),]
  
  ### Rank children
  df_children <- dplyr::group_by(df_children, assistance_no)
  df_children <- dplyr::mutate(df_children, child_rank = order(order(age, decreasing=TRUE)))
  
  ### Date of birth
  df_children$date_of_birth <- lubridate::ymd(df_children$date_of_birth)
  
  ### Keep those born during program
  born_during_program <- df_children[which(df_children$date_of_birth >= ymd('2016-12-01')),]
  
  birth_2 <- plyr::rename(born_during_program[which(born_during_program$child_rank==2),][c('assistance_no', 'date_of_birth')],
                          c('date_of_birth'='birth_child2'))
  birth_1_2 <- merge(plyr::rename(df_children[which(df_children$child_rank==1),][c('assistance_no', 'date_of_birth')], c('date_of_birth'='birth_child1')), 
                     birth_2, by='assistance_no', all=FALSE)
  birth_1_2$int <- interval(ymd(birth_1_2$birth_child1), ymd(birth_1_2$birth_child2)) %/% months(1)
  M_1_2 <- mean(birth_1_2$int)
  
  birth_3 <- plyr::rename(born_during_program[which(born_during_program$child_rank==3),][c('assistance_no', 'date_of_birth')],
                          c('date_of_birth'='birth_child3'))
  birth_2_3 <- merge(plyr::rename(df_children[which(df_children$child_rank==2),][c('assistance_no', 'date_of_birth')], c('date_of_birth'='birth_child2')), 
                     birth_3, by='assistance_no', all=FALSE)
  birth_2_3$int <- interval(ymd(birth_2_3$birth_child2), ymd(birth_2_3$birth_child3)) %/% months(1)
  M_2_3 <- mean(birth_2_3$int)
  
  birth_4 <- plyr::rename(born_during_program[which(born_during_program$child_rank==4),][c('assistance_no', 'date_of_birth')],
                          c('date_of_birth'='birth_child4'))
  birth_3_4 <- merge(plyr::rename(df_children[which(df_children$child_rank==3),][c('assistance_no', 'date_of_birth')], c('date_of_birth'='birth_child3')), 
                     birth_4, by='assistance_no', all=FALSE)
  birth_3_4$int <- interval(ymd(birth_3_4$birth_child3), ymd(birth_3_4$birth_child4)) %/% months(1)
  M_3_4 <- mean(birth_3_4$int)
  
  return(c(M_1_2, M_2_3, M_3_4))
}
