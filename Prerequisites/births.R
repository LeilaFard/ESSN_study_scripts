library(dplyr)
library(plyr)

dstDir = './Data/Processed/'


###### APRES 2017/6 ON DISPOSE DE TOUS LES UNIQUE ID 

#### Initialisation

#### On itère sur les dates suivantes
dates_list = list(c(2017, 6), c(2017, 7), c(2017, 8), c(2017, 9), c(2017, 10), c(2017, 11), 
                  c(2017, 12), c(2018, 1), c(2018, 2), c(2018, 3), c(2018, 4), c(2018, 5), 
                  c(2018, 6), c(2018, 7), c(2018, 8), c(2018, 9), c(2018, 10), c(2018, 11), 
                  c(2018, 12), c(2019, 1), c(2019, 2), c(2019, 3), c(2019, 4))

for (i in 1:22){
  date0 = dates_list[[i]]
  date = dates_list[[i+1]]
  
  print(paste('T =', paste(date[1], date[2], sep='/')))
  
  
  # T-1
  ie <- read_individual_data(date0[1], date0[2], 'eligible')
  ie[['eligible']] = TRUE
  ii <- read_individual_data(date0[1], date0[2], 'ineligible')
  ii[['eligible']] = FALSE
  
  # Enfants entre 0 et 1 ans 
  idv0 <- rbind(ii[which(ii$age==0),][, c('assistance_no', 'unique_id','eligible')],
                ie[which(ie$age==0),][, c('assistance_no', 'unique_id', 'eligible')])
  # Agrégé par ménage (assistance_no): unique_id des enf en vecteur
  births0 <- plyr::rename(aggregate(unique_id ~ assistance_no, idv0, I), c('unique_id'=paste('child01', date0[1], date0[2], sep='_')))
  #eligible0 <- rename(aggregate(eligible ~ assistance_no, idv, mean), c('eligible'=paste('eligible', year, month, sep='_')))
  
  # T
  ie <- read_individual_data(date[1], date[2], 'eligible')
  ie[['eligible']] = TRUE
  ii <- read_individual_data(date[1], date[2], 'ineligible')
  ii[['eligible']] = FALSE
  
  idv <- rbind(ii[which(ii$age==0),][, c('assistance_no', 'unique_id','eligible')],
               ie[which(ie$age==0),][, c('assistance_no', 'unique_id', 'eligible')])
  
  births <- plyr::rename(aggregate(unique_id ~ assistance_no, idv, I), c('unique_id'=paste('child01', date[1], date[2], sep='_')))
  eligible <- plyr::rename(aggregate(eligible ~ assistance_no, idv, mean), c('eligible'=paste('eligible',date[1], date[2], sep='_')))
  
  # Ajout de l'observation à la base births
  births <- merge(births, births0, by='assistance_no', all=FALSE)
  #eligible <- merge(eligible, eligible0, by='assistance_no', all=TRUE)
  
  #rm(births0) #, eligible0
  
  # Comparaison : check that t-1 is subset of t 
  births[[paste('births', date[1], date[2], sep='_')]] <- mapply(function(x,y){return(setdiff(x,y))}, 
                                                                 x=births[[paste('child01', date0[1], date0[2], sep='_')]], 
                                                                 y=births[[paste('child01', date[1], date[2], sep='_')]])
  births[[paste('n_births', date[1], date[2], sep='_')]] <- lapply(births[[paste('births', date[1], date[2], sep='_')]],
                                                                   function(x){return(length(x))})
  births[[paste('births', date[1], date[2], sep='_')]][which(births[[paste('n_births', date[1], date[2], sep='_')]]==0)]=NA
  
  # Nettoyage
  births[[paste('child01', date0[1], date0[2], sep='_')]] <- NULL
  births[[paste('child01', date[1], date[2], sep='_')]] <- NULL
  
  #births <- apply(births,2,as.character)
  #write.csv(births, file = paste(dstDir, paste('births', date[1], date[2], sep='_'), '.csv', sep=''), row.names=FALSE)
  saveRDS(births, file = paste(dstDir, paste('births', date[1], date[2], sep='_'), '.rds', sep=''))
}




## TODO : improve this part
###### AVANT 2017/6 ON N'A PAS D'UNIQUE ID POUR TOUT LE MONDE
# on identifie comme naissance la présence en T d'un enfant entre 0 et 1 ans 
# Enfants entre 0 et 1 ans
# qui n'était pas là en T-1
