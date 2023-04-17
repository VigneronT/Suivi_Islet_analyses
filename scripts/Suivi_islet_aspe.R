library(aspe)
library(tidyverse)
library(DT)

load(file = "E:/Documents/R/ASPE/raw_data/tables_sauf_mei_2023_03_08_15_08_27.RData")

#########Passerelle#########
passerelle <- mef_creer_passerelle()

names(passerelle)
########### Filtrage données BZH#########
passerelle <- passerelle %>% 
  mef_ajouter_dept() %>% 
  filter(dept %in% c(22, 29, 35, 56))

########### Filtrage Objectif #########
#RNSORMCE – Réseau National de Suivi des Opérations de Restauration hydroMorphologiques des Cours d'Eau

#passerelle <- passerelle %>%
  #mef_ajouter_objectif() %>% 
  #filter(obj_libelle %in% c("RNSORMCE – Réseau National de Suivi des Opérations de Restauration hydroMorphologiques des Cours d'Eau
#"))#


##############  Filtre par code opération ##############

passerelle <- passerelle %>%
  
  filter(ope_id %in% c("87152","87155","87154", "87156"))

##########  Bilan par stations ###########

ipr <- passerelle %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_libelle() %>% 
  filter(ope_date > lubridate::dmy("01/01/2005")) %>% 
  mef_ajouter_libelle() %>% 
  droplevels()

view(ipr)

#########visualisation du tableau ###############
ipr %>% head() %>% DT::datatable()

########## mis een tableau large ###########

ipr_1c_par_an <- ipr_pivoter_1colonne_par_an(ipr_df = ipr)

names(ipr_1c_par_an)

view(ipr_1c_par_an)

######### export en tablau .csv ##########

write.csv2(ipr_1c_par_an, file = "processed_data/ipr_bzh_pdl_large.csv",
           row.names = FALSE, na = "")