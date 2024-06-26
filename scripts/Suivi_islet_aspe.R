library(aspe)
library(tidyverse)
library(DT)

load(file = "../raw_data/rdata/tables_sauf_mei_2024_02_13_11_00_28.RData")
# load(file = "../../../../projets/ASPE/raw_data/rdata/tables_sauf_mei_2024_02_13_11_00_28.RData")

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
  
 # filter(ope_id %in% c("87156","87155","87154", "87152")) et renommage variables
  
  filter(sta_id %in% c("12619", "12618", "10671")) %>% 
  mutate(libelle_sta_islet=case_when(
    #sta_id=="12617"~"Retenue",
    sta_id=="12618"~"Ref_amont",
    sta_id=="12619"~"Aval_Ret",
    sta_id=="10671"~"Amont_el"),
    libelle_sta_islet = as.factor(libelle_sta_islet),
    libelle_sta_islet = fct_relevel(libelle_sta_islet,
                                    "Amont_el",
                                    "Ref_amont",
                                    "Retenue",
                                    "Aval_Ret"))
    
names(passerelle)

##########  Bilan par stations IPR & Métriques ###########

ipr <- passerelle %>% 
  select(sta_id:ope_id, libelle_sta_islet) %>% 
  distinct() %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_metriques() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_libelle() %>% 
  filter(ope_date > lubridate::dmy("01/01/2005")) %>% 
  mef_ajouter_libelle() %>% 
  droplevels() %>% 
  filter(!is.na(ipr))

#########
view(ipr)

##### Export CSV IPR ##########

write.csv2(ipr,"IPR_Islet_Aspe.csv", row.names = TRUE)


#### Pivoter le tableau  avec pivot_long ####

metric_long <- ipr %>%
  select(sta_id,ope_id,libelle_sta_islet,annee, ner:dti) %>% 
   pivot_longer(
    cols = ner:dti,
    names_to = "metric",
    values_to = "metric_value", 
    values_drop_na = TRUE) %>% 
  mutate(metric=as.factor(metric),
         metric=fct_rev(metric))




######### Traçage graphique en bâton IPR sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= ipr,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet", x = "station", y = "IPR", fill="Année")

######### graphiques "à la Benoît" ###############
# ajout des codes couleur au dataframe classe_ipr
classe_ipr_islet <- classe_ipr %>%
  aspe::ip_completer_classes_couleur()

# graphique pour l'IPR
aspe::gg_temp_ipr(df_ipr = ipr,
                  var_id_sta = libelle_sta_islet,
                  var_ipr = ipr,
                  df_classes = classe_ipr_islet) +
  coord_cartesian(ylim = c(0, 45))

# Pour les métriques
mes_sites <- levels(metric_long$libelle_sta_islet)
graph_metr <- list()

# faite varier l'index de 1 à 3
site <- 3

metric_long %>% 
  filter(libelle_sta_islet == mes_sites[site]) %>% 
  ggplotExtra::gg_lattice_ipr(
    var_y = metric_value,
    var_lattice = metric,
    metriques = TRUE,
    interactif = FALSE,
    df_classes = classe_ipr_islet,
    nb_colonnes = 7
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.x = ggplot2::element_line(size = .2, color = "grey80"),
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::ggtitle(mes_sites[site])

######### fin graphique "à la Benoît" ###############


######### Traçage graphique en ligne IPR par année sous ggplot ###############
ggplot2::ggplot(data=ipr,
       aes(x = as.factor(annee), 
               y = ipr, fill=libelle_sta_islet)) +
  geom_line()
labs(title = "Suivi IPR Islet", x = "station", y = "IPR", fill="Année")

######### Traçage graphique en points sous ggplot ###############

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    size = ipr,
                    fill = ipr,
                    y=as.factor(annee)))+
  geom_point()+
  labs(title = "Suivi IPR Islet", x = "station", y = "IPR", fill="Année")
######### Traçage graphique en points sous ggplot ###############

ggplot2::ggplot(data=ipr, 
                aes(x=as.factor(annee), 
                    size = ipr,
                    fill = ipr,
                    y=libelle_sta_islet))+
  geom_point()+
  labs(title = "Suivi IPR Islet", x = "station", y = "IPR")

######### Graph en bâton  NTE sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= nte,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique Nb total d'espèces", x = "station", y = "Nb total d'espèces", fill="Année")

######### Graph en bâton  rhéophile sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= ner,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique rhéophile", x = "station", y = "Rhéophiles", fill="Année")

######### Graph en bâton  lithophile sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= nel,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique lithophile", x = "station", y = "Lithophile", fill="Année")

######### Graph en bâton  omnivores  sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= dio,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique densité d'omnivores", x = "station", y = "densité d'omnivores", fill="Année")

######### Graph en bâton  tolerants sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= dio,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique densité de tolerants", x = "station", y = "densité de tolerants", fill="Année")

######### Graph en bâton tolerants sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= dio,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique densité de tolerants", x = "station", y = "densité de tolerants", fill="Année")

########## Graf métriques face_grid ###########
metric_long %>% 
 #sortie de la station eloig
  filter(libelle_sta_islet!="Amont_eloig") %>% 
  mutate(metric_value=log(1+metric_value)) %>% 
ggplot2::ggplot(aes(x=as.factor(annee), 
                    y= metric_value,
                    fill=as.factor(annee)
                    ))+
  geom_bar(position="dodge",
           stat="identity") +
  labs(title = "Suivi IPR Islet : valeur métrique ", x = "annee", y = "valeur métrique", fill="Année"
       )+
  facet_grid(rows=vars(metric),
             cols=vars(libelle_sta_islet),
             scales="free_y")#+
 # scale_y_continuous(#limits=c(0,20)
    #trans = "log")

########## Graf métriques face_grid geom line###########
metric_long %>% 
  #sortie de la station eloig
  filter(libelle_sta_islet!="Amont_eloig") %>% 
  mutate(metric_value=log(1+metric_value)) %>% 
  ggplot2::ggplot(aes(x=as.factor(annee), group=1,
                      y= metric_value#,
                      #fill=as.factor(annee)
  ))+
  geom_line()+
  labs(title = "Suivi IPR Islet : valeur métrique ", x = "annee", y = "valeur métrique"#, fill="Année"
  )+
  facet_grid(rows=vars(metric),
             cols=vars(libelle_sta_islet),
             scales="free_y")#+
# scale_y_continuous(#limits=c(0,20)
#trans = "log")
######### export en tablau .csv ##########
?geom_line


write.csv2(ipr_1c_par_an, file = "processed_data/ipr_bzh_pdl_large.csv",
           row.names = FALSE, na = "")

?facet_grid

# #####Traitemets pour stations Retenue ########
#########Passerelle#########
passerelle <- mef_creer_passerelle()

names(passerelle)
##############  Filtre par code opération ##############

passerelle <- passerelle %>%
  
  # filter(ope_id %in% c("87156","87155","87154", "87152")) et renommage variables
  
  filter(sta_id %in% c("12617")) %>% 
  mutate(libelle_sta_islet=case_when(
    sta_id=="12617"~"Retenue",
    
    libelle_sta_islet = as.factor(libelle_sta_islet),
    libelle_sta_islet = fct_relevel(libelle_sta_islet,
                                    "Retenue"))

names(passerelle)

##########  Bilan par stations IPR & Métriques ###########

ipr <- passerelle %>% 
  select(sta_id:ope_id, libelle_sta_islet) %>% 
  distinct() %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_metriques() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_libelle() %>% 
  filter(ope_date > lubridate::dmy("01/01/2005")) %>% 
  mef_ajouter_libelle() %>% 
  droplevels() %>% 
  filter(!is.na(ipr))

#########
view(ipr)

##### Export CSV IPR ##########

write.csv2(ipr,"IPR_Islet_Aspe_retenue.csv", row.names = TRUE)


#### Pivoter le tableau  avec pivot_long ####

metric_long <- ipr %>%
  select(sta_id,ope_id,libelle_sta_islet,annee, ner:dti) %>% 
  pivot_longer(
    cols = ner:dti,
    names_to = "metric",
    values_to = "metric_value", 
    values_drop_na = TRUE) %>% 
  mutate(metric=as.factor(metric),
         metric=fct_rev(metric))




######### Traçage graphique en bâton IPR sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= ipr,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet", x = "station", y = "IPR", fill="Année")

######### Traçage graphique en ligne IPR par année sous ggplot ###############
ggplot2::ggplot(data=ipr,
                aes(x = as.factor(annee), 
                    y = ipr, fill=libelle_sta_islet)) +
  geom_line()
labs(title = "Suivi IPR Islet", x = "station", y = "IPR", fill="Année")

######### Traçage graphique en points sous ggplot ###############

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    size = ipr,
                    fill = ipr,
                    y=as.factor(annee)))+
  geom_point()+
  labs(title = "Suivi IPR Islet", x = "station", y = "IPR", fill="Année")
######### Traçage graphique en points sous ggplot ###############

ggplot2::ggplot(data=ipr, 
                aes(x=as.factor(annee), 
                    size = ipr,
                    fill = ipr,
                    y=libelle_sta_islet))+
  geom_point()+
  labs(title = "Suivi IPR Islet", x = "station", y = "IPR")

######### Graph en bâton  NTE sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= nte,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique Nb total d'espèces", x = "station", y = "Nb total d'espèces", fill="Année")
