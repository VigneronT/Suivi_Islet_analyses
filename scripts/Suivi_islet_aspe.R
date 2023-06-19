library(aspe)
library(tidyverse)
library(DT)

load(file = "../raw_data/rdata/tables_sauf_mei_2023_03_08_15_08_27.RData")

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
  
  filter(sta_id %in% c("12617", "12619", "12618", "10671")) %>% 
  mutate(libelle_sta_islet=case_when(
    sta_id=="12617"~"Plan d'eau",
    sta_id=="12618"~"Ref_amont",
    sta_id=="12619"~"Aval_plan d'eau",
    sta_id=="10671"~"Amont_eloig"),
    libelle_sta_islet = as.factor(libelle_sta_islet),
    libelle_sta_islet = fct_relevel(libelle_sta_islet,
                                    "Amont_eloig",
                                    "Ref_amont",
                                    "Plan d'eau",
                                    "Aval_plan d'eau"))
    
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

######### Traçage graphique en bâton métriques NTE sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= nte,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique Nb total d'espèces", x = "station", y = "Nb total d'espèces", fill="Année")

######### Traçage graphique en bâton métriques rhéophile sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= ner,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique rhéophile", x = "station", y = "Rhéophiles", fill="Année")

######### Traçage graphique en bâton métriques lithophile sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= nel,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique lithophile", x = "station", y = "Lithophile", fill="Année")

######### Traçage graphique en bâton métriques omnivores  sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= dio,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique densité d'omnivores", x = "station", y = "densité d'omnivores", fill="Année")

######### Traçage graphique en bâton métriques tolerants sous ggplot ###############

ipr %>% head() %>% DT::datatable()

ggplot2::ggplot(data=ipr, 
                aes(x=libelle_sta_islet, 
                    y= dio,
                    fill=as.factor(annee)))+
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Suivi IPR Islet : métrique densité de tolerants", x = "station", y = "densité de tolerants", fill="Année")

######### Traçage graphique en bâton métriques tolerants sous ggplot ###############

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

