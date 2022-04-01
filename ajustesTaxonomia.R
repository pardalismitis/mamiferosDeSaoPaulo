rm(list = ls(all = TRUE))
library(here)
library(stringr)
library(dbplyr)

mamiferos = 
  read.csv("occ_v_sp.csv",
           #sep = ";",
           na.strings = c("", "NA"),
           encoding = "UTF-8"
  )

unique(sort(mamiferos$species))

mamiferos <- mamiferos[!grepl("sp.", mamiferos$species),]
mamiferos <- mamiferos[!grepl("Monodelphis americana/scalops", mamiferos$species),]
mamiferos <- mamiferos[!grepl("Callithrix jacchus X Callithrix aurita", mamiferos$species),]
mamiferos <- mamiferos[!grepl("Callithrix jacchus X Callithrix penicillata", mamiferos$species),]
mamiferos <- mamiferos[!grepl("Callithrix penicillata x Callithrix aurita", mamiferos$species),]
mamiferos <- mamiferos[!grepl("Unidentified rodent", mamiferos$species),]

mamiferos$species <- gsub("Puma yagouaroundi","Herpailurus yagouaroundi",
                     gsub("marmosops paulensis","Marmosops paulensis",
                     gsub("Dasypus septemcinctus septemcinctus","Dasypus septemcinctus",
                     gsub("Leopardus tigrinus","Leopardus guttulus",
                                         mamiferos$species))))
#check
unique(sort(mamiferos$species))

unique(sort(mamiferos$genus))

#get genus from species
mamiferos$genus <- word(mamiferos$species,1)

#check family
unique(sort(mamiferos$family))

#correction
mamiferos$family <- gsub("Felídeos","Felidae", mamiferos$family)

#check order
unique(sort(mamiferos$order))

write.csv(mamiferos, "occ_v_sp.csv")

#alternative
patterns <- c("sp.",
              "Callithrix penicillata x Callithrix aurita",
              "Callithrix jacchus X Callithrix aurita",
              "Callithrix jacchus X Callithrix penicillata",
              "Monodelphis americana/scalops",
              "Unidentified rodent")


occ_v_sp_filter <- occ_v_sp %>%
  dplyr::filter(!grepl(paste(patterns, collapse = "|"), species)) %>%
  dplyr::mutate(species = case_when(species == "Puma yagouaroundi" ~ "Herpailurus yagouaroundi",
                                    species == "marmosops paulensis" ~ "Marmosops paulensis",
                                    species == "Dasypus septemcinctus septemcinctus" ~ "Dasypus septemcinctus",
                                    species == "Leopardus tigrinus" ~ "Leopardus guttulus" #add
                                    TRUE ~ species)) %>%
  dplyr::mutate(genus = case_when(genus == "marmosops" ~ "Marmosops",
                                  genus == "Herpailurus" ~ "Puma" #add
                                  TRUE ~ genus))
  dplyr::mutate(family = case_when(family == "Felídeos" ~ "Felidae", #add
                                 TRUE ~ family)) #add