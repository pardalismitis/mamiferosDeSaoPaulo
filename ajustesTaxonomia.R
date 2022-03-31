rm(list = ls(all = TRUE))
#install.packages("here")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("vctrs")
#library(tidyverse)
library(xlsx)
#library(ggplot2)
library(here)
library(stringr)
library(dbplyr)
#library(vctrs)

mamiferos = 
  read.csv("occ_v_sp.csv",
           #sep = ";",
           na.strings = c("", "NA"),
           encoding = "UTF-8"
  )

occ_v_sp <- read.csv("occ_v_sp.csv",
                     #sep = ";",
                     na.strings = c("", "NA"),
                     encoding = "UTF-8"
)

mamiferos$genus <- word(mamiferos$species,1)

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
                     gsub("Dasypus septemcinctus septemcinctus","Dasypus septemcinctus",
                                         mamiferos$species))))

unique(sort(mamiferos$species))

write.csv(mamiferos, "occ_v_sp_Clean.csv")

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
                                    TRUE ~ species)) %>%
  dplyr::mutate(genus = case_when(genus == "marmosops" ~ "Marmosops",
                                  TRUE ~ genus)

familia <- subset(mamiferos$genus, is.na(mamiferos$family))

familiaLista <- unique(familia)
