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
#library(vctrs)

mamiferos = 
  read.csv("occ_v_sp.csv",
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

familia <- subset(mamiferos$genus, is.na(mamiferos$family))

familiaLista <- unique(familia)

AccChars <- "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ "
RegChars <- "SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy_"

for(i in 1:ncol(mapeamento)) {       
  mapeamento[ , i] <- chartr(AccChars, RegChars, mapeamento[ , i])
  mapeamento[ , i] <- tolower(mapeamento[ , i])
}




AccChars <- "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ "
RegChars <- "SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy_"

for(i in 1:ncol(mapeamento)) {       
  mapeamento[ , i] <- chartr(AccChars, RegChars, mapeamento[ , i])
  mapeamento[ , i] <- tolower(mapeamento[ , i])
}


mapeamento$X.U.FEFF.UC <- gsub("rio_cautario","Rio Cautário",
                               gsub("ouro_preto","Rio Ouro Preto",
                                    gsub("cazumba","Cazumbá-Iracema",
                                         mapeamento$X.U.FEFF.UC)))


mapeamento$cap <- as.integer(mapeamento$cap)
mapeamento$cap[is.na(mapeamento$cap)] <- 0

for(i in 1:nrow(mamiferos)){
  if (mamiferos$family[!is.na(mamiferos$family)]) {
    mamiferos$family <- mamiferos$family
  } else if (mamiferos$genus == "Alouatta" | "Brachyteles"){
    family <- "Atelidae"
  #} #else if (mapeamento$cap[i] > 50 & mapeamento$cap[i] <= 100){
    #tamanho <- "jovem-adulto"
  #} #else if (mapeamento$cap[i] > 100 & mapeamento$cap[i] <= 150){
   # tamanho <- "adulto"
  #} else if (mapeamento$cap[i] > 150 & mapeamento$cap[i] <= 200){
   # tamanho <- "adulto-senescente"
  } else {
     family <- ""
  }
  mamiferos$family[i] <- family
}


write.xlsx(mapeamento, here("output", "formulario6Mapeamento.xlsx"), sheetName = "formulario6Mapeamento", col.names = TRUE, row.names = TRUE, append = FALSE)