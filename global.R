library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)
library(tidyverse)

options(shiny.maxRequestSize=100*1024^2) 
library(pmeasyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(editData)
library(nomensland)
library(shinyjs)
options(gsubfn.engine = "R")

# lr <- temp

#input <- list(finess = '750712184', anno = 2019, mese1 = 5, path = '~/Documents/data/mco/')
# listes_api <- referime::get_table('listes_api')
dico_l <- get_dictionnaire_listes()
# get_table('dictionnaire_listes')

struc_listes <- function(){
  listes_api <- purrr::map(dico_l$nom_abrege, function(x){get_liste(x)}) %>%
    purrr::map_depth(1, names) %>%
    unlist() %>%
    unique()
  
  temp <- matrix("", ncol = length(listes_api))
  colnames(temp) <- listes_api
  as_tibble(temp)
}

listes_api <- struc_listes()

listes_api$nom <- "Requête Exemple"
listes_api$abrege <- "exemple"
listes_api$thematique <- "Thématique de l'exemple"
listes_api$actes <- "EBLA003, EPLF002"
listes_api$diags <- "E66"
listes_api$positions_diags <- "toutes"
listes_api$ghm_exclus <- "05K"
listes_api$agemin <- "16"

