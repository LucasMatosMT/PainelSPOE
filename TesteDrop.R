library(rdrop2)
library(Rcpp)
library(tidyverse)
library(readxl)
library(readr)


token <- drop_auth()
saveRDS(token, file = "token.rds")


drop_acc() %>% data.frame()


token <<- readRDS("token.rds")

drop_download(dtoken = token,"Produtividade.csv", overwrite = TRUE,"~/PainelSPOE/PainelSPOE")
Produtividade = read_delim("Produtividade.csv", ";", escape_double = FALSE, trim_ws = TRUE)


drop_download(dtoken = token,"BaseUnica.xlsx", overwrite = TRUE,"~/PainelSPOE/PainelSPOE")
drop_download(dtoken = token,"Prisao.xlsx", overwrite = TRUE,"~/PainelSPOE/PainelSPOE")
drop_download(dtoken = token,"homicidios18-19.xlsx", overwrite = TRUE,"~/PainelSPOE/PainelSPOE")
drop_download(dtoken = token,"RouboSROP-3.csv", overwrite = TRUE,"~/PainelSPOE/PainelSPOE")
drop_download(dtoken = token,"FurtoSROP-1-SemAcento1.csv", overwrite = TRUE,"~/PainelSPOE/PainelSPOE")

