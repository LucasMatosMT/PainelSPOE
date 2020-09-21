library(rdrop2)
library(tidyverse)
library(readxl)
library(readr)


token <- drop_auth()
saveRDS(token, file = "token.rds")


drop_acc() %>% data.frame()


token <<- readRDS("droptoken.rds")

drop_download(dtoken = token,"Produtividade.csv", overwrite = TRUE)
Produtividade = read_delim("Produtividade.csv", ";", escape_double = FALSE, trim_ws = TRUE)


