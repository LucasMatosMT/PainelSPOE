
library(readxl)
Prisao <- read_excel("Prisao.xlsx")

mes2 = c("JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO","JULHO","AGOSTO",   
         "SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO" )
mes = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO",
        "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO") 

Prisao %<>% 
  mutate(Idade = case_when(Idade > 100 ~ NaN,
                           TRUE ~ as.numeric(Idade))) %>% 
  mutate(FaixaIdade = case_when(Idade < 18 ~ "",
                                Idade < 23 & Idade >= 18 ~ "18 a 22 anos",
                                Idade < 28 & Idade >= 23~ "23 a 27 anos",
                                Idade < 33 & Idade >= 28~ "28 a 32 anos",
                                Idade < 38 & Idade >= 33~ "33 a 37 anos",
                                Idade < 43 & Idade >= 38~ "38 a 42 anos",
                                Idade < 48 & Idade >= 43~ "43 a 47 anos",
                                Idade < 53 & Idade >= 28~ "48 a 52 anos",
                                Idade < 58 & Idade >= 53~ "53 a 58 anos",
                                Idade < 63 & Idade >= 58~ "58 a 62 anos",
                                Idade < 68 & Idade >= 63~ "63 a 67 anos",
                                Idade < 73 & Idade >= 68~ "68 a 72 anos",
                                Idade < 78 & Idade >= 73~ "73 a 77 anos",
                                Idade < 83 & Idade >= 78~ "78 a 82 anos",
                                Idade < 88 & Idade >= 83~ "83 a 87 anos",
                                Idade >= 88 & Idade < 120 ~ "88 ou +anos",
                                TRUE ~ "")) %>% 
  mutate(CRs = parse_number(Comando) %>% replace_na(list("CE")) %>% unlist()) %>% 
  mutate(CRs = factor(CRs,levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","CE","MT"))) %>% 
  mutate(RISP = case_when(CRs == "1" ~ "CUIABA",
                          CRs == "2" ~ "VARZEA GRANDE",
                          CRs == "3" ~ "SINOP",
                          CRs == "4" ~ "RONDONOPOLIS",
                          CRs == "5" ~ "BARRA DO GARCAS",
                          CRs == "6" ~ "CACERES",
                          CRs == "7" ~ "TANGARA DA SERRA",
                          CRs == "8" ~ "JUINA",
                          CRs == "9" ~ "ALTA FLORESTA",
                          CRs == "10" ~ "VILA RICA",
                          CRs == "11" ~ "PRIMAVERA DO LESTE",
                          CRs == "12" ~ "PONTES E LACERDA",
                          CRs == "13" ~ "AGUA BOA",
                          CRs == "14" ~ "NOVA MUTUM",
                          CRs == "15" ~ "PEIXOTO DE AZEVEDO",
                          TRUE ~ "" ),
         mes = case_when(Mês == "jan" ~ "JANEIRO" ,
                         Mês == "fev" ~ "FEVEREIRO" ,
                         Mês == "mar" ~ "MARCO" ,
                         Mês == "abr" ~ "ABRIL" ,
                         Mês == "mai" ~ "MAIO" ,
                         Mês == "jun" ~ "JUNHO" ,
                         Mês == "jul" ~ "JULHO" ,
                         Mês == "ago" ~ "AGOSTO" ,
                         Mês == "set" ~ "SETEMBRO" ,
                         Mês == "out" ~ "OUTUBRO" ,
                         Mês == "nov" ~ "NOVEMBRO" ,
                         Mês == "dez" ~ "DEZEMBRO" ,
                         TRUE ~ "")) %>% 
  mutate(mes = factor(mes,levels = c("JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO",
                                     "JULHO","AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")),
         Mês = factor(Mês,levels = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")))


######### distribuição idade ##########

priIdadeMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
 
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>%
    filter(Ano == ano & mes %in% taq3) %>% 
    filter(FaixaIdade != "" & Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    count(FaixaIdade) %>% 
    ggplot()+
    geom_bar(aes(x = FaixaIdade, y = n,fill = factor(Sexo)),stat = "identity",
             color = "grey50",fill = "navy",size = 0) 
  
}

priIdade = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>%
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    filter(FaixaIdade != "" & Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    count(FaixaIdade) %>% 
    ggplot()+
    geom_bar(aes(x = FaixaIdade, y = n,fill = factor(Sexo)),stat = "identity",
             color = "grey50",fill = "navy",size = 0) 
  
}
######## cidade ################

priCidadeMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & mes %in% taq3) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    count(CRs,RISP ,`Cidade Ocorrencia`,sort = T) %>% 
    mutate(ComandosRegionais = case_when(CRs == "CE" ~ "COMANDO ESPECIALIZADO",
                                         TRUE ~ paste(CRs,"º CR - ",RISP,sep = ""))) %>%
    mutate(Porcentagem = percent(n/sum(n))) %>% 
    select(ComandosRegionais,`Cidade Ocorrencia`,n,Porcentagem) %>% 
    head(15)
}

priUnidade = function(crs,ano, int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    count(Upm,`Cidade Ocorrencia`,sort = T) %>% 
    mutate(Porcentagem = percent(n/sum(n))) %>% 
    select(Upm,`Cidade Ocorrencia`,n,Porcentagem) %>% 
    head(15)
}

########## tabela #############


priTabelaMT = function(ano,  int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>%
    filter(Ano == ano | Ano == ano - 1) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO" & mes %in% taq3) %>% 
    count(CRs,Ano,sort = T) %>% 
    spread(key = "Ano",value = "n") %>% 
    setNames(c("CRs","AnoAnterior","Ano")) %>% 
    left_join(Prisao %>% select(CRs,RISP) %>% unique()) %>% 
    replace_na(list("AnoAnterior" = 0,"Ano" = 0)) %>%
    mutate(ComandosRegionais = case_when( CRs != "CE" ~ paste(CRs,"º CR - ",RISP,sep = ""),
                                          TRUE ~ "COMANDO ESPECIALIZADO")) %>% 
    rbind(c("MT",sum(.$AnoAnterior),sum(.$Ano),"","MATO GROSSO")) %>% 
    mutate(Ano = Ano %>% as.integer(),
           AnoAnterior = AnoAnterior %>% as.numeric(),
           Variaçao = percent(Ano/AnoAnterior - 1 %>% as.numeric())) %>% 
    select(ComandosRegionais,Ano,Variaçao)
}

priTabela = function(crs,ano,  int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }

  if(crs == "CE"){
    Prisao %>%
      filter(Ano == ano | Ano == ano - 1) %>% 
      filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO" & mes %in% taq3 & CRs == crs) %>% 
      count(`Cidade Ocorrencia`,Ano,sort = T) %>% 
      spread(key = "Ano",value = "n") %>% 
      setNames(c("Cidades","AnoAnterior","Ano")) %>% 
      replace_na(list("AnoAnterior" = 0,"Ano" = 0)) %>% 
      rbind(c(case_when(crs == "CE" ~ "COMANDO ESPECIALIZADO",
                        TRUE ~ paste(crs,"º COMANDO REGIONAL")),
              sum(.$AnoAnterior),sum(.$Ano))) %>% 
      mutate(Ano = Ano %>% as.integer(),
             AnoAnterior = AnoAnterior %>% as.numeric(),
             Variaçao = percent(Ano/AnoAnterior - 1 %>% as.numeric())) %>% 
      select(Cidades,Ano,Variaçao)
  }else{
    DadosIBGE_mt %>% 
      filter(cr == crs) %>% 
      select(Município,MunicipioUp2) %>% 
      mutate(Cidades = MunicipioUp2) %>%
      left_join( Prisao %>%
                   filter(Ano == ano | Ano == ano - 1) %>% 
                   filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO" & mes %in% taq3 & CRs == crs) %>% 
                   count(`Cidade Ocorrencia`,Ano,sort = T) %>% 
                   spread(key = "Ano",value = "n") %>% 
                   setNames(c("Cidades","AnoAnterior","Ano")) %>% 
                   replace_na(list("AnoAnterior" = 0,"Ano" = 0))) %>% 
      replace_na(list("AnoAnterior" = 0,"Ano" = 0)) %>% 
      rbind(c(case_when(crs == "CE" ~ "COMANDO ESPECIALIZADO",
                        TRUE ~ paste(crs,"º COMANDO REGIONAL",sep = "")),"","",
              sum(.$AnoAnterior),sum(.$Ano))) %>% 
      mutate(Ano = Ano %>% as.integer(),
             AnoAnterior = AnoAnterior %>% as.numeric(),
             Variaçao = percent(Ano/AnoAnterior - 1 %>% as.numeric())) %>% 
      mutate(Variaçao = case_when(Variaçao == "NaN%" ~ "0.0%",
                                  Variaçao == "Inf%" ~ "100.0%",
                                  TRUE ~ Variaçao)) %>% 
      select(Município,Ano,Variaçao)
  }
 
}  
############ semana ################

priSemanaMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  
  if(length(taq3)<=3){
    spa = 0.9
  }else{
    if(length(taq3)<= 6){
      spa = 0.58
    }else{
      spa = 0.4
    }
  }
  
  Prisao %>% 
    filter(Ano == ano & Natureza == "PRISAO POR MANDADO - CUMPRIDO" & mes %in% taq3) %>% 
    count(`Numero de Semana`) %>% 
    ggplot(aes(x = `Numero de Semana`,y = n))+
    geom_line(color = "grey50")+
    geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
    geom_text(aes(x = `Numero de Semana`,y = n + max(n)/25,label = n),size = 3.2)+
    ylab(label = "Prisões por Mandado")+
    xlab(label = "Semana")+
    xlim(c(0,53))+
    geom_smooth(method = "loess",se = FALSE,span = spa,linetype = "dashed",color = "red",size = 0.5)
  
}

priSemana = function(crs,ano, int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  
  if(length(taq3)<=3){
    spa = 0.9
  }else{
    if(length(taq3)<= 6){
      spa = 0.58
    }else{
      spa = 0.4
    }
  }
  
  Prisao %>% 
    filter(Ano == ano & Natureza == "PRISAO POR MANDADO - CUMPRIDO" & mes %in% taq3 & CRs == crs) %>% 
    count(`Numero de Semana`) %>% 
    ggplot(aes(x = `Numero de Semana`,y = n))+
    geom_line(color = "grey50")+
    geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
    geom_text(aes(x = `Numero de Semana`,y = n + max(n)/25,label = n),size = 3.2)+
    ylab(label = "Prisões por Mandado")+
    xlab(label = "Semana")+
    xlim(c(0,53))+
    geom_smooth(method = "loess",se = FALSE,span = spa,linetype = "dashed",color = "red",size = 0.5)
  
}

##### modo ############

PriModoMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & Natureza == "PRISAO POR MANDADO - CUMPRIDO" & mes %in% taq3) %>% 
    count(`Modo Operandi`,sort = T) %>% 
    mutate(porcentagem = percent(n/sum(n))) 
    
}

PriModo = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & Natureza == "PRISAO POR MANDADO - CUMPRIDO" & mes %in% taq3 & CRs == crs) %>% 
    count(`Modo Operandi`,sort = T) %>% 
    mutate(porcentagem = percent(n/sum(n))) 
  
}
####### sexo ###############
PriSexoMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & Natureza == "PRISAO POR MANDADO - CUMPRIDO" & Sexo != "0" & mes %in% taq3) %>% 
    count(Sexo,sort = T) %>%  ggplot(aes(x = "", y = n, fill = Sexo))+
    scale_fill_manual(name = "Sexo",values = c("#F08080","#6495ED","#FFE4C4"))+
    geom_bar(width = 1,stat = 'identity',color = "gray80")+
    coord_polar("y",start = 0)+
    geom_text(aes(y = n, x = 1,label = percent(n/sum(n))),
              position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
    theme_void()
}

PriSexo = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & Natureza == "PRISAO POR MANDADO - CUMPRIDO" & Sexo != "0" & mes %in% taq3 & CRs == crs) %>% 
    count(Sexo,sort = T) %>%  ggplot(aes(x = "", y = n, fill = Sexo))+
    scale_fill_manual(name = "Sexo",values = c("#F08080","#6495ED","#FFE4C4"))+
    geom_bar(width = 1,stat = 'identity',color = "gray80")+
    coord_polar("y",start = 0)+
    geom_text(aes(y = n, x = 1,label = percent(n/sum(n))),
              position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
    theme_void()
}
######## por caracteristica social ########

PriCaractMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
Prisao %>% 
    filter(Ano == ano & mes %in% taq3) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
  filter(Sexo != "0") %>% 
    filter(Raça != "NI" & Raça != "NÃO INFORMADO" & Raça != "NÃO INFORMADO") %>% 
    filter(Estatura != "NI" & Estatura != "NÃO INFORMADO" & Estatura != "NÃO INFORMADO") %>% 
    filter(`Porte Fisico` != "NI" & `Porte Fisico` != "NÃO INFORMADO" & `Porte Fisico` != "NÃO INFORMADO") %>% 
    count(Sexo,Raça,Estatura,`Porte Fisico`,sort = T) %>% 
  mutate(Porcentagem = percent(n/sum(n)))
    
    
}

PriCaract = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    filter(Sexo != "0") %>% 
    filter(Raça != "NI" & Raça != "NÃO INFORMADO" & Raça != "NÃO INFORMADO") %>% 
    filter(Estatura != "NI" & Estatura != "NÃO INFORMADO" & Estatura != "NÃO INFORMADO") %>% 
    filter(`Porte Fisico` != "NI" & `Porte Fisico` != "NÃO INFORMADO" & `Porte Fisico` != "NÃO INFORMADO") %>% 
    count(Sexo,Raça,Estatura,`Porte Fisico`,sort = T) %>% 
    mutate(Porcentagem = percent(n/sum(n)))
  
  
} 

########### mapa ######################

  
cor_mapaPrisao =   function(ent2){
  cor = c()
  lab = c()
  if(any(ent2$n <= 0)){cor = c(cor,"#ADD8E6");lab = c(lab,"0")}
  if(any(ent2$n <= 5 & ent2$n >0)){cor = c(cor,"#48D1CC");lab = c(lab,"1 a 5")}
  if(any(ent2$n <= 15 & ent2$n >5)){cor = c(cor,"#00BFFF");lab = c(lab,"6 a 15")}
  if(any(ent2$n <= 40 & ent2$n >15)){cor = c(cor,"#0000CD");lab = c(lab,"16 a 40")}
  if(any(ent2$n <= 500 & ent2$n >40)){cor = c(cor,"#000080");lab = c(lab,"40 ou mais")}
  return(cor)
}

cor2 = c("#ADD8E6","#48D1CC","#00BFFF","#0000CD","#000080")
divcor2 = c(-Inf,1,5,15,40,Inf)

MapaPrisao = function(crs,ano, int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(crs == "CE"){
    ent2 = DadosIBGE_mt %>%
      select(Município,Código,MunicipioUp,X,Y) %>% 
      left_join(Prisao %>% 
                  filter(Ano == ano & mes %in% taq3 & CRs == "CE") %>%
                  filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
                  count(`Cidade Ocorrencia`),
                c("MunicipioUp" = "Cidade Ocorrencia"))%>%
    replace_na(list(n = 0))
    
    eco.sp@data = ent2
    tmap_mode("view")
    
    map = tm_shape(eco.sp[eco.sp$n > 0 ,])+
      tm_fill("n",breaks = divcor2,
              #style = "cont",
              palette = cor2,
              title = "Armas por Cidade",
              alpha = 0.5,
              popup.vars = c("Prisões" = "n")
              #popup.format=(Quantidade=list(digits=2))
      )+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      tm_text(text = "Município",col = "black",size = 0.85)+
      tm_layout(frame = FALSE)
    
    tmap_leaflet(map)

    
  }else{
    ent2 = DadosIBGE_mt %>%
      select(Município,Código,MunicipioUp,X,Y,cr) %>% 
      left_join( Prisao %>% 
                   filter(Ano == ano & mes %in% taq3) %>% 
                   filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
                   count(`Cidade Ocorrencia`),
                 c("MunicipioUp" = "Cidade Ocorrencia")) %>% 
      replace_na(list(n = 0))
    
    eco.sp@data = ent2
    tmap_mode("view")
    
    map = tm_shape(eco.sp[eco.sp$cr == crs ,])+
      tm_fill("n",breaks = divcor2,
              #style = "cont",
              palette = cor2,
              title = "Armas por Cidade",
              alpha = 0.5,
              popup.vars = c("Prisões" = "n")
              #popup.format=(Quantidade=list(digits=2))
      )+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      tm_text(text = "Município",col = "black",size = 0.85)+
      tm_layout(frame = FALSE)
    
    tmap_leaflet(map)
  }
  
}

MapaPrisaoMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  ent2 = DadosIBGE_mt %>%
    select(Município,Código,MunicipioUp) %>% 
    left_join( Prisao %>% 
                 filter(Ano == ano & mes %in% taq3) %>% 
                 filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
                 count(`Cidade Ocorrencia`),
               c("MunicipioUp" = "Cidade Ocorrencia")) %>% 
    replace_na(list(n = 0)) 
  
  eco.sp@data = ent2
  tmap_mode("view")
  
  map = tm_shape(eco.sp)+
    tm_fill("n",breaks = divcor2,
            #style = "cont",
            palette = cor2,
            title = "Prisões por Cidade",
            alpha = 0.5,
            popup.vars = c("Prisões" = "n")
            #popup.format=(Quantidade=list(digits=2))
    )+
    tm_borders()+
    #tm_layout(frame = FALSE)+
    #tm_text(text = "Município",col = "black",size = 0.85)
    tm_layout(frame = FALSE)
  
  tmap_leaflet(map)
  
}

######### horas ############

PriHorasMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & mes %in% taq3) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    mutate(Horas = parse_number(Hora %>% as.character())) %>% 
    count(Horas) %>% 
    ggplot()+
    geom_bar(aes(x = as.numeric(Horas),y =n),
             stat = "identity",fill = "navy")+
    xlab(label = "")+
    ylab(label = "")+
    xlim(-0.6,23.6)+
    theme_minimal()
  
}

PriHoras = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    mutate(Horas = parse_number(Hora %>% as.character())) %>% 
    count(Horas) %>% 
    ggplot()+
    geom_bar(aes(x = as.numeric(Horas),y =n),
             stat = "identity",fill = "navy")+
    xlab(label = "")+
    ylab(label = "")+
    xlim(-0.6,23.6)+
    theme_minimal()
  
}
########### DiaSemana ###########

PriDiaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
             recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                    "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                    "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                    "7" = "SÁBADO")) %>% 
    filter(Ano == ano & mes %in% taq3) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    mutate(DiaSemana = factor(DiaSemana,
                              levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                         "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
    count(DiaSemana, sort = T) %>% 
    ggplot()+
    geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
    ylab(label = "Quantidade")
}

PriDia = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
             recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                    "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                    "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                    "7" = "SÁBADO")) %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    mutate(DiaSemana = factor(DiaSemana,
                              levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                         "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
    count(DiaSemana, sort = T) %>% 
    ggplot()+
    geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
    ylab(label = "Quantidade")
}


######## por mes ###########

PriMesMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & mes %in% taq3) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    count(mes) %>% 
    ggplot(aes(x = mes,y = n))+
    geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
    geom_text(aes(x = mes,y = n + max(n)/20,label = n))+
    ylab(label = "Numero de Prisões por Mandato")+
    xlab(label = "Semana")
}

PriMes = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Prisao %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
    count(mes) %>% 
    ggplot(aes(x = mes,y = n))+
    geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
    geom_text(aes(x = mes,y = n + max(n)/20,label = n))+
    ylab(label = "Numero de Prisões por Mandato")+
    xlab(label = "Semana")
}


####### idade x modo operandi ######

PriCidOcocMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  ((Prisao %>% 
      filter(Ano == ano & mes %in% taq3) %>% 
      filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
      filter(`Cidade Ocorrencia` != `Cidade Suspeito`) %>% nrow())/(Prisao %>% filter(Ano == ano & mes %in% taq3) %>% 
                                                                    filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
                                                                      nrow())*100) %>% 
    round(2) %>% 
    paste(.,"%",sep = "")
  
} 

PriCidOcoc = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  ((Prisao %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
      filter(`Cidade Ocorrencia` != `Cidade Suspeito`) %>% nrow())/(Prisao %>% filter(Ano == ano & mes %in% taq3) %>% 
                                                                      filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% 
                                                                      nrow())*100) %>% 
    round(2) %>% 
    paste(.,"%",sep = "")
  
} 

##### variação ######

    PriVarMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
      taq3 = int_mes %>% toupper()
      taq3 = which(mes == taq3[1]| mes == taq3[2])
      if(length(taq3)==1){
        taq3 = mes2[taq3[1]]
      }else{
        taq3 = mes2[taq3[1]:taq3[2]] 
      }
      
      ((Prisao %>% 
          filter(Ano == ano & mes %in% taq3) %>% 
          filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% nrow()/Prisao %>% 
          filter(Ano == (ano %>% as.numeric()-1) & mes %in% taq3) %>% 
          filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% nrow())-1) %>% 
        round(2) 
    }
    
    
    PriVar = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
      taq3 = int_mes %>% toupper()
      taq3 = which(mes == taq3[1]| mes == taq3[2])
      if(length(taq3)==1){
        taq3 = mes2[taq3[1]]
      }else{
        taq3 = mes2[taq3[1]:taq3[2]] 
      }
      
      ((Prisao %>% 
          filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
          filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% nrow()/Prisao %>% 
          filter(Ano == (ano %>% as.numeric()-1) & mes %in% taq3 & CRs == crs) %>% 
          filter(Natureza == "PRISAO POR MANDADO - CUMPRIDO") %>% nrow())-1) %>% 
        round(2)
    }
  ######## Idade Media ##########
  
  PriIdMdMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
    taq3 = int_mes %>% toupper()
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes2[taq3[1]]
    }else{
      taq3 = mes2[taq3[1]:taq3[2]] 
    }
    
    Prisao %>% 
      filter(Ano == ano & mes %in% taq3) %>% 
      select(Idade) %>%
      filter(Idade != "NaN") %>% 
      pull() %>% mean()
  }
  
  PriIdMd = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
    taq3 = int_mes %>% toupper()
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes2[taq3[1]]
    }else{
      taq3 = mes2[taq3[1]:taq3[2]] 
    }
    
    Prisao %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      select(Idade) %>%
      filter(Idade != "NaN") %>% pull() %>% mean()
  }
  

