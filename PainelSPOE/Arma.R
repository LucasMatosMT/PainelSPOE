Produtividade <- read_delim("Produtividade.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Produtividade$Upm = str_replace_all(Produtividade$Upm,"\xba","°")

mes2 = c("JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO","JULHO","AGOSTO",   
         "SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO" )
mes = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO",
        "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO") 


Produtividade %<>% 
  mutate(CRs = parse_number(Comando) %>% replace_na(list("CE")) %>% unlist()) %>% 
  mutate(CRs = factor(CRs,levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","CE"))) %>% 
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
         mes = case_when(Mes == "jan" ~ "JANEIRO" ,
                         Mes == "fev" ~ "FEVEREIRO" ,
                         Mes == "mar" ~ "MARCO" ,
                         Mes == "abr" ~ "ABRIL" ,
                         Mes == "mai" ~ "MAIO" ,
                         Mes == "jun" ~ "JUNHO" ,
                         Mes == "jul" ~ "JULHO" ,
                         Mes == "ago" ~ "AGOSTO" ,
                         Mes == "set" ~ "SETEMBRO" ,
                         Mes == "out" ~ "OUTUBRO" ,
                         Mes == "nov" ~ "NOVEMBRO" ,
                         Mes == "dez" ~ "DEZEMBRO" ,
                         TRUE ~ "")) %>% 
  mutate(mes = factor(mes,levels = c("JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO",
                                     "JULHO","AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")),
         Mes = factor(Mes,levels = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")))



############ Filtrar Especializado ########
Produtividade %>% count(CRs)





Produtividade %>% 
  filter(Unidade == "REAIS" | Unidade == "REIAS") %>% 
  filter(Ano == 2018) %>% 
  arrange(desc(Quantidade)) %>% 
  select(Cidade,Comando,Quantidade,Ano)





############ Tabela Arma de Fogo Principal MT #########

TabelaArmaMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
    select(CRs,RISP ,Quantidade) %>% aggregate(Quantidade ~ CRs + RISP,.,sum)  %>% 
    arrange(CRs) %>% 
    setNames(c("Comando","RISP","QuantidadeAno")) %>% 
    left_join(Produtividade %>% 
                filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                filter(Ano == ano -1 & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
                select(CRs, Quantidade) %>% aggregate(Quantidade ~ CRs,.,sum)  %>% 
                setNames(c("Comando","QuantidadeAnoAnterior"))) %>% 
    left_join(Produtividade %>% 
                filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                filter(Ano == ano & Material == "ARMA DE BRINQUEDO" & mes %in% taq3 ) %>%
                select(CRs, Quantidade) %>% aggregate(Quantidade ~ CRs,.,sum) %>% 
                setNames(c("Comando","Simulacros"))
    ) %>% 
    replace_na(list(Simulacros = 0)) %>% 
    mutate(QuantidadeAno = QuantidadeAno %>% as.integer(),
           Simulacros = Simulacros %>% as.integer(),
           Comando = case_when(Comando != "CE" ~ paste(Comando,"º CR - ",RISP,sep = ""),
                               TRUE ~ "COMANDO ESPECIALIZADO")) %>% 
    rbind(list("MATO GROSSO","",sum(.$QuantidadeAno),sum(.$QuantidadeAnoAnterior),
               sum(.$Simulacros))) %>% 
    mutate(Variação = paste(((QuantidadeAno/QuantidadeAnoAnterior - 1)*100) %>% round(2),"%",sep = "")) %>% 
    select(Comando,QuantidadeAno,Simulacros,Variação)
}

TabelaArma = function(crs, ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(crs == "CE"){
  DadosIBGE_mt %>% 
    select(Município,MunicipioUp) %>% 
    left_join(Produtividade %>% 
                filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                filter(Ano == ano & Material == "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == "CE") %>% 
                select(Cidade, Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum),
              c("MunicipioUp" = "Cidade")) %>% 
    replace_na(list(Quantidade = 0)) %>%
    setNames(c("Município","MunicipioUp","Simulacros")) %>% 
    left_join(Produtividade %>% 
                filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == "CE") %>% 
                select(Cidade,Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum)  %>% 
                setNames(c("Municipio","QuantidadeAno")) %>% 
                replace_na(list("QuantidadeAno" = 0)),c("MunicipioUp" = "Municipio")) %>% 
    replace_na(list(QuantidadeAno = 0)) %>% 
    left_join(Produtividade %>% 
                filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                filter(Ano == ano - 1 & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == "CE") %>% 
                select(Cidade,Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum)  %>% 
                setNames(c("Municipio","QuantidadeAnoAnterior")),c("MunicipioUp" = "Municipio")) %>% 
    replace_na(list(QuantidadeAnoAnterior = 0)) %>% 
    mutate(QuantidadeAno = QuantidadeAno %>% as.integer(),
           Simulacros = Simulacros %>% as.integer()) %>% 
    rbind(list("TOTAL COMANDO","",sum(.$Simulacros),
               sum(.$QuantidadeAno),sum(.$QuantidadeAnoAnterior))) %>%  
    mutate(Variaçao = paste(((QuantidadeAno/QuantidadeAnoAnterior - 1)*100) %>% round(2),"%",sep = "")) %>% 
    mutate(Variaçao = case_when(Variaçao == "Inf%" ~ "100%",
                                Variaçao == "NaN%" ~ "0.0%",
                                TRUE ~ Variaçao)) %>% 
    select(Município,QuantidadeAno,Simulacros,Variaçao) %>% 
    filter(QuantidadeAno != 0) %>% 
    head(20) %>% arrange(QuantidadeAno %>% desc())
  }else{
    DadosIBGE_mt %>% 
      select(Município,MunicipioUp,cr) %>% 
      left_join(Produtividade %>% 
                  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                  filter(Ano == ano & Material == "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
                  select(Cidade, Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum),
                c("MunicipioUp" = "Cidade")) %>% 
      replace_na(list(Quantidade = 0)) %>%
      setNames(c("Município","MunicipioUp","cr","Simulacros")) %>% 
      filter(cr == crs) %>% 
      left_join(Produtividade %>% 
                  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                  filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
                  select(Cidade,Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum)  %>% 
                  setNames(c("Municipio","QuantidadeAno")) %>% 
                  replace_na(list("QuantidadeAno" = 0)),c("MunicipioUp" = "Municipio")) %>% 
      replace_na(list(QuantidadeAno = 0)) %>% 
      left_join(Produtividade %>% 
                  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                  filter(Ano == ano - 1 & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
                  select(Cidade,Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum)  %>% 
                  setNames(c("Municipio","QuantidadeAnoAnterior")),c("MunicipioUp" = "Municipio")) %>% 
      replace_na(list(QuantidadeAnoAnterior = 0)) %>% 
      mutate(QuantidadeAno = QuantidadeAno %>% as.integer(),
             Simulacros = Simulacros %>% as.integer()) %>% 
      rbind(list("TOTAL COMANDO","",crs,sum(.$Simulacros),sum(.$QuantidadeAno),sum(.$QuantidadeAnoAnterior))) %>% 
      mutate(Variaçao = paste(((QuantidadeAno/QuantidadeAnoAnterior - 1)*100) %>% round(2),"%",sep = "")) %>% 
      mutate(Variaçao = case_when(Variaçao == "Inf%" ~ "100%",
                                  Variaçao == "NaN%" ~ "0.0%",
                                  TRUE ~ Variaçao)) %>% 
      select(Município,QuantidadeAno,Simulacros,Variaçao)
  }
}


############ mapa ###########

cor_mapaArma =   function(ent2){
  cor = c()
  if(any(ent2$Quantidade <= 8)){cor = c(cor,"#ADD8E6")}
  if(any(ent2$Quantidade <= 15 & ent2$Quantidade >8)){cor = c(cor,"#48D1CC")}
  if(any(ent2$Quantidade <= 30 & ent2$Quantidade >15)){cor = c(cor,"#00BFFF")}
  if(any(ent2$Quantidade <= 50 & ent2$Quantidade >30)){cor = c(cor,"#0000CD")}
  if(any(ent2$Quantidade <= 500 & ent2$Quantidade >50)){cor = c(cor,"#000080")}
  return(cor)
}

cor2 = c("#ADD8E6","#48D1CC","#00BFFF","#0000CD","#000080")
divcor2 = c(0,8,15,30,50,Inf)

MapaArmaMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  ent2 = DadosIBGE_mt %>%
    select(Município,Código,MunicipioUp) %>% 
    left_join(Produtividade %>% 
                filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
                select(Cidade ,Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum),
              c("MunicipioUp" = "Cidade")) %>% 
    replace_na(list(Quantidade = 0))
  
  eco.sp@data = ent2
  tmap_mode("view")
  
  map = tm_shape(eco.sp)+
    tm_fill("Quantidade",breaks = divcor2,
            #style = "cont",
            palette = cor2,
            title = "Armas por Cidade",
            alpha = 0.5
            #popup.vars = c("Taxa Roubo" = "TaxaRoubo","População" = "População"),
            #popup.format=(Quantidade=list(digits=2))
    )+
    tm_borders()+
    #tm_layout(frame = FALSE)+
    #tm_text(text = "Município",col = "black",size = 0.85)
    tm_layout(frame = FALSE)
  
  tmap_leaflet(map)
  
  

}

MapaArma = function(crs, ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }

  if(crs == "CE"){
    ent2 = DadosIBGE_mt %>%
      select(Município,Código,MunicipioUp,X,Y) %>% 
      left_join(Produtividade %>% 
                  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO") & Comando == "COMANDO ESPECIALIZADO") %>% 
                  filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
                  select(Cidade ,Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum),
                c("MunicipioUp" = "Cidade")) %>% 
      replace_na(list(Quantidade = 0))
    
    eco.sp@data = ent2
    tmap_mode("view")
    
    map = tm_shape(eco.sp[eco.sp$Quantidade > 0 ,])+
      tm_fill("Quantidade",breaks = divcor2,
              #style = "cont",
              palette = cor2,
              title = "Armas por Cidade",
              alpha = 0.5
              #popup.vars = c("Taxa Roubo" = "TaxaRoubo","População" = "População"),
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
      left_join(Produtividade %>% 
                  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
                  filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs %in% 1:15) %>% 
                  select(Cidade ,Quantidade) %>% aggregate(Quantidade ~ Cidade,.,sum),
                c("MunicipioUp" = "Cidade")) %>% 
      replace_na(list(Quantidade = 0))
    
    eco.sp@data = ent2
    tmap_mode("view")
    
    map = tm_shape(eco.sp[eco.sp$cr == crs ,])+
      tm_fill("Quantidade",breaks = divcor2,
              #style = "cont",
              palette = cor2,
              title = "Armas por Cidade",
              alpha = 0.5
              #popup.vars = c("Taxa Roubo" = "TaxaRoubo","População" = "População"),
              #popup.format=(Quantidade=list(digits=2))
      )+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      tm_text(text = "Município",col = "black",size = 0.85)+
      tm_layout(frame = FALSE)
    
    tmap_leaflet(map)
  }  
}


############ Especeficar Armas (Grafico de Pizza)#######

EspecificarArmaMT = function(ano,modo = "grafico", int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  } 
  
  if(modo == "grafico"){
    Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano & Material != "ARMA DE BRINQUEDO") %>% 
      select(Material ,Quantidade) %>% aggregate(Quantidade ~ Material,.,sum) %>% 
      arrange(desc(Quantidade)) %>% 
      mutate(prop = Quantidade/sum(.$Quantidade)*100) %>% 
      mutate(ypos = cumsum(prop) - 0.5 * prop) %>% 
      ggplot(aes(x="", y=Quantidade, fill= Material)) +
      geom_bar(stat="identity", width=1,color = "grey80") +
      coord_polar("y", start=0)+
      theme_void()+
      scale_color_brewer(palette = "Set1")+
      geom_text(aes(x = 1,y = Quantidade,label= percent(Quantidade/sum(Quantidade))),
                color = "black",size = 3.5,position = position_stack(vjust = 0.5))
  }else{
    if(modo == "tabela"){
      Produtividade %>% 
        filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
        filter(Ano == ano & Material != "ARMA DE BRINQUEDO") %>% 
        select(Material ,Quantidade) %>% aggregate(Quantidade ~ Material,.,sum) %>% 
        arrange(desc(Quantidade)) %>% 
        mutate(Porcentagem = percent(Quantidade/sum(Quantidade)),
               Quantidade = Quantidade %>% as.integer())
    }else{
      return("escolha um modo Valido")
    }
  }
}
 
EspecificarArma = function(crs, ano,modo = "grafico", int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  } 
  
  if(modo == "grafico"){
    Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
      select(Material ,Quantidade) %>% aggregate(Quantidade ~ Material,.,sum) %>% 
      arrange(desc(Quantidade)) %>% 
      mutate(prop = Quantidade/sum(.$Quantidade)*100) %>% 
      mutate(ypos = cumsum(prop) - 0.5 * prop) %>% 
      ggplot(aes(x="", y=Quantidade, fill= Material)) +
      geom_bar(stat="identity", width=1,color = "grey80") +
      coord_polar("y", start=0)+
      theme_void()+
      scale_color_brewer(palette = "Set1")+
      geom_text(aes(x = 1,y = Quantidade,label= percent(Quantidade/sum(Quantidade))),
                color = "black",size = 3.5,position = position_stack(vjust = 0.5))
  }else{
    if(modo == "tabela"){
      Produtividade %>% 
        filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
        filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
        select(Material ,Quantidade) %>% aggregate(Quantidade ~ Material,.,sum) %>% 
        arrange(desc(Quantidade)) %>% 
        mutate(Porcentagem = percent(Quantidade/sum(Quantidade)),
               Quantidade = Quantidade %>% as.integer())
    }else{
      return("escolha um modo Valido")
    }
  }
}
############ Apreensões por Cidades ######### 


BatalhaoArma = function(crs, ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  } 
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
    select(Upm,Cidade ,Quantidade) %>% aggregate(Quantidade ~ Upm + Cidade,.,sum) %>% 
    arrange(desc(Quantidade)) %>% 
    mutate(Porcentagem = percent(Quantidade/sum(Quantidade)),
           Quantidade = Quantidade %>% as.integer()) %>%
    select(Upm,Cidade,Quantidade , Porcentagem) %>% 
    head(15)
}

CidadeArmaMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  } 
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
    select(Cidade,Comando ,Quantidade) %>% aggregate(Quantidade ~ Cidade + Comando,.,sum) %>% 
    arrange(desc(Quantidade)) %>% 
    mutate(Porcentagem = percent(Quantidade/sum(Quantidade)),
           Quantidade = Quantidade %>% as.integer()) %>%
    mutate(CR = case_when(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",Comando) ~ paste(parse_number(Comando),"º CR",sep = ""),
                          TRUE ~ "CE")) %>% 
    select(Cidade,CR,Quantidade , Porcentagem) %>% 
    head(15)
}

############ Apreensões por Semana ##########


SemanaArmaMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  } 
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
    select(`Numero da Semana`,Quantidade) %>% aggregate(Quantidade ~ `Numero da Semana`,.,sum) %>%
    ggplot(aes(x = `Numero da Semana`,y = Quantidade))+
    geom_line(color = "grey50")+
    geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
    geom_text(aes(x = `Numero da Semana`,y = Quantidade+ max(Quantidade)/22,label = Quantidade))+
    ylab(label = "Numero de Homicidio")+
    xlab(label = "Semana")
}

SemanaArma = function(crs, ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  } 
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
    select(`Numero da Semana`,Quantidade) %>% aggregate(Quantidade ~ `Numero da Semana`,.,sum) %>%
    ggplot(aes(x = `Numero da Semana`,y = Quantidade))+
    geom_line(color = "grey50")+
    geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
    geom_text(aes(x = `Numero da Semana`,y = Quantidade + max(Quantidade)/22,label = Quantidade))+
    ylab(label = "Numero de Homicidio")+
    xlab(label = "Semana")
}


############ Apreensoes por Mes ########

MesArmaMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }  
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 ) %>% 
    select(Mes,mes,Quantidade) %>% aggregate(Quantidade ~ Mes + mes,.,sum) %>%
    arrange(mes) %>% 
    ggplot(aes(x = Mes,y = Quantidade))+
    geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
    geom_text(aes(x = Mes,y = Quantidade+ 5,label = Quantidade))+
    ylab(label = "Numero de Homicidio")+
    xlab(label = "Semana")
}

MesArma = function(crs, ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }  
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
    select(Mes,mes,Quantidade) %>% aggregate(Quantidade ~ Mes + mes,.,sum) %>%
    arrange(mes) %>% 
    ggplot(aes(x = Mes,y = Quantidade))+
    geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
    geom_text(aes(x = Mes,y = Quantidade+ 2,label = Quantidade))+
    ylab(label = "Numero de Homicidio")+
    xlab(label = "Semana")
}

############ Dia Semana ###########

DiaSemanaArmaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){

taq3 = int_mes %>% toupper()
taq3 = which(mes == taq3[1]| mes == taq3[2])
if(length(taq3)==1){
  taq3 = mes2[taq3[1]]
}else{
  taq3 = mes2[taq3[1]:taq3[2]] 
}  

Produtividade %>% 
  mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
           recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                  "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                  "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                  "7" = "SÁBADO")) %>%             
  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
  filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 ) %>% 
  mutate(DiaSemana = factor(DiaSemana,
                            levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                       "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
  select(DiaSemana,Quantidade) %>% aggregate(Quantidade ~ DiaSemana,.,sum) %>% 
  arrange(DiaSemana) %>% 
  ggplot()+
  geom_bar(aes(x = DiaSemana,y = Quantidade),stat = "identity",fill = "navy",width = 0.6)+
  ylab(label = "Drogas (Kg)")
}  

DiaSemanaArma = function(ano,crs,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes %>% toupper()
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }  
  
  Produtividade %>% 
    mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
             recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                    "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                    "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                    "7" = "SÁBADO")) %>%             
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs ) %>% 
    mutate(DiaSemana = factor(DiaSemana,
                              levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                         "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
    select(DiaSemana,Quantidade) %>% aggregate(Quantidade ~ DiaSemana,.,sum) %>% 
    arrange(DiaSemana) %>% 
    ggplot()+
    geom_bar(aes(x = DiaSemana,y = Quantidade),stat = "identity",fill = "navy",width = 0.6)+
    ylab(label = "Drogas (Kg)")
}  

############ tabela Natureza #########

NaturezaArmaMT = function(ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }  
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
    count(Natureza) %>% 
    arrange(desc(n)) %>%
    mutate(Porcentagem = percent(n/sum(n))) %>% 
    head(15)
}

NaturezaArma = function(crs, ano, int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }  
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
    count(Natureza) %>% 
    arrange(desc(n)) %>%
    mutate(Porcentagem = percent(n/sum(n))) %>% 
    head(15)
}

############ tabela do Local #######

LocalArmaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){

  taq3 = int_mes
taq3 = toupper(taq3)
taq3 = which(mes == taq3[1]| mes == taq3[2])
if(length(taq3)==1){
  taq3 = mes2[taq3[1]]
}else{
  taq3 = mes2[taq3[1]:taq3[2]] 
}  
  
Produtividade %>% 
  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
  filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>%
  count(`Local Fato`) %>% arrange(desc(n)) %>% 
  mutate(Porcentagem = percent(n/sum(n))) %>% 
  head(15)
}


LocalArma = function(crs, ano,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }  
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>%
    count(`Local Fato`) %>% arrange(desc(n)) %>% 
    mutate(Porcentagem = percent(n/sum(n))) %>% 
    head(15)
}
############ Grafico Horas #############

HorasArmaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){

taq3 = int_mes
taq3 = toupper(taq3)
taq3 = which(mes == taq3[1]| mes == taq3[2])
if(length(taq3)==1){
  taq3 = mes2[taq3[1]]
}else{
  taq3 = mes2[taq3[1]:taq3[2]] 
}

Produtividade %>% 
  filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
  filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
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

HorasArma = function(crs, ano,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
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
  
############ Variação ###################

varArmaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano | Ano == ano - 1 ) %>% 
      filter(Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>%
      aggregate(Quantidade ~ Ano,.,sum) %>% 
      spread(key = "Ano",value = "Quantidade") %>% 
      setNames(c("Ano1","Ano2")) %>% 
      mutate(var = (Ano2/Ano1)-1) %>% select(var) %>% pull()
}      


varArma = function(crs, ano,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  ((Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
      nrow() / Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano -1 & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
      nrow())-1)
}

############ Solução ###############

SoluçaoArmaMT = function(ano,soluçao = "FLAGRANTE",int_mes = c("JANEIRO","DEZEMBRO")){
  
  ano = ano %>% as.integer()
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  sol1 = Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
      filter(Solucao == soluçao) %>% nrow()/
      Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
      nrow()
  
  sol2 = Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano  & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
    filter(Solucao == soluçao) %>% nrow() - Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano - 1 & Material != "ARMA DE BRINQUEDO" & mes %in% taq3) %>% 
    filter(Solucao == soluçao) %>% nrow()
  
  return(list(sol1,sol2))
}

SoluçaoArma = function(crs, ano,soluçao = "FLAGRANTE",int_mes = c("JANEIRO","DEZEMBRO")){
  
  ano = ano %>% as.integer()
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  sol1 = Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>%
      filter(Solucao == soluçao) %>% nrow()/
      Produtividade %>% 
      filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
      filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>% 
      nrow()
  
  sol2 = Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>%
    filter(Solucao == soluçao) %>% nrow() - Produtividade %>% 
    filter(str_detect(.$`Tipo Material`,"ARMA DE FOGO")) %>% 
    filter(Ano == ano - 1 & Material != "ARMA DE BRINQUEDO" & mes %in% taq3 & CRs == crs) %>%
    filter(Solucao == soluçao) %>% nrow()
  
  return(list(sol1,sol2))
}





