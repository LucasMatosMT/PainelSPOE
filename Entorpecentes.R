Produtividade <- read_delim("Produtividade.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Produtividade$Upm = str_replace_all(Produtividade$Upm,"\xba","°")

mes2 = c("JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO","JULHO","AGOSTO",   
         "SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO" )
mes = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO",
        "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO") 


Produtividade %<>% 
  mutate(CRs = parse_number(Comando) %>% replace_na(list("CE")) %>% unlist()) %>% 
  mutate(CRs = factor(CRs,levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","CE","0"))) %>% 
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

 Droga = Produtividade %>% 
  filter(`Tipo Material`== "ENTORPECENTE") %>% 
  mutate(Mult = case_when(Unidade == "GRAMAS" ~ 0.001,
                          Unidade == "KILO" ~ 1,
                          Unidade == "PAPELOTE" ~ 0.01,
                          Unidade == "UNIDADE" ~ 0.01,
                          TRUE ~ 0)) %>% 
  mutate(Kilos = Quantidade * Mult) %>% filter(Material %in% c("COCAINA","CRACK","ESCTAZY, MDMA","HAXIXE","HEROINA",
                                                               "MACONHA","PASTA"))

############ tabela Comandos #########

TabelaDrogaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){

taq3 = int_mes
taq3 = toupper(taq3)
taq3 = which(mes == taq3[1]| mes == taq3[2])
if(length(taq3)==1){
  taq3 = mes2[taq3[1]]
}else{
  taq3 = mes2[taq3[1]:taq3[2]] 
}

Droga %>% 
  select(Cidade, CRs, Upm, Kilos,Ano,mes,RISP) %>% 
  filter(mes %in% taq3) %>% aggregate(Kilos ~ CRs + Ano + RISP ,.,sum) %>%
  filter(Ano == ano | Ano == ano - 1 ) %>% 
  spread(key = "Ano",value = "Kilos",c(-1)) %>% 
  arrange(CRs) %>%
  left_join(Droga %>% 
              select(Cidade, CRs, Upm, Kilos,Ano,mes,RISP) %>% 
              filter(Ano == ano & mes %in% taq3) %>% 
              count(CRs)
              ) %>% 
  mutate(CRs = case_when( CRs != "CE" ~ paste(CRs,"º CR - ",RISP,sep = ""),
                          TRUE ~ "COMANDO ESPECIALIZADO")) %>%
  mutate(RISP = NULL) %>% 
  setNames(c("CRs","AnoAnterior","Ano (Kg)","Ocorrências")) %>% 
  rbind(c("MATO GROSSO",sum(.$AnoAnterior),sum(.$`Ano (Kg)`),sum(.$Ocorrências))) %>%
  mutate(Variaçao = percent((`Ano (Kg)` %>% as.numeric()/AnoAnterior %>% as.numeric())-1)) %>% 
  select(CRs,`Ano (Kg)`,Ocorrências,Variaçao)
}

TabelaDroga = function(ano,crs,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(crs == "CE"){
    
    Droga %>% 
      select(Cidade, CRs, Upm, Kilos,Ano,mes,RISP) %>% 
      filter(CRs == "CE" & mes %in% taq3) %>%
      filter(Ano == ano | Ano == ano - 1 ) %>% 
      aggregate(Kilos ~ Cidade + Ano,.,sum) %>% 
      spread(key = "Ano",value = "Kilos") %>% 
      left_join(Droga %>% 
                  select(Cidade, CRs, Upm, Kilos,Ano,mes,RISP) %>% 
                  filter(CRs == "CE" & mes %in% taq3 & Ano == ano) %>% 
                  count(Cidade)) %>% 
      setNames(c("Cidade","AnoAnterior","Ano","Ocorrencia")) %>% 
      replace_na(list(AnoAnterior = 0,Ano = 0,Ocorrencia = 0)) %>%
      rbind(c("COMANDO ESPECIALIZADO",sum(.$AnoAnterior),sum(.$Ano),sum(.$Ocorrencia))) %>% 
      mutate(Ano = Ano %>% as.numeric(),
             AnoAnterior = AnoAnterior %>% as.numeric(),
             Variaçao = percent(Ano/AnoAnterior - 1)) %>% 
      mutate(Variaçao = case_when(Variaçao == "NaN%" ~ "0.0%",
                                  Variaçao == "Inf%" ~ "100.0%",
                                  TRUE ~ Variaçao)) %>% 
      select(Cidade,Ano,Ocorrencia,Variaçao)
  
  }else{
    DadosIBGE_mt %>% 
      select(Município,MunicipioUp,cr) %>% 
      filter(cr == crs) %>% 
      left_join(Droga %>% 
                  select(Cidade, CRs, Upm, Kilos,Ano,mes,RISP) %>% 
                  filter(CRs == crs & mes %in% taq3) %>%
                  filter(Ano == ano | Ano == ano - 1 ) %>% 
                  aggregate(Kilos ~ Cidade + Ano,.,sum) %>% 
                  spread(key = "Ano",value = "Kilos") %>% 
                  left_join(Droga %>% 
                              select(Cidade, CRs, Upm, Kilos,Ano,mes,RISP) %>% 
                              filter(CRs == crs & mes %in% taq3 & Ano == ano) %>% 
                              count(Cidade)) %>% 
                  setNames(c("Cidade","AnoAnterior","Ano","Ocorrencias")),
                by = c("MunicipioUp" = "Cidade")) %>% 
      replace_na(list(AnoAnterior = 0,Ano = 0,Ocorrencias = 0)) %>% 
      rbind(c(paste(crs,"º COMANDO REGIONAL"),"",crs,sum(.$AnoAnterior),sum(.$Ano),sum(.$Ocorrencias))) %>% 
      mutate(Ano = Ano %>% as.numeric(),
             AnoAnterior = AnoAnterior %>% as.numeric(),
             Variaçao = percent(Ano/AnoAnterior - 1)) %>% 
      mutate(Variaçao = case_when(Variaçao == "NaN%" ~ "0.0%",
                                  Variaçao == "Inf%" ~ "100.0%",
                                  TRUE ~ Variaçao)) %>% 
      select(Município,Ano,Ocorrencias,Variaçao)
  }
}  
    


########## Especificar Drogas ##########

EspecifDrogaMT = function(ano,modo = "grafico",int_mes = c("JANEIRO","DEZEMBRO")){

taq3 = int_mes
taq3 = toupper(taq3)
taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  if(modo == "grafico"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3) %>% 
      select(Material,Kilos) %>%
      aggregate(Kilos ~ Material ,.,sum) %>%
      arrange(desc(Kilos)) %>% 
      ggplot(aes(x="", y=Kilos, fill= Material)) +
      geom_bar(stat="identity", width=1,color = "grey80") +
      coord_polar("y", start=0)+
      theme_void()+
      scale_fill_brewer(palette = "Set2")+
      geom_text(aes(x = 1,y = Kilos,label= percent(Kilos/sum(Kilos))),
                color = "black",size = 3.5,position = position_stack(vjust = 0.5))
  }else{
    if(modo == "tabela"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3) %>% 
        select(Material,Kilos) %>%
        aggregate(Kilos ~ Material ,.,sum) %>%
        arrange(desc(Kilos)) %>% 
        mutate(Porcentagem = percent(Kilos/sum(Kilos)))
    }else{
      return("Escolha entre os modos grafico ou tabela")
    }
  }
}

EspecifDroga = function(ano,crs,modo = "grafico",int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  if(modo == "grafico"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      select(Material,Kilos) %>%
      aggregate(Kilos ~ Material ,.,sum) %>%
      arrange(desc(Kilos)) %>% 
      ggplot(aes(x="", y=Kilos, fill= Material)) +
      geom_bar(stat="identity", width=1,color = "grey80") +
      coord_polar("y", start=0)+
      theme_void()+
      scale_color_brewer(palette = "Set1")+
      geom_text(aes(x = 1,y = Kilos,label= percent(Kilos/sum(Kilos))),
                color = "black",size = 3.5,position = position_stack(vjust = 0.5))
  }else{
    if(modo == "tabela"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
        select(Material,Kilos) %>%
        aggregate(Kilos ~ Material ,.,sum) %>%
        arrange(desc(Kilos)) %>% 
        mutate(Porcentagem = percent(Kilos/sum(Kilos)))
    }else{
      return("Escolha entre os modos grafico ou tabela")
    }
  }
}

########## Cidades ##########


CidadeDrogaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){

taq3 = int_mes
taq3 = toupper(taq3)
taq3 = which(mes == taq3[1]| mes == taq3[2])
if(length(taq3)==1){
  taq3 = mes2[taq3[1]]
}else{
  taq3 = mes2[taq3[1]:taq3[2]] 
}

Droga %>% 
  filter(Ano == ano & mes %in% taq3) %>% 
  select(Cidade,CRs,Kilos) %>%
  aggregate(Kilos ~ Cidade + CRs ,.,sum) %>%
  mutate(CRs = case_when( CRs != "CE" ~ paste(CRs,"º CR"),
                          TRUE ~ "CE")) %>% 
  arrange(desc(Kilos)) %>% 
  mutate(Porcentagem = percent(Kilos/sum(Kilos))) %>% 
  head(15)
}

BatalhaoDroga = function(ano,crs,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Droga %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    select(Upm,Cidade,Kilos) %>%
    aggregate(Kilos ~ Upm + Cidade ,.,sum) %>%
    arrange(desc(Kilos)) %>% 
    mutate(Porcentagem = percent(Kilos/sum(Kilos))) %>% 
    head(15)
}

############# Semana ###########

SemanaDrogaMT = function(ano,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){

taq3 = int_mes
taq3 = toupper(taq3)
taq3 = which(mes == taq3[1]| mes == taq3[2])
if(length(taq3)==1){
  taq3 = mes2[taq3[1]]
}else{
  taq3 = mes2[taq3[1]:taq3[2]] 
}

  if(modo == "kilos"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3) %>% 
      select(`Numero da Semana`,Kilos) %>%
      aggregate(Kilos ~ `Numero da Semana`,.,sum) %>% 
      ggplot(aes(x = `Numero da Semana`,y = Kilos))+
      geom_line(color = "grey50")+
      geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
      geom_text(aes(x = `Numero da Semana`,y = Kilos + max(Kilos)/20,label = Kilos %>% round()))+
      ylab(label = "Drogas (Kg)")+
      xlab(label = "Semana")
  }else{
    if(modo == "ocorrencias"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3) %>% 
        select(`Numero da Semana`,Kilos) %>%
        count(`Numero da Semana`) %>% 
        ggplot(aes(x = `Numero da Semana`,y = n))+
        geom_line(color = "grey50")+
        geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
        geom_text(aes(x = `Numero da Semana`,y = n + max(n)/25,label = n %>% round()))+
        ylab(label = "Nº de Ocorrências")+
        xlab(label = "Semana")
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}

SemanaDroga = function(ano,crs,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(modo == "kilos"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      select(`Numero da Semana`,Kilos) %>%
      aggregate(Kilos ~ `Numero da Semana`,.,sum) %>% 
      ggplot(aes(x = `Numero da Semana`,y = Kilos))+
      geom_line(color = "grey50")+
      geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
      geom_text(aes(x = `Numero da Semana`,y = Kilos + max(Kilos)/20,label = Kilos %>% round()))+
      ylab(label = "Drogas (Kg)")+
      xlab(label = "Semana")
  }else{
    if(modo == "ocorrencias"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
        select(`Numero da Semana`,Kilos) %>%
        count(`Numero da Semana`) %>% 
        ggplot(aes(x = `Numero da Semana`,y = n))+
        geom_line(color = "grey50")+
        geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
        geom_text(aes(x = `Numero da Semana`,y = n + max(n)/25,label = n %>% round()))+
        ylab(label = "Nº de Ocorrências")+
        xlab(label = "Semana")
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}

########## Tabela Natureza ##########

NaturezaDrogaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){

taq3 = int_mes
taq3 = toupper(taq3)
taq3 = which(mes == taq3[1]| mes == taq3[2])
if(length(taq3)==1){
  taq3 = mes2[taq3[1]]
}else{
  taq3 = mes2[taq3[1]:taq3[2]] 
}

Droga %>% 
  filter(Ano == ano & mes %in% taq3) %>% 
  select(Natureza,Kilos) %>%
  aggregate(Kilos ~ Natureza ,.,sum) %>%
  left_join(Droga %>% 
              filter(Ano == ano & mes %in% taq3) %>% 
              select(Natureza,Kilos) %>% 
              count(Natureza)
              ) %>% 
  arrange(desc(Kilos)) %>% 
  mutate(Porcentagem = percent(Kilos/sum(Kilos))) %>% 
  head(15)
}

NaturezaDroga = function(ano,crs,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Droga %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    select(Natureza,Kilos) %>%
    aggregate(Kilos ~ Natureza ,.,sum) %>%
    left_join(Droga %>% 
                filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
                select(Natureza,Kilos) %>% 
                count(Natureza)
    ) %>% 
    arrange(desc(Kilos)) %>% 
    mutate(Porcentagem = percent(Kilos/sum(Kilos))) %>% 
    head(15)
}

############ mapa #######################

cor_mapaDroga =   function(ent2){
  cor = c()
  if(any(ent2$Kilos <= 1)){cor = c(cor,"#ADD8E6")}
  if(any(ent2$Kilos <= 30 & ent2$Kilos >1)){cor = c(cor,"#48D1CC")}
  if(any(ent2$Kilos <= 150 & ent2$Kilos >30)){cor = c(cor,"#00BFFF")}
  if(any(ent2$Kilos <= 300 & ent2$Kilos >150)){cor = c(cor,"#0000CD")}
  if(any(ent2$Kilos <= Inf & ent2$Kilos >300)){cor = c(cor,"#000080")}
  return(cor)
}

cor2 = c("#ADD8E6","#48D1CC","#00BFFF","#0000CD","#000080")
divcor2 = c(0,1,30,150,300,Inf)
divcor3 = c(-Inf,0,1,30,300,Inf)

MapaDrogaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){

  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  ent2  = DadosIBGE_mt %>% 
    select(Município,Código,MunicipioUp) %>% 
    left_join(Droga %>% 
                filter(Ano == ano & mes %in% taq3) %>% 
                select(Cidade,Kilos) %>% 
                aggregate(Kilos ~ Cidade ,.,sum),by = c("MunicipioUp" = "Cidade")) %>% 
      replace_na(list(Kilos = 0))
  
  eco.sp@data = ent2
  tmap_mode("view")
  
  map = tm_shape(eco.sp)+
    tm_fill("Kilos",breaks = divcor2,
            #style = "cont",
            palette = cor2,
            title = "Kilos por Cidade",
            alpha = 0.5,
            #popup.vars = c("Taxa Roubo" = "TaxaRoubo","População" = "População"),
            popup.format=(Kilos=list(digits=2))
            )+
    tm_borders()+
    #tm_layout(frame = FALSE)+
    #tm_text(text = "Município",col = "black",size = 0.85)
    tm_layout(frame = FALSE)
  
  tmap_leaflet(map)
}

MapaDroga = function(ano,crs,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(crs == "CE"){
    
    ent2  = DadosIBGE_mt %>% 
      select(Município,Código,MunicipioUp,cr,X,Y) %>% 
      left_join(Droga %>% 
                  filter(Ano == ano & mes %in% taq3 & CRs == "CE") %>% 
                  select(Cidade,Kilos,CRs) %>% 
                  aggregate(Kilos ~ Cidade + CRs,.,sum),by = c("MunicipioUp" = "Cidade")) %>% 
      replace_na(list(Kilos = 0,CRs = "0")) %>%
      mutate(CRs = as.character(CRs)) %>% 
      select(Município,Código,CRs,Kilos,X,Y)
    
    eco.sp@data = ent2
    tmap_mode("view")
    
    map = tm_shape(eco.sp[eco.sp$CRs == "CE",])+
      tm_fill("Kilos",breaks = divcor3,
              #style = "cont",
              palette = cor2,
              title = "Kilos por Cidade",
              alpha = 0.5,
              #popup.vars = c("Taxa Roubo" = "TaxaRoubo","População" = "População"),
              popup.format=(Kilos=list(digits=2))
      )+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      tm_text(text = "Município",col = "black",size = 0.85)+
      tm_layout(frame = FALSE)
    
    tmap_leaflet(map)
    
  }else{
    
    ent2  = DadosIBGE_mt %>% 
      select(Município,Código,MunicipioUp,cr,X,Y) %>% 
      left_join(Droga %>% 
                  filter(Ano == ano & mes %in% taq3) %>% 
                  select(Cidade,Kilos,CRs) %>% 
                  aggregate(Kilos ~ Cidade + CRs,.,sum),by = c("MunicipioUp" = "Cidade")) %>% 
      replace_na(list(Kilos = 0,CRs = "0")) %>% 
      mutate(CRs = as.character(CRs)) %>% 
      mutate(CRs = case_when(CRs == "0" ~ cr,
                             TRUE ~ CRs)) %>%
      filter(CRs != "CE")%>%
      filter(CRs != 2 | Município != "Cuiabá")%>%
      select(Município,Código,CRs,Kilos,X,Y) 
    
    eco.sp@data = ent2
    tmap_mode("view")
    
    map = tm_shape(eco.sp[eco.sp$CRs == crs,])+
      tm_fill("Kilos",breaks = divcor2,
              #style = "cont",
              palette = cor2,
              title = "Kilos por Cidade",
              alpha = 0.5,
              #popup.vars = c("Taxa Roubo" = "TaxaRoubo","População" = "População"),
              popup.format=(Kilos=list(digits=2))
      )+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      tm_text(text = "Município",col = "black",size = 0.85)+
      tm_layout(frame = FALSE)
    
    tmap_leaflet(map)
  
  }
  
}

  ########## DIA SEMANA ########
  
DiaDrogaMT = function(ano,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(modo == "kilos"){
    Droga %>% 
      mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
               recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                      "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                      "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                      "7" = "SÁBADO")) %>%             
      filter(Ano == ano & mes %in% taq3) %>% 
      select(DiaSemana,Kilos) %>% 
      mutate(DiaSemana = factor(DiaSemana,
                                levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                           "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
      aggregate(Kilos ~ DiaSemana ,.,sum) %>% 
      ggplot()+
      geom_bar(aes(x = DiaSemana,y = Kilos),stat = "identity",fill = "navy",width = 0.6)+
      ylab(label = "Drogas (Kg)")
  }else{
    if(modo == "ocorrencias"){
      Droga %>% 
        mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
                 recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                        "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                        "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                        "7" = "SÁBADO")) %>%             
        filter(Ano == ano & mes %in% taq3) %>% 
        select(DiaSemana,Kilos) %>% 
        mutate(DiaSemana = factor(DiaSemana,
                                  levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                             "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
        count(DiaSemana) %>% 
        ggplot()+
        geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
        ylab(label = "Nº de Ocorrências")
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}                

DiaDroga = function(ano,crs,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(modo == "kilos"){
    Droga %>% 
      mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
               recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                      "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                      "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                      "7" = "SÁBADO")) %>%             
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      select(DiaSemana,Kilos) %>% 
      mutate(DiaSemana = factor(DiaSemana,
                                levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                           "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
      aggregate(Kilos ~ DiaSemana ,.,sum) %>% 
      ggplot()+
      geom_bar(aes(x = DiaSemana,y = Kilos),stat = "identity",fill = "navy",width = 0.6)+
      ylab(label = "Drogas (Kg)")
  }else{
    if(modo == "ocorrencias"){
      Droga %>% 
        mutate(DiaSemana = Data %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
                 recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                        "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                        "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                        "7" = "SÁBADO")) %>%             
        filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
        select(DiaSemana,Kilos) %>% 
        mutate(DiaSemana = factor(DiaSemana,
                                  levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                             "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
        count(DiaSemana) %>% 
        ggplot()+
        geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
        ylab(label = "Nº de Ocorrências")
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}   

########### Tabela do Local #############
  
LocalDrogaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){  
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Droga %>% 
    filter(Ano == ano & mes %in% taq3) %>% 
    select(`Local Fato`,Kilos) %>% 
    aggregate(Kilos ~ `Local Fato` ,.,sum) %>% 
    arrange(desc(Kilos)) %>% 
    mutate(Porcentagem = percent(Kilos/sum(Kilos)),
           Kilos = Kilos %>% round(2)) %>% 
    head(15)
} 

LocalDroga = function(ano,crs,int_mes = c("JANEIRO","DEZEMBRO")){  
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  Droga %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
    select(`Local Fato`,Kilos) %>% 
    aggregate(Kilos ~ `Local Fato` ,.,sum) %>% 
    arrange(desc(Kilos)) %>% 
    mutate(Porcentagem = percent(Kilos/sum(Kilos)),
           Kilos = Kilos %>% round(2)) %>% 
    head(15)
}
  
  ############### grafico do mes #######
  
MesDrogaMT = function(ano,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }

  if(modo == "kilos"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3) %>% 
      select(Mes,Kilos) %>% 
      aggregate(Kilos ~ Mes ,.,sum) %>% 
      ggplot(aes(x = Mes,y = Kilos))+
      geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
      geom_text(aes(x = Mes,y = Kilos + max(Kilos)/20,label = Kilos %>% round(2)))+
      ylab(label = "Drogas (Kg)")+
      xlab(label = "Semana") 
  }else{
    if(modo == "ocorrencias"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3) %>% 
        count(Mes) %>% 
        ggplot(aes(x = Mes,y = n))+
        geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
        geom_text(aes(x = Mes,y = n + max(n)/20,label = n))+
        ylab(label = "Nº de Ocorrência")+
        xlab(label = "Semana") 
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}

MesDroga = function(ano,crs,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(modo == "kilos"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      select(Mes,Kilos) %>% 
      aggregate(Kilos ~ Mes ,.,sum) %>% 
      ggplot(aes(x = Mes,y = Kilos))+
      geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
      geom_text(aes(x = Mes,y = Kilos + max(Kilos)/20,label = Kilos %>% round(2)))+
      ylab(label = "Drogas (Kg)")+
      xlab(label = "Semana") 
  }else{
    if(modo == "ocorrencias"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
        count(Mes) %>% 
        ggplot(aes(x = Mes,y = n))+
        geom_bar(fill = "#69b3a2",color = "grey80",stat = "identity")+
        geom_text(aes(x = Mes,y = n + max(n)/20,label = n))+
        ylab(label = "Nº de Ocorrência")+
        xlab(label = "Semana") 
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}  
    
############ grafico de Horas ##################
  
 
HorasDrogaMT = function(ano,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){ 
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(modo == "ocorrencias"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3) %>% 
      mutate(Horas = parse_number(Hora %>% as.character())) %>% 
      count(Horas) %>% 
      ggplot()+
      geom_bar(aes(x = as.numeric(Horas),y =n),
               stat = "identity",fill = "navy")+
      xlab(label = "Horas")+
      ylab(label = "Nº de Ocorrência")+
      xlim(-0.6,23.6)+
      theme_minimal()
  }else{
    if(modo == "kilos"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3) %>% 
        mutate(Horas = parse_number(Hora %>% as.character())) %>% 
        aggregate(Kilos ~ Horas, .,sum) %>% 
        ggplot()+
        geom_bar(aes(x = as.numeric(Horas),y =Kilos),
                 stat = "identity",fill = "navy")+
        xlab(label = "Horas")+
        ylab(label = "Drogas (Kg)")+
        xlim(-0.6,23.6)+
        theme_minimal()
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}

HorasDroga = function(ano,crs,modo = "kilos",int_mes = c("JANEIRO","DEZEMBRO")){ 
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  if(modo == "kilos"){
    Droga %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      mutate(Horas = parse_number(Hora %>% as.character())) %>% 
      aggregate(Kilos ~ Horas, .,sum) %>% 
      ggplot()+
      geom_bar(aes(x = as.numeric(Horas),y =Kilos),
               stat = "identity",fill = "navy")+
      xlab(label = "Horas")+
      ylab(label = "Drogas (Kg)")+
      xlim(-0.6,23.6)+
      theme_minimal()
  }else{
    if(modo == "ocorrencias"){
      Droga %>% 
        filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
        mutate(Horas = parse_number(Hora %>% as.character())) %>% 
        count(Horas) %>% 
        ggplot()+
        geom_bar(aes(x = as.numeric(Horas),y =n),
                 stat = "identity",fill = "navy")+
        xlab(label = "Horas")+
        ylab(label = "Nº de Ocorrência")+
        xlim(-0.6,23.6)+
        theme_minimal()
    }else{
      return("Escolha entre os modos kilos ou ocorrencias")
    }
  }
}  
  
########## Variação ############

varDrogaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  sol1 = (Droga %>% 
      filter(mes %in% taq3 & Ano == ano) %>% 
    aggregate(Kilos ~ Ano,.,sum) %>% select(Kilos) %>% pull() /
      Droga %>% 
      filter(mes %in% taq3 & Ano == ano - 1) %>% 
               aggregate(Kilos ~ Ano,.,sum) %>% select(Kilos) %>% pull())-1
  
  sol2 = Droga %>% 
    filter(mes %in% taq3 & Ano == ano) %>% 
    aggregate(Kilos ~ Ano,.,sum) %>% 
    select(Kilos) %>% pull() -  Droga %>% 
    filter(mes %in% taq3 & Ano == ano - 1) %>% 
    aggregate(Kilos ~ Ano,.,sum) %>% 
    select(Kilos) %>% pull()
  
  return(list(sol1,sol2))
  
}    
    
varDroga = function(ano,crs,int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  sol1 = (Droga %>% 
      filter(mes %in% taq3 & Ano == ano & CRs == crs) %>%
      aggregate(Kilos ~ Ano,.,sum) %>% select(Kilos) %>% pull() /
      Droga %>% 
      filter(mes %in% taq3 & Ano == ano - 1 & CRs == crs) %>% 
      aggregate(Kilos ~ Ano,.,sum) %>% select(Kilos) %>% pull())-1
  
  sol2 = Droga %>% 
    filter(mes %in% taq3 & Ano == ano & CRs == crs) %>% 
    aggregate(Kilos ~ Ano,.,sum) %>% 
    select(Kilos) %>% pull() - Droga %>% 
    filter(mes %in% taq3 & Ano == ano - 1 & CRs == crs) %>% 
    aggregate(Kilos ~ Ano,.,sum) %>% select(Kilos) %>% pull()
  
  return(list(sol1,sol2))
} 


#################### solução ###################

SoluçaoDrogaMT = function(ano,soluçao = "FLAGRANTE",int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  sol1 = (Droga %>% 
      filter(Ano == ano & mes %in% taq3) %>%
      filter(Solucao == soluçao) %>% nrow()/
      Droga %>% 
      filter(Ano == ano & mes %in% taq3) %>% 
      nrow())
  
  sol2 = Droga %>% 
    filter(Ano == ano & mes %in% taq3) %>%
    filter(Solucao == soluçao) %>% 
    nrow() - Droga %>% 
    filter(Ano == ano  - 1 & mes %in% taq3) %>%
    filter(Solucao == soluçao) %>% nrow()
  
  return(list(sol1,sol2))
}

SoluçaoDroga = function(ano,crs ,soluçao = "FLAGRANTE",int_mes = c("JANEIRO","DEZEMBRO")){
  
  taq3 = int_mes
  taq3 = toupper(taq3)
  taq3 = which(mes == taq3[1]| mes == taq3[2])
  if(length(taq3)==1){
    taq3 = mes2[taq3[1]]
  }else{
    taq3 = mes2[taq3[1]:taq3[2]] 
  }
  
  sol1 = (Droga %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>%
      filter(Solucao == soluçao) %>% nrow()/
      Droga %>% 
      filter(Ano == ano & mes %in% taq3 & CRs == crs) %>% 
      nrow())
  
  sol2 = Droga %>% 
    filter(Ano == ano & mes %in% taq3 & CRs == crs) %>%
    filter(Solucao == soluçao) %>% 
    nrow() - Droga %>% 
    filter(Ano == ano - 1 & mes %in% taq3 & CRs == crs) %>%
    filter(Solucao == soluçao) %>% nrow()
  
  return(list(sol1,sol2))
}
