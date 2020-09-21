library(readr)
library(magrittr)
FurtoSROP <- read_delim("FurtoSROP-1-SemAcento1.csv",";", escape_double = FALSE, trim_ws = TRUE)

FurtoSROP$`Tipo Logradouro` %<>% recode("ESCADARIA" = "OUTROS",
                                        "LADEIRA" = "OUTROS",
                                        "TRILHA" = "OUTROS",
                                        "TREVO" = "OUTROS",
                                        "LARGO" = "OUTROS",
                                        "VIELA" = "OUTROS",
                                        "CAMINHO" = "OUTROS",
                                        "BECO" = "OUTROS",
                                        "PASSAGEM" = "OUTROS",
                                        "CALCADAO" = "OUTROS") 
FurtoSROP %<>% mutate(CRs = parse_number(CRs))
FurtoSROP$`Bairros Corrigidos` %<>% str_replace("JD.","JARDIM")
FurtoSROP$`Bairros Corrigidos` %<>% str_replace("JD","JARDIM")
FurtoSROP$`Bairros Corrigidos` %<>% str_replace("PALMEIRAS","PALMEIRA")
FurtoSROP %<>% 
  mutate(Municipio = recode(.$Municipio,"\"FIGUEIROPOLIS D\"\"OESTE\"" = "FIGUEIROPOLIS D\"OESTE",
                                        "\"MIRASSOL D\"\"OESTE\"" = "MIRASSOL D\"OESTE",
                                        "LAMBARI DOESTE" = "LAMBARI D'OESTE",
                                        "CONQUISTA DOESTE" = "CONQUISTA D'OESTE"))


mes2 = c("JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO","JULHO","AGOSTO",   
         "SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO" )
mes = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO",
        "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO") 

######## SEMANA #######
  
  ftSemana = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){  

    taq3 = int_mes
    taq3 = toupper(taq3)
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
    
  FurtoSROP %>% 
    filter(AnoFato == ano) %>% 
    filter(`Mes Extenso Fato` %in% taq3) %>% 
    select(SemanaFato) %>% 
    unique() %>% 
    arrange(SemanaFato) %>% 
    left_join(FurtoSROP %>% 
                filter(AnoFato == ano & CRs == crs) %>% 
                filter(`Mes Extenso Fato` %in% taq3) %>% 
                count(SemanaFato)) %>% 
    replace_na(list(n= 0)) %>% 
    arrange(SemanaFato) %>% 
    ggplot(aes(x = SemanaFato,y = n,group = 1))+
    geom_line(color = "grey50")+
    #geom_line(data = data.frame(X = seq(8,53),Y = zoo::rollmean(ent3$n,8)),
              #mapping = aes(x =X ,y =Y))+
    geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
    geom_text(aes(x = SemanaFato,y = n + mean(n)/14,label = n))+
    ylab(label = "Numero de Homicidio")+
    xlab(label = "Semana")+
    geom_smooth(method = "loess",se = FALSE,span = spa,linetype = "dashed",color = "red",size = 0.5)
    }
    
      ftSemanaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){  
        
        taq3 = int_mes
        taq3 = toupper(taq3)
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
        
        FurtoSROP %>% 
          filter(AnoFato == ano) %>% 
          filter(`Mes Extenso Fato` %in% taq3) %>% 
          select(SemanaFato) %>% 
          unique() %>% 
          arrange(SemanaFato) %>% 
          left_join(FurtoSROP %>% 
                      filter(AnoFato == ano) %>% 
                      filter(`Mes Extenso Fato` %in% taq3) %>% 
                      count(SemanaFato)) %>% 
          replace_na(list(n= 0)) %>% 
          arrange(SemanaFato) %>% 
          ggplot(aes(x = SemanaFato,y = n,group =1))+
          geom_line(color = "grey50")+
          #geom_line(data = data.frame(X = seq(8,53),Y = zoo::rollmean(ent3$n,8)),
          #mapping = aes(x =X ,y =Y))+
          geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
          geom_text(aes(x = SemanaFato,y = n + 50,label = n))+
          ylab(label = "Numero de Homicidio")+
          xlab(label = "Semana")+
          geom_smooth(method = "loess",se = FALSE,span = spa,linetype = "dashed",color = "red",size = 0.5)
      }
  
  
  ####### mapa por cr ##############
  cor = c("#69F585","#12961F","#F5F542","#F5AC40","#FF4D4D")

  ftMapa = function(crs,ano){

    ent3 = DadosIBGE_mt %>% 
      select(Município,Código,cr,MunicipioUp,paste("população_",ano,sep = ""),X,Y) %>% 
      left_join(FurtoSROP%>% 
                  filter(AnoFato == ano) %>%
                  count(Municipio) %>% 
                  mutate(Furto = n,MunicipioUp = Municipio) %>% 
                  select(MunicipioUp,Furto)) %>% 
      setNames(c("Municipio","Codigo","cr","Municipio2","População","X","Y","Furto")) %>% 
      mutate(TaxaFurto = 100000*Furto/População) %>%
      replace_na(list(Furto = 0,TaxaFurto = 0))
    
    eco.sp@data = ent3
    
    tmap_mode("view")
    
    map = tm_shape(eco.sp[eco.sp$cr == crs,])+
      tm_fill("TaxaFurto",breaks = c(0,400,800,1200,2000,5000),
              #style = "cont",
              palette = cor,
              title = "Taxa por 100 mil Habitantes",
              alpha = 0.5,
              popup.vars = c("Taxa Furto" = "TaxaFurto","População" = "População"),
              popup.format=list(TaxaFurto=list(digits=2)))+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      tm_text(text = "Municipio",col = "black",size = 0.85)
      tm_layout(frame = FALSE)
  
    tmap_leaflet(map)
    
    }

    
    cor_mapa3 =   function(ent3){
      cor = c()
      if(any(ent3$TaxaFurto <= 400)){cor = c(cor,"#69F585")}
      if(any(ent3$TaxaFurto <= 800 & ent3$TaxaFurto >400)){cor = c(cor,"#12961F")}
      if(any(ent3$TaxaFurto <= 1200 & ent3$TaxaFurto >800)){cor = c(cor,"#F5F542")}
      if(any(ent3$TaxaFurto <= 2000 & ent3$TaxaFurto >1200)){cor = c(cor,"#F5AC40")}
      if(any(ent3$TaxaFurto <= 5000 & ent3$TaxaFurto >2000)){cor = c(cor,"#FF4D4D")}
      return(cor)
    }                
  
  
  ############## mapa MT  ##########
    
    ftMapaMT = function(ano){
              
    ent3 = DadosIBGE_mt %>% 
      select(Município,Código,cr,MunicipioUp,paste("população_",ano,sep = ""),X,Y) %>% 
      left_join(FurtoSROP%>% 
                  filter(AnoFato == ano) %>%
                  count(Municipio) %>% 
                  mutate(Furto = n,MunicipioUp = Municipio) %>% 
                  select(MunicipioUp,Furto)) %>% 
      setNames(c("Municipio","Codigo","cr","Municipio2","População","X","Y","Furto")) %>% 
      mutate(TaxaFurto = 100000*Furto/População) %>%
      replace_na(list(Furto = 0,TaxaFurto = 0))
    
    eco.sp@data = ent3
    
    tmap_mode("view")
    
    map = tm_shape(eco.sp)+
      tm_fill("TaxaFurto",breaks = c(0,400,800,1200,2000,5000),
              #style = "cont",
              palette = cor,
              title = "Taxa por 100 mil Habitantes",
              alpha = 0.5,
              popup.vars = c("Taxa Furto" = "TaxaFurto","População" = "População"),
              popup.format=list(TaxaFurto=list(digits=2)))+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      #tm_text(text = "Municipio",col = "black",size = 0.85)
      tm_layout(frame = FALSE)
    
    tmap_leaflet(map)
    
    }
    
    
   ########  hora #########
    
    ftHora = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
      
      taq3 = int_mes
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes2[taq3[1]]
    }else{
      taq3 = mes2[taq3[1]:taq3[2]] 
    }
    
    HoraSNI = FurtoSROP %>% 
      filter(AnoFato == ano & CRs == crs & HoraFato != "NI") %>% 
      filter(`Mes Extenso Fato` %in% taq3) %>% 
      select(CRs,AnoFato,HoraFato)
    
    ggplot(HoraSNI)+
      geom_bar(data = HoraSNI %>% count(HoraFato),
               aes(x = as.numeric(HoraFato),y =n/sum(n)),
               stat = "identity",fill = "navy")+
      geom_density(aes(as.numeric(HoraFato)),alpha = 0.6,fill = "gray70")+
      xlab(label = "")+
      ylab(label = "")+
      xlim(-0.6,24)+
      theme_minimal()}
    
    ftHoraMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
      
      taq3 = int_mes
      taq3 = toupper(taq3)
      taq3 = which(mes2 == taq3[1]| mes2 == taq3[2])
      if(length(taq3)==1){
        taq3 = mes2[taq3[1]]
      }else{
        taq3 = mes2[taq3[1]:taq3[2]] 
      }
      
      HoraSNI = FurtoSROP %>% 
        filter(AnoFato == ano& HoraFato != "NI") %>% 
        filter(`Mes Extenso Fato` %in% taq3) %>% 
        select(CRs,AnoFato,HoraFato)
      
      ggplot(HoraSNI)+
        geom_bar(data = HoraSNI %>% count(HoraFato),
                 aes(x = as.numeric(HoraFato),y =n/sum(n)),
                 stat = "identity",fill = "navy")+
        geom_density(aes(as.numeric(HoraFato)),alpha = 0.6,fill = "gray70")+
        xlab(label = "")+
        ylab(label = "")+
        xlim(-0.6,24)+
        theme_minimal()}
    
    
    ######### dia semana ###########
    
      ftDia = function(cr,ano,int_mes = c("JANEIRO","DEZEMBRO")){
      
      taq = cr
      taq2 = ano
      taq3 = int_mes
      taq3 = toupper(taq3)
      taq3 = which(mes == taq3[1]| mes == taq3[2])
      if(length(taq3)==1){
        taq3 = mes2[taq3[1]]
      }else{
        taq3 = mes2[taq3[1]:taq3[2]] 
      }
      
      FurtoSROP %>% 
      filter(AnoFato == taq2 & CRs == taq) %>% 
      filter(`Mes Extenso Fato` %in% taq3) %>%
       mutate(DiaSemana = DataFato %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
                recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                       "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                       "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                       "7" = "SÁBADO")) %>% 
      select(CRs,AnoFato,DiaSemana) %>% 
      count(DiaSemana) %>% 
      mutate(DiaSemana = factor(DiaSemana,
                                levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                           "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
      arrange(DiaSemana) %>% 
      drop_na(DiaSemana) %>% 
      ggplot()+
      geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
      ylab(label = "Frequência")}
    
    ftDiaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
      
      taq2 = ano
      taq3 = int_mes
      taq3 = toupper(taq3)
      taq3 = which(mes == taq3[1]| mes == taq3[2])
      if(length(taq3)==1){
        taq3 = mes2[taq3[1]]
      }else{
        taq3 = mes2[taq3[1]:taq3[2]] 
      }
      
      FurtoSROP %>% 
        filter(AnoFato == taq2) %>% 
        filter(`Mes Extenso Fato` %in% taq3) %>%
        mutate(DiaSemana = DataFato %>% as.Date(format="%d/%m/%Y") %>% lubridate::wday() %>% 
                 recode("1" = "DOMINGO","2" ="SEGUNDA-FEIRA",
                        "3" = "TERÇA-FEIRA","4" = "QUARTA-FEIRA",
                        "5" = "QUINTA-FEIRA","6" = "SEXTA-FEIRA",
                        "7" = "SÁBADO")) %>% 
        select(CRs,AnoFato,DiaSemana) %>% 
        count(DiaSemana) %>% 
        mutate(DiaSemana = factor(DiaSemana,
                                  levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                             "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
        arrange(DiaSemana) %>% 
        drop_na(DiaSemana) %>% 
        ggplot()+
        geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
        ylab(label = "Frequência")}
    
     ######### local ###########
     
     ftLocal = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){                   
     
           taq3 = int_mes
           taq3 = toupper(taq3)
           taq3 = which(mes == taq3[1]| mes == taq3[2])
           if(length(taq3)==1){
             taq3 = mes2[taq3[1]]
           }else{
             taq3 = mes2[taq3[1]:taq3[2]] 
           }
           
          logradouro =  FurtoSROP %>% 
             filter(AnoFato == ano & CRs == crs) %>% 
             filter(`Mes Extenso Fato` %in%  taq3) %>%
             count(`Tipo Logradouro`) %>% 
            arrange(desc(n)) %>% head(7)
           
           ggplot(data = logradouro,aes(x = "",y = n,fill = `Tipo Logradouro`))+
             geom_bar(width = 1,stat = 'identity',color = "gray80")+
             coord_polar("y")+
             scale_fill_brewer(palette="Set2")+
             geom_text(data = logradouro %>% 
                         filter(n > 0) %>% 
                         arrange(desc(n)) %>% 
                         head(5),
                       aes(y = n, x = 1,label = percent(n/sum(n))),
                       position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
             theme_void()
     }
     
     ftLocal = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){                   
       
       taq3 = int_mes
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
       
       logradouro =  FurtoSROP %>% 
         filter(AnoFato == ano & CRs == crs) %>% 
         filter(`Mes Extenso Fato` %in%  taq3) %>%
         count(`Tipo Logradouro`) %>% 
         arrange(desc(n)) %>% head(7)
       
       ggplot(data = logradouro,aes(x = "",y = n,fill = `Tipo Logradouro`))+
         geom_bar(width = 1,stat = 'identity',color = "gray80")+
         coord_polar("y")+
         scale_fill_brewer(palette="Set2")+
         geom_text(data = logradouro %>% 
                     filter(n > 0) %>% 
                     arrange(desc(n)) %>% 
                     head(5),
                   aes(y = n, x = 1,label = percent(n/sum(n))),
                   position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
         theme_void()
     }
     
     ftLocalMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){                   
       
       taq3 = int_mes
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
       
       logradouro =  FurtoSROP %>% 
         filter(AnoFato == ano) %>% 
         filter(`Mes Extenso Fato` %in%  taq3) %>%
         count(`Tipo Logradouro`) %>% 
         arrange(desc(n)) %>% head(7)
       
       ggplot(data = logradouro,aes(x = "",y = n,fill = `Tipo Logradouro`))+
         geom_bar(width = 1,stat = 'identity',color = "gray80")+
         coord_polar("y")+
         scale_fill_brewer(palette="Set2")+
         geom_text(data = logradouro %>% 
                     filter(n > 0) %>% 
                     arrange(desc(n)) %>% 
                     head(5),
                   aes(y = n, x = 1,label = percent(n/sum(n))),
                   position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
         theme_void()
     }
     
      ######### variação Furto ###########
     
     
     ftVar = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
     
       taq3 = int_mes
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
     
       (FurtoSROP %>% 
           filter(AnoFato == ano & CRs == crs) %>% 
           filter(`Mes Extenso Fato` %in%  taq3) %>%
           nrow()/FurtoSROP%>% 
           filter(AnoFato == ano -1 & CRs == crs) %>% 
           filter(`Mes Extenso Fato` %in%  taq3) %>%
           nrow() - 1) %>% percent()
     }
     
     ftVarMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
       
       taq3 = int_mes
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
       
       (FurtoSROP %>% 
           filter(AnoFato == ano) %>% 
           filter(`Mes Extenso Fato` %in%  taq3) %>%
           nrow()/FurtoSROP %>% 
           filter(AnoFato == ano -1) %>% 
           filter(`Mes Extenso Fato` %in%  taq3) %>%
           nrow() - 1) %>% percent()
     }
     
     ######## indice #########
     
     ftIndice = function(crs, ano,tipo,int_mes = c("JANEIRO","DEZEMBRO")){ 
         
         taq3 = int_mes
         taq3 = toupper(taq3)
         taq3 = which(mes == taq3[1]| mes == taq3[2])
         if(length(taq3)==1){
           taq3 = mes2[taq3[1]]
         }else{
           taq3 = mes2[taq3[1]:taq3[2]] 
         }
         
         
         (FurtoSROP %>% 
           filter(AnoFato == ano & CRs == crs & TipoLocal == tipo) %>% 
           filter(`Mes Extenso Fato` %in%  taq3 ) %>% 
           count(TipoLocal) %>% 
           select(n) %>% 
           pull() %>%
           sum()/
          FurtoSROP %>% 
           filter(AnoFato == ano & CRs == crs) %>% 
           filter(`Mes Extenso Fato` %in%  taq3) %>% 
           count(TipoLocal)%$%
           sum(n))%>% percent()}   
     
     ftIndiceMT = function(ano,tipo,int_mes = c("JANEIRO","DEZEMBRO")){ 
       
       taq3 = int_mes
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
       
       
       (FurtoSROP %>% 
           filter(AnoFato == ano & TipoLocal == tipo) %>% 
           filter(`Mes Extenso Fato` %in%  taq3 ) %>% 
           count(TipoLocal) %>% 
           select(n) %>% 
           pull() %>%
           sum()/
           FurtoSROP %>% 
           filter(AnoFato == ano) %>% 
           filter(`Mes Extenso Fato` %in%  taq3) %>% 
           count(TipoLocal)%$%
           sum(n))%>% percent()} 
         
    
      ############ tabelas ######

     ftTabelaMT = function(ano,int_mes = c("JANEIRO","DEZEMBRO")){
     
       taq3 = int_mes
       
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
     
       FurtoSROP %>% 
        filter(AnoFato == ano & `Mes Extenso Fato` %in%  taq3) %>% 
        mutate(cr = CR) %>% 
        count(cr) %>%
        left_join( DadosIBGE_mt %>% 
                    select(cr,paste("população_",ano,sep = "")) %>% 
                    setNames(c("cr","População_Ano")) %>%    
                    aggregate(População_Ano~cr,.,sum) %>%
                    as_tibble() %>% 
                    type.convert()) %>% 
        left_join(FurtoSROP %>% 
                   filter(AnoFato == ano - 1 & `Mes Extenso Fato` %in%  taq3) %>% 
                   mutate(cr = CR) %>% 
                   count(cr) %>% 
                   setNames(c("cr","n2"))%>% 
                   left_join(DadosIBGE_mt %>% 
                               select(cr,paste("população_",ano - 1,sep = "")) %>% 
                               setNames(c("cr","População_AnoAnterior")) %>%    
                               aggregate(População_AnoAnterior~cr,.,sum) %>%
                               as_tibble() %>% 
                               type.convert())) %>% 
        left_join(homicidios18_19 %>% 
                   mutate(cr = parse_number(`RISP Regiao`)) %>% 
                   count(cr,RISP) %>% 
                   select(cr,RISP)) %>% 
       mutate(ComandoRegional = paste(cr,"° CR - ",RISP,sep = "")) %>% 
       rbind(list("Mato Grosso",sum(.$n),sum(.$População_Ano),sum(.$n2),sum(.$População_AnoAnterior),"Mato Grosso","MATO GROSSO")) %>% 
       mutate(TaxaAno = 100000*n/População_Ano, TaxaAnoAnterior = 100000*n2/População_AnoAnterior) %>%
       mutate(Variação = TaxaAno - TaxaAnoAnterior, Furtos = n %>% as.integer(),População = População_Ano %>% as.integer()) %>% 
       select(ComandoRegional,Furtos,População,TaxaAno,Variação)
     
     }
       
 ######### tabela cr ##########################
     
     ftTabela = function(crs,ano,int_mes = c("JANEIRO","DEZEMBRO")){
     
                   taq3 = int_mes
                   
                   taq3 = toupper(taq3)
                   taq3 = which(mes == taq3[1]| mes == taq3[2])
                   if(length(taq3)==1){
                     taq3 = mes2[taq3[1]]
                   }else{
                     taq3 = mes2[taq3[1]:taq3[2]] 
                   }
                   
                   FurtoSROP %>% 
                     filter(AnoFato %in% c(ano,ano-1) & `Mes Extenso Fato` %in%  taq3 & CR == crs) %>%
                     count(Municipio,AnoFato) %>% 
                     spread(key = AnoFato,value = n) %>% 
                     setNames(c("MunicipioUp",paste("Furto_",ano-1,sep = ""),paste("Furto_",ano,sep = ""))) %>% 
                     left_join(DadosIBGE_mt %>% 
                                 filter(parse_number(CR) == crs) %>% 
                                 select(MunicipioUp2,Município,paste("população_",ano,sep = ""),paste("população_",ano - 1,sep = "")) %>% 
                                 mutate(MunicipioUp = MunicipioUp2) %>% 
                                 select(MunicipioUp,Município,paste("população_",ano,sep = ""),paste("população_",ano - 1,sep = ""))) %>% 
                     setNames(c("MunicipioUp","FurtoAnoAnterior",'FurtoAno',"Municipio","PopulaçãoAno","PopulaçãoAnoAnterior")) %>% 
                     replace_na(list(FurtoAno = 0,FurtoAnoAnterior = 0)) %>% 
                     rbind(list("Municipios",sum(.$FurtoAnoAnterior),sum(.$FurtoAno),paste(crs,"° COMANDO REGIONAL",sep = ""),
                                sum(.$PopulaçãoAno),sum(.$PopulaçãoAnoAnterior))) %>% 
                       mutate(TaxaAno = 100000*FurtoAno/PopulaçãoAno,TaxaAnoAnterior = 100000*FurtoAnoAnterior/PopulaçãoAnoAnterior,
                              Var = TaxaAno - TaxaAnoAnterior) %>% 
                     mutate(Furto = FurtoAno %>% as.integer(),População = PopulaçãoAno %>% as.integer()) %>% 
                     select(Municipio,Furto,População,TaxaAno,Var)
                   }
     
     ############## NOVA TABELA ###########
     
     ftBairros = function(crs,ano,tipo,int_mes = c("JANEIRO","DEZEMBRO")){
       taq3 = int_mes
       
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
     
     if(tipo == "TOTAL"){
       FurtoSROP %>% 
         filter( AnoFato == ano & CRs == crs &  `Mes Extenso Fato` %in%  taq3) %>% 
         count(Municipio,`Bairros Corrigidos`) %>% 
         arrange(desc(n)) %>% 
         head(15)
       
     }else{
       if(tipo == "ESTABELECIMENTO COMERCIAL"){
     FurtoSROP %>% 
       filter( AnoFato == ano & CRs == crs & TipoLocal == tipo &  `Mes Extenso Fato` %in%  taq3) %>% 
       count(Municipio,`Bairros Corrigidos`,Tipo_Local_2) %>% 
       arrange(desc(n)) %>% 
       head(15)
       }else{
         FurtoSROP %>% 
           filter( AnoFato == ano & CRs == crs & TipoLocal == tipo &  `Mes Extenso Fato` %in%  taq3) %>% 
           count(Municipio,`Bairros Corrigidos`) %>% 
           arrange(desc(n)) %>% 
           head(15)
        }
       }
      }
      
     
     ftMunicipioMT = function(ano,tipo,int_mes = c("JANEIRO","DEZEMBRO")){
       taq3 = int_mes
       
       taq3 = toupper(taq3)
       taq3 = which(mes == taq3[1]| mes == taq3[2])
       if(length(taq3)==1){
         taq3 = mes2[taq3[1]]
       }else{
         taq3 = mes2[taq3[1]:taq3[2]] 
       }
       
       if(tipo == "TOTAL"){
         FurtoSROP %>% 
           filter( AnoFato == ano &  `Mes Extenso Fato` %in%  taq3) %>% 
           count(CRs,Municipio) %>% 
           mutate(CR = paste(CRs,"°CR",sep = "")) %>% 
           select(CR,Municipio,n) %>% 
           arrange(desc(n)) %>% 
           head(15)
         
       }else{
         FurtoSROP %>% 
           filter( AnoFato == ano & TipoLocal == tipo &  `Mes Extenso Fato` %in%  taq3) %>% 
           count(CRs,Municipio) %>% 
           mutate(CR = paste(CRs,"°CR",sep = "")) %>% 
           select(CR,Municipio,n) %>% 
           arrange(desc(n)) %>% 
           head(15)
       }
     } 
     
                 