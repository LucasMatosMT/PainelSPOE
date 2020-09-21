server <- function(input, output,session) {
  library(shinydashboard)
  library(tidyverse)
  library(readxl)
  library(shinyWidgets)
  library(stringr)
  library(leaflet)
  library(leaflet.extras)
  library(rgeos)
  library(rgdal)
  library(tmap)
  library(rvest)

  
  DadosIBGE_mt <- read_excel("DadosIBGE_mt.xlsx",range = "a1:w142")
  BaseUnica <- read_excel("BaseUnica.xlsx")
  
  meses = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho",
    "Agosto","Setembro","Outubro","Novembro","Dezembro")
  
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Mapa das Unidades",tabName = "mapaunidades",icon = icon("map-marked-alt")
              ),
      menuItem("Homicidios",tabName = "homicidios",icon = icon("hospital-symbol"),
               menuItem("Mato Grosso", icon = icon("globe-americas"),
                        menuSubItem("Painel Mato Grosso", tabName = "estado",icon = icon("chart-bar")),
               selectInput(inputId = "anomt",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
               sliderTextInput(
                 inputId = "mesMT",
                 label = "Intervalo de Meses:",
                 choices = meses,
                 selected = meses[c(1, 12)]
               )),
      menuItem("Comandos Regionais", icon = icon("info"),
               menuSubItem("Painel Comandos", tabName = "comandos",icon = icon("chart-bar")),
               sliderInput(inputId = "classes",
                             label = "Comando Regional:",
                             min = 1,
                             max = 15,
                             value = 1),
               selectInput(inputId = "ano",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
               sliderTextInput(
                 inputId = "mes",
                 label = "Intervalo de Meses:",
                 choices = meses,
                 selected = meses[c(1, 12)]
                )
      )),
      menuItem("Roubo",tabName = "rb",icon = icon("walking"),
               menuItem("Mato Grosso", icon = icon("globe-americas"),
                        menuSubItem("Painel Mato Grosso",tabName = "rbEstado",icon = icon("chart-bar")),
                           selectInput(inputId = "rbAnoMT",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                           sliderTextInput(inputId = "rbMesMT",
                                           label = "Intervalo de Meses:",
                                           choices = meses,
                                           selected = meses[c(1, 12)]
                                            )
                           ),
               menuItem("Comandos Regionais", icon = icon("chart-bar"),
                        menuSubItem("Painel Comandos", tabName = "rbComando",icon = icon("chart-bar")),
                           sliderInput(inputId = "rbComandos",
                                        label = "Comando Regional:",
                                        min = 1,
                                        max = 15,
                                        value = 1),
                           selectInput(inputId = "rbAno",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                           sliderTextInput(
                             inputId = "rbMes",
                             label = "Intervalo de Meses:",
                             choices = meses,
                             selected = meses[c(1, 12)])
                            )
                ),
      menuItem("Furto",tabName = "rb",icon = icon("user-ninja",class = "solid"),
               menuItem("Mato Grosso", icon = icon("globe-americas"),
                        menuSubItem("Painel Mato Grosso",tabName = "ftEstado",icon = icon("chart-bar")),
                        selectInput(inputId = "ftAnoMT",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                        sliderTextInput(inputId = "ftMesMT",
                                        label = "Intervalo de Meses:",
                                        choices = meses,
                                        selected = meses[c(1, 12)]
                        )
               ),
               menuItem("Comandos Regionais", icon = icon("chart-bar"),
                        menuSubItem("Painel Comandos", tabName = "ftComando",icon = icon("chart-bar")),
                        sliderInput(inputId = "ftComandos",
                                    label = "Comando Regional:",
                                    min = 1,
                                    max = 15,
                                    value = 1),
                        selectInput(inputId = "ftAno",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                        sliderTextInput(
                          inputId = "ftMes",
                          label = "Intervalo de Meses:",
                          choices = meses,
                          selected = meses[c(1, 12)])
               )
      ),
      menuItem("Produtividade",tabName = "Prod",icon = icon("bars"),
               menuItem("Armas de Fogo",tabName = "ArmasFogo",icon = icon("crosshairs"),
                        menuItem("Mato Grosso",icon = icon("globe-americas"),
                                 menuSubItem("Painel Mato Grosso",tabName = "ArmaEstado",icon = icon("chart-bar")),
                                 selectInput(inputId = "ArmaAnoMT",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                                 sliderTextInput(inputId = "ArmaMesMT",
                                                 label = "Intervalo de Meses:",
                                                 choices = meses,
                                                 selected = meses[c(1, 12)]
                                 )),
                        menuItem("Comandos Regionais",icon = icon("chart-bar"),
                                 menuSubItem("Painel Comandos",tabName = "ArmaComandos",icon = icon("chart-bar")),
                                 sliderTextInput(
                                   inputId = "ArmaComandos",
                                   label = "Comandos Regionais:",
                                   choices = c(1:15,"CE"),
                                   selected = "1"
                                 ),
                                 selectInput(inputId = "ArmaAno",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                                 sliderTextInput(
                                   inputId = "ArmaMes",
                                   label = "Intervalo de Meses:",
                                   choices = meses,
                                   selected = meses[c(1, 12)])
                                 )),
               menuItem("Entorpecentes",tabName = "Drogas",icon = icon("cannabis"),
                        menuItem("Mato Grosso",icon = icon("globe-americas"),
                                 menuSubItem("Painel Mato Grosso",tabName = "DrogaEstado",icon = icon("chart-bar")),
                                 selectInput(inputId = "DrogaAnoMT",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                                 sliderTextInput(inputId = "DrogaMesMT",
                                                 label = "Intervalo de Meses:",
                                                 choices = meses,
                                                 selected = meses[c(1, 12)]
                                 )
                        ),
                        menuItem("Comandos Regionais",icon = icon("chart-bar"),
                                 menuSubItem("Painel Comandos",tabName = "DrogasComandos",icon = icon("chart-bar")),
                                 sliderTextInput(
                                   inputId = "DrogaComandos",
                                   label = "Comandos Regionais:",
                                   choices = c(1:15,"CE"),
                                   selected = "1"
                                 ),
                                 selectInput(inputId = "DrogaAno",label = "Ano",c("2018" = 2018,"2019" = 2019,"2020" = 2020)),
                                 sliderTextInput(
                                   inputId = "DrogaMes",
                                   label = "Intervalo de Meses:",
                                   choices = meses,
                                   selected = meses[c(1, 12)])
                        )),
               menuItem("Prisao por Mandato",tabName = "Prisao",icon = icon("balance-scale"),
                        menuItem("Mato Grosso",icon = icon("globe-americas"),
                                 menuSubItem("Painel Mato Grosso",tabName = "PrisaoEstado",icon = icon("chart-bar")),
                                 selectInput(inputId = "PrisaoAnoMT",label = "Ano",c("2018" = 2018,"2019" = 2019)),
                                 sliderTextInput(inputId = "PrisaoMesMT",
                                                 label = "Intervalo de Meses:",
                                                 choices = meses,
                                                 selected = meses[c(1, 12)]
                                 )
                        ),
                        menuItem("Comandos Regionais",icon = icon("chart-bar"),
                                 menuSubItem("Painel Comandos",tabName = "PrisaoComandos",icon = icon("chart-bar")),
                                 sliderTextInput(
                                   inputId = "PrisaoComandos",
                                   label = "Comandos Regionais:",
                                   choices = c(1:15,"CE"),
                                   selected = "1"
                                 ),
                                 selectInput(inputId = "PrisaoAno",label = "Ano",c("2018" = 2018,"2019" = 2019)),
                                 sliderTextInput(
                                   inputId = "PrisaoMes",
                                   label = "Intervalo de Meses:",
                                   choices = meses,
                                   selected = meses[c(1, 12)])
                        ))
               
               )
    )
  })
  
  ############ download ####
  
  
   observe({
     a = input$inputTest
     
     if(is.null(a)){
       a = 1:15
     }else{
       a = parse_number(a)
     }
     a = DadosIBGE_mt %>% 
       filter(cr %in% a) %>%
       mutate(new = paste(Município,CR))%>%
       pull()
       
     
     updateSelectInput(session,"inputTest2",
                      choices = a)
   })
  
  ############### mapa unidades MT #####################
  arq = "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2014/MT/mt_municipios.zip"
  
  
  tf <- tempfile()
  download.file(arq,tf,mode = "w",method = "auto")
  
  dir.create("mt_municipios")
  unzip(tf,exdir = "mt_municipios")
  eco.sp <- rgdal::readOGR(dsn = "mt_municipios")
  
  eco.sp$CD_GEOCMU  = DadosIBGE_mt$Código
  eco.sp@data = DadosIBGE_mt %>% left_join(eco.sp@data,by = c("Código" = "CD_GEOCMU"))
  
  map1 = tm_shape(eco.sp)+
    tm_fill("cr",alpha = 0.4,popup.vars = F,style = "kmeans")
  
  lf = tmap_leaflet(map1)
  
  ###### base Mineiração ############3
  
  url = "http://www.pm.mt.gov.br/unidades?p_p_id=101_INSTANCE_kP90SIH7NSur&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-2&p_p_col_pos=1&p_p_col_count=2&_101_INSTANCE_kP90SIH7NSur_delta=20&_101_INSTANCE_kP90SIH7NSur_keywords=&_101_INSTANCE_kP90SIH7NSur_advancedSearch=false&_101_INSTANCE_kP90SIH7NSur_andOperator=true&p_r_p_564233524_resetCur=false&_101_INSTANCE_kP90SIH7NSur_cur="
  pmmt1 = read_html(paste(url,1,sep = ""))
  #pmmt2 = read_html(paste(url,2,sep = ""))
  
  cleanpm = function(pmmt){
    pmmt %>% 
      html_nodes('.news-list')%>% 
      html_text() %>% 
      str_split(.,fixed("Ver localização no Mapa \n\t  \t\t")) %>% unlist()%>%
      str_replace(.,"\n            \n                        \n                          \n                       \n                        \n             \n                        \n                        \n               \t\t ","")%>% 
      reshape2::colsplit(.,"COMANDANTE:",names = c("Unidade","Comandante"))%>%
      filter(Unidade != "")%>%
      separate(Comandante,c("Comandante","ComandoRegional"),"CR:") %>% 
      separate(ComandoRegional,c("ComandoRegional","Local"),"LOCAL:") %>% 
      separate(Local,c("Local","Cidade"),"CIDADE:") %>%
      separate(Cidade,c("Cidade","Endereço"),"ENDEREÇO:") %>%
      separate(Endereço,c("Endereço","Telefone"),"TELEFONE:") %>%
      separate(Telefone,c("Telefone","Email"),"EMAIL:") %>%
      separate(Email,c("Email","Coordenadas"),"COORDENADAS:") %>%
      separate(Coordenadas,c("xy","Lixo"),"LOCALIZAÇÃO:",fill = "left") %>%
      mutate(Lixo = str_replace(Lixo,fixed("LOCALIZAÇÃO:"),""))%>%
      separate(xy,c("x","y"),"----")%>%
      select(-Lixo)%>%
      mutate(x = x %>% stringr::str_trim()%>% as.numeric(),
             y = y %>% stringr::str_trim()%>% as.numeric())
  }
  
  base2 = rbind(cleanpm(pmmt1))%>% 
    mutate(gid = 1:20,descriçao = paste(Unidade,paste("Comandante:",Comandante),
                                        paste("Cidade:",Cidade),
                                        paste("Endereço:",Endereço),
                                        paste("Telefone:",Telefone),
                                        paste("Email:",Email),sep = "\r\n"),
           crs = case_when(Cidade == "Várzea Grande" ~ 2,
                           TRUE ~ 1),
           ComandoRegional = ComandoRegional %>% str_trim())%>%
    select(ComandoRegional,y,x,gid,Unidade,descriçao,Cidade,crs) %>%
    filter(ComandoRegional == "Comando Geral")%>%
    setNames(c("Unidades","x","y","gid","nome","descriçao","cidade","crs"))
    
  ############# Unindo as bases #######
  
  
  BaseUnica = BaseUnica %>%
    rbind(base2)%>%
    mutate(descriçao2 = str_replace_all(descriçao,pattern = fixed("\r\n"),replacement = "<br/>"))
  
  
  ######## filtros ###########
  
  
  data = eventReactive(input$act,{
    
    a = input$inputTest
    b = input$inputTest2
    c = input$filUnidades
    if(is.null(a)){
      a = 1:15
    }else{
      a = parse_number(a)
    }
    
    if(is.null(b)){
      b = BaseUnica$cidade %>% unique()
    }else{
      b = str_sub(b,end = -6) %>% str_trim()
    }
    
    if(is.null(c)){
      c = BaseUnica$Unidades %>% unique()
    }else{
      c = c
    }
    
    GeoBase = BaseUnica %>%
      filter(cidade %in% b & Unidades %in% c & crs %in% a)
    
    markColor <- function(GeoBase) {
      sapply(GeoBase$Unidades, function(Unidades) {
        if(Unidades == "Companhias_Independentes") {
          "darkblue"
        } else if(Unidades == "Companhias") {
          "blue"
        } else if (Unidades == "Força_Tatica"){
          "gray"
        } else if(Unidades == "Nucleo"){
          "green"
        } else if(Unidades == "Pelotao"){
          "purple"
        } else if(Unidades == "Batalhoes"){
          "orange"
        } else if(Unidades == "Comandos_Regionais"){
          "red"
        } else if(Unidades == "Comando Geral"){
          "black"
        }})
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = markColor(GeoBase[,1]) %>% as.vector())
    
    
    if(input$shpcrs){
      lf %>%
        addTiles() %>% 
        addAwesomeMarkers(GeoBase$x, GeoBase$y, icon=icons, popup = GeoBase$descriçao2) %>% 
        addLegend(colors = c("black","red","gray","orange","purple","green","blue","darkblue"),
                  labels = c("Comando Geral","Comandos Regionais","Força Tática","Batalhões",
                             "Pelotão","Nucleo","Companhias","Companhias Independentes"))
    }else{
      
      leaflet() %>%
        addTiles() %>% 
        addAwesomeMarkers(GeoBase$x, GeoBase$y, icon=icons, popup = GeoBase$descriçao2) %>% 
        addLegend(colors = c("black","red","gray","orange","purple","green","blue","darkblue"),
                  labels = c("Comando Geral","Comandos Regionais","Força Tática","Batalhões",
                             "Pelotão","Nucleo","Companhias","Companhias Independentes"))
    }
  })
  
    output$unidadesmt <- renderLeaflet({
      if(input$act == 0){
        
        GeoBase = BaseUnica 
        
        markColor <- function(GeoBase) {
          sapply(GeoBase$Unidades, function(Unidades) {
            if(Unidades == "Companhias_Independentes") {
              "darkblue"
            } else if(Unidades == "Companhias") {
              "blue"
            } else if (Unidades == "Força_Tatica"){
              "gray"
            } else if(Unidades == "Nucleo"){
              "green"
            } else if(Unidades == "Pelotao"){
              "purple"
            } else if(Unidades == "Batalhoes"){
              "orange"
            } else if(Unidades == "Comandos_Regionais"){
              "red"
            } else if(Unidades == "Comando Geral"){
              "black"
            }})
        }
        
        icons <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = markColor(GeoBase[,1]) %>% as.vector())
        
        
        if(input$shpcrs){
          lf %>%
            addTiles() %>% 
            addAwesomeMarkers(GeoBase$x, GeoBase$y, icon=icons, popup = GeoBase$descriçao2) %>% 
            addLegend(colors = c("black","red","gray","orange","purple","green","blue","darkblue"),
                      labels = c("Comando Geral","Comandos Regionais","Força Tática","Batalhões",
                                 "Pelotão","Nucleo","Companhias","Companhias Independentes"))
        }else{
          
          leaflet() %>%
            addTiles() %>% 
            addAwesomeMarkers(GeoBase$x, GeoBase$y, icon=icons, popup = GeoBase$descriçao2) %>% 
            addLegend(colors = c("black","red","gray","orange","purple","green","blue","darkblue"),
                      labels = c("Comando Geral","Comandos Regionais","Força Tática","Batalhões",
                                 "Pelotão","Nucleo","Companhias","Companhias Independentes"))
        }
      }else{
        data()
      }
 })
  
  
  ############################# CALCULOS #######################
    library(readxl)
    homicidios18_19 <- read_excel("homicidios18-19.xlsx", 
                                  col_types = c("text", "text", "text", 
                                                "text", "date", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "date", "numeric", "text", 
                                                "numeric", "text", "text", 
                                                "text", "text", "numeric", "numeric"))
    homicidios17 <- read_excel("homicidios18-19.xlsx", sheet = "Planilha2",range = "a1:m16")
    max17 <- read_excel("homicidios18-19.xlsx",
                        sheet = "Planilha4", range = "a19:b34",col_names = c("cr","lim"))

  mes = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO",
          "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")  
  
cor_mapa =   function(ent2){
    cor = c()
    if(any(ent2$pop2017 <= 8)){cor = c(cor,"#69F585")}
    if(any(ent2$pop2017 <= 15 & ent2$pop2017 >8)){cor = c(cor,"#12961F")}
    if(any(ent2$pop2017 <= 30 & ent2$pop2017 >15)){cor = c(cor,"#F5F542")}
    if(any(ent2$pop2017 <= 50 & ent2$pop2017 >30)){cor = c(cor,"#F5AC40")}
    if(any(ent2$pop2017 <= 300 & ent2$pop2017 >50)){cor = c(cor,"#FF4D4D")}
    return(cor)
}
  cor = c("#69F585","#12961F","#F5F542","#F5AC40","#FF4D4D")
  
  output$distPlot <- renderLeaflet({
    
    anos = input$ano
    crs = input$classes
    
    ent2 = DadosIBGE_mt %>% 
      select(Município,Código,cr,MunicipioUp,paste("população_",anos,sep = ""),X,Y) %>% 
      left_join(homicidios18_19 %>% 
                  filter(`Ano Fato`== anos)%>%
                  mutate(MunicipioUp = `Municipio Fato`)%>%
                  count(MunicipioUp))%>%
      setNames(c("Municipio","Codigo","cr","Municipio2","População","X","Y","Homicidios"))%>%
      mutate(TaxaHom = 100000*Homicidios/População) %>%
      replace_na(list(Homicidios = 0,TaxaHom = 0))
    
    eco.sp@data = ent2
    
    tmap_mode("view")
    map = tm_shape(eco.sp[eco.sp$cr == crs,])+
      tm_fill("TaxaHom",breaks = c(0,8,15,30,50,Inf),
              #style = "cont",
              palette = cor,
              title = "Taxa por 100 mil Habitantes",
              alpha = 0.5,
              popup.vars = c("Taxa Homicidios" = "TaxaHom",
                             "Homicidios" = "Homicidios",
                             "População" = "População"),
              popup.format=list(TaxaHom=list(digits=2)))+
      tm_borders(col = "gray50")+
      #tm_layout(frame = FALSE)+
      tm_text(text = "Municipio",col = "black",size = 0.85)+
      tm_layout(frame = FALSE)

      return(tmap_leaflet(map))
  })
  
  tabela_taxa = function(anos,crs,int_mes = c("janeiro","dezembro")){  
    
    anos = as.numeric(anos)
    taq3 = int_mes %>% toupper()
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    if(anos == 2018){ 
      DadosIBGE_mt %>%
        mutate(cr = parse_number(CR))%>%
        filter(cr == crs) %>% 
        select(Município,MunicipioUp,homicidios_2018,população_2018,homicidios_2017,população_2017) %>%
        left_join(homicidios18_19 %>%
                    filter(ano == anos & `RISP Regiao` == paste("RISP",crs)) %>%
                    filter(`Mes Fato` %in% taq3) %>% 
                    mutate(MunicipioUp = `Municipio Fato`) %>% 
                    count(MunicipioUp)) %>% 
        replace_na(list(n= 0)) %>% 
        mutate(Homicidios = as.integer(n)) %>% 
        select(Município,Homicidios,população_2018,população_2017,homicidios_2018,homicidios_2017) %>% 
        rbind(list(paste(crs,"º Comando Regional",sep = ""),sum(.$Homicidios), 
                   sum(.$população_2018),sum(.$população_2017),
                   sum(.$homicidios_2018),sum(.$homicidios_2017))) %>% 
        mutate(TaxaAno = (homicidios_2018/população_2018)*100000 %>% round(3),
               tx2017 = (homicidios_2017/população_2017)*100000 %>% round(3),Variaçao = TaxaAno - tx2017,
               populaçao_2018 = população_2018 %>% as.integer(),Taxa = TaxaAno,
               População = população_2018 %>% as.integer()) %>% 
        select(Município,Homicidios,População,Taxa,Variaçao)
      
      
    }else{
      
      DadosIBGE_mt %>%
        mutate(cr = parse_number(CR))%>%
        filter(cr == crs) %>% 
        select(Município,MunicipioUp,cr,paste("população_",anos,sep = ""),paste("população_",anos - 1,sep = "")) %>%
        setNames(c("Município","MunicipioUp","cr","PopulaçãoAno","PopulaçãoAnoAnterior")) %>% 
        left_join(homicidios18_19 %>%
                    filter(ano == anos & `RISP Regiao` == paste("RISP",crs)) %>%
                    mutate(MunicipioUp = `Municipio Fato`) %>% 
                    count(MunicipioUp) %>%
                    mutate(HomicidiosAno = n) %>% 
                    select(MunicipioUp,HomicidiosAno) %>% 
                    left_join( homicidios18_19 %>%
                                 filter(ano == anos - 1 & `RISP Regiao` == paste("RISP",crs)) %>%
                                 mutate(MunicipioUp = `Municipio Fato`) %>% 
                                 count(MunicipioUp) %>% 
                                 mutate(HomicidiosAnoAnterior = n) %>% 
                                 select(MunicipioUp,HomicidiosAnoAnterior))) %>% 
        replace_na(list(HomicidiosAno = 0,HomicidiosAnoAnterior = 0)) %>% 
        select(Município,MunicipioUp,PopulaçãoAno,PopulaçãoAnoAnterior,HomicidiosAno,HomicidiosAnoAnterior) %>% 
        left_join(homicidios18_19 %>%
                    filter(ano == anos & `RISP Regiao` == paste("RISP",crs)) %>%
                    filter(`Mes Fato` %in% taq3) %>% 
                    mutate(MunicipioUp = `Municipio Fato`) %>% 
                    count(MunicipioUp)) %>% 
        replace_na(list(n= 0)) %>% 
        mutate(Homicidios = as.integer(n)) %>% 
        select(Município,Homicidios,PopulaçãoAno,PopulaçãoAnoAnterior,HomicidiosAno,HomicidiosAnoAnterior) %>% 
        rbind(list(paste(crs,"º Comando Regional",sep = ""),sum(.$Homicidios), 
                   sum(.$PopulaçãoAno),sum(.$PopulaçãoAnoAnterior),
                   sum(.$HomicidiosAno),sum(.$HomicidiosAnoAnterior))) %>% 
        mutate(TaxaAno = (HomicidiosAno/PopulaçãoAno)*100000 %>% round(3),
               txAnoAnterior= (HomicidiosAnoAnterior/PopulaçãoAnoAnterior)*100000 %>% round(3),Variaçao = TaxaAno - txAnoAnterior,
               PopulaçãoAno = PopulaçãoAno %>% as.integer(),Taxa = TaxaAno,
               População = PopulaçãoAno %>% as.integer()) %>% 
        select(Município,Homicidios,População,Taxa,Variaçao)  
    }
  }
  
  output$tablePlot <- renderTable({
    tabela_taxa(input$ano,input$classes,input$mes)
  })
  
  mapaMt = function(anos){
    
    ent2 = DadosIBGE_mt %>% 
      select(Município,Código,cr,MunicipioUp,paste("população_",anos,sep = ""),X,Y) %>% 
      left_join(homicidios18_19 %>% 
                  filter(`Ano Fato`== anos)%>%
                  mutate(MunicipioUp = `Municipio Fato`)%>%
                  count(MunicipioUp)
                )%>%
      setNames(c("Municipio","Codigo","cr","Municipio2","População","X","Y","Homicidios"))%>%
      mutate(TaxaHom = 100000*Homicidios/População) %>%
      replace_na(list(Homicidios = 0,TaxaHom = 0))
    
    eco.sp@data = ent2
    
    tmap_mode("view")
    map = tm_shape(eco.sp)+
      tm_fill("TaxaHom",breaks = c(0,8,15,30,50,Inf),
              #style = "cont",
              palette = cor,
              title = "Taxa por 100 mil Habitantes",
              alpha = 0.5,
              popup.vars = c("Taxa Homicidios" = "TaxaHom",
                             "Homicidios" = "Homicidios",
                             "População" = "População"),
              popup.format=list(TaxaHom=list(digits=2)))+
      tm_borders()+
      #tm_layout(frame = FALSE)+
      #tm_text(text = "Municipio",col = "black",size = 0.85)
      tm_layout(frame = FALSE)
      
    
    tmap_leaflet(map)
    
  }
  
  

  ####################### NOVOS GRAFICOS ################
 
  
  percent = function(value){
    value2 = c()
    for(i in 1:length(value)){
      value2 = c(value2 , paste(sprintf("%.1f",value[i]*100),"%",sep = ""))
    }
    return(value2)
  }
  
  sexo_mes = function(cr,ano,int_mes){
    
    taq = 1
    taq2 = ano
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    dados = homicidios18_19 %>% 
      filter(`Ano Fato` == taq2 & `RISP Regiao` == paste('RISP',taq)) %>%
      filter(`Mes Fato` %in% taq3) %>% 
      count(Sexo) %>% 
      arrange(desc(n))
    if(length(dados$n) == 0){
      stop("O numero de Homicidios nesse intervalo é 0")
    }else{
      return(
        dados %>% 
          ggplot(aes(x = "", y = n, fill = Sexo))+
          scale_fill_manual(name = "Sexo",values = c("#F08080","#6495ED","#FFE4C4"))+
          geom_bar(width = 1,stat = 'identity',color = "gray80")+
          coord_polar("y",start = 0, direction = -1)+
          geom_text(aes(y = n, x = 1,label = percent(n/sum(n))),
                    position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
          theme_void()
      )
    }
  }
  

  output$GenPlot <- renderPlot({
    sexo_mes(input$classes,input$ano,input$mes)
  })
  
  meio_emp = function(cr,ano,ord,int_mes){
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    p =  as.data.frame(table(homicidios18_19$`RISP Regiao`,
                             homicidios18_19$`Meio Empregado`,
                             homicidios18_19$ano,
                             homicidios18_19$`Mes Fato`))
    p$Var1 = as.character(p$Var1)
    p$Var2 = as.character(p$Var2)
    p$Var3 = as.character(p$Var3)
    p$Var4 = as.character(p$Var4)
    p = p %>% 
      filter(Var1 == paste('RISP',cr) & Var3 == ano & Var4 %in% taq3) %>% 
      arrange(desc(Freq))
    if(ord == 1){
      meio = "ARMA DE FOGO"
    }else{
      meio = "ARMA CORTANTE OU PERFURANTE"
    }
    meio = sprintf("%.2f",as.numeric(p %>%
                                       filter(Var2 == meio) %>% 
                                       select(Freq) %>%
                                       sum()/sum(p$Freq))*100)
    return(paste(meio,"%",sep = ""))
  }
  ##################### caixa de valores ###########
  
  taxaBrasil = c(31.95,27.5)
  taxaMT = c(((homicidios18_19 %>% 
                 filter(ano == 2018) %>% 
                 nrow())/DadosIBGE_mt$população_2018 %>% sum())*100000,
             ((homicidios18_19 %>% 
                 filter(ano == 2019) %>% 
                 nrow())/DadosIBGE_mt$população_2019 %>% sum())*100000)
  
  output$TaxaMT <- renderValueBox({
    valueBox(value = taxaMT[as.numeric(input$ano) - 2017] %>% round(2),subtitle = "Taxa Mato Grosso",
             icon = icon("flag"), color = "olive",width = NULL)
  })
  
  output$taxaBR <- renderValueBox({
    valueBox(value = taxaBrasil[as.numeric(input$ano) - 2017],subtitle = "Taxa Brasil",icon = icon("globe"),
             color = "yellow",width = NULL)
    
  })
  
  output$ArmaFogo <- renderValueBox({
    valueBox(meio_emp(input$classes,input$ano,1,input$mes),
             "Arma de Fogo",icon = icon("percentage"),color = "blue",width = NULL)
  })
  
  output$ArmaCortante <- renderValueBox({
    valueBox(meio_emp(input$classes,input$ano,2,input$mes),
             "Arma Cortante ou Perfurante",
             icon = icon("percentage"),color = "blue",width = NULL)
  })
  
  output$indice <- renderValueBox({
    
    valor = as.data.frame(table(homicidios18_19$`RISP Regiao`,homicidios18_19$ano))
    ano = input$ano
    cr = input$classes
    int_mes = input$mes
    
    valor = as.data.frame(table(homicidios18_19$`RISP Regiao`,
                                homicidios18_19$ano,
                                homicidios18_19$`Mes Fato`))
    
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    if(ano == 2018){
      valor = valor %>% 
        filter(Var1 == paste('RISP',cr) & Var3 %in% taq3)
      
      val1 = valor %>% filter(Var2 == 2018) %>% select(Freq) %>% sum()
      val2 =  homicidios17 %>% 
        gather(key = "Mes Fato",value = "freq",c(-1)) %>%
        filter(`Mes Fato` %in% taq3 & `RISP Regiao` == paste("RISP",cr)) %>%
        select(freq) %>% sum()
      
      if(val2 == 0 & val1 == 0){
        val = 0
      }else{
        if(val2 == 0){
          val = 1
        }else{
          val = (val1/val2)-1
        }
      }
      
      if(val<= 0){
        icone= "angle-double-down"
        cor = "green"
      }else{
        icone = "angle-double-up"
        cor = "red"
      }
      
      
    }else{
      valor = valor %>% 
        filter(Var1 == paste('RISP',cr) & Var3 %in% taq3)
      
      val1 = valor %>% filter(Var2 == ano %>% as.numeric()) %>% select(Freq) %>% sum() 
      val2 = valor %>% filter(Var2 == ano %>% as.numeric() - 1) %>% select(Freq) %>% sum()
      
      if(val2 == 0 & val1 == 0){
        val = 0
      }else{
        if(val2 == 0){
          val = 1
        }else{
          val = (val1/val2)-1
        }
      }
      
      if(val<= 0){
        icone= "angle-double-down"
        cor = "green"
      }else{
        icone = "angle-double-up"
        cor = "red"
      }
    }
    
    
    valueBox(value = paste(round(val*100,2),"%",sep = " "),
             "Variação em relação ao Ultimo Ano",
             icon = icon(icone),color = cor,width = NULL)
  })
  
  faixa_etaria2 = function(cr,anos,meses){
    mes = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO",
            "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")
    
    if(anos < 2020){
    
    homicidios18_19$`Faixa Etaria`[is.na(homicidios18_19$`Faixa Etaria`)] = "NÃO INFORMADO"
    homicidios18_19$`Faixa Etaria` = recode(homicidios18_19$`Faixa Etaria`,
                                            'NAO INFORMADO'  = "NÃO INFORMADO",
                                            'MASCULINO' = "NÃO INFORMADO",'MENOR DE 11 ANOS' = 'DE 0 A 11 ANOS')
    
    taq3 = toupper(meses)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]]} 
    
    age = as.data.frame(table(homicidios18_19$`Faixa Etaria`,
                              homicidios18_19$`RISP Regiao`,
                              homicidios18_19$ano,
                              homicidios18_19$`Mes Fato`))
    age$Var1 = as.character(age$Var1)
    age$Var2 = as.character(age$Var2)
    age$Var3 = as.character(age$Var3)
    age$Var4 = as.character(age$Var4)
    names(age) = c("Faixa_Etaria","CR","Ano","Mes","Freq")
    age2 = filter(age,CR == paste('RISP',cr)& Ano == anos & Mes %in% taq3)
    age2 = aggregate(Freq~Faixa_Etaria,age2,sum)
    age2 = age2[1:7,]
    
    ggplot(data = age2,aes(x = "",y = Freq,fill = Faixa_Etaria))+
      geom_bar(width = 1,stat = 'identity',color = "gray80")+
      coord_polar("y")+
      geom_text(data = age2 %>% 
                  filter(Freq > 0) %>% 
                  arrange(desc(Freq)) %>% 
                  head(6),
                aes(y = Freq, x = 1
                    ,label = percent(Freq/sum(Freq))),
                position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
      theme_void()
    }else{
      taq3 = toupper(meses)
      taq3 = which(mes == taq3[1]| mes == taq3[2])
      if(length(taq3)==1){
        taq3 = mes[taq3[1]]
      }else{
        taq3 = mes[taq3[1]:taq3[2]]} 
      
      homicidios18_19 %>% mutate(Idade = as.numeric(Idade)) %>% 
        mutate(FaixaIdade = case_when(Idade < 10 ~ "-10 anos",
                                      Idade < 15 & Idade >= 10 ~ "10-14 anos",
                                      Idade < 20 & Idade >= 15 ~ "15-19 anos",
                                      Idade < 25 & Idade >= 20~ "20-24 anos",
                                      Idade < 30 & Idade >= 25~ "25-29 anos",
                                      Idade < 35 & Idade >= 30~ "30-34 anos",
                                      Idade < 40 & Idade >= 35~ "35-39 anos",
                                      Idade < 45 & Idade >= 40~ "40-44 anos",
                                      Idade < 50 & Idade >= 45~ "45-49 anos",
                                      Idade < 55 & Idade >= 50~ "50-54 anos",
                                      Idade < 60 & Idade >= 55~ "55-59 anos",
                                      Idade < 65 & Idade >= 60~ "60-64 anos",
                                      Idade < 70 & Idade >= 65~ "65-69 anos",
                                      Idade < 75 & Idade >= 70~ "70-74 anos",
                                      Idade < 80 & Idade >= 75~ "75-79 anos",
                                      Idade < 85 & Idade >= 80~ "80-84 anos",
                                      Idade >= 85 & Idade < 120 ~ "85+ anos",
                                      TRUE ~ "")) %>%
        filter(FaixaIdade != "" & `Mes Fato` %in% taq3) %>%
        count(FaixaIdade,`RISP Regiao`,ano)%>% spread(key = `FaixaIdade`,value = n)%>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        gather("FaixaIdade","n",3:18)%>%
        filter(`RISP Regiao` == paste("RISP",cr) & ano == anos)%>%
        ggplot()+
        geom_bar(aes(x = FaixaIdade, y = n,fill = factor(Sexo)),stat = "identity",
                 color = "grey50",fill = "navy",size = 0)+
        theme(axis.text.x = element_text(size=11, angle=90))
    }
  } 
  
  output$IdadePlot <- renderPlot({
    faixa_etaria2(input$classes,input$ano,input$mes)
  })
  
  

  ######################## FAIXA HORARIO #################

  
  homicidios18_19$`Hora Minuto Fato` = recode(homicidios18_19$`Hora Minuto Fato`,
                                              "2" = "02","7" = "07","0"="00","3"="03")
  
  hora_homicid = function(cr,ano,int_mes){
    taq = cr
    taq2 = ano
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    
    HoraSNI = homicidios18_19 %>% 
      filter(`Hora Minuto Fato` != "NI" & ano == taq2 &
               `RISP Regiao` == paste("RISP",taq)) %>% 
      filter(`Mes Fato` %in% taq3) %>% 
      select(`RISP Regiao`,ano,`Hora Minuto Fato`)
    
    ggplot(HoraSNI)+
      geom_bar(data = HoraSNI %>% count(`Hora Minuto Fato`),
               aes(x = as.numeric(`Hora Minuto Fato`),y =n/sum(n)),
               stat = "identity",fill = "navy")+
      geom_density(aes(as.numeric(`Hora Minuto Fato`)),alpha = 0.6,fill = "gray70")+
      xlab(label = "")+
      ylab(label = "")+
      xlim(-0.6,23.)+
      theme_minimal()
    
  }
  
  output$horasPlot <- renderPlot({
    hora_homicid(input$classes,input$ano,input$mes)
  })
  

  ########################### Por Semana ###############

  por_semana = function(cr,ano,int_mes = c("janeiro","dezembro")){
    taq = as.numeric(cr)
    taq2 = as.numeric(ano)
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
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
    
    dados = homicidios18_19 %>% 
      filter(`Ano Fato` == taq2) %>% 
      filter(`Mes Fato` %in% taq3) %>% 
      select(`SEMANA DO FATO`) %>%
      unique() %>%
      left_join(homicidios18_19 %>% 
                  filter(`Ano Fato` == taq2 & `RISP Regiao` == paste('RISP',taq)) %>%
                  filter(`Mes Fato` %in% taq3) %>% 
                  count(`SEMANA DO FATO`)) %>% 
      as.tibble() %>%  
      replace_na(list(n= 0))
    
    if(max(dados$n)<=6){
      maximo = 6
    }else{
      maximo = max(dados$n)
    }
    
    if(taq2 == 2018){
      x1 = max17$lim[taq]
    }else{
      dados2 = homicidios18_19 %>% 
        filter(ano == taq2 - 1 & `RISP Regiao` == paste('RISP',taq)) %>% 
        count(`SEMANA DO FATO`)
      
      x1 = 1.96*sd(dados2$n)+ mean(dados2$n)
    }
    
    dados %>% 
      ggplot(aes(x = `SEMANA DO FATO`,y = n))+
      geom_line(color = "grey50")+
      geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
      geom_text(aes(x = `SEMANA DO FATO`,y = n + 0.3,label = n))+
      ylab(label = "Numero de Homicidio")+
      xlab(label = "Semana")+
      #geom_hline(yintercept = x1,color = "red",linetype = "dashed")+
      #geom_label(aes(x = max( `SEMANA DO FATO`)-1,y = max17$lim[taq] + 0.3,
                     #label = paste("Fora do comum",taq2 - 1,":",x1 %>% round(2))),
                 #color = "red",size = 3.2)+
      geom_smooth(method = "loess",se = FALSE,span = spa,linetype = "dashed",color = "red",size = 0.5)
    
    
  }
  
  output$semana <- renderPlot({
    por_semana(input$classes,input$ano,input$mes)
  })
  
  ########################## MATO GROSSO ###################
  
  homicidios18_19$`CONFRONTO COM A PM`[!is.na(homicidios18_19$`CONFRONTO COM A PM`)] = 1
  homicidios18_19$`CONFRONTO COM A PM`[is.na(homicidios18_19$`CONFRONTO COM A PM`)] = 0
  
  output$tableMTconf <- renderTable({
    anos = input$anomt
    
    taq3 = input$mesMT
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    homicidios18_19 %>% 
      filter(ano== anos) %>%
      filter(`Mes Fato` %in% taq3) %>%
      select(RISP,`RISP Regiao`) %>%
      count(RISP,`RISP Regiao`) %>%
      mutate(cr = as.numeric(str_sub(`RISP Regiao`,start = 5) %>% 
                               str_trim())) %>%
      arrange(cr) %>% 
      mutate(ComandosRegionais = paste(cr,"° CR - ",RISP,sep = ""),
             Homicidios = n) %>% 
      left_join(homicidios18_19 %>% 
                  filter(`CONFRONTO COM A PM` == 1 & ano == anos) %>% 
                  filter(`Mes Fato` %in% taq3) %>%
                  count(`RISP Regiao`) %>% 
                  mutate(Confronto = n) %>% 
                  select(`RISP Regiao`,Confronto)) %>% 
      select(ComandosRegionais,Homicidios,Confronto) %>% 
      mutate(Confronto = as.integer(replace_na(Confronto,0))) %>%
      mutate(Porcentagem = paste(sprintf("%.2f",(Confronto/Homicidios)*100),"%"))
  })

  ############################ HorarioMT #########
  
  horaMT = function(ano,int_mes = c("janeiro","dezembro")){
    taq2 = ano
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    
    HoraSNI = homicidios18_19 %>% 
      filter(`Hora Minuto Fato` != "NI" & ano == taq2) %>% 
      filter(`Mes Fato` %in% taq3) %>% 
      select(`RISP Regiao`,ano,`Hora Minuto Fato`)
    
    ggplot(HoraSNI)+
      geom_bar(data = HoraSNI %>% count(`Hora Minuto Fato`),
               aes(x = as.numeric(`Hora Minuto Fato`),y =n/sum(n)),
               stat = "identity",fill = "navy")+
      geom_density(aes(as.numeric(`Hora Minuto Fato`)),alpha = 0.6,fill = "gray70")+
      xlab(label = "")+
      ylab(label = "")+
      xlim(-0.6,23.6)+
      theme_minimal()
    
  }
  ########################## ConfrontoMT ########
  
  confrontoMT = function(ano,int_mes = c("janeiro","dezembro")){
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    taq2 = ano
    
    confr = homicidios18_19 %>%
      filter(`Ano Fato` == taq2  & `Mes Fato` %in% taq3) %>% 
      count(`CONFRONTO COM A PM`)
    
    if(confr$n %>% length() == 1){
      return("0.00%")
    }else{
      return(paste(round((confr$n[2]/sum(confr$n))*100,2),"%",sep = ""))
    }
  }  
  ########################### sexo/anoMT ########
  sexo_mesMT = function(ano,int_mes = c("janeiro","dezembro")){
    
    
    taq2 = ano
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    dados = homicidios18_19 %>% 
      filter(`Ano Fato` == taq2 ) %>%
      filter(`Mes Fato` %in% taq3) %>% 
      count(Sexo) %>% 
      arrange(desc(n))
    if(length(dados$n) == 0){
      stop("O numero de Homicidios nesse intervalo é 0")
    }else{
      return(dados %>% 
               ggplot(aes(x = "", y = n, fill = Sexo))+
               scale_fill_manual(name = "Sexo",values = c("#F08080","#6495ED","#FFE4C4"))+
               geom_bar(width = 1,stat = 'identity',color = "gray80")+
               coord_polar("y")+
               geom_text(aes(y = n, x = 1,label = percent(n/sum(n))),
                         position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
               theme_void())
    }
  }
  #################### FaixaEtaria/anoMT #######
  faixa_etariaMT = function(anos,meses = c("janeiro","dezembro")){
    
    mes = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO",
            "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")
    taq3 = toupper(meses)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]]} 
    
    if(anos <2020){
    homicidios18_19$`Faixa Etaria`[is.na(homicidios18_19$`Faixa Etaria`)] = "NÃO INFORMADO"
    homicidios18_19$`Faixa Etaria` = recode(homicidios18_19$`Faixa Etaria`,
                                            'NAO INFORMADO'  = "NÃO INFORMADO",
                                            'MASCULINO' = "NÃO INFORMADO",'MENOR DE 11 ANOS' = 'DE 0 A 11 ANOS')
    
    
    age = as.data.frame(table(homicidios18_19$`Faixa Etaria`,
                              homicidios18_19$`RISP Regiao`,
                              homicidios18_19$ano,
                              homicidios18_19$`Mes Fato`))
    age$Var1 = as.character(age$Var1)
    age$Var2 = as.character(age$Var2)
    age$Var3 = as.character(age$Var3)
    age$Var4 = as.character(age$Var4)
    names(age) = c("Faixa_Etaria","CR","Ano","Mes","Freq")
    age2 = filter(age, Ano == anos & Mes %in% taq3)
    age2 = aggregate(Freq~Faixa_Etaria,age2,sum)
    age2 = age2[1:7,]
    
    ggplot(data = age2,aes(x = "",y = Freq,fill = Faixa_Etaria))+
      geom_bar(width = 1,stat = 'identity',color = "gray80")+
      coord_polar("y")+
      geom_text(data = age2 %>% 
                  filter(Freq > 0) %>% 
                  arrange(desc(Freq)) %>% 
                  head(6),
                aes(y = Freq, x = 1,label = percent(Freq/sum(Freq))),
                position = position_stack(vjust = 0.5), size=3.5,color = "grey20")+
      theme_void()
    }else{
      
      homicidios18_19 %>% mutate(Idade = as.numeric(Idade)) %>% 
        mutate(FaixaIdade = case_when(Idade < 10 ~ "-10 anos",
                                      Idade < 15 & Idade >= 10 ~ "10-14 anos",
                                      Idade < 20 & Idade >= 15 ~ "15-19 anos",
                                      Idade < 25 & Idade >= 20~ "20-24 anos",
                                      Idade < 30 & Idade >= 25~ "25-29 anos",
                                      Idade < 35 & Idade >= 30~ "30-34 anos",
                                      Idade < 40 & Idade >= 35~ "35-39 anos",
                                      Idade < 45 & Idade >= 40~ "40-44 anos",
                                      Idade < 50 & Idade >= 45~ "45-49 anos",
                                      Idade < 55 & Idade >= 50~ "50-54 anos",
                                      Idade < 60 & Idade >= 55~ "55-59 anos",
                                      Idade < 65 & Idade >= 60~ "60-64 anos",
                                      Idade < 70 & Idade >= 65~ "65-69 anos",
                                      Idade < 75 & Idade >= 70~ "70-74 anos",
                                      Idade < 80 & Idade >= 75~ "75-79 anos",
                                      Idade < 85 & Idade >= 80~ "80-84 anos",
                                      Idade >= 85 & Idade < 120 ~ "85+ anos",
                                      TRUE ~ "")) %>%
        filter(FaixaIdade != "" & `Mes Fato` %in% taq3) %>%
        count(FaixaIdade,ano)%>% spread(key = `FaixaIdade`,value = n)%>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        gather("FaixaIdade","n",2:17)%>%
        filter(ano == anos)%>%
        ggplot()+
        geom_bar(aes(x = FaixaIdade, y = n,fill = factor(Sexo)),stat = "identity",
                 color = "grey50",fill = "navy",size = 0)+
        theme(axis.text.x = element_text(size=11, angle=90))
    }
  }
  ################# Homicidios/Semana MT #######
  por_semanaMT = function(ano,int_mes = c("janeiro","dezembro")){
    taq2 = as.numeric(ano)
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
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
    
    dados = homicidios18_19 %>% 
      filter(`Ano Fato` == taq2) %>% 
      filter(`Mes Fato` %in% taq3) %>% 
      select(`SEMANA DO FATO`) %>%
      unique() %>%
      left_join(homicidios18_19 %>% 
                  filter(`Ano Fato` == taq2) %>%
                  filter(`Mes Fato` %in% taq3) %>% 
                  count(`SEMANA DO FATO`)) %>% 
      as.tibble() %>%  
      replace_na(list(n= 0))
    
    if(max(dados$n)<=6){
      maximo = 6
    }else{
      maximo = max(dados$n)
    }
    
    if(taq2 == 2018){
      x1 = max17$lim[16]
    }else{
      dados2 = homicidios18_19 %>% 
        filter(ano == taq2 - 1) %>% 
        count(`SEMANA DO FATO`)
      
      x1 = 1.96*sd(dados2$n)+ mean(dados2$n)
    }
    
    dados %>% 
      ggplot(aes(x = `SEMANA DO FATO`,y = n))+
      geom_line(color = "grey50")+
      geom_point(shape = 21 ,size = 3,color = "black",fill="#69b3a2")+
      geom_text(aes(x = `SEMANA DO FATO`,y = n + 1,label = n))+
      ylab(label = "Numero de Homicidio")+
      xlab(label = "Semana")+
      #geom_hline(yintercept = x1,color = "red",linetype = "dashed")+
      #geom_label(aes(x = max( `SEMANA DO FATO`)-1,y = x1 + 1,
                     #label = paste("Fora do comum",taq2 - 1,":",x1 %>% round(2))),
                 #color = "red",size = 3.2)+
      geom_smooth(method = "loess",se = FALSE,span = spa,linetype = "dashed",color = "red",size = 0.5)
    
    
  }
  ###################### MeioEmpregadoMT #####
  
  meio_empMT = function(ano,ord,int_mes = c("janeiro","dezembro")){
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    p =  as.data.frame(table(homicidios18_19$`RISP Regiao`,
                             homicidios18_19$`Meio Empregado`,
                             homicidios18_19$ano,
                             homicidios18_19$`Mes Fato`))
    p$Var1 = as.character(p$Var1)
    p$Var2 = as.character(p$Var2)
    p$Var3 = as.character(p$Var3)
    p$Var4 = as.character(p$Var4)
    p = p %>% 
      filter(Var3 == ano & Var4 %in% taq3) %>% 
      arrange(desc(Freq))
    if(ord == 1){
      meio = "ARMA DE FOGO"
    }else{
      meio = c("ARMA CORTANTE OU PERFURANTE","ARMA CONTUNDENTE")
    }
    meio = sprintf("%.2f",as.numeric(p %>%
                                       filter(Var2 %in% meio) %>% 
                                       select(Freq) %>%
                                       sum()/sum(p$Freq))*100)
    return(paste(meio,"%",sep = ""))
  }
  
  ########################### Dia Semana #############
  
  dia_semana = function(cr,ano,int_mes = c("janeiro","dezembro")){
    taq = cr
    taq2 = ano
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    homicidios18_19 %>% 
      filter(ano == taq2 & `RISP Regiao` == paste("RISP",taq)) %>% 
      filter(`Mes Fato` %in% taq3) %>%
      select(`RISP Regiao`,ano,`Dia Semana Fato`) %>% 
      count(`Dia Semana Fato`) %>% 
      mutate(DiaSemana = factor(`Dia Semana Fato`,
                                levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                           "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
      arrange(DiaSemana) %>% 
      drop_na(DiaSemana) %>% 
      ggplot()+
      geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
      ylab(label = "Frequência")
  }
  
  dia_semanaMT = function(ano,int_mes = c("janeiro","dezembro")){
    
    taq2 = ano
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    homicidios18_19 %>% 
      filter(ano == taq2) %>% 
      filter(`Mes Fato` %in% taq3) %>%
      select(`RISP Regiao`,ano,`Dia Semana Fato`) %>% 
      count(`Dia Semana Fato`) %>% 
      mutate(DiaSemana = factor(`Dia Semana Fato`,
                                levels = c("SEGUNDA-FEIRA","TERÇA-FEIRA","QUARTA-FEIRA",
                                           "QUINTA-FEIRA","SEXTA-FEIRA","SÁBADO","DOMINGO"))) %>% 
      arrange(DiaSemana) %>% 
      drop_na(DiaSemana) %>% 
      ggplot()+
      geom_bar(aes(x = DiaSemana,y = n),stat = "identity",fill = "navy",width = 0.6)+
      ylab(label = "Frequência")
  }
  ##################### tabelas taxas MT ############
  
  HomTaxasMT = function(anos,int_mes = c("JANEIRO","DEZEMBRO")){
    
    taq3 = int_mes
    
    taq3 = toupper(taq3)
    taq3 = which(mes == taq3[1]| mes == taq3[2])
    if(length(taq3)==1){
      taq3 = mes[taq3[1]]
    }else{
      taq3 = mes[taq3[1]:taq3[2]] 
    }
    
    DadosIBGE_mt %>% 
      select(cr,paste("população_",anos,sep = ""),
             paste("população_",anos - 1,sep = ""),
             paste("homicidios_",anos,sep = ""),
             paste("homicidios_",anos - 1,sep = "")) %>% 
      setNames(c("cr","População","PopulaçãoAnoAnterior",
                 "HomicidiosAno","HomicidiosAnoAnterior")) %>%    
      aggregate(.~cr,.,sum) %>%
      as_tibble() %>% 
      type.convert() %>%
      left_join(homicidios18_19 %>% 
                  filter(ano == anos & `Mes Fato` %in%  taq3) %>% 
                  mutate(cr = parse_number(`RISP Regiao`)) %>% 
                  count(cr,RISP)) %>%
      replace_na(list(n = 0)) %>% 
      arrange(cr) %>% 
      mutate(ComandoRegional = paste(cr,"° CR - ",RISP,sep = "")) %>% 
      rbind(list("MATO GROSSO",sum(.$População),sum(.$PopulaçãoAnoAnterior),
                 sum(.$HomicidiosAno),sum(.$HomicidiosAnoAnterior),"MATO GROSSO",sum(.$n),"MATO GROSSO")) %>% 
      mutate(Taxa= 100000*HomicidiosAno/População,
             TaxaAnoAnterior= 100000*HomicidiosAnoAnterior/PopulaçãoAnoAnterior,
             Var = Taxa - TaxaAnoAnterior, Homicidios = as.integer(n)) %>% 
      select(ComandoRegional,Homicidios,População,Taxa,Var)
     
  }
  
  ############################# OutPutMT ##########
 
  output$HomTaxasMT <- renderTable({
    HomTaxasMT(input$anomt %>% as.numeric(),input$mesMT)
  })
  output$indiceMT <- renderValueBox({
   
   ano = input$anomt %>% as.numeric()
   int_mes = input$mesMT
   
   valor = as.data.frame(table(homicidios18_19$`RISP Regiao`,
                               homicidios18_19$ano,
                               homicidios18_19$`Mes Fato`))
   
   taq3 = int_mes
   
   taq3 = toupper(taq3)
   taq3 = which(mes == taq3[1]| mes == taq3[2])
   if(length(taq3)==1){
     taq3 = mes[taq3[1]]
   }else{
     taq3 = mes[taq3[1]:taq3[2]] 
   }
   
   if(ano != 2018){
     valor = valor %>% 
       filter(Var3 %in% taq3)
     
     val1 = valor %>% filter(Var2 == ano) %>% select(Freq) %>% sum() 
     val2 = valor %>% filter(Var2 == ano - 1) %>% select(Freq) %>% sum()
     
     if(val2 == 0 & val1 == 0){
       val = 0
     }else{
       if(val2 == 0){
         val = 1
       }else{
         val = (val1/val2)-1
       }
     }
     
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
   }else{
     valor = valor %>% 
       filter(Var3 %in% taq3)
     
     val1 = valor %>% filter(Var2 == 2018) %>% select(Freq) %>% sum()
     val2 =  homicidios17 %>% 
       gather(key = "Mes Fato",value = "freq",c(-1)) %>%
       filter(`Mes Fato` %in% taq3) %>%
       select(freq) %>% sum()
     
     if(val2 == 0 & val1 == 0){
       val = 0
     }else{
       if(val2 == 0){
         val = 1
       }else{
         val = (val1/val2)-1
       }
     }
     
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
   }
   
   valueBox(value = paste(round(val*100,2),"%",sep = " "),
            "Variação em relação ao Ultimo Ano",
            icon = icon(icone),color = cor,width = NULL)
   
 })
 output$ArmaFogoMT <- renderValueBox({
   valueBox(meio_empMT(input$anomt,1,input$mesMT),
            "Arma de Fogo",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ArmaCortanteMT <- renderValueBox({
   valueBox(meio_empMT(input$anomt,2,input$mesMT),
            "Arma Cortante ou Perfurante",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ConfrontoMT <- renderValueBox({
   valueBox(confrontoMT(input$anomt,input$mesMT),
            "Confrontos",icon = icon("percentage"),color = "purple",width = NULL)
 })
 output$GenPlotMT <- renderPlot({
   sexo_mesMT(input$anomt,input$mesMT)
 })
 output$IdadePlotMT <- renderPlot({
   faixa_etariaMT(input$anomt,input$mesMT)
 })
 output$horasPlotMT <- renderPlot({
   horaMT(input$anomt,input$mesMT)
 })
 output$semanaMT <- renderPlot({
   por_semanaMT(input$anomt,input$mesMT)
 })
 output$diaPlot <- renderPlot({
   dia_semana(input$classes,input$ano,input$mes)
 })
 output$diaPlotMT <- renderPlot({
   dia_semanaMT(input$anomt,input$mesMT)
 })
 
 output$mapamt = renderLeaflet({
   mapaMt(input$anomt)
 })
 
  ##################### ###########roubo ################
 source("Roubo.R",local = TRUE)
 
  ###################### Roubo output MT ##########
 
 output$rbIndiceMT <- renderValueBox({
   val = rbVarMT(input$rbAnoMT %>% as.integer(),input$rbMesMT) %>% parse_number()
   
   if(val<= 0){
     icone= "angle-double-down"
     cor = "green"
   }else{
     icone = "angle-double-up"
     cor = "red"
   }
   
   valueBox(value = paste(round(val,2),"%",sep = " "),
            "Variação em relação ao Ultimo Ano",
            icon = icon(icone),color = cor,width = NULL)
   
 })
 output$rbPessoaMT <- renderValueBox({
   valueBox(rbIndiceMT(input$rbAnoMT,"PESSOA",input$rbMesMT),
           "Roubo a Pessoa",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$rbResidenciaMT <- renderValueBox({
   valueBox(rbIndiceMT(input$rbAnoMT,"RESIDENCIA",input$rbMesMT),
            "Roubo a Residencia",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$rbComercioMT <- renderValueBox({
   valueBox(rbIndiceMT(input$rbAnoMT,"ESTABELECIMENTO COMERCIAL",input$rbMesMT),
            "Roubo a Comercio",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$rbMapaMT <- renderLeaflet({
   rbMapaMT(input$rbAnoMT)
 })
 output$rbHorasMT <- renderPlot({
   rbHoraMT(input$rbAnoMT,input$rbMesMT)
 })
 output$rbDiaMT <- renderPlot({
   rbDiaMT(input$rbAnoMT,input$rbMesMT)
 })
 output$rbTabelaMT <- renderTable({
   rbTabelaMT(input$rbAnoMT %>% as.integer(),input$rbMesMT)
 })
 output$rbLocalMT <- renderPlot({
   rbLocalMT(input$rbAnoMT,input$rbMesMT)
 })
 output$rbMunicipioMT <- renderTable({
   rbMunicipioMT(input$rbAnoMT,input$rbMesMT,tipo = "TOTAL")
 })
 output$rbMunicipioPesMT <- renderTable({
   rbMunicipioMT(input$rbAnoMT,input$rbMesMT,tipo = "PESSOA")
 })
 output$rbMunicipioResMT <- renderTable({
   rbMunicipioMT(input$rbAnoMT,input$rbMesMT,tipo = "RESIDENCIA")
 })
 output$rbMunicipioComMT <- renderTable({
   rbMunicipioMT(input$rbAnoMT,input$rbMesMT,tipo = "ESTABELECIMENTO COMERCIAL")
 })
 output$rbSemanaMT <- renderPlot({
   rbSemanaMT(input$rbAnoMT,input$rbMesMT)
 })
 
 
  ####################### Roubo Comandos ###########
 
 output$rbIndice <- renderValueBox({
 
   val = rbVar(input$rbComandos,input$rbAno %>% as.integer(),input$rbMes) %>% parse_number()
   
   if(val<= 0){
     icone= "angle-double-down"
     cor = "green"
   }else{
     icone = "angle-double-up"
     cor = "red"
   }
   
   valueBox(paste(val,"%",sep=""),
            "Variação em relação ao Ultimo Ano",
            icon = icon(icone),color = cor,width = NULL)
   
 })
 output$rbPessoa <- renderValueBox({
   valueBox(rbIndice(input$rbComandos,input$rbAno,"PESSOA",input$rbMes),
            "Roubo a Pessoa",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$rbResidencia <- renderValueBox({
   valueBox(rbIndice(input$rbComandos,input$rbAno,"RESIDENCIA",input$rbMes),
            "Roubo a Residencia",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$rbComercio <- renderValueBox({
   valueBox(rbIndice(input$rbComandos,input$rbAno,"ESTABELECIMENTO COMERCIAL",input$rbMes),
            "Roubo a Comercio",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$rbMapa <- renderLeaflet({
   rbMapa(input$rbComandos,input$rbAno)
 })
 output$rbHoras <- renderPlot({
   rbHora(input$rbComandos,input$rbAno,input$rbMes)
 })
 output$rbDia <- renderPlot({
   rbDia(input$rbComandos,input$rbAno,input$rbMes)
 })
 output$rbTabela <- renderTable({
   rbTabela(input$rbComandos,input$rbAno %>% as.integer(),input$rbMes)
 })
 output$rbLocal <- renderPlot({
   rbLocal(input$rbComandos,input$rbAno,input$rbMes)
 })
 output$rbBairroTot <- renderTable({
   rbBairros(input$rbComandos,input$rbAno,tipo = "TOTAL",input$rbMes)
 })
 output$rbBairroPes <- renderTable({
   rbBairros(input$rbComandos,input$rbAno,tipo = "PESSOA",input$rbMes)
 })
 output$rbBairroRes <- renderTable({
   rbBairros(input$rbComandos,input$rbAno,tipo = "RESIDENCIA",input$rbMes)
 })
 output$rbBairroCom <- renderTable({
   rbBairros(input$rbComandos,input$rbAno,tipo = "ESTABELECIMENTO COMERCIAL",input$rbMes)
 })
 output$rbSemana <- renderPlot({
   rbSemana(input$rbComandos,input$rbAno,input$rbMes)
 })
 
 
  ################################ Furto ################
 source("Furto.R",local = TRUE)
 
  ###################### Furto output MT ##########
 
 output$ftIndiceMT <- renderValueBox({
   val = ftVarMT(input$ftAnoMT %>% as.integer(),input$ftMesMT) %>% parse_number()
   
   if(val<= 0){
     icone= "angle-double-down"
     cor = "green"
   }else{
     icone = "angle-double-up"
     cor = "red"
   }
   
   valueBox(value = paste(round(val,2),"%",sep = " "),
            "Variação em relação ao Ultimo Ano",
            icon = icon(icone),color = cor,width = NULL)
   
 })
 output$ftPessoaMT <- renderValueBox({
   valueBox(ftIndiceMT(input$ftAnoMT,"PESSOA",input$ftMesMT),
            "Roubo a Pessoa",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ftResidenciaMT <- renderValueBox({
   valueBox(ftIndiceMT(input$ftAnoMT,"RESIDENCIA",input$ftMesMT),
            "Roubo a Residencia",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ftComercioMT <- renderValueBox({
   valueBox(ftIndiceMT(input$ftAnoMT,"ESTABELECIMENTO COMERCIAL",input$ftMesMT),
            "Roubo a Comercio",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ftMapaMT <- renderLeaflet({
   ftMapaMT(input$ftAnoMT)
 })
 output$ftHorasMT <- renderPlot({
   ftHoraMT(input$ftAnoMT,input$ftMesMT)
 })
 output$ftDiaMT <- renderPlot({
   ftDiaMT(input$ftAnoMT,input$ftMesMT)
 })
 output$ftTabelaMT <- renderTable({
   ftTabelaMT(input$ftAnoMT %>% as.integer(),input$ftMesMT)
 })
 output$ftLocalMT <- renderPlot({
   ftLocalMT(input$ftAnoMT,input$ftMesMT)
 })
 output$ftMunicipioMT <- renderTable({
   ftMunicipioMT(input$ftAnoMT,input$ftMesMT,tipo = "TOTAL")
 })
 output$ftMunicipioPesMT <- renderTable({
   ftMunicipioMT(input$ftAnoMT,input$ftMesMT,tipo = "PESSOA")
 })
 output$ftMunicipioResMT <- renderTable({
   ftMunicipioMT(input$ftAnoMT,input$ftMesMT,tipo = "RESIDENCIA")
 })
 output$ftMunicipioComMT <- renderTable({
   ftMunicipioMT(input$ftAnoMT,input$ftMesMT,tipo = "ESTABELECIMENTO COMERCIAL")
 })
 output$ftSemanaMT <- renderPlot({
   ftSemanaMT(input$ftAnoMT,input$ftMesMT)
 })
 
 
  ####################### Furto Comandos ###########
 
 output$ftIndice <- renderValueBox({
   
   val = ftVar(input$ftComandos,input$ftAno %>% as.integer(),input$ftMes) %>% parse_number()
   
   if(val<= 0){
     icone= "angle-double-down"
     cor = "green"
   }else{
     icone = "angle-double-up"
     cor = "red"
   }
   
   valueBox(paste(val,"%",sep=""),
            "Variação em relação ao Ultimo Ano",
            icon = icon(icone),color = cor,width = NULL)
   
 })
 output$ftPessoa <- renderValueBox({
   valueBox(ftIndice(input$ftComandos,input$ftAno,"PESSOA",input$ftMes),
            "Roubo a Pessoa",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ftResidencia <- renderValueBox({
   valueBox(ftIndice(input$ftComandos,input$ftAno,"RESIDENCIA",input$ftMes),
            "Roubo a Residencia",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ftComercio <- renderValueBox({
   valueBox(ftIndice(input$ftComandos,input$ftAno,"ESTABELECIMENTO COMERCIAL",input$ftMes),
            "Roubo a Comercio",icon = icon("percentage"),color = "blue",width = NULL)
 })
 output$ftMapa <- renderLeaflet({
   ftMapa(input$ftComandos,input$ftAno)
 })
 output$ftHoras <- renderPlot({
   ftHora(input$ftComandos,input$ftAno,input$ftMes)
 })
 output$ftDia <- renderPlot({
   ftDia(input$ftComandos,input$ftAno,input$ftMes)
 })
 output$ftTabela <- renderTable({
   ftTabela(input$ftComandos,input$ftAno %>% as.integer(),input$ftMes)
 })
 output$ftLocal <- renderPlot({
   ftLocal(input$ftComandos,input$ftAno,input$ftMes)
 })
 output$ftBairroTot <- renderTable({
   ftBairros(input$ftComandos,input$ftAno,tipo = "TOTAL",input$ftMes)
 })
 output$ftBairroPes <- renderTable({
   ftBairros(input$ftComandos,input$ftAno,tipo = "PESSOA",input$ftMes)
 })
 output$ftBairroRes <- renderTable({
   ftBairros(input$ftComandos,input$ftAno,tipo = "RESIDENCIA",input$ftMes)
 })
 output$ftBairroCom <- renderTable({
   ftBairros(input$ftComandos,input$ftAno,tipo = "ESTABELECIMENTO COMERCIAL",input$ftMes)
 })
 output$ftSemana <- renderPlot({
   ftSemana(input$ftComandos,input$ftAno,input$ftMes)
 })
 
  ######################## produtividade base ###################### 
 
 source("Arma.R",local = TRUE)
 source("Entorpecentes.R",local = TRUE)
 
  ####################### Arma output MT ##########
 
   output$ArmaIndiceMT <- renderValueBox({
     val = varArmaMT(input$ArmaAnoMT %>% as.integer(),input$ArmaMesMT)
     
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
     
     valueBox(percent(val),
              "Variação em relação ao Ultimo Ano",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaFlagranteMT <- renderValueBox({
     val = SoluçaoArmaMT(input$ArmaAnoMT,soluçao = "FLAGRANTE",input$ArmaMesMT)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Flagrante",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaConduçãoMT <- renderValueBox({
     val = SoluçaoArmaMT(input$ArmaAnoMT,soluçao = "CONDUCAO DELPOL",input$ArmaMesMT)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Condução DELPOL",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaBoletimMT <- renderValueBox({
     val = SoluçaoArmaMT(input$ArmaAnoMT,soluçao = "BOLETIM REGISTRADO",input$ArmaMesMT)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Boletim Registrado",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaMapaMT <- renderLeaflet({
     MapaArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   output$ArmaHorasMT <- renderPlot({
     HorasArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   output$ArmaDiaMT <- renderPlot({
     DiaSemanaArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   output$ArmaMesMT <- renderPlot({
     MesArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   output$ArmaTabelaMT <- renderTable({
     TabelaArmaMT(input$ArmaAnoMT %>% as.integer(),input$ArmaMesMT)
   })
   output$ArmaTipoChartMT <- renderPlot({
     EspecificarArmaMT(input$ArmaAnoMT,modo = "grafico",input$ArmaMesMT)
   })
   output$ArmaTipoTableMT <- renderTable({
     EspecificarArmaMT(input$ArmaAnoMT,modo = "tabela",input$ArmaMesMT)
   })
   output$ArmaNaturezaMT <- renderTable({
     NaturezaArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   output$ArmaCidadeMT <- renderTable({
     CidadeArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   output$ArmaLocalMT <- renderTable({
     LocalArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   output$ArmaSemanaMT <- renderPlot({
     SemanaArmaMT(input$ArmaAnoMT,input$ArmaMesMT)
   })
   
  ######################## Arma Comandos ########
   
   output$ArmaIndice <- renderValueBox({
     val = varArma(input$ArmaComandos,input$ArmaAno %>% as.integer(),input$ArmaMes)
     
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
     
     valueBox(percent(val),
              "Variação em relação ao Ultimo Ano",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaFlagrante <- renderValueBox({
     val = SoluçaoArma(input$ArmaComandos,input$ArmaAno,soluçao = "FLAGRANTE",input$ArmaMes)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Flagrante",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaConducao <- renderValueBox({
     val = SoluçaoArma(input$ArmaComandos,input$ArmaAno,soluçao = "CONDUCAO DELPOL",input$ArmaMes)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Condução DELPOL",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaBoletim <- renderValueBox({
     val = SoluçaoArma(input$ArmaComandos,input$ArmaAno,soluçao = "BOLETIM REGISTRADO",input$ArmaMes)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Boletim Registrado",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$ArmaMapa <- renderLeaflet({
     MapaArma(input$ArmaComandos,input$ArmaAno,input$ArmaMes)
   })
   output$ArmaHoras <- renderPlot({
     HorasArma(input$ArmaComandos,input$ArmaAno,input$ArmaMes)
   })
   output$ArmaDia <- renderPlot({
     DiaSemanaArma(crs = input$ArmaComandos,ano = input$ArmaAno,input$ArmaMes)
   })
   output$ArmaMes <- renderPlot({
     MesArma(input$ArmaComandos,input$ArmaAno,input$ArmaMes)
   })
   output$ArmaTabela <- renderTable({
     TabelaArma(input$ArmaComandos,input$ArmaAno %>% as.integer(),input$ArmaMes)
   })
   output$ArmaTipoChart <- renderPlot({
     EspecificarArma(input$ArmaComandos,input$ArmaAno,modo = "grafico",input$ArmaMes)
   })
   output$ArmaTipoTable <- renderTable({
     EspecificarArma(input$ArmaComandos,input$ArmaAno,modo = "tabela",input$ArmaMes)
   })
   output$ArmaNatureza <- renderTable({
     NaturezaArma(input$ArmaComandos,input$ArmaAno,input$ArmaMes)
   })
   output$ArmaBatalhao <- renderTable({
     BatalhaoArma(input$ArmaComandos,input$ArmaAno,input$ArmaMes)
   })
   output$ArmaLocal <- renderTable({
     LocalArma(input$ArmaComandos,input$ArmaAno,input$ArmaMes)
   })
   output$ArmaSemana <- renderPlot({
     SemanaArma(input$ArmaComandos,input$ArmaAno,input$ArmaMes)
   })
  
  ##################### Drogas output MT #######
   
   output$DrogaIndiceMT <- renderValueBox({
     val = varDrogaMT(input$DrogaAnoMT %>% as.numeric(),input$DrogaMesMT)
  
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Variação em relação ao Ultimo Ano",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaFlagranteMT <- renderValueBox({
     val = SoluçaoDrogaMT(input$DrogaAnoMT %>% as.integer(),soluçao = "FLAGRANTE",input$DrogaMesMT)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Flagrante",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaConduçãoMT <- renderValueBox({
     val = SoluçaoDrogaMT(input$DrogaAnoMT %>% as.integer(),soluçao = "CONDUCAO DELPOL",input$DrogaMesMT)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Conduçao DELPOL",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaBoletimMT <- renderValueBox({
     val = SoluçaoDrogaMT(input$DrogaAnoMT %>% as.integer(),soluçao = "BOLETIM REGISTRADO",input$DrogaMesMT)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Boletim Registrado",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaMapaMT <- renderLeaflet({
     MapaDrogaMT(input$DrogaAnoMT,input$DrogaMesMT)
   })
   output$DrogaHorarioKgMT <- renderPlot({
     HorasDrogaMT(input$DrogaAnoMT,modo = "kilos",input$DrogaMesMT)
   })
   output$DrogaHorarioNMT <- renderPlot({
     HorasDrogaMT(input$DrogaAnoMT,modo = "ocorrencias",input$DrogaMesMT)
   })
   output$DrogaDiaKgMT <- renderPlot({
     DiaDrogaMT(input$DrogaAnoMT,modo = "kilos",input$DrogaMesMT)
   })
   output$DrogaDiaNMT <- renderPlot({
     DiaDrogaMT(input$DrogaAnoMT,modo = "ocorrencias",input$DrogaMesMT)
   })
   output$DrogaMesKgMT <- renderPlot({
     MesDrogaMT(input$DrogaAnoMT,modo = "kilos",input$DrogaMesMT)
   })
   output$DrogaMesNMT <- renderPlot({
     MesDrogaMT(input$DrogaAnoMT,modo = "ocorrencias",input$DrogaMesMT)
   })
   output$DrogaTabelaMT <- renderTable({
     TabelaDrogaMT(input$DrogaAnoMT %>% as.integer(),input$DrogaMesMT)
   })
   output$DrogaTipoChartMT <- renderPlot({
     EspecifDrogaMT(input$DrogaAnoMT,modo = "grafico",input$DrogaMesMT)
   })
   output$DrogaTipoTableMT <- renderTable({
     EspecifDrogaMT(input$DrogaAnoMT,modo = "tabela",input$DrogaMesMT)
   })
   output$DrogaNaturezaMT <- renderTable({
     NaturezaDrogaMT(input$DrogaAnoMT,input$DrogaMesMT)
   })
   output$DrogaCidadeMT <-renderTable({
     CidadeDrogaMT(input$DrogaAnoMT,input$DrogaMesMT)
   })
   output$DrogaLocalMT <- renderTable({
     LocalDrogaMT(input$DrogaAnoMT,input$DrogaMesMT)
   })
   output$DrogaSemanaKgMT <- renderPlot({
     SemanaDrogaMT(input$DrogaAnoMT,modo = "kilos",input$DrogaMesMT)
   })
   output$DrogaSemanaNMT <- renderPlot({
     SemanaDrogaMT(input$DrogaAnoMT,modo = "ocorrencias",input$DrogaMesMT)
   })
   
  ###################### Drogas Comandos #######
   
   output$DrogaIndice <- renderValueBox({
     val = varDroga(input$DrogaAno %>% as.integer(),input$DrogaComandos,input$DrogaMes)
     
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Variação em relação ao Ultimo Ano",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaFlagrante <- renderValueBox({
     val = SoluçaoDroga(input$DrogaAno %>% as.integer(),input$DrogaComandos,soluçao = "FLAGRANTE",input$DrogaMes)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Flagrante",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaCondução <- renderValueBox({
     val = SoluçaoDroga(input$DrogaAno %>% as.integer(),input$DrogaComandos,soluçao = "CONDUCAO DELPOL",input$DrogaMes)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Condução DELPOL",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaBoletim <- renderValueBox({
     val = SoluçaoDroga(input$DrogaAno %>% as.integer(),input$DrogaComandos,soluçao = "BOLETIM REGISTRADO",input$DrogaMes)
     
     if(val[[2]] <= 0){
       icone= "angle-double-up"
       cor = "green"
     }else{
       icone = "angle-double-down"
       cor = "red"
     }
     
     valueBox(percent(val[[1]]),
              "Boletim Registrado",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$DrogaMapa <- renderLeaflet({
     MapaDroga(input$DrogaAno,input$DrogaComandos,input$DrogaMes)
   })
   output$DrogaHorarioKg <- renderPlot({
     HorasDroga(input$DrogaAno,input$DrogaComandos,modo = "kilos",input$DrogaMes)
   })
   output$DrogaHorarioN <- renderPlot({
     HorasDroga(input$DrogaAno,input$DrogaComandos,modo = "ocorrencias",input$DrogaMes)
   })
   output$DrogaDiaKg <- renderPlot({
     DiaDroga(input$DrogaAno,input$DrogaComandos,modo = "kilos",input$DrogaMes)
   })
   output$DrogaDiaN <- renderPlot({
     DiaDroga(input$DrogaAno,input$DrogaComandos,modo = "ocorrencias",input$DrogaMes)
   })
   output$DrogaMesKg <- renderPlot({
     MesDroga(input$DrogaAno,input$DrogaComandos,modo = "kilos",input$DrogaMes)
   })
   output$DrogaMesN <- renderPlot({
     MesDroga(input$DrogaAno,input$DrogaComandos,modo = "ocorrencias",input$DrogaMes)
   })
   output$DrogaTabela <- renderTable({
     TabelaDroga(input$DrogaAno %>% as.integer(),input$DrogaComandos,input$DrogaMes)
   })
   output$DrogaTipoChart <- renderPlot({
     EspecifDroga(input$DrogaAno,input$DrogaComandos,modo = "grafico",input$DrogaMes)
   })
   output$DrogaTipoTable <- renderTable({
     EspecifDroga(input$DrogaAno,input$DrogaComandos,modo = "tabela",input$DrogaMes)
   })
   output$DrogaNatureza <- renderTable({
     NaturezaDroga(input$DrogaAno,input$DrogaComandos,input$DrogaMes)
   })
   output$DrogaBatalhao <-renderTable({
     BatalhaoDroga(input$DrogaAno,input$DrogaComandos,input$DrogaMes)
   })
   output$DrogaLocal <- renderTable({
     LocalDroga(input$DrogaAno,input$DrogaComandos,input$DrogaMes)
   })
   output$DrogaSemanaKg <- renderPlot({
     SemanaDroga(input$DrogaAno,input$DrogaComandos,modo = "kilos",input$DrogaMes)
   })
   output$DrogaSemanaN <- renderPlot({
     SemanaDroga(input$DrogaAno,input$DrogaComandos,modo = "ocorrencias",input$DrogaMes)
   })
 
   
   ################## prisao por mandato base #########
   
   source("PrisaoMandato.R",local = TRUE)
   
   ############ prisao estado ##################
   
   output$PrisaoMapaMT <- renderLeaflet({
     MapaPrisaoMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })
   output$PrisaoIdadeMT <- renderPlot({
     priIdadeMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })
   output$PrisaoHorasMT <- renderPlot({
     PriHorasMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })
   output$PrisaoDiaMT <- renderPlot({
     PriDiaMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })  
   output$PrisaoMesMT <- renderPlot({
     PriMesMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })
   output$PrisaoTabelaMT <- renderTable({
     priTabelaMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })  
   output$PrisaoSexoMT <- renderPlot({
     PriSexoMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })
   output$PrisaoLocalMT <- renderTable({
     priCidadeMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   }) 
   output$PrisaoModoMT <- renderTable({
     PriModoMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })  
   output$PrisaoCaractMT <- renderTable({
     PriCaractMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)%>% head(10)
   })  
   output$PrisaoSemanaMT <- renderPlot({
     priSemanaMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
   })
   
   output$PrisaoIndiceMT <- renderValueBox({
     val = PriVarMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT)
     
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
     
     valueBox(percent(val),
              "Variação em relação ao Ultimo Ano",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$PrisaoIdMdMT <- renderValueBox({
     valueBox(PriIdMdMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT) %>% round(2),
              "Idade Média",
              icon = icon("globe-americas"),color = "blue",width = NULL)
   })
   output$PrisaoCidadeMT <- renderValueBox({
     valueBox(PriCidOcocMT(input$PrisaoAnoMT %>% as.numeric(),input$PrisaoMesMT),
              "Cidade Ocorrencia diferente da do Suspeito",
              icon = icon("globe-americas"),color = "blue",width = NULL)
   })
   
  
   ########## prisao comandos ###############
   
   output$PrisaoMapa <- renderLeaflet({
     MapaPrisao(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })
   output$PrisaoIdade <- renderPlot({
     priIdade(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })
   output$PrisaoHoras <- renderPlot({
     PriHoras(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })
   output$PrisaoDia <- renderPlot({
     PriDia(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })  
   output$PrisaoMes <- renderPlot({
     PriMes(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })
   output$PrisaoTabela <- renderTable({
     priTabela(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })  
   output$PrisaoSexo <- renderPlot({
     PriSexo(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })
   output$PrisaoLocal <- renderTable({
     priUnidade(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   }) 
   output$PrisaoBatalhao <- renderTable({
     PriModo(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })  
   output$PrisaoCaract <- renderTable({
     PriCaract(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes) %>% head(10)
   })  
   output$PrisaoSemana <- renderPlot({
     priSemana(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
   })
   
   output$PrisaoIndice <- renderValueBox({
     val = PriVar(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes)
     
     if(val<= 0){
       icone= "angle-double-down"
       cor = "green"
     }else{
       icone = "angle-double-up"
       cor = "red"
     }
     
     valueBox(percent(val),
              "Variação em relação ao Ultimo Ano",
              icon = icon(icone),color = cor,width = NULL)
   })
   output$PrisaoIdMd <- renderValueBox({
     valueBox(PriIdMd(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes) %>% round(2),
     "Idade Média",
       icon = icon("globe-americas"),color = "blue",width = NULL)
     })
   output$PrisaoCidade <- renderValueBox({
     valueBox(PriCidOcoc(input$PrisaoComandos,input$PrisaoAno %>% as.numeric(),input$PrisaoMes),
              "Cidade Ocorrencia diferente da do Suspeito",
              icon = icon("globe-americas"),color = "blue",width = NULL)
   })
   
} 
 
 

