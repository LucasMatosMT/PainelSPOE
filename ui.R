library(shinydashboard)
library(dashboardthemes)
library(leaflet)
library(leaflet.extras)
library(readxl)

DadosIBGE_mt <- read_excel("DadosIBGE_mt.xlsx",range = "a1:w142")
BaseUnica <- read_excel("BaseUnica.xlsx")

logo_blue_gradient <- shinyDashboardLogoDIY(
  
  boldText = "SPOE "
  ,mainText = "- Mato Grosso"
  ,textSize = 16
  ,badgeText = "BETA"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
  
)


ui <- dashboardPage(title = "SPOE - Mato Grosso",
  dashboardHeader(title = logo_blue_gradient
                    ,titleWidth = 285),
  dashboardSidebar(sidebarMenu(id = "menu", sidebarMenuOutput("menu")),width = 285),
  dashboardBody(shinyDashboardThemes(
    theme = "blue_gradient"
  ),
    tabItems(tabItem(
      tabName = "mapaunidades",
      tags$h1("Mapa e Informações das Unidades de Mato Grosso",align = "center"),
      tags$h5("Informações atualizadas até : 20/07/2017",align = "center"),
      fluidPage(box(selectInput("inputTest",label =  "Comandos Regionais:",
                      choices = DadosIBGE_mt$CR %>% factor(levels = c("1°CR",
                                                                      "2°CR",
                                                                      "3°CR",
                                                                      "4°CR",
                                                                      "5°CR",
                                                                      "6°CR",
                                                                      "7°CR",
                                                                      "8°CR",
                                                                      "9°CR",
                                                                      "10°CR",
                                                                      "11°CR",
                                                                      "12°CR",
                                                                      "13°CR",
                                                                      "14°CR",
                                                                      "15°CR"))%>%
                        unique()%>%
                        sort(), multiple=TRUE, selectize=TRUE,
                      width = '95%'),width = 3,status = 'primary'),
      box(
        selectInput("inputTest2",label = "Cidades:",
                  choices = NULL, multiple=TRUE, selectize=TRUE,
                  width = '95%'),width = 3,status = 'primary'),
      box(
        selectInput(
          "filUnidades",label = "Unidades",
          choices = c(BaseUnica$Unidades %>% unique(),"Comando Geral"),
          multiple=TRUE, selectize=TRUE,width = '95%'
                  ),width = 3,status = 'primary'
        ),
      box(
          actionButton("act","Filtrar"),
          checkboxInput("shpcrs","Territorios dos CRs",value = F),
          width = 3,status = "primary"
        )
      ),
      box(leafletOutput("unidadesmt",height = "800px"),status = 'primary',width = NULL,
          solidHeader = TRUE)
    ),
      tabItem(
        tabName = "estado",
        fluidRow(valueBoxOutput("indiceMT",width = 3),
                 valueBoxOutput("ArmaFogoMT",width = 3),
                 valueBoxOutput("ArmaCortanteMT",width = 3),
                 valueBoxOutput("ConfrontoMT",width = 3)
        ),
        fluidRow(column(width = 7,
                        box(title = "Mapa da Taxa de Homicidio",status = 'primary',width = NULL,
                            solidHeader = TRUE,leafletOutput("mapamt",height = "650px",width = "100%")),
                        box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                            plotOutput(outputId = "horasPlotMT")),
                        box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                            plotOutput(outputId = "diaPlotMT"))
                        ),
                 column(width = 5,
                        tabBox(title = "Tabela dos Comandos",width = NULL,
                               tabPanel("Taxas",tableOutput("HomTaxasMT")),
                               tabPanel("Confronto",tableOutput("tableMTconf"))),
                         box(title = "Grafico por sexo",width = NULL,
                             solidHeader = TRUE,plotOutput(outputId = "GenPlotMT"),status = "success"),
                         box(title = "Grafico por Faixa Etaria",width = NULL,
                             solidHeader = TRUE,plotOutput(outputId = "IdadePlotMT"),status = "success"))),
        fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                     plotOutput(outputId = "semanaMT"),width = 12))
        ),
      tabItem(
        tabName = "comandos",
        fluidRow(valueBoxOutput("indice",width = 3),
                 valueBoxOutput("ArmaFogo",width = 2),
                 valueBoxOutput("ArmaCortante",width = 3),
                 valueBoxOutput("TaxaMT",width = 2),
                 valueBoxOutput("taxaBR",width = 2)
        ),
        fluidRow(
          column(width = 7,
                 box(title = 'Mapa',status = 'primary',width = NULL,solidHeader = TRUE,
                     leafletOutput(outputId = "distPlot",height = "700px")),
                 box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                     plotOutput(outputId = "horasPlot")),
                 box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                     plotOutput(outputId = "diaPlot"))
          ),
          column(width = 5,
                 box(title = "Tabela",tableOutput('tablePlot'),
                     solidHeader = TRUE,width = NULL,status = "success"),
                 box(title = "Grafico por sexo",width = NULL,
                     solidHeader = TRUE,plotOutput(outputId = "GenPlot"),status = "success"),
                 box(title = "Grafico por Faixa Etaria",width = NULL,
                     solidHeader = TRUE,plotOutput(outputId = "IdadePlot"),status = "success"))
        ),fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                       plotOutput(outputId = "semana"),width = 12)
                   )
      ),
      tabItem(tabName = "rbEstado",
              fluidRow(valueBoxOutput("rbIndiceMT",width = 3),
                       valueBoxOutput("rbPessoaMT",width = 3),
                       valueBoxOutput("rbResidenciaMT",width = 3),
                       valueBoxOutput("rbComercioMT",width = 3)
              ),
              fluidRow(column(width = 7,
                              box(title = "Mapa da Taxa de Roubo",status = 'primary',width = NULL,
                                  solidHeader = TRUE,leafletOutput("rbMapaMT",height = "650px",width = "100%")),
                              box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "rbHorasMT")),
                              box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "rbDiaMT"))
              ),
              column(width = 5,
                     box(title = "Tabela dos Comandos",width = NULL,
                         solidHeader = TRUE,tableOutput("rbTabelaMT"),status = "success"),
                     box(title = "Grafico dos Locais",width = NULL,
                         solidHeader = TRUE,plotOutput(outputId = "rbLocalMT"),status = "success"),
                     tabBox(title = "Municipios",width = NULL,
                         tabPanel("Total",tableOutput(outputId = "rbMunicipioMT")),
                         tabPanel("Pessoa",tableOutput(outputId = "rbMunicipioPesMT")),
                         tabPanel("Residencia",tableOutput(outputId = "rbMunicipioResMT")),
                         tabPanel("Comercio",tableOutput(outputId = "rbMunicipioComMT"))
                         ))),
              fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                           plotOutput(outputId = "rbSemanaMT"),width = 12))),
      tabItem(tabName = "rbComando",
              fluidRow(valueBoxOutput("rbIndice",width = 3),
                       valueBoxOutput("rbPessoa",width = 3),
                       valueBoxOutput("rbResidencia",width = 3),
                       valueBoxOutput("rbComercio",width = 3)
              ),
              fluidRow(
                column(width = 7,
                       box(title = 'Mapa',status = 'primary',width = NULL,solidHeader = TRUE,
                           leafletOutput("rbMapa",height = "650px",width = "100%")),
                       box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "rbHoras")),
                       box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "rbDia"))
                ),
                column(width = 5,
                       box(title = "Tabela",tableOutput('rbTabela'),
                           solidHeader = TRUE,width = NULL,status = "success"),
                       box(title = "Grafico dos Locais",width = NULL,
                           solidHeader = TRUE,plotOutput(outputId = "rbLocal"),status = "success"),
                       tabBox(title = "Bairros",width = NULL,
                              tabPanel("Total",tableOutput(outputId = "rbBairroTot")),
                              tabPanel("Pessoa",tableOutput(outputId = "rbBairroPes")),
                              tabPanel("Residencia",tableOutput(outputId = "rbBairroRes")),
                              tabPanel("Comercio",tableOutput(outputId = "rbBairroCom"))
                       ))
              ),fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                             plotOutput(outputId = "rbSemana"),width = 12)
              )
            ),
      tabItem(tabName = "ftEstado",
              fluidRow(valueBoxOutput("ftIndiceMT",width = 3),
                       valueBoxOutput("ftPessoaMT",width = 3),
                       valueBoxOutput("ftResidenciaMT",width = 3),
                       valueBoxOutput("ftComercioMT",width = 3)
              ),
              fluidRow(column(width = 7,
                              box(title = "Mapa da Taxa de Furto",status = 'primary',width = NULL,
                                  solidHeader = TRUE,leafletOutput("ftMapaMT",height = "650px",width = "100%")),
                              box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "ftHorasMT")),
                              box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "ftDiaMT"))
              ),
              column(width = 5,
                     box(title = "Tabela dos Comandos",width = NULL,
                         solidHeader = TRUE,tableOutput("ftTabelaMT"),status = "success"),
                     box(title = "Grafico dos Locais",width = NULL,
                         solidHeader = TRUE,plotOutput(outputId = "ftLocalMT"),status = "success"),
                     tabBox(title = "Municipios",width = NULL,
                            tabPanel("Total",tableOutput(outputId = "ftMunicipioMT")),
                            tabPanel("Pessoa",tableOutput(outputId = "ftMunicipioPesMT")),
                            tabPanel("Residencia",tableOutput(outputId = "ftMunicipioResMT")),
                            tabPanel("Comercio",tableOutput(outputId = "ftMunicipioComMT"))
                     ))),
              fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                           plotOutput(outputId = "ftSemanaMT"),width = 12))),
      tabItem(tabName = "ftComando",
              fluidRow(valueBoxOutput("ftIndice",width = 3),
                       valueBoxOutput("ftPessoa",width = 3),
                       valueBoxOutput("ftResidencia",width = 3),
                       valueBoxOutput("ftComercio",width = 3)
              ),
              fluidRow(
                column(width = 7,
                       box(title = 'Mapa',status = 'primary',width = NULL,solidHeader = TRUE,
                           leafletOutput("ftMapa",height = "700px",width = "100%")),
                       box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "ftHoras")),
                       box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "ftDia"))
                ),
                column(width = 5,
                       box(title = "Tabela",tableOutput('ftTabela'),
                           solidHeader = TRUE,width = NULL,status = "success"),
                       box(title = "Grafico dos Locais",width = NULL,
                           solidHeader = TRUE,plotOutput(outputId = "ftLocal"),status = "success"),
                       tabBox(title = "Bairros",width = NULL,
                              tabPanel("Total",tableOutput(outputId = "ftBairroTot")),
                              tabPanel("Pessoa",tableOutput(outputId = "ftBairroPes")),
                              tabPanel("Residencia",tableOutput(outputId = "ftBairroRes")),
                              tabPanel("Comercio",tableOutput(outputId = "ftBairroCom"))
                       ))
              ),fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                             plotOutput(outputId = "ftSemana"),width = 12)
              )
      ),
      tabItem(tabName = "ArmaEstado",
              fluidRow(valueBoxOutput("ArmaIndiceMT",width = 3),
                       valueBoxOutput("ArmaFlagranteMT",width = 3),
                       valueBoxOutput("ArmaConduçãoMT",width = 3),
                       valueBoxOutput("ArmaBoletimMT",width = 3)
              ),
              fluidRow(column(width = 7,
                              box(title = "Mapa de Apreensões de Arma de Fogo",status = 'primary',
                                  width = NULL,solidHeader = TRUE,
                                  leafletOutput("ArmaMapaMT",height = "650px",width = "100%")),
                              box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "ArmaHorasMT")),
                              box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "ArmaDiaMT")),
                              box(title = "Apreensões por Mês",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "ArmaMesMT"))
              ),
              column(width = 5,
                     box(title = "Tabela dos Comandos",width = NULL,
                         solidHeader = TRUE,tableOutput("ArmaTabelaMT"),status = "success"),
                     tabBox(title = "Tipo de Armas",width = NULL,
                         tabPanel("Grafico",plotOutput(outputId = "ArmaTipoChartMT")),
                         tabPanel("Tabela",tableOutput(outputId = "ArmaTipoTableMT"))
                         ),
                     box(title = "Tabela das Naturezas",width = NULL,
                        solidHeader = TRUE,tableOutput("ArmaNaturezaMT"),status = "success"),
                     box(title = "Tabela das Cidades",width = NULL,
                         solidHeader = TRUE,tableOutput("ArmaCidadeMT"),status = "success"),
                     box(title = "Tabela dos Locais",width = NULL,
                         solidHeader = TRUE,tableOutput("ArmaLocalMT"),status = "success")
                     )),
              fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                           plotOutput(outputId = "ArmaSemanaMT"),width = 12))),
      tabItem("ArmaComandos",
              fluidRow(valueBoxOutput("ArmaIndice",width = 3),
                       valueBoxOutput("ArmaFlagrante",width = 3),
                       valueBoxOutput("ArmaConducao",width = 3),
                       valueBoxOutput("ArmaBoletim",width = 3)
              ),
              fluidRow(
                column(width = 7,
                       box(title = 'Mapa de apreensão de Arma de Fogo',status = 'primary',width = NULL,solidHeader = TRUE,
                           leafletOutput(outputId = "ArmaMapa",height = "700px",width = "100%")),
                       box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "ArmaHoras")),
                       box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "ArmaDia")),
                       box(title = "Apreensões por Mês",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "ArmaMes"))
                ),
                column(width = 5,
                       box(title = "Tabela dos Comandos",width = NULL,
                           solidHeader = TRUE,tableOutput("ArmaTabela"),status = "success"),
                       tabBox(title = "Tipo de Armas",width = NULL,
                              tabPanel("Grafico",plotOutput(outputId = "ArmaTipoChart")),
                              tabPanel("Tabela",tableOutput(outputId = "ArmaTipoTable"))
                       ),
                       box(title = "Tabela das Naturezas",width = NULL,
                           solidHeader = TRUE,tableOutput("ArmaNatureza"),status = "success"),
                       box(title = "Tabela dos Batalhões",width = NULL,
                           solidHeader = TRUE,tableOutput("ArmaBatalhao"),status = "success"),
                       box(title = "Tabela dos Locais",width = NULL,
                           solidHeader = TRUE,tableOutput("ArmaLocal"),status = "success")
                       ))
              ,fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                             plotOutput(outputId = "ArmaSemana"),width = 12)
              )),
      tabItem("DrogaEstado",
              fluidRow(valueBoxOutput("DrogaIndiceMT",width = 3),
                       valueBoxOutput("DrogaFlagranteMT",width = 3),
                       valueBoxOutput("DrogaConduçãoMT",width = 3),
                       valueBoxOutput("DrogaBoletimMT",width = 3)
              ),
              fluidRow(column(width = 7,
                              box(title = "Mapa de Apreesão de Entorpecente pela PM",status = 'primary',
                                  width = NULL,solidHeader = TRUE,
                                  leafletOutput("DrogaMapaMT",height = "650px",width = "100%")),
                              tabBox(title = "Faixa de Horario",width = NULL,
                                     tabPanel("Kilos",plotOutput(outputId = "DrogaHorarioKgMT")),
                                     tabPanel("Ocorrências",plotOutput(outputId = "DrogaHorarioNMT"))),
                              tabBox(title = "Dia da Semana",width = NULL,
                                     tabPanel("Kilos",plotOutput(outputId = "DrogaDiaKgMT")),
                                     tabPanel("Ocorrências",plotOutput(outputId = "DrogaDiaNMT"))),
                              tabBox(title = "Por Mes",width = NULL,
                                     tabPanel("Kilos",plotOutput(outputId = "DrogaMesKgMT")),
                                     tabPanel("Ocorrências",plotOutput(outputId = "DrogaMesNMT")))
              ),
              column(width = 5,
                     box(title = "Tabela dos Comandos",width = NULL,
                         solidHeader = TRUE,tableOutput("DrogaTabelaMT"),status = "success"),
                     tabBox(title = "Tipo de Drogas",width = NULL,
                            tabPanel("Grafico",plotOutput(outputId = "DrogaTipoChartMT")),
                            tabPanel("Tabela",tableOutput(outputId = "DrogaTipoTableMT"))
                     ),
                     box(title = "Tabela das Naturezas",width = NULL,
                         solidHeader = TRUE,tableOutput("DrogaNaturezaMT"),status = "success"),
                     box(title = "Tabela das Cidades",width = NULL,
                         solidHeader = TRUE,tableOutput("DrogaCidadeMT"),status = "success"),
                     box(title = "Tabela dos Locais",width = NULL,
                         solidHeader = TRUE,tableOutput("DrogaLocalMT"),status = "success")
              )),
              fluidRow(tabBox(title = "Por Semana",width = 12,
                              tabPanel("Kilos",plotOutput(outputId = "DrogaSemanaKgMT")),
                              tabPanel("Ocorrências",plotOutput(outputId = "DrogaSemanaNMT"))
              ))
              ),
      tabItem("DrogasComandos",
              fluidRow(valueBoxOutput("DrogaIndice",width = 3),
                       valueBoxOutput("DrogaFlagrante",width = 3),
                       valueBoxOutput("DrogaCondução",width = 3),
                       valueBoxOutput("DrogaBoletim",width = 3)
              ),
              fluidRow(
                column(width = 7,
                       box(title = 'Mapa de apreensão de Entorpecente pela PM',status = 'primary',width = NULL,solidHeader = TRUE,
                           leafletOutput(outputId = "DrogaMapa",height = "700px",width = "100%")),
                       tabBox(title = "Faixa de Horario",width = NULL,
                              tabPanel("Kilos",plotOutput(outputId = "DrogaHorarioKg")),
                              tabPanel("Ocorrências",plotOutput(outputId = "DrogaHorarioN"))),
                       tabBox(title = "Dia da Semana",width = NULL,
                              tabPanel("Kilos",plotOutput(outputId = "DrogaDiaKg")),
                              tabPanel("Ocorrências",plotOutput(outputId = "DrogaDiaN"))),
                       tabBox(title = "Por Mes",width = NULL,
                              tabPanel("Kilos",plotOutput(outputId = "DrogaMesKg")),
                              tabPanel("Ocorrências",plotOutput(outputId = "DrogaMesN")))
                ),
                column(width = 5,
                       box(title = "Tabela dos Comandos",width = NULL,
                           solidHeader = TRUE,tableOutput("DrogaTabela"),status = "success"),
                       tabBox(title = "Tipo de Drogas",width = NULL,
                              tabPanel("Grafico",plotOutput(outputId = "DrogaTipoChart")),
                              tabPanel("Tabela",tableOutput(outputId = "DrogaTipoTable"))
                       ),
                       box(title = "Tabela das Naturezas",width = NULL,
                           solidHeader = TRUE,tableOutput("DrogaNatureza"),status = "success"),
                       box(title = "Tabela dos Batalhões",width = NULL,
                           solidHeader = TRUE,tableOutput("DrogaBatalhao"),status = "success"),
                       box(title = "Tabela dos Locais",width = NULL,
                           solidHeader = TRUE,tableOutput("DrogaLocal"),status = "success")
                )),
              fluidRow(tabBox(title = "Por Semana",width = 12,
                              tabPanel("Kilos",plotOutput(outputId = "DrogaSemanaKg")),
                              tabPanel("Ocorrências",plotOutput(outputId = "DrogaSemanaN"))
              ))
              ),
      tabItem(tabName = "PrisaoEstado",
              fluidRow(valueBoxOutput("PrisaoIndiceMT",width = 4),
                       valueBoxOutput("PrisaoCidadeMT",width = 4),
                       valueBoxOutput("PrisaoIdMdMT",width = 4)
              ),
              fluidRow(column(width = 7,
                              box(title = "Mapa de Prisao por Mandato",status = 'primary',
                                  width = NULL,solidHeader = TRUE,
                                  leafletOutput("PrisaoMapaMT",height = "650px","100%")),
                              box(title = "Distribuição Idade",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "PrisaoIdadeMT")),
                              box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "PrisaoHorasMT")),
                              box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "PrisaoDiaMT")),
                              box(title = "Apreensões por Mês",status = "success",width = NULL,solidHeader = T,
                                  plotOutput(outputId = "PrisaoMesMT"))
              ),
              column(width = 5,
                     box(title = "Tabela dos Comandos",width = NULL,
                         solidHeader = TRUE,tableOutput("PrisaoTabelaMT"),status = "success"),
                     box(title = "Grafico por Sexo",width = NULL,
                         solidHeader = TRUE,plotOutput("PrisaoSexoMT"),status = "success"),
                     box(title = "Tabela das Cidades",width = NULL,
                         solidHeader = TRUE,tableOutput("PrisaoLocalMT"),status = "success"),
                     box(title = "Tabela do Modo",width = NULL,
                         solidHeader = TRUE,tableOutput("PrisaoModoMT"),status = "success"),
                     box(title = "Tabela das Caracteristicas Socias",width = NULL,
                         solidHeader = TRUE,tableOutput("PrisaoCaractMT"),status = "success")
              )),
              fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                           plotOutput(outputId = "PrisaoSemanaMT"),width = 12))),
      tabItem("PrisaoComandos",
              fluidRow(valueBoxOutput("PrisaoIndice",width = 4),
                       valueBoxOutput("PrisaoCidade",width = 4),
                       valueBoxOutput("PrisaoIdMd",width = 4)
              ),
              fluidRow(
                column(width = 7,
                       box(title = 'Mapa de Prisao por Mandato',status = 'primary',width = NULL,solidHeader = TRUE,
                           leafletOutput(outputId = "PrisaoMapa",height = "700px","100%")),
                       box(title = "Distribuição de Idade",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "PrisaoIdade")),
                       box(title = "Faixa de Horario",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "PrisaoHoras")),
                       box(title = "Dia da Semana",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "PrisaoDia")),
                       box(title = "Prisões por Mês",status = "success",width = NULL,solidHeader = T,
                           plotOutput(outputId = "PrisaoMes"))
                ),
                column(width = 5,
                       box(title = "Tabela dos Comandos",width = NULL,
                           solidHeader = TRUE,tableOutput("PrisaoTabela"),status = "success"),
                       box(title = "Grafico por Sexo",width = NULL,
                           solidHeader = TRUE,plotOutput("PrisaoSexo"),status = "success"),
                       box(title = "Tabela dos Batalhões",width = NULL,
                           solidHeader = TRUE,tableOutput("PrisaoBatalhao"),status = "success"),
                       box(title = "Tabela do Modo",width = NULL,
                           solidHeader = TRUE,tableOutput("PrisaoModo"),status = "success"),
                       box(title = "Tabela das Caracteristicas Socias",width = NULL,
                           solidHeader = TRUE,tableOutput("PrisaoCaract"),status = "success")
                ))
              ,fluidRow(box(title = "Por Semana",solidHeader = TRUE,status = "success",
                            plotOutput(outputId = "PrisaoSemana"),width = 12)
              ))
    )
  )
)
