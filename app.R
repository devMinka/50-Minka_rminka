#################################################################################
###############################################################################

#APP web analisis de dades per especie de BBDD Minka amb conexio POstgres

#Sortida dades amb grau de recerca brutes sense tractar l esforç per quadricula

#Data posada en produccio: 06/09/2026

#llibreria  rminka

#versió: 0

################################################################################
################################################################################

library(waiter)

library(tidyverse)

library(jsonlite)

library(RColorBrewer)

library(leaflet)

library(sf)

library(httr)

library(DT)

library(bslib)

library(rminka)

library(shiny)

library(shinyjs)

library(htmltools)

library(leaflet.extras)

library(KernSmooth)

library(data.table)

library(raster)

library(htmlwidgets)

library(leafpop)

library(base64enc)

library(DBI)

library(RPostgres)

library(purrr)

library(pool)

options(shiny.sanitize.errors = FALSE)

options(shiny.maxRequestSize = 1000*1024^2)



#Creo la conexio pero atraves de pool perque nomes es vaci servir quan ho demani la funcio

pool <- pool::dbPool(RPostgres::Postgres(),
                     bigint = "numeric",
                     dbname = "bvuvabcrsqjxpsfu2hjg",
                     host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
                     port = 8400,
                     user = "ugnzz7lvvtl57ckg8oxa",
                     password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r")

#Per tancar les conexions quan acabi la app

onStop(function() pool::poolClose(pool))

Quadricules_10x10_sf <- st_read(pool, query = "SELECT * FROM utm10_litoral")%>% 
  st_transform(4326)


municipis_litorals_sf <-st_read(pool, query ="SELECT * FROM mun_lit")%>% 
  st_transform(4326)




#----------Funcio per comprobar si la 1era lletra es majuscula------------------

majuscula_n_cientific <- function(nom_cientific) {
  primera_lletra <- substr(nom_cientific, 1, 1)
  primera_lletra_majuscula <- toupper(primera_lletra)
  
  return(primera_lletra == primera_lletra_majuscula)
  
  #si retorna TRUE es majuscula
  
}



###############################################################################
###############################################################################

#PROMACIO FORMULARI

###############################################################################
###############################################################################

ui <- fluidPage(
  
  waiter::use_waiter(), # per fer servir els waiters
  
  shinyjs::useShinyjs(), # per bloquejar els botons
  
  #Head de la pag web
  
  tags$head(
    
    
    #Treu el missatge de l atribucio del mapa a leaflet
    
    tags$style(HTML(".leaflet-control-attribution { display: none; }")),
    
    #Carregem l arxiu css pel document a imprimir
    
    tags$link(rel = "stylesheet", href = "style.css"),
    
    #  Carga la llibreria html2pdf i l escript per crear el pdf i configurar
    
    tags$script(src = "pdfHandler.js")
    
  ), # tancament del Head
  
  #Carregem l estil de la pagina
  
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  
  #bslib::bs_theme_preview(theme), #Tema alternatiu
  
  br(),
  
  h1("Anàlisi Observacións Marines   ",img(src="logo_minka.png", width="120" ,height="40"),),
  
  navlistPanel(
    
    id = "tabset","MENU",well = TRUE,
    
    #===========================TABPANEL 1=====Quadricula 10x10==================
    
    tabPanel("Observacions de l´especie per quadricules marines 10x10","Seguir les indicacions en l ordre de la barra de navegació. Seleciona la quadricula del mapa generat per veure el pop-up de les observacions. Es poden treure capes deseleccionat del control dret superior.",br() ,
             
             
             sidebarLayout(
               
               sidebarPanel(
                 fluidRow(
                   
                   sliderInput(
                     
                     inputId = "rango",
                     
                     label = "1- Selecciona l´ interval d´anys:",
                     
                     min = 2017, # Valor mínimo
                     
                     max = lubridate::year(lubridate::today()), # Valor máximo
                     
                     value = c(2017, lubridate::year(lubridate::today()) ) # Valor inicial (mínimo y máximo)
                     
                   ),
                   br(),
                   
                   br(),
                   
                   textInput(inputId = "especie"," 2- Introdueix nom cientific","Gobius xanthocephalus"),
                   
                   br(),
                   
                   tags$p("3- Pulsa el botó de 'Generar mapa'"), br(),
                   
                   actionButton(  inputId = "mi_boton", label = "Generar mapa"),
                   
                   br(),
                   br(),
                   
                   div(style = "margin-top: 20px;",
                       tags$p("4- Genera el pdf un cop generat plànol")),
                   
                   div(style = "margin-top: 10px;",
                       actionButton("pdfBtn", "Descarregar en PDF")),
                   
                   br()
                   
                 )),
               
               #----Sortida Quadricula 10x10--------------------------------------------------
               
               
               mainPanel(
                 
                 
                 br(),
                 
                 div(id = "contingut_a_imprimir_1",
                     
                     tags$div(
                       class = "titol",textOutput(outputId = "titol_espec_catalunya")),
                     
                     br(),
                     
                     textOutput(outputId = "espec_catalunya"),
                     
                     
                     textOutput(outputId = "espec_quadricula10x10"),
                     
                     br(),
                     
                     tags$div(
                       class = "titol_sec",textOutput(outputId = "titol_quadricula")),
                     
                     tags$div(
                       class = "Planol_1",leafletOutput(outputId = 'map1')),
                     
                     textOutput(outputId = "peu_map1"),
                     
                     br(),
                     
                     tags$div(
                       class = "titol_sec",textOutput(outputId = "titol_graf_mes")),
                     
                     plotOutput(outputId = "grafica_mes"),
                     
                     br(),
                     
                     tags$div(
                       class = "titol_sec",textOutput(outputId = "titol_graf_any")),
                     
                     tags$div(
                       class = "ultima_grafica" ,plotOutput(outputId = "grafica_any")),
                     
                     br(),
                     
                 )#tancament contingut a imprimir1
                 
               ) ) ),
    
    #===========================TABPANEL 2=====Quadricula 1x1====================
    
    tabPanel("Observacions de l´especie per quadricules marines 1x1",
             tags$b("Seleciona la quadricula per veure el pop-up de les observacions. Es poden treure capes deseleccionat del control dret superior") ,
             
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 fluidRow(
                   
                   
                   selectInput(
                     
                     inputId = "quadricula",
                     
                     label = "5- Selecciona quadricula 10x10",
                     
                     choices = Quadricules_10x10_sf$COORD_10K),
                   
                   div(style = "margin-top: 20px;",
                       tags$p("6- Pulsa el botó per generar el plànol de l´UTM")),
                   
                   div(style = "margin-top: 10px;",
                       actionButton(  inputId = "quadricula_button",
                                      label = "Generar mapa")),
                   
                   div(style = "margin-top: 20px;",
                       tags$p("7- Genera el pdf un cop generat plànol")),
                   
                   div(style = "margin-top: 10px;",
                       actionButton("pdfBtn_1x1", "Descarregar en PDF")),
                   
                   br()
                   
                   
                 )),
               
               #----Sortida Quadricula 1x1---------------------------------------------------
               
               mainPanel(
                 
                 div(id = "contingut_a_imprimir_1x1",
                     
                     tags$div(
                       class = "titol",textOutput(outputId = "titol_espec_catalunya_1x1")),
                     
                     br(),
                     
                     
                     br(),
                     
                     textOutput(outputId = "espec_quadricula"),
                     
                     textOutput(outputId = "total_quadr1x1"),
                     
                     br(),
                     
                     tags$div(
                       class = "titol_sec",textOutput(outputId = "titol_quadricula_1x1")),
                     
                     
                     tags$div(
                       class = "Planol_1",leafletOutput(outputId = 'map2')),
                     
                     
                     textOutput(outputId = "peu_map2"),
                     
                     br(),
                     
                     tags$div(
                       class = "titol_sec",textOutput(outputId = "titol_graf_mes_1x1")),
                     
                     plotOutput(outputId = "grafica_mes_1x1"),
                     
                     br(),
                     
                     tags$div(
                       class = "titol_sec",textOutput(outputId = "titol_graf_any_1x1")),
                     
                     plotOutput(outputId = "grafica_any_1x1")
                     
                     
                 )#tancament contingut a imprimir1
               ) #tancament main panel
               
             )),
    
    #===========================TABPANEL 3=====Observac puntuals====================
    
    tabPanel("Observacions de l´especie individuals",
             tags$b( "Seleciona la xinxeta per veure el pop-up amb el detall de l´observació. Es poden treure capes deseleccionat del control dret superior") ,
             
             
             sidebarLayout(
               
               sidebarPanel(
                 fluidRow(
                   
                   p("Detall de les observacions de la quadricula"),
                   
                   div(style = "margin-top: 20px;",
                       tags$p("6- Pulsa el botó per generar el plànol de les obs indiv")),
                   
                   div(style = "margin-top: 10px;",
                       actionButton(  inputId = "quadricula_button_indiv",
                                      label = "Generar mapa"))
                   
                 )),
               
               #----Sortida observac puntuals---------------------------------------------------
               
               
               mainPanel(
                 
               tags$div(
                     class = "Planol_1",leafletOutput(outputId = 'map3')),
               
              div(style="background:white; padding:10px; border:1px solid #ccc; border-radius:6px; max-height:750px; overflow-y:auto;",
                              uiOutput("leyenda_habitats")),
                   
                 
                 br(),
                 
                 h5(tags$b("TAULA OBSERVACIONS INDIVIDUALS")),
                 
                 
                 DTOutput("valors_seleccionats")
                 
               )
               
             )),
    
    #===========================TABPANEL 4=====HeatMap==================
    
    tabPanel("Mapa de densitats especie", tags$b( "Mapa de densitat del kernel de les  observacions de l especie seleccionada en el primer menu"),br() ,
             
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 fluidRow(
                   
                   actionButton(  inputId = "button_heatmap", label = "Crea mapa densitats")
                   
                 )),
               
               #----Sortida Heatmap-----------------------------------------------
               
               mainPanel(
                 
                 br(),
                 
                 textOutput(outputId = "espec_heatmap"),
                 
                 
                 br(),
                 
                 tags$div(
                   class = "Planol_1",leafletOutput(outputId = 'heatmap')),
                 
                 
               ) ) ),
    
    #Tancament tabset
    
  ))


###############################################################################
###############################################################################

#PROGRAMACIO SERVER

###############################################################################
###############################################################################


server <- function(input, output,session) {
  
  #Gestio del waiter
  
  w <- waiter::Waiter$new(
    html = tagList(
      spin_timer(),
      h3("Conectant amb la BBDD. La càrrega pot durar entre 15 i 45 segons depenent la quantitat de dades...", style = "color:white;")
    ),
    color = "rgba(30,40,50,0.9)"
  )
  
  #El primer boto selecciona anys i especie i engloba tota la app-----------------
  rv <- reactiveValues(
   
    quad_10x10 = NULL,     # Especies_Minka_quadricula10x10
    listo_10x10 = FALSE,   #  token
    pal_ = NULL,           # per compartir paletes i mantenir el format
    paleta_muni_ = NULL,    # per compartir paletes i mantenir el format
    especie_actual = NULL,  # per posar en titol nomes si no dona error
    rango_actual = NULL,    # per posar en titol nomes si no dona error
    quad_actual = NULL,     #quadricula actual 10x10 seleccionada
    carto_api_key = "cb1_4c84_1_36f348fbae5415a2dff496ac",
    quad_10_selecc_sf = NULL,            ## capa de la quadricula  10x10 seleccionada
    municipis_litorals_sf_1x1 = NULL,    #capa municipis de la quadricula 10x10 seleccionada
    Quadricules_1x1_en_10x10_sf = NULL,  # capa de la quadricula 1x1 de la 10x10 seleccionada
    seleccio_10x10 = FALSE,  # flag quadricula 10x10 seleccionada
    habitat_sf = NULL,     #capa d habitats
    pal_hab = NULL,       #Paleta de a capa d habitats
    observacio_quadricula_10 = NULL,
    map3_generat = FALSE #per cativar els filtres del DT del map3
  )
  
  # Funció per errors
  
  mostrar_error <- function(mensaje) {
    w$hide()
    shinyjs::enable("mi_boton")
    shinyjs::enable("quadricula_button")
    shinyjs::enable("quadricula_button_indiv")
    shinyjs::enable("button_heatmap")
    
    showModal(modalDialog(
      title = tags$h2("⚠️ ATENCIÓ", style = "color:#d9534f; font-weight:bold; font-size:32px; text-align:center;"),
      tags$div(
        style = "font-size:24px; line-height:1.6; padding:30px; text-align:center; font-weight:500;",
        HTML(mensaje) # <- per a que  <b> y <br> funcionin i donar format al missatge
      ),
      size = "l",
      easyClose = FALSE,
      footer = modalButton("Entesos") # <- sin class
    ))
  }
  
  observeEvent(input$mi_boton, {
    
    #----------comprovem quela primera lletra del nom cientific es majuscula----
    
    if ( majuscula_n_cientific(input$especie) == FALSE) {
      mostrar_error("El nom científic te que començar en majúscula")
      return()
    }
    
    if ( stringr::str_count(input$especie, "\\w+") != 2) {
      mostrar_error("El nom científic te que que tindre genere i especie")
      return()
    }
    
    #FIX WAITER per bloquejar botons mentre esperem
    
    w$show()
    shinyjs::disable("mi_boton")
    shinyjs::disable("quadricula_button")
    shinyjs::disable("quadricula_button_indiv")
    shinyjs::disable("button_heatmap")
    
    # per desbloquejar blotons si falla
    on.exit({
      shinyjs::enable("mi_boton")
      shinyjs::enable("quadricula_button")
      shinyjs::enable("quadricula_button_indiv")
      shinyjs::enable("button_heatmap")
    }, add = TRUE)
    
    #FI gestio del WAITER
    
    rv$listo_10x10 <- FALSE
    rv$especie_actual <- input$especie
    rv$rango <- input$rango
    output$leyenda_habitats <- renderUI({ NULL })
    output$map2 <- renderLeaflet({ NULL })
    output$map3 <- renderLeaflet({ NULL })
    output$valors_seleccionats <- renderDT({ NULL })
    output$total_quadr1x1 <- renderText({ "" })
    output$espec_quadricula <- renderText({ "" })
    output$heatmap <- renderLeaflet({ NULL })
    output$espec_heatmap <- renderText({ "" })
    output$map1 <- renderLeaflet({ NULL })
    output$grafica_mes <- renderPlot({ NULL })
    output$grafica_any <- renderPlot({ NULL })
    output$titol_espec_catalunya <- renderText({ "" })
    output$espec_catalunya <- renderText({ "" })
    output$espec_quadricula10x10 <- renderText({ "" })
    output$titol_quadricula <- renderText({ "" })
    output$peu_map1 <- renderText({ "" })
    output$titol_graf_mes <- renderText({ "" })
    output$titol_graf_any <- renderText({ "" })
    output$titol_espec_catalunya_1x1 <- renderText({ "" })
    output$titol_quadricula_1x1 <- renderText({ "" })
    output$peu_map2 <- renderText({ "" })
    output$titol_graf_mes_1x1 <- renderText({ "" })
    output$grafica_mes_1x1 <- renderPlot({ NULL })
    output$titol_graf_any_1x1 <- renderText({ "" })
    output$grafica_any_1x1 <- renderPlot({ NULL })
    
    
    
 
    #-----------Consulta observacions totals per catalunya-----------------------------
    sql_total <- sqlInterpolate(pool,
                                "SELECT COUNT(*) as total
   FROM obs_cat
   WHERE taxon_name =?esp AND year BETWEEN ?y1 AND ?y2",
                                esp = input$especie, y1 = input$rango[1], y2 = input$rango[2]
    )
    
    
    total_catalunya <- dbGetQuery(pool, sql_total)$total[1] %>% as.numeric()
    
    #---------Consulta observacions per quadricula---------------------------------------
    
    sql_map <- sqlInterpolate(pool,
                              "SELECT u.*, c.n_obs FROM utm10_litoral u
   JOIN (SELECT utm10_code, COUNT(*)::int as n_obs FROM obs_cat
         WHERE taxon_name =?esp AND year BETWEEN ?y1 AND ?y2
         GROUP BY utm10_code) c
   ON c.utm10_code = u.coord_10k",
                              esp = input$especie, y1 = input$rango[1], y2 = input$rango[2]
    )
    Especies_Minka_quadricula10x10 <- st_read(pool, query = sql_map) 
    
    
    if (is.null(Especies_Minka_quadricula10x10) || nrow(Especies_Minka_quadricula10x10) == 0) {
      mostrar_error(paste0("No s'han trobat registres per <b>", rv$especie_actual, "</b> a Minka.<br><br>
  Comprova que el nom estigui ben escrit o prova amb un altre interval d'anys.<br><br>
  Si el nom és correcte, pot ser que no hi hagi observacions de recerca per aquesta espècie."))
      return()
    } else{
      
      Especies_Minka_quadricula10x10 <- Especies_Minka_quadricula10x10 %>% st_transform(4326)
        
      #----------------Total observacions dins les quadricules litorals-----------------------
      
      total_litoral <- Especies_Minka_quadricula10x10 %>% 
        sf::st_drop_geometry() %>% 
        summarise(total = sum(n_obs)) %>% 
        pull(total)
      

      
      output$espec_catalunya <- renderText(paste("El numero d observacions de ",
                                                rv$especie_actual," a tot Catalunya del",rv$rango[1]," al ",rv$rango[2],
                                                " es de : ",total_catalunya))
      

      
      #---------------------Observacionns total a Catalunya dins de les quadricules 10x10-------------------
      
      output$titol_espec_catalunya <- renderText({
        req(rv$listo_10x10)
        paste("ANALISIS OBSERVACIONS ",rv$especie_actual)
      })
      
      output$espec_quadricula10x10 <- renderText({
        req(rv$listo_10x10)
        paste("El número d´observacions de",
              rv$especie_actual,"a Catalunya dins del total de quadricules 10x10 marines del",rv$rango[1]," al ",rv$rango[2],
              " és de : ",total_litoral, " observacions. Per tant hi han ", total_catalunya - total_litoral, " observacions fora les quadricules litorals.")})

      #----------------------Titol mapa quadricula 10x10--------------------------------
      
      output$titol_quadricula <- renderText({
        req(rv$listo_10x10);
        "Mapa distribució observacions a Catalunya UTM 10mx10km"})
      
      
      #-----------Definicio de les paletes-----------------------------------------
      
      
      pal <- colorRampPalette(c("yellow","red"))(4)
      
      paleta_espec <- colorBin(palette = pal , domain = Especies_Minka_quadricula10x10$n_obs, bins = 4)
      
      #4 colors per la batimetria
      
      pal_bat <- c("#CCFBFF","#66BFFF","#3388FF","#0040FF")
      
      # paleta_batimetr <-colorFactor(palette = pal_bat, domain = ((batimetria_sf$PROF)))
      
      #12 colors per les comarques
      
      pal_mun <- rainbow(12)
      
      paleta_muni <- colorFactor(palette = pal_mun, domain = ((municipis_litorals_sf$comarques_)))
      
      
      #-------------Mapa de quadricules 10x10----------------------------------------
      
      
      mymap1 <- reactive({
        
        leaflet() %>%
        
        addTiles(
          urlTemplate = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", rv$carto_api_key),
          attribution = '© CARTO © OpenStreetMap',
          group = "CARTO"
          ) %>%
          
          addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
          
          
          addPolygons(data = municipis_litorals_sf ,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.15,
                      fillColor =  ~paleta_muni(municipis_litorals_sf$comarques_),
                      weight = 1,
                      color = "black",
                      
                      popup =  paste("Municipi: ",municipis_litorals_sf$nommuni,
                                      "<br>",
                                      "Comarca: ",municipis_litorals_sf$comarques_,
                                      "<br>",
                                      "Provincia: ",municipis_litorals_sf$comarque_1),
                      
                      group = "Municipis") %>%
          
          
          addPolygons(data = Quadricules_10x10_sf,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      weight = 1,
                      color = "black",
                      popup =  paste("Codi quadricula: ",Quadricules_10x10_sf$coord_10k),
                      group = "Quadricula 10x10") %>%
          
          addPolygons(data = Especies_Minka_quadricula10x10,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      fillColor = ~paleta_espec(Especies_Minka_quadricula10x10$n_obs),
                      weight = 1,
                      color = "black",
                      group = "Nº d´observacions x quadricula",
                      popup =  paste("Nº d observacions:", as.character(Especies_Minka_quadricula10x10$n_obs),
                                      "<br>",
                                      "Quadricula:",(Especies_Minka_quadricula10x10$coord_10))) %>%
          
          addLayersControl(baseGroups = c("CARTO", "Satel.lit"),
                           overlayGroups = c("Nº d´observacions x quadricula","Municipis","Quadricula 10x10"),
                           options = layersControlOptions(collapsed = TRUE))  %>%
          
          addLegend(
            
            position = "bottomright",
            pal = paleta_espec,
            values = Especies_Minka_quadricula10x10$n_obs,
            title = "Nº Observ.",
            opacity = 0.8,
            
            group = "Nº d´observacions x quadricula"
          )
          

        
      })
      
      output$map1 <- renderLeaflet( mymap1() )
      
      #--------------------TEXT Peu de mapa-------------------------------------------------------
      
      output$peu_map1 <- renderText({ req(rv$listo_10x10)
        paste("Observacions per quadricula UTM 10x10  de ", rv$especie_actual," a tot Catalunya de l´any",rv$rango[1], " al ",  rv$rango[2] )})
      
     
      #--------------------Titol agrupacio per mes 10x10--------------------------------
      
      output$titol_graf_mes <- renderText({req(rv$listo_10x10);
        "Observacions a Catalunya per Mes"})
      
      
      #--------Dades observacions per mes 10x10----------------------------------------------
      
      sql_meses <- sqlInterpolate(pool,
                                  "SELECT month, COUNT(*) as observacions_mes
   FROM obs_cat
   WHERE taxon_name =?esp AND year BETWEEN ?y1 AND ?y2
   GROUP BY month
   ORDER BY month",
                                  esp = input$especie, y1 = input$rango[1], y2 = input$rango[2]
      )
      
      Especies_Minka_mes <- dbGetQuery(pool, sql_meses)
      
      Especies_Minka_mes$mes <- factor(Especies_Minka_mes$month, levels = 1:12, labels = c("Gener", "Febrer", "Març",
                                                                                          "Abril","Maig","Juny","Juliol","Agost","Setembre","Octubre", "Novembre","Decembre"))
      
      #-------------------------------Grafic per mes 10x10 --------------------------------------------------
      
      output$grafica_mes <- renderPlot({
        
        ggplot(Especies_Minka_mes,aes (x=mes, y= observacions_mes))+
          
          geom_col(fill ='#32CD75')+scale_x_discrete(drop=FALSE)+
          
          geom_text(aes(label = observacions_mes), vjust= -0.5) +
          
          geom_smooth(aes(x= as.numeric(month),y=observacions_mes), method = 'loess', formula = y ~x ,se=FALSE, color = 'red', inherit.aes =FALSE) +
          
          labs(x="Mes", y= "Num observ", caption =stringr::str_wrap( paste("Observacions mensuals acumulades per ", rv$especie_actual," dins tot  Catalunya de l´any",rv$rango[1], " al ",  rv$rango[2] )))+
          
          theme_classic()+theme( panel.background = element_rect(fill = '#f5f5f5'), plot.caption = element_text( hjust = 0.5, size =14))
        
      })
      
      
      #---------------------Observacions agrupades per any------------------------------------------------------------
      
      #--------------------Titol agrupacio per any 10x10--------------------------------
      
      output$titol_graf_any <- renderText({req(rv$listo_10x10);
        "Observacions a Catalunya anuals"})
      
      #----------------------Dades agrupacio any 10x10----------------------------------------------------
      
      #----------Consulta n observacions per any----------------------------------------
      
      sql_anios <- sqlInterpolate(pool,
                                  "SELECT year, COUNT(*) as observacions_any
   FROM obs_cat
   WHERE taxon_name =?esp AND year BETWEEN ?y1 AND ?y2
   GROUP BY year
   ORDER BY year",
                                  esp = input$especie, y1 = input$rango[1], y2 = input$rango[2]
      )
      
      Especies_Minka_any <- dbGetQuery(pool, sql_anios)
      
     #----------------------- #grafic agrupacio anual 10x10-------------------------------------------
      
      
      output$grafica_any <- renderPlot({
        
        ggplot(Especies_Minka_any,aes (x=year, y= observacions_any))+
          
          geom_col(fill='#3498db')+ scale_x_continuous(breaks = rv$rango[1]:rv$rango[2],limits = (x = c(rv$rango[1]-1,rv$rango[2]+1)))+
          
          geom_text(aes(label = observacions_any), vjust= -0.5) +
          
          geom_smooth(method = 'loess',formula = y ~ x, se=FALSE, color = 'red') +
          
          labs(x="Any", y= "Num observ",caption = stringr::str_wrap(paste("Observacions anuals ", rv$especie_actual," a tot Catalunya de l´any",rv$rango[1], " al ",  rv$rango[2] ))) +
          
          theme_classic()+theme( panel.background = element_rect(fill = '#f7f9fc'),
                                 
                                 plot.caption = element_text( hjust = 0.5, size =14))
        
      })
      
      
      #-------------------------------------------------------------------------------------------------
      
      #guardem les variables reactives  i posem el token a TRUE
      
     
      rv$quad_10x10 <- Especies_Minka_quadricula10x10
      rv$listo_10x10 <- TRUE
      rv$pal_ <- pal
      rv$paleta_muni_ <- paleta_muni
      rv$seleccio_10x10 <- FALSE # Al canviar d especie cal primer seleccionar quadricla
      updateSelectInput(session, "quadricula", choices = rv$quad_10x10$COORD_10K)
      rv$map3_generat <- FALSE
      rv$quad_actual <- FALSE
      
      #tancament del waiter
      
      session$onFlushed(function() {
        w$hide()
      }, once = TRUE)
      
      
    } #tancament de l else
    
  })# Tancament del button incial de cerca 10 x10
  
  
  #=======================================================================================================
  
  ################Sortida global de les Observacions per quadricula 1x1 ####################################
  
  #========================================================================================================
  
  #----------------------------Actualitzacio de select list de quadricules amb nomesles qudricules amb presencai
  
  observeEvent(rv$quad_10x10, {
    req(rv$quad_10x10)
    updateSelectInput(session, inputId="quadricula", 
                      choices = sort(unique(rv$quad_10x10$coord_10k)))
  })
  
  observeEvent(input$quadricula_button, {
    req(rv$listo_10x10) # si no hay datos de 10x10, no hace nada. Adiós token.
    req(rv$quad_10x10)
    rv$quad_actual <- input$quadricula
    
    
    
    if (rv$listo_10x10 == FALSE ){
      
      mostrar_error("Selecciona primer l especie")
      
      return()
      
    } else {
      
      #FIX WAITER per bloquejar botons mentre esperem
      
      w$show()
      shinyjs::disable("mi_boton")
      shinyjs::disable("quadricula_button")
      shinyjs::disable("quadricula_button_indiv")
      shinyjs::disable("button_heatmap")
      
      # per desbloquejar blotons si falla
      on.exit({
        shinyjs::enable("mi_boton")
        shinyjs::enable("quadricula_button")
        shinyjs::enable("quadricula_button_indiv")
        shinyjs::enable("button_heatmap")
        w$hide()
      }, add = TRUE)
      
      #FI gestio del WAITER
      
      #---------Flag per gestionar la seleccio de la quadricula 10x10
      
      rv$seleccio_10x10 <- TRUE
      #----------------------------------------------------------------------------
      
      #Consultes sql per quadricula 1x1
      
      # Capa base quadricula 1x1 de la UTM 10x10 seleccionada
      
      # Capa base quadricula 1x1 de la UTM 10x10 seleccionada 
      
      quad_10_selecc_sf <- Quadricules_10x10_sf %>% filter(coord_10k == input$quadricula)
      
      sql_q1x1_base <- sqlInterpolate(pool,
                                      "SELECT u.* FROM utm1_litoral u, utm10_litoral d
   WHERE d.coord_10k = ?quad10 
   AND ST_Intersects(ST_Centroid(u.geom), d.geom)",
                              quad10 = input$quadricula)
      
      Quadricules_1x1_en_10x10_sf <- st_read(pool, query = sql_q1x1_base, quiet=TRUE) %>% 
        st_transform(4326)
      
     
      # Capa de la quadricula UTM 10x10
      
      quad_10_selecc_sf <- Quadricules_10x10_sf %>%
        filter(coord_10k == rv$quad_actual)
      
      # Capa de Municipis que toquen aquesta 10x10, també per SQL
      
      municipis_litorals_sf_1x1 <- municipis_litorals_sf %>%
        st_make_valid() %>%
        st_filter(quad_10_selecc_sf, .predicate = st_intersects)
      
      
      # consulta reconta observacions per quadricula  1x1
      
      sql_1x1 <- sqlInterpolate(pool,
                                "SELECT u.geom, u.cod1x1, u.cod10x10, c.n_obs
   FROM utm1_litoral u
   JOIN (
     SELECT utm1_code, COUNT(*)::int as n_obs
     FROM obs_cat
     WHERE taxon_name =?esp
       AND utm10_code =?quad10
       AND year BETWEEN ?y1 AND ?y2
     GROUP BY utm1_code
   ) c ON c.utm1_code = u.cod1x1
   WHERE u.cod10x10 =?quad10",
                                esp = rv$especie_actual,
                                quad10 = input$quadricula,
                                y1 = as.integer(rv$rango[1]),
                                y2 = as.integer(rv$rango[2])
      )
   
      #Passem consulta a capa
      
      tmp_1x1 <- st_read(pool, query = sql_1x1, quiet = TRUE)
      
      
      if (is.null(tmp_1x1) || nrow(tmp_1x1) == 0) {
        
        # No hi ha observacions a 1x1 -> capa buida
        
        Especies_Minka_quadricula1x1 <- st_sf(
          cod1x1 = character(0),
          n_observacions1x1 = numeric(0),
          geom = st_sfc(crs = 4326)
        )
        observ_quadr_1x1 <- 0
        
      } else {
        
        #Si hi han observacions dins de les quadricules 1x1
        
        Especies_Minka_quadricula1x1 <- tmp_1x1 %>%
          mutate(n_observacions1x1 = as.numeric(n_obs)) %>%
          st_transform(4326)
        observ_quadr_1x1 <- sum(Especies_Minka_quadricula1x1$n_observacions1x1, na.rm=TRUE)
      }
      
     
      #Titol especie encapçalament 1x1
      
      output$titol_espec_catalunya_1x1 <-renderText({req(input$quadricula)
        paste("ANALISIS OBSERVACIONS ",rv$especie_actual, "PER UTM ",input$quadricula)})
      
      total_en_quad_10 <- rv$quad_10x10 %>%
        st_drop_geometry() %>%
        filter(coord_10k == rv$quad_actual) %>%
        pull(n_obs) %>%
        as.numeric()
      
      
      #Sortida de text del n d observacion de la quadricula 10x10
      
      output$espec_quadricula <-renderText({req(input$quadricula)
      paste("El numero d observacions de" ,rv$especie_actual,"dins les quadricula 10x10 ", rv$quad_actual," és de : ",total_en_quad_10)})
      
      #--------Text del n d obsercacions 1x1----------------------
      
      output$total_quadr1x1 <-renderText({req(rv$quad_actual)
        
        paste("D´aquestes observacions, dins les quadricules 1x1 n´hi han: ",observ_quadr_1x1," . Per tant les observacions que queden fora de les quadricules 1x1 son: ", total_en_quad_10 - observ_quadr_1x1, "observacions")})
      
      #----------------------Titol mapa quadricula 10x10--------------------------------
      
      output$titol_quadricula_1x1 <- renderText({req(rv$quad_actual)
        paste("Mapa observacions UTM ",rv$quad_actual)})
      
      #------Mapa quadricules 1x1 de la quadricula seleccionada
      
      
      #---------------Paleta per mapes a resolucio 1x1--------------------------------------------
      
      paleta_espec1x1 <- colorBin(palette = rv$pal_ , domain = Especies_Minka_quadricula1x1$n_observacions1x1 , bins = 4)
      
      
      
      output$map2 <- renderLeaflet({
        
        leaflet()%>%
          
          addTiles(
            urlTemplate = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", rv$carto_api_key),
            attribution = '© CARTO © OpenStreetMap',
            group = "CARTO"
          ) %>%
          
          addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
          
          addPolygons(data = quad_10_selecc_sf,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      weight = 1.5,
                      color = "black",
                      popup =  paste ("Codi quadricula: ",quad_10_selecc_sf$coord_10k),
                      group ="Quadricula 10x10") %>%

          addPolygons(data = municipis_litorals_sf_1x1 ,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.25,
                      fillColor =  ~rv$paleta_muni_(municipis_litorals_sf_1x1$comarques_),
                      weight = 1,
                      color = "black",
                      
                      popup =  paste ("Municipi: ",municipis_litorals_sf_1x1$nommuni,
                                      "<br>",
                                      "Comarca: ",municipis_litorals_sf_1x1$comarques_,
                                      "<br>",
                                      "Provincia: ",municipis_litorals_sf_1x1$comarque_1),
                      
                      group = "Municipis") %>%
          
          
          addWMSTiles(
            baseUrl = "https://geoserveis.icgc.cat/servei/catalunya/batimetria/wms?",
            layers = "isobates_clar_2500000,isobates_clar_600000,isobates_clar_300000,isobates_clar_100000,isobates_clar_5000",
            group = "batimetria",
            options = WMSTileOptions(format = "image/png", transparent = TRUE, version = "1.3.0"),
            attribution = "ICGC"
          ) %>%
          
          addPolygons(data =  Quadricules_1x1_en_10x10_sf,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      weight = 1,
                      color = "red",
                      popup =  paste ("Codi quadricula: ", Quadricules_1x1_en_10x10_sf$cod1x1 ),
                      group ="Quadricula 1x1") %>%
          
          addPolygons(data = Especies_Minka_quadricula1x1,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      fillColor = ~paleta_espec1x1(Especies_Minka_quadricula1x1$n_observacions1x1),
                      weight = 1,
                      color = "black",
                      group = "Nº d´observacions x quadricula 1x1",
                      popup =  ~paste ("Nº d observacions:", as.character(Especies_Minka_quadricula1x1$n_observacions1x1),
                                      "<br>",
                                      "Quadricula:",(Especies_Minka_quadricula1x1$cod1x1)))%>%
          
          
          
          
          addLayersControl(baseGroups = c("CARTO", "Satel.lit"),
                           overlayGroups = c("Nº d´observacions x quadricula 1x1","Municipis","Quadricula 1x1","Quadricula 10x10","batimetria"),
                           options = layersControlOptions(collapsed = TRUE))  %>%
          
          addLegend(
            
            position = "bottomright",
            pal = paleta_espec1x1,
            values = Especies_Minka_quadricula1x1$n_observacions1x1,
            title = "Nº Observ.",
            opacity = 0.8,
            
            group= "Nº d´observacions x quadricula"
          )
        
        
      })
      
      
      #--------------------TEXT Peu de mapa-------------------------------------------------------
      
      output$peu_map2 <- renderText(paste("Observacions dins la quadricula UTM 10x10 ",input$quadricula ," de ", rv$especie_actual,"  de l´any",rv$rango[1], " al ",  rv$rango[2] ))
      
      #---------------------------------------------------------------------------------------------------------
      
      #-------------Analisis anual der quadicula 1x1-----------------------------------------------------------
  
      #----------------------Titol grafic anual 1x1--------------------------------
      
      output$titol_graf_any_1x1 <- renderText({req(rv$quad_actual)
        paste("Observacions UTM ",rv$quad_actual," totals anual")})
      
    #-------------------------Dades anuals 1x1----------------------------------------
    
      sql_anios_1x1 <- sqlInterpolate(pool,
                                      "SELECT year, COUNT(*) as observacions_any
   FROM obs_cat
   WHERE taxon_name =?esp
   AND year BETWEEN ?y1 AND ?y2
   AND utm10_code =?quad
   GROUP BY year
   ORDER BY year",
                                      esp = input$especie,
                                      y1 = input$rango[1],
                                      y2 = input$rango[2],
                                      quad = rv$quad_actual
      )
      
      
      Especies_Minka_any_1x1 <- dbGetQuery(pool, sql_anios_1x1)
      
 
     # ----------------------#grafic anual----------------------------------------------------------------
      
      output$grafica_any_1x1 <- renderPlot({
        
        ggplot(Especies_Minka_any_1x1,aes(x = year, y = observacions_any)) +
          
          geom_col(fill = '#3498db') + scale_x_continuous(breaks = rv$rango[1]:rv$rango[2],limits = (x = c(rv$rango[1] - 1,rv$rango[2]+1)))+
          
          geom_text(aes(label = observacions_any), vjust= -0.5) +
          
          geom_smooth(method = 'loess',formula = y ~ x, se = FALSE, color = 'red') +
          
          labs(x = "Any", y = "Num observ",caption = stringr::str_wrap(paste("Observacions anuals ", rv$especie_actual," a tot Catalunya de l´any",rv$rango[1], " al ",  rv$rango[2] ))) +
          
          theme_classic() + theme( panel.background = element_rect(fill = '#f7f9fc'),
                                 
                                 plot.caption = element_text( hjust = 0.5, size =14))
        
      })
      #----------------------Variables globals a compartir en el següent apartat-----------
      
      rv$quad_10_selecc_sf = quad_10_selecc_sf
      rv$municipis_litorals_sf_1x1 = municipis_litorals_sf_1x1
      rv$Quadricules_1x1_en_10x10_sf= Quadricules_1x1_en_10x10_sf 
      
      #---------------------------------------------------------------------------------------------------------
      
      #----------------------Titol grafic mensuals 1x1--------------------------------
      
      output$titol_graf_mes_1x1 <- renderText({req(rv$quad_actual)
        paste("Observacions UTM ",rv$quad_actual," totals mensuals")})
      
      #---------------------Dades mensuasl 1x1------------------------------------------------------------
      
      sql_meses <- sqlInterpolate(pool,
                                  "SELECT month, COUNT(*) as observacions_mes
   FROM obs_cat
   WHERE taxon_name =?esp
   AND year BETWEEN ?y1 AND ?y2
   AND utm10_code =?quad
   GROUP BY month
   ORDER BY month",
                                  esp = input$especie,
                                  y1 = input$rango[1],
                                  y2 = input$rango[2],
                                  quad = rv$quad_actual
      )
      
      Especies_Minka_mes_1x1 <- dbGetQuery(pool, sql_meses)
      
      Especies_Minka_mes_1x1$mes <- factor(Especies_Minka_mes_1x1$month, levels = 1:12, labels = c("Gener","Febrer","Març","Abril","Maig","Juny","Juliol","Agost","Setembre","Octubre","Novembre","Decembre"))
      
      
      
      #----------------------Grafic mensuals 1x1---------------------------------------------------------
    
      # 
      output$grafica_mes_1x1 <- renderPlot({

        ggplot(Especies_Minka_mes_1x1,aes(x = mes, y = observacions_mes)) +

          geom_col(fill = '#32CD75') + scale_x_discrete(drop = FALSE) +

          geom_text(aes(label = observacions_mes), vjust= -0.5) +

          geom_smooth(aes(x = as.numeric(month),y=observacions_mes), method = 'loess', formula = y ~x, se=FALSE, color = 'red', inherit.aes = FALSE) +

          labs(x = "Mes", y = "Num observ",caption = stringr::str_wrap(paste("Observacions mensuals acumulades per ", rv$especie_actual," dins la quadricula ", quad_10_selecc_sf, "de l´any",rv$rango[1], " al ",  rv$rango[2] )))+

          theme_classic() + theme( panel.background = element_rect(fill = '#f5f5f5'),

                                 plot.caption = element_text( hjust = 0.5, size =14))

      })
    #-----------------------------------------------------------------------------
      
      #Poso a NULL els valors per contruir el map3 del seguent apartat
      

      output$leyenda_habitats <- renderUI({ NULL})
      output$map3 <- renderLeaflet({ NULL })
      output$valors_seleccionats <- DT::renderDataTable({ NULL})
      rv$map3_generat <- FALSE
      # #---------------------------------------------------------------------------------------------------------
      
    }  # tancament de l else
    
  }) #tancament del button 1x1
  
  #=======================================================================================================
  
  ################Sortida individual ( xinxetes) de les Observacions per quadricula 1x1 ####################################
  
  #========================================================================================================
  
  observeEvent(input$quadricula_button_indiv, {
    req(rv$listo_10x10) # si no hay datos de 10x10, no hace nada. Adiós token.
    req(rv$quad_10x10)
    
    if (rv$listo_10x10 == FALSE ){
      
      mostrar_error("Selecciona primer l especie")
      
      return()
    
    } else if (rv$seleccio_10x10 == FALSE){
      
      mostrar_error("Avans tens que seleccionar la quadricula UTM 10x10 i generar el mapa de quadricules UTM 1x1")
      
      return()
      
    } else {
      
      #FIX WAITER per bloquejar botons mentre esperem
      
      w$show()
      shinyjs::disable("mi_boton")
      shinyjs::disable("quadricula_button")
      shinyjs::disable("quadricula_button_indiv")
      shinyjs::disable("button_heatmap")
      
      # per desbloquejar blotons si falla
      on.exit({
        shinyjs::enable("mi_boton")
        shinyjs::enable("quadricula_button")
        shinyjs::enable("quadricula_button_indiv")
        shinyjs::enable("button_heatmap")
        w$hide()
      }, add = TRUE)
      
      #FI gestio del WAITER
      
  #-----------Obtencio capa habitat de la Gene---------------------------------------
      
      #La capa esta a postgres. ens conectem per obtenirla

      sql_hab <- pool::sqlInterpolate(pool,
              "SELECT * FROM public.habitats_x_utm10_litoral_diss WHERE coord_10k = ?quad",
              quad = rv$quad_actual)
      
      habitat_sf <- sf::st_read(pool, query = sql_hab, quiet = TRUE)


      if(is.null(habitat_sf) || nrow(habitat_sf) == 0){
        habitat_sf <- st_sf(cat_lleg=character(0), geometry=st_sfc(crs=4326))
        pal_hab <- colorFactor("Paired", domain = character(0))
        
      } else {
        
        habitat_sf <- habitat_sf %>% st_make_valid() %>% st_transform(4326)
        pal_hab <- colorFactor("Paired", domain = habitat_sf$cat_lleg)
      }

      #guardem en variable globals la capa ambiental  y la paleta
      
      rv$habitat_sf <- habitat_sf
      rv$pal_hab <- pal_hab
      
      
   #--------Obtencio dades per individu--------------------------------------------------------------------
      
      sql_punts <- sqlInterpolate(pool,
                                  "SELECT o.id_minka, o.observed_on, o.user_login, o.uri, o.url_picture,
                              
                                  ST_Y(ST_Transform(o.geom,4326)) AS latitude,
                                  ST_X(ST_Transform(o.geom,4326)) AS longitude,
                                  h.cat_lleg
                                  FROM obs_cat o
                                  LEFT JOIN habitats h ON o.habitat_id = h.id
                                  WHERE o.utm10_code = ?quad
                                  AND o.taxon_name = ?esp
                                  AND o.year BETWEEN ?y1 AND ?y2",
                                  quad = rv$quad_actual,
                                  esp = rv$especie_actual,
                                  y1 = as.integer(rv$rango[1]),
                                  y2 = as.integer(rv$rango[2]))
                                 
      
      observacio_quadricula_10 <- dbGetQuery(pool, sql_punts)
      
      observacio_quadricula_10 <- observacio_quadricula_10 %>%
        filter(latitude!= 0, longitude!= 0,!is.na(latitude))
      
      
   # <--- guarda-ho al rv
      
 #----------------Tractament link imatges----------------------------------------
      
      observacio_quadricula_10$url_picture <- gsub(
        "minka-sdg.org",
        "observe.minka-sdg.org",
        observacio_quadricula_10$url_picture,
        fixed = TRUE
      )
      
      rv$observacio_quadricula_10 <- observacio_quadricula_10
      
  #----------Mapa de punts d observacio amb capa d habitat-----------------------    
      
      output$map3 <- renderLeaflet({
        
         
          
          m <- leaflet() %>%
          
          addTiles(
            urlTemplate = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", rv$carto_api_key),
            attribution = '© CARTO © OpenStreetMap',
            group = "CARTO") %>%
          
          addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
         
                 
          addPolygons(data= rv$quad_10_selecc_sf, 
                      fillOpacity=0, 
                      weight=2, 
                      color="black",
                      group ="Quadricula 10x10") %>%
          
          addPolygons(data =  rv$Quadricules_1x1_en_10x10_sf,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      weight = 1,
                      color = "red",
                      popup =  paste ("Codi quadricula: ", rv$Quadricules_1x1_en_10x10_sf$cod1x1 ),
                      group ="Quadricula 1x1") %>%
          
          addPolygons(data = rv$municipis_litorals_sf_1x1 ,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.25,
                      fillColor =  ~rv$paleta_muni_(rv$municipis_litorals_sf_1x1$comarques_),
                      weight = 1,
                      color = "black",
                      popup =  paste ("Municipi: ",rv$municipis_litorals_sf_1x13$nommuni,
                                      "<br>",
                                      "Comarca: ",rv$municipis_litorals_sf_1x1$comarques_,
                                      "<br>",
                                      "Provincia: ",rv$municipis_litorals_sf_1x1$comarque_1),
                      group = "Municipis") %>%
          
          addWMSTiles(
            baseUrl = "https://geoserveis.icgc.cat/servei/catalunya/batimetria/wms?",
            layers = "isobates_clar_2500000,isobates_clar_600000,isobates_clar_300000,isobates_clar_100000,isobates_clar_5000",
            group = "batimetria",
            options = WMSTileOptions(format = "image/png", transparent = TRUE, version = "1.3.0"),
            attribution = "ICGC") %>%
            
            addMarkers(data = rv$observacio_quadricula_10,
                     lng = rv$observacio_quadricula_10$longitude, 
                     lat =  rv$observacio_quadricula_10$latitude,
                     clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE),
                     popup = ~paste0("Link observació: ",'<a href= "',rv$observacio_quadricula_10$uri,'" target="_blank">', 'ID ',observacio_quadricula_10$id,'</a>',
                                     '<br>',"Observador/a: ",rv$observacio_quadricula_10$user_login,
                                     '<br>',"Data: ",rv$observacio_quadricula_10$observed_on,
                                     '<br>'),
                     options = markerOptions(draggable = FALSE),
                     group = "observacions quadricula 1x1")
            
            # Només afegeix hàbitats si n'hi ha
            if(!is.null(habitat_sf) && nrow(habitat_sf) > 0){
              m <- m %>%
                addPolygons(data= habitat_sf,
                            fillColor = ~pal_hab(cat_lleg), 
                            fillOpacity=0.7, 
                            weight=0.5,
                            popup = ~paste0(cat_lleg), 
                            group="Habitats")}
          m %>%       
            
            addPopupImages( observacio_quadricula_10$url_picture,
                            group = "observacions quadricula 1x1", width = 150) %>%
        
            addLayersControl(baseGroups = c("CARTO", "Satel.lit"),
                         overlayGroups = c("observacions quadricula 1x1","Municipis","Habitats","Quadricula 1x1","Quadricula 10x10","batimetria"),
                         options = layersControlOptions(collapsed = TRUE))
        
        
      }) #tancament map3
          
  #------------------Llegenda d habitats fora del leaflet---------------------
      
      output$leyenda_habitats <- renderUI({
        req(rv$habitat_sf)
        if(nrow(rv$habitat_sf)==0) return(NULL)
        
        cats <- sort(unique(rv$habitat_sf$cat_lleg))
        cols <- rv$pal_hab(cats)
        
        div(style="background:white; padding:8px 10px; border:1px solid #ddd; border-top:0; border-radius:0 0 6px 6px;",
            div(style="font-size:11px; font-weight:bold; margin-bottom:6px;", paste0("Hàbitats UTM ", rv$quad_actual)),
            div(style="display:flex; flex-wrap:wrap; gap:6px 16px;",
                lapply(seq_along(cats), function(i){
                  div(style="display:flex; align-items:center;",
                      div(style=paste0("width:11px; height:11px; background:", cols[i], "; margin-right:5px; border:1px solid #999; flex-shrink:0;")),
                      span(style="font-size:11px; line-height:11px;", cats[i])
                  )
                })
            )
        )
      })
      
      
#-----DataTble d observacions puntuals---------------------------------------      
      
      #-----DataTable d'observacions puntuals---------------------------------------
      req(nrow(observacio_quadricula_10)> 0)
      
      # Assegura't noms reals
 
      df_plot <- data.frame(
        ID = paste0('<a href="', rv$observacio_quadricula_10$uri, '" target="_blank">', rv$observacio_quadricula_10$id_minka, '</a>'),
        Imatge = ifelse(is.na(rv$observacio_quadricula_10$url_picture), "",
                        paste0("<img src='",rv$observacio_quadricula_10$url_picture, "' width='75' height='75' style='object-fit:cover; border-radius:4px;'>")),
        Data = rv$observacio_quadricula_10$observed_on,
        Observador = rv$observacio_quadricula_10$user_login,
        Codi_CORINE =rv$observacio_quadricula_10$cat_lleg,
        stringsAsFactors = FALSE
      )


      output$valors_seleccionats <- DT::renderDataTable({
        DT::datatable(df_plot, escape = FALSE, filter='top',
                      options=list(pageLength=25), rownames=FALSE)
      })
  #   Per activar el mapa amb filtre de datatable
      
      rv$map3_generat <- TRUE
      
      
    }  # tancament de l else

  }) #tancament del button individula

  #================================================================================================
  
  #--------------------Execucio map3 observ puntuals amb filtre al DT-------------------------------------------------------
  
  #===================================================================================================
  
  observeEvent(input$valors_seleccionats_rows_all, {
    req(rv$map3_generat == TRUE)
    req(rv$observacio_quadricula_10)
    
    idx <- input$valors_seleccionats_rows_all
    if(is.null(idx)) return()
    
    dades_filtrades <- rv$observacio_quadricula_10[idx, , drop=FALSE]
    
    leafletProxy("map3") %>%
      clearMarkers() %>%
      clearGroup("observacions quadricula 1x1") %>%
      addMarkers(data = dades_filtrades,
                 lng = ~longitude, lat = ~latitude,
                 clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE),
                 group = "observacions quadricula 1x1") %>%
      addPopupImages(dades_filtrades$url_picture, group="observacions quadricula 1x1", width=150)
  })
  
  
  #================================================================================================
  
  #--------------------Execucio Heatmap-------------------------------------------------------
  
  #===================================================================================================
  
  
  observeEvent(input$button_heatmap, {
    req(rv$listo_10x10) 
    req(rv$quad_10x10)
    rv$quad_actual <- input$quadricula
    
  
    
    if (rv$listo_10x10 == FALSE ){
      
      mostrar_error("Selecciona primer l especie")
      
      return()
      
    } else {
      
      #FIX WAITER per bloquejar botons mentre esperem
      
      w$show()
      shinyjs::disable("mi_boton")
      shinyjs::disable("quadricula_button")
      shinyjs::disable("quadricula_button_indiv")
      shinyjs::disable("button_heatmap")
      
      # per desbloquejar blotons si falla
      on.exit({
        shinyjs::enable("mi_boton")
        shinyjs::enable("quadricula_button")
        shinyjs::enable("quadricula_button_indiv")
        shinyjs::enable("button_heatmap")
        w$hide()
      }, add = TRUE)
      
      #FI gestio del WAITER
      
      #--------Obtencio dades per tot catalunya per el heatmap--------------------------------------------------------------------
      
      sql_kde <- sqlInterpolate(pool,
                                "SELECT
     ST_X(ST_Transform(geom,4326)) as longitude,
     ST_Y(ST_Transform(geom,4326)) as latitude
   FROM obs_cat
   WHERE taxon_name =?esp
   AND year BETWEEN ?y1 AND ?y2
   AND geom IS NOT NULL
   AND ST_X(ST_Transform(geom,4326))!= 0
   AND ST_Y(ST_Transform(geom,4326))!= 0",
                                esp = rv$especie_actual,
                                y1 = as.integer(rv$rango[1]),
                                y2 = as.integer(rv$rango[2])
      )
      
      Observacions_especie_NA10x10 <- dbGetQuery(pool, sql_kde)
      
      # ara si
      dat <- as.data.table(Observacions_especie_NA10x10)
      

      
      #Cal treballar amb matrius i cal que els noms de la latitut i long estiguin definits
      
      colnames(dat)<-c("longitude","latitude")
      
      kde <- bkde2D (dat[ , list(longitude, latitude)],
                     bandwidth=c(.0045, .0068), gridsize = c(1000,1000))
      
      #En la funcio quan mes gran es la grid size mes definit esta
      
      #Converitm a raster la funcio de densitat del kernel delspunts
      
      KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))
      
      #Paleta del heatmap
      
      palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)
      
      #Carreguem els valosr petits com NA per despres fer-los transparents
      
      KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA
      
      palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")
      
      ##Text heatmap
      
      output$espec_heatmap <-renderText(paste("El mapa de desitat del kernel de observacions de",
                                              input$especie,"a Catalunya del",input$rango[1]," al ",input$rango[2]))
      
      
      
      ## Heatmap amb leaflet
      
      output$heatmap <- renderLeaflet({
        
        leaflet() %>%
          
          addTiles(
            urlTemplate = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", rv$carto_api_key),
            attribution = '© CARTO © OpenStreetMap') %>%

          addRasterImage(KernelDensityRaster,
                         colors = palRaster,
                         opacity = .8) %>%
          
          addLegend(pal = palRaster,
                    values = KernelDensityRaster@data@values,
                    title = "Dens. Kernel Obs",
                    bins = 7,
                    position = "bottomleft")
      })
     
      
    }  # tancament de l else del waiter
    
  }) #tancament del button heatmap
  
  
  }  # Tancament del server


shinyApp(ui = ui, server = server)

