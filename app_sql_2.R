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

library(future)

library(furrr)

library(pool)

future::plan(multisession, workers = 6)

options(shiny.maxRequestSize = 1000*1024^2)

#Creo la conexio pero atraves de pool perque nomes es vaci servir quan ho demani la funcio

pool <- pool::dbPool(RPostgres::Postgres(),
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

View(municipis_litorals_sf)

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
                   
                   p("Detall de les observacions de la quadricula")
                   
                 )),
               
               #----Sortida observac puntuals---------------------------------------------------
               
               
               mainPanel(
                 
                 tags$div(
                   class = "Planol_1",leafletOutput(outputId = 'map3')),
                 
                 
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
      h3("Conectant amb Minka. La càrrega pot durar entre 1 i 5 minuts segons quantitat de dades...", style="color:white;")
    ),
    color = "rgba(30,40,50,0.9)"
  )
  
  #El primer boto selecciona anys i especie i engloba tota la app-----------------
  rv <- reactiveValues(
    capa_especie = NULL,
    obs_10x10 = NULL,      # Observacions_especie_NA10x10
    quad_10x10 = NULL,     # Especies_Minka_quadricula10x10
    listo_10x10 = FALSE,   #  token
    pal_ = NULL,           # per compartir paletes i mantenir el format
    paleta_muni_ =NULL,    # per compartir paletes i mantenir el format
    especie_actual = NULL,  # per posar en titol nomes si no dona error
    rango_actual = NULL,    # per posar en titol nomes si no dona error
    quad_actual = NULL
  )
  
  # Funció per errors
  
  mostrar_error <- function(mensaje) {
    w$hide()
    shinyjs::enable("mi_boton")
    shinyjs::enable("quadricula_button")
    shinyjs::enable("button_heatmap")
    
    showModal(modalDialog(
      title = tags$h2("⚠️ ATENCIÓ", style="color:#d9534f; font-weight:bold; font-size:32px; text-align:center;"),
      tags$div(
        style="font-size:24px; line-height:1.6; padding:30px; text-align:center; font-weight:500;",
        HTML(mensaje) # <- per a que  <b> y <br> funcionin i donar format al missatge
      ),
      size = "l",
      easyClose = FALSE,
      footer = modalButton("Entesos") # <- sin class
    ))
  }
  
  observeEvent(input$mi_boton, {
    
    #----------comprovem quela primera lletra del nom cientific es majuscula----
    
    if ( majuscula_n_cientific(input$especie)==FALSE){
      mostrar_error("El nom científic te que començar en majúscula")
      return()
    }
    
    if(stringr::str_count(input$especie, "\\w+")!=2) {
      mostrar_error("El nom científic te que que tindre genere i especie")
      return()
    }
    
    #FIX WAITER per bloquejar botons mentre esperem
    
    w$show()
    shinyjs::disable("mi_boton")
    shinyjs::disable("quadricula_button")
    shinyjs::disable("button_heatmap")
    
    # per desbloquejar blotons si falla
    on.exit({
      shinyjs::enable("mi_boton")
      shinyjs::enable("quadricula_button")
      shinyjs::enable("button_heatmap")
    }, add = TRUE)
    
    #FI gestio del WAITER
    
    rv$listo_10x10 <- FALSE
    rv$especie_actual <- input$especie
    rv$rango <- input$rango
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
    
    
    # #---------------------- Ens conectem aMinka i generem el df d observacions------
    # #Obtenim les observacions de l especie amb la funcio creada al principi de l app
    
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
    
    
    #--------onsulta n observacions per mes----------------------------------------------
    
    sql_meses <- sqlInterpolate(pool,
                                "SELECT month, COUNT(*) as observacions_mes
   FROM obs_cat
   WHERE taxon_name =?esp AND year BETWEEN ?y1 AND ?y2
   GROUP BY month
   ORDER BY month",
                                esp = input$especie, y1 = input$rango[1], y2 = input$rango[2]
    )
    
    Especies_Minka_mes <- dbGetQuery(pool, sql_meses)
    
    Especies_Minka_mes$mes <- factor(Especies_Minka_mes$month, levels = 1:12, labels =c("Gener", "Febrer", "Març",
                                                                    "Abril","Maig","Juny","Juliol","Agost","Setembre","Octubre", "Novembre","Decembre"))
    
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
    
    
    if (is.null(Especies_Minka_quadricula10x10) || nrow(Especies_Minka_quadricula10x10) == 0){
      mostrar_error(paste0("No s'han trobat registres per <b>", rv$especie_actual, "</b> a Minka.<br><br>
  Comprova que el nom estigui ben escrit o prova amb un altre interval d'anys.<br><br>
  Si el nom és correcte, pot ser que no hi hagi observacions de recerca per aquesta espècie."))
      return()
    } else{
      
      #----------------Total observacions dins les quadricules litorals-----------------------
      
      Especies_Minka_quadricula10x10<- Especies_Minka_quadricula10x10 %>% st_transform(4326)
      
      total_litoral <- Especies_Minka_quadricula10x10 %>% 
        sf::st_drop_geometry() %>% 
        summarise(total = sum(n_obs)) %>% 
        pull(total)
      

      
      #-----------------adjuntem el mes com a factor al dataframe obtingut-----------------------------------------
   
      
      
      output$espec_catalunya<- renderText(paste("El numero d observacions de ",
                                                rv$especie_actual," a tot Catalunya del",rv$rango[1]," al ",rv$rango[2],
                                                " es de : ",total_catalunya))
      
  

      
      
      #---------------------Observacionns total a Catalunya dins de les quadricules 10x10-------------------
      
      output$titol_espec_catalunya <-renderText({
        req(rv$listo_10x10)
        paste("ANALISIS OBSERVACIONS ",rv$especie_actual)
      })
      
      output$espec_quadricula10x10 <-renderText({
        req(rv$listo_10x10)
        paste("El número d´observacions de",
              rv$especie_actual,"a Catalunya dins del total de quadricules 10x10 marines del",rv$rango_actual[1]," al ",rv$rango_actual[2],
              " és de : ",total_litoral, " observacions. Per tant hi han ", total_catalunya - total_litoral, " observacions fora les quadricules litorals.")})

      #----------------------Titol mapa quadricula 10x10--------------------------------
      
      output$titol_quadricula <- renderText({
        req(rv$listo_10x10);
        "Mapa distribució observacions a Catalunya UTM 10mx10km"})
      
      
      #-----------Definicio de les paletes-----------------------------------------
      
      
      pal <- colorRampPalette(c("yellow","red"))(4)
      
      paleta_espec <- colorBin(palette = pal , domain =Especies_Minka_quadricula10x10$n_obs, bins = 4)
      
      #4 colors per la batimetria
      
      pal_bat <-c("#CCFBFF","#66BFFF","#3388FF","#0040FF")
      
      # paleta_batimetr <-colorFactor(palette = pal_bat, domain = ((batimetria_sf$PROF)))
      
      #12 colors per les comarques
      
      pal_mun <- rainbow (12)
      
      paleta_muni <-colorFactor(palette = pal_mun, domain = ((municipis_litorals_sf$comarques_)))
      
      
      #-------------Mapa de quadricules 10x10----------------------------------------
      
      
      mymap1 <- reactive({
        
        leaflet()%>%
          
          addProviderTiles(providers$Esri.WorldGrayCanvas, group = "CARTO",
                           options = providerTileOptions(crossOrigin = TRUE)) %>%
          addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
          
          
          addPolygons(data = municipis_litorals_sf ,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.15,
                      fillColor =  ~paleta_muni(municipis_litorals_sf$comarques_),
                      weight = 1,
                      color = "black",
                      
                      popup =  paste ("Municipi: ",municipis_litorals_sf$nommuni,
                                      "<br>",
                                      "Comarca: ",municipis_litorals_sf$comarques_,
                                      "<br>",
                                      "Provincia: ",municipis_litorals_sf$comarque_1),
                      
                      group = "Municipis")%>%
          
          # 
          # addPolylines(data =batimetria_sf,
          #              stroke = TRUE,
          #              smoothFactor = 0.2,
          #              fillOpacity = 0,
          #              weight = 1,
          #              color = ~paleta_batimetr(batimetria_sf$PROF),
          #              popup =  paste ("Isobata: ",batimetria_sf$PROF),
          #              group ="Batimetria") %>%
          
          addPolygons(data =Quadricules_10x10_sf,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      weight = 1,
                      color = "black",
                      popup =  paste ("Codi quadricula: ",Quadricules_10x10_sf$coord_10k),
                      group ="Quadricula 10x10") %>%
          
          addPolygons(data = Especies_Minka_quadricula10x10,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      fillColor = ~paleta_espec(Especies_Minka_quadricula10x10$n_obs),
                      weight = 1,
                      color = "black",
                      group = "Nº d´observacions x quadricula",
                      popup =  paste ("Nº d observacions:", as.character(Especies_Minka_quadricula10x10$n_obs),
                                      "<br>",
                                      "Quadricula:",(Especies_Minka_quadricula10x10$coord_10)))%>%
          
          addLayersControl(baseGroups = c("CARTO", "Satel.lit"),
                           overlayGroups = c("Nº d´observacions x quadricula","Municipis","Quadricula 10x10"),
                           options = layersControlOptions(collapsed = TRUE))  %>%
          
          addLegend(
            
            position = "bottomright",
            pal = paleta_espec,
            values = Especies_Minka_quadricula10x10$n_obs,
            title = "Nº Observ.",
            opacity = 0.8,
            
            group= "Nº d´observacions x quadricula"
          )
          

        
      })
      
      output$map1 <- renderLeaflet( mymap1() )
      
      #--------------------TEXT Peu de mapa-------------------------------------------------------
      
      output$peu_map1 <- renderText({ req(rv$listo_10x10)
        paste("Observacions per quadricula UTM 10x10  de ", rv$especie_actual," a tot Catalunya de l´any",rv$rango[1], " al ",  rv$rango[2] )})
      
      #---------------------Observacions agrupades per mes------------------------------------------------------------
      
      
      #--------------------Titol agrupacio per mes--------------------------------
      
      output$titol_graf_mes <- renderText({req(rv$listo_10x10);
        "Observacions a Catalunya per Mes"})
      
      #--------------------------------------------------------------------------
      
      
      #grafic mensual
      
      #-------------------------------Grafic per mes--------------------------------------------------
      
      output$grafica_mes <- renderPlot({
        
        ggplot(Especies_Minka_mes,aes (x=mes, y= observacions_mes))+
          
          geom_col(fill ='#32CD75')+scale_x_discrete(drop=FALSE)+
          
          geom_text(aes(label = observacions_mes), vjust= -0.5) +
          
          geom_smooth(aes(x= as.numeric(month),y=observacions_mes), method = 'loess', formula = y ~x ,se=FALSE, color = 'red', inherit.aes =FALSE) +
          
          labs(x="Mes", y= "Num observ", caption =stringr::str_wrap( paste("Observacions mensuals acumulades per ", rv$especie_actual," dins tot  Catalunya de l´any",rv$rango[1], " al ",  rv$rango[2] )))+
          
          theme_classic()+theme( panel.background = element_rect(fill = '#f5f5f5'), plot.caption = element_text( hjust = 0.5, size =14))
        
      })
      
      
      #---------------------Observacions agrupades per any------------------------------------------------------------
      
      #--------------------Titol agrupacio per any--------------------------------
      
      output$titol_graf_any <- renderText({req(rv$listo_10x10);
        "Observacions a Catalunya anuals"})
      
      #--------------------------------------------------------------------------
      
      #grafic anual
      
      
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
      
      updateSelectInput(session, "quadricula", choices = rv$quad_10x10$COORD_10K)
      
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
    req(rv$obs_10x10)
    rv$quad_actual <- input$quadricula
    
    
    
    if (rv$listo_10x10 == FALSE ){
      
      mostrar_error("Selecciona primer l especie")
      
      return()
      
    } else {
      
      #FIX WAITER per bloquejar botons mentre esperem
      
      w$show()
      shinyjs::disable("mi_boton")
      shinyjs::disable("quadricula_button")
      shinyjs::disable("button_heatmap")
      
      # per desbloquejar blotons si falla
      on.exit({
        shinyjs::enable("mi_boton")
        shinyjs::enable("quadricula_button")
        shinyjs::enable("button_heatmap")
      }, add = TRUE)
      
      #FI gestio del WAITER
      
      #----------------------------------------------------------------------------
      
      #Consultes sql per quadricula 1x1
      
      # Dins de observeEvent(input$quadricula_button
      
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
                                y1 = as.integer(rv$rango_actual[1]),
                                y2 = as.integer(rv$rango_actual[2])
      )
      
      Especies_Minka_quadricula1x1 <- st_read(pool, query = sql_1x1) %>%
        mutate(n_observacions1x1 = as.numeric(n_obs)) %>%
        st_transform(4326)
      
      View(Especies_Minka_quadricula1x1)
      
      # Total 1x1
      observ_quadr_1x1 <- sum(Especies_Minka_quadricula1x1$n_observacions1x1, na.rm = TRUE)
      
      # Malla base 1x1 buida per dibuixar la graella vermella
      sql_q1x1_base <- sqlInterpolate(pool,
                                      "SELECT * FROM utm1_litoral WHERE cod10x10 =?quad10",
                                      quad10 = input$quadricula
      )
      Quadricules_1x1_en_10x10_sf <- st_read(pool, query = sql_q1x1_base) %>% st_transform(4326)
      
      # Municipis que toquen aquesta 10x10, també per SQL
      sql_muni <- sqlInterpolate(pool,
                                 "SELECT m.* FROM mun_lit m, utm10_litoral u
   WHERE u.coord_10k =?quad10 AND ST_Intersects(m.geom, u.geom)",
                                 quad10 = input$quadricula
      )
      municipis_litorals_sf_1x1 <- st_read(pool, query = sql_muni) %>% st_transform(4326) %>% st_make_valid()              
      
                                            

            #Titol especie encapçalament 1x1

            output$titol_espec_catalunya_1x1 <-renderText({req(input$quadricula)
              paste("ANALISIS OBSERVACIONS ",rv$especie_actual, "PER UTM ",input$quadricula)})

            #Sortida de text del n d observacion de la quadricula 10x10

            output$espec_quadricula <-renderText({req(input$quadricula)
              paste("El numero d observacions de" ,rv$especie_actual,"dins les quadricula 10x10 ", input$quadricula," és de : ",nrow(observacio_quadricula_10))})


            #---------------------------------------------------------------------------------------------------------

            #--------------------Tractament per capes a 1x1-------------------------------------


           

            #-------------------Observacions en quadricules 1x1 dins de la quadricula 10x10 objectiu





            #--------Text del n d obsercacions 1x1----------------------

            output$total_quadr1x1 <-renderText({req(rv$quad_actual)

              paste("D´aquestes observacions, dins les quadricules 1x1 n´hi han: ",observ_quadr_1x1," . Per tant les observacions que queden fora de les quadricules 1x1 son: ", nrow(observacio_quadricula_10)-observ_quadr_1x1, "observacions")})

            #----------------------Titol mapa quadricula 10x10--------------------------------

            output$titol_quadricula_1x1 <- renderText({req(rv$quad_actual)
              paste("Mapa observacions UTM ",rv$quad_actual)})

            #------Mapa quadricules 1x1 de la quadricula seleccionada


            #---------------Paleta per mapes a resolucio 1x1--------------------------------------------

            paleta_espec1x1 <- colorBin(palette = rv$pal_ , domain = Especies_Minka_quadricula1x1$n_observacions1x1 , bins = 4)



            output$map2 <- renderLeaflet({

              leaflet()%>%

                addProviderTiles(providers$CartoDB.Positron, group = "CARTO",
                                 options = providerTileOptions(crossOrigin = TRUE)) %>%
                addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%

                addPolygons(data = Quadricules_10x10_sf[index[1],],
                            stroke = TRUE,
                            smoothFactor = 0.2,
                            fillOpacity = 0,
                            weight = 1.5,
                            color = "black",
                            popup =  paste ("Codi quadricula: ",quadricula_10x10_selecc$COORD_10K),
                            group ="Quadricula 10x10") %>%

                addPolygons(data = municipis_litorals_sf_1x1 ,
                            stroke = TRUE,
                            smoothFactor = 0.2,
                            fillOpacity = 0.25,
                            fillColor =  ~rv$paleta_muni_(municipis_litorals_sf_1x1$Comarques_),
                            weight = 1,
                            color = "black",

                            popup =  paste ("Municipi: ",municipis_litorals_sf_1x1$NOMMUNI,
                                            "<br>",
                                            "Comarca: ",municipis_litorals_sf_1x1$Comarques_,
                                            "<br>",
                                            "Provincia: ",municipis_litorals_sf_1x1$Comarque_1),

                            group = "Municipis") %>%

                addWMSTiles(baseUrl = "https://geoserveis.icgc.cat/servei/catalunya/batimetria/wms?", layers= "isobates_clar_2500000", group= "batimetria",options=

                              WMSTileOptions(format = "image/png",transparent = TRUE )) %>%

                addWMSTiles(baseUrl = "https://geoserveis.icgc.cat/servei/catalunya/batimetria/wms?", layers= "isobates_clar_600000", group= "batimetria", options=

                              WMSTileOptions(format = "image/png",transparent = TRUE)) %>%

                addWMSTiles(baseUrl = "https://geoserveis.icgc.cat/servei/catalunya/batimetria/wms?", layers= "isobates_clar_300000", group= "batimetria", options=

                              WMSTileOptions(format = "image/png",transparent = TRUE )) %>%


                addWMSTiles(baseUrl = "https://geoserveis.icgc.cat/servei/catalunya/batimetria/wms?", layers= "isobates_clar_100000", group= "batimetria", options=

                              WMSTileOptions(format = "image/png",transparent = TRUE )) %>%

                addWMSTiles(baseUrl = "https://geoserveis.icgc.cat/servei/catalunya/batimetria/wms?", layers= "isobates_clar_5000", group= "batimetria" , options=

                              WMSTileOptions(format = "image/png",transparent = TRUE )) %>%


                # addPolylines(data =batimetria_sf_1x1,
                #              stroke = TRUE,
                #              smoothFactor = 0.2,
                #              fillOpacity = 0,
                #              weight = 1,
                #              color = ~paleta_batimetr( batimetria_sf_1x1$PROF),
                #              popup =  paste ("Isobata: ", batimetria_sf_1x1$PROF),
                #              group ="Batimetria") %>%

                addPolygons(data =  Quadricules_1x1_en_10x10_sf,
                            stroke = TRUE,
                            smoothFactor = 0.2,
                            fillOpacity = 0,
                            weight = 1,
                            color = "red",
                            popup =  paste ("Codi quadricula: ",Quadricules_1x1_sf$COD1X1),
                            group ="Quadricula 1x1") %>%

                addPolygons(data = Especies_Minka_quadricula1x1,
                            stroke = TRUE,
                            smoothFactor = 0.2,
                            fillOpacity = 0.7,
                            fillColor = ~paleta_espec1x1(Especies_Minka_quadricula1x1$n_observacions1x1),
                            weight = 1,
                            color = "black",
                            group = "Nº d´observacions x quadricula 1x1",
                            popup =  paste ("Nº d observacions:", as.character(Especies_Minka_quadricula1x1$n_observacions1x1),
                                            "<br>",
                                            "Quadricula:",(Especies_Minka_quadricula1x1$COD1X1)))%>%




                addLayersControl(baseGroups = c("CARTO", "Satel.lit"),
                                 overlayGroups = c("Nº d´observacions x quadricula 1x1","Municipis","Quadricula 1x1","Quadricula 10x10","Batimetria"),
                                 options = layersControlOptions(collapsed = TRUE))  %>%

                addLegend(

                  position = "bottomright",
                  pal = paleta_espec1x1,
                  values = Especies_Minka_quadricula1x1$n_observacions1x1,
                  title = "Nº Observ.",
                  opacity = 0.8,

                  group= "Nº d´observacions x quadricula"
                )# %>%

              # addLegend(
              #
              #   position = "bottomleft",
              #   pal = paleta_batimetr,
              #   values = batimetria_sf_1x1$PROF,
              #   title = "Isobates",
              #   opacity = 0.8,
              #   group= "Batimetria"
              # )


            })


            #--------------------TEXT Peu de mapa-------------------------------------------------------

            output$peu_map2 <- renderText(paste("Observacions dins la quadricula UTM 10x10 ",input$quadricula ," de ", rv$especie_actual,"  de l´any",rv$rango[1], " al ",  rv$rango[2] ))

            #---------------------------------------------------------------------------------------------------------

            #-------------Analisis mensual der quadicula 1x1-----------------------------------------------------------

            #---------------------------------------------------------------------------------------------------------

            #----------------------Titol grafic mensuals 1x1--------------------------------

            output$titol_graf_mes_1x1 <- renderText({req(rv$quad_actual)
              paste("Observacions UTM ",rv$quad_actual," totals mensuals")})


            #----------------------Grafic mensuals 1x1---------------------------------------------------------

            Especies_Minka_mes_1x1 <- observacio_quadricula_10 %>% group_by(month)  %>% summarise( observacions_mes = n())

            #grafic mensual


            output$grafica_mes_1x1 <- renderPlot({

              ggplot(Especies_Minka_mes_1x1,aes (x=month, y= observacions_mes))+

                geom_col(fill ='#32CD75')+scale_x_discrete(drop=FALSE)+

                geom_text(aes(label = observacions_mes), vjust= -0.5) +

                geom_smooth(aes(x= as.numeric(month),y=observacions_mes), method = 'loess', formula = y ~x, se=FALSE, color = 'red', inherit.aes =FALSE) +

                labs(x="Mes", y= "Num observ",caption = stringr::str_wrap(paste("Observacions mensuals acumulades per ", rv$especie_actual," dins la quadricula ", quadricula_10x10_selecc, "de l´any",rv$rango[1], " al ",  rv$rango[2] )))+

                theme_classic()+theme( panel.background = element_rect(fill = '#f5f5f5'),

                                       plot.caption = element_text( hjust = 0.5, size =14))

            })

            #---------------------------------------------------------------------------------------------------------

            #-------------Analisis anual der quadicula 1x1-----------------------------------------------------------

            #---------------------------------------------------------------------------------------------------------

            #----------------------Titol grafic anual 1x1--------------------------------

            output$titol_graf_any_1x1 <- renderText({req(rv$quad_actual)
              paste("Observacions UTM ",rv$quad_actual," totals anual")})

            #---------------------Grafic anual 1x1---------------------------------------------

            Especies_Minka_any_1x1 <- observacio_quadricula_10 %>% group_by(year)  %>% summarise( observacions_any = n())

            output$grafica_any_1x1 <- renderPlot({

              ggplot(Especies_Minka_any_1x1,aes (x=year, y= observacions_any))+

                geom_col(fill='#3498db')+ scale_x_continuous(breaks = rv$rango[1]:rv$rango[2],limits = (x = c(rv$rango[1]-1,rv$rango[2]+1)))+

                geom_text(aes(label = observacions_any), vjust= -0.5) +

                geom_smooth(method = 'loess',formula = y ~ x, se=FALSE, color = 'red') +

                labs(x="Any", y= "Num observ",caption =stringr::str_wrap( paste("Observacions anuals ", rv$especie_actual," dins la quadricula ", quadricula_10x10_selecc, "de l´any",rv$rango[1], " al ",  rv$rango[2] ))) +

                theme_classic()+theme( panel.background = element_rect(fill = '#f7f9fc'),

                                       plot.caption = element_text( hjust = 0.5, size =14))

            })




}  # tancament de l else
    
}) #tancament del button 1x1
      
}  # Tancament del server

# Run the application

shinyApp(ui = ui, server = server)








