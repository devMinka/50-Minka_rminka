#################################################################################
###############################################################################

#APP web analisis de dades per especie de BBDD Minka

#Sortida dades amb grau de recerca brutes sense tractar l esforç per quadricula

#Data posada en produccio: 25/08/26

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

options(shiny.maxRequestSize = 1000*1024^2)

#Preparem capes geografiques-----------------------------------------------------

Quadricules_10x10_sf <-st_read ("./Quadricules/Quadricules_10x10_lit_30m_.shp")

Quadricules_1x1_sf <-st_read ("./Quadricules/Quadricules_1x1_lit_30m_.shp")

municipis_litorals_sf <-st_read ("./Capes/Municipis_costaners_1.shp")

extensio_catalunya <- st_read( "./Capes/Extensio_cat_1.shp")

batimetria_sf <- st_read("./Capes/Batimetria_IHM_.shp")

#Funcio per extreure les dades de Minka----------------------------------------

Observ_Minka <- function(nom_cientific,year1,year2){
  
  observ<-NULL
  
  observ2_1<-NULL
  
  #bounds_catalunya <- c(40.52298284926719, 0.5122798874332668, 42.43533113908204, 3.5583525641628726)
  
  for (i in year1:year2){
    
    observ <-tryCatch(
      
      expr = {rminka::mnk_obs(query = nom_cientific, year =i, quality = "research",quiet =TRUE)#, bounds = bounds_catalunya)
      },
      
      error = function(e){
        return(NULL)
        
      })
    
    
    observ2_1 <-rbind(observ2_1,observ)
    
  }
  
  
  observ2 <- dplyr::filter(observ2_1,taxon_name == nom_cientific)
  
  df_especie <- mutate(observ2, latitut= as.numeric(latitude),longitut=as.numeric(longitude))
  
  df_sin_na1 <- subset(df_especie, !is.na(latitut))
  
  df_distrib <- subset(df_sin_na1, !is.na(longitut))
  
  return (df_distrib)
  
}


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
                     
                     leafletOutput(outputId = 'map2'),
                     
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
                 
                 
                 leafletOutput(outputId = 'map3'),
                 
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
                 
                 
                 leafletOutput(outputId = 'heatmap')
                 
                 
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
    quad_actual = NULL      # per posar en titol nomes si no dona error
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
    
    
    dades_df<- tryCatch(
      
      expr= {data.frame (Observ_Minka(rv$especie_actual, rv$rango[1],rv$rango[2]))},
      
      error = function(e){
        
        return()
        
      })
    
    
    if (is.null(dades_df) || nrow(dades_df) == 0){
      mostrar_error(paste0("No s'han trobat registres per <b>", rv$especie_actual, "</b> a Minka.<br><br>
  Comprova que el nom estigui ben escrit o prova amb un altre interval d'anys.<br><br>
  Si el nom és correcte, pot ser que no hi hagi observacions de recerca per aquesta espècie."))
      return()
    } else{
      
      #-------------------Adjuntem el any al data frame obtingut--------------------------------------------------
      
      year <-year(dades_df$observed_on )
      
      names(year) <-"year"
      
      dades_df <- cbind( dades_df ,year)
      
      #-----------------adjuntem el mes com a factor al dataframe obtingut-----------------------------------------
      
      
      month <-  month(dades_df$observed_on)
      
      names(month) <-"month"
      
      dades_df <- cbind(dades_df,month)
      
      dades_df$month <- factor(dades_df$month, levels = 1:12, labels =c("Gener", "Febrer", "Març",
                                                                        "Abril","Maig","Juny","Juliol","Agost","Setembre","Octubre", "Novembre","Decembre"))
      
      #-----------------------#Ho pasem a capa geografica-------------------------------
      
      capa_especie <-  st_as_sf(dades_df, coords = c("longitut","latitut"),crs =  "WGS84" )
      
      #La creuem amb la capa d extensio de catalunya per obdindre les obs de Catalunya
      
      Observacions_especie<- st_join( extensio_catalunya, capa_especie, join=st_intersects,largest=FALSE)
      
      
      output$espec_catalunya<- renderText(paste("El numero d observacions de ",
                                                rv$especie_actual," a tot Catalunya del",rv$rango[1]," al ",rv$rango[2],
                                                " es de : ",nrow(Observacions_especie)))
      
      
      #----------Creuem les observacions totals amb les quadricules de 10x10---------
      
      
      Observacions_especie10x10<- st_join(Quadricules_10x10_sf, capa_especie
                                          , join=st_intersects,largest=FALSE)
      
      
      Observacions_especie_NA10x10 <- subset(Observacions_especie10x10, !is.na(taxon_name))
      
      
      #---------------------Observacionns total a Catalunya dins de les quadricules 10x10-------------------
      
      output$titol_espec_catalunya <-renderText({
        req(rv$listo_10x10)
        paste("ANALISIS OBSERVACIONS ",rv$especie_actual)
      })
      
      output$espec_quadricula10x10 <-renderText({
        req(rv$listo_10x10)
        paste("El número d´observacions de",
              rv$especie_actual,"a Catalunya dins del total de quadricules 10x10 marines del",rv$rango_actual[1]," al ",rv$rango_actual[2],
              " és de : ",nrow(Observacions_especie_NA10x10), " observacions. Si aquet número es inferior vol dir que hi han observacions fora de les quadricules UTM")})
      
      #---------------------Observacions agrupades que quadricula 10x10-------------------------------
      
      Especies_Minka_quadricula10x10<-  Observacions_especie_NA10x10 %>%
        
        group_by(COORD_10K)  %>%
        
        summarise ( n_observacions10x10=n())
      
      
      #----------------------Titol mapa quadricula 10x10--------------------------------
      
      output$titol_quadricula <- renderText({
        req(rv$listo_10x10);
        "Mapa distribució observacions a Catalunya UTM 10mx10km"})
      
      
      #-----------Definicio de les paletes-----------------------------------------
      
      
      pal <- colorRampPalette(c("yellow","red"))(4)
      
      paleta_espec <- colorBin(palette = pal , domain =Especies_Minka_quadricula10x10$n_observacions10x10, bins = 4)
      
      #4 colors per la batimetria
      
      pal_bat <-c("#CCFBFF","#66BFFF","#3388FF","#0040FF")
      
      paleta_batimetr <-colorFactor(palette = pal_bat, domain = ((batimetria_sf$PROF)))
      
      #12 colors per les comarques
      
      pal_mun <- rainbow (12)
      
      paleta_muni <-colorFactor(palette = pal_mun, domain = ((municipis_litorals_sf$Comarques_)))
      
      
      #-------------Mapa de quadricules 10x10----------------------------------------
      
      
      mymap1 <- reactive({
        
        leaflet()%>%
          
          addProviderTiles(providers$CartoDB.Positron, group = "CARTO",
                           options = providerTileOptions(crossOrigin = TRUE)) %>%
          addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
          
          
          addPolygons(data = municipis_litorals_sf ,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.15,
                      fillColor =  ~paleta_muni(municipis_litorals_sf$Comarques_),
                      weight = 1,
                      color = "black",
                      
                      popup =  paste ("Municipi: ",municipis_litorals_sf$NOMMUNI,
                                      "<br>",
                                      "Comarca: ",municipis_litorals_sf$Comarques_,
                                      "<br>",
                                      "Provincia: ",municipis_litorals_sf$Comarque_1),
                      
                      group = "Municipis")%>%
          
          
          addPolylines(data =batimetria_sf,
                       stroke = TRUE,
                       smoothFactor = 0.2,
                       fillOpacity = 0,
                       weight = 1,
                       color = ~paleta_batimetr(batimetria_sf$PROF),
                       popup =  paste ("Isobata: ",batimetria_sf$PROF),
                       group ="Batimetria") %>%
          
          addPolygons(data =Quadricules_10x10_sf,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      weight = 1,
                      color = "black",
                      popup =  paste ("Codi quadricula: ",Quadricules_10x10_sf$COORD_10K),
                      group ="Quadricula 10x10") %>%
          
          addPolygons(data = Especies_Minka_quadricula10x10,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      fillColor = ~paleta_espec(Especies_Minka_quadricula10x10$n_observacions10x10),
                      weight = 1,
                      color = "black",
                      group = "Nº d´observacions x quadricula",
                      popup =  paste ("Nº d observacions:", as.character(Especies_Minka_quadricula10x10$n_observacions10x10),
                                      "<br>",
                                      "Quadricula:",(Especies_Minka_quadricula10x10$COORD_10K)))%>%
          
          addLayersControl(baseGroups = c("CARTO", "Satel.lit"),
                           overlayGroups = c("Nº d´observacions x quadricula","Municipis","Quadricula 10x10","Batimetria"),
                           options = layersControlOptions(collapsed = TRUE))  %>%
          
          addLegend(
            
            position = "bottomright",
            pal = paleta_espec,
            values = Especies_Minka_quadricula10x10$n_observacions10x10,
            title = "Nº Observ.",
            opacity = 0.8,
            
            group= "Nº d´observacions x quadricula"
          )  %>%
          
          addLegend(
            
            position = "bottomleft",
            pal = paleta_batimetr,
            values = batimetria_sf$PROF,
            title = "Isobates",
            opacity = 0.8,
            
            group= "Batimetria"
          )
        
      })
      
      output$map1 <- renderLeaflet( mymap1() )
      
      #--------------------TEXT Peu de mapa-------------------------------------------------------
      
      output$peu_map1 <- renderText({ req(rv$listo_10x10)
        paste("Observacions per quadricula UTM 10x10  de ", rv$especie_actual," a tot Catalunya de l´any",rv$rango_actual[1], " al ",  rv$rango_actual[2] )})
      
      #---------------------Observacions agrupades per mes------------------------------------------------------------
      
      
      #--------------------Titol agrupacio per mes--------------------------------
      
      output$titol_graf_mes <- renderText({req(rv$listo_10x10);
        "Observacions a Catalunya per Mes"})
      
      #--------------------------------------------------------------------------
      
      Especies_Minka_mes <- Observacions_especie_NA10x10 %>% group_by(month)  %>% summarise( observacions_mes = n())
      
      #grafic mensual
      
      #-------------------------------Grafic per mes--------------------------------------------------
      
      output$grafica_mes <- renderPlot({
        
        ggplot(Especies_Minka_mes,aes (x=month, y= observacions_mes))+
          
          geom_col(fill ='#32CD75')+scale_x_discrete(drop=FALSE)+
          
          geom_text(aes(label = observacions_mes), vjust= -0.5) +
          
          geom_smooth(aes(x= as.numeric(month),y=observacions_mes), method = 'loess', formula = y ~x ,se=FALSE, color = 'red', inherit.aes =FALSE) +
          
          labs(x="Mes", y= "Num observ", caption =stringr::str_wrap( paste("Observacions mensuals acumulades per ", rv$especie_actual," dins tot  Catalunya de l´any",rv$rango[1], " al ",  rv$rango[2] )))+
          
          theme_classic()+theme( panel.background = element_rect(fill = '#f5f5f5'), plot.caption = element_text( hjust = 0.5, size =14))
        
      })
      
      
      #---------------------Observacions agrupades per any------------------------------------------------------------
      
      #--------------------Titol agrupacio per mes--------------------------------
      
      output$titol_graf_any <- renderText({req(rv$listo_10x10);
        "Observacions a Catalunya anuals"})
      
      #--------------------------------------------------------------------------
      
      Especies_Minka_any <- Observacions_especie_NA10x10 %>% group_by(year)  %>% summarise( observacions_any = n())
      
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
      
      rv$capa_especie <- capa_especie
      rv$obs_10x10 <- Observacions_especie_NA10x10
      rv$quad_10x10 <- Especies_Minka_quadricula10x10
      rv$dades_df <- dades_df
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
  
  #updateSelectInput(inputId="quadricula", choices = rv$quad_10x10$COORD_10K)
  
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
      
      #Obtenir index de la qudricula seleccionada en la capa 10x10
      
      index <- which(Quadricules_10x10_sf$COORD_10K == input$quadricula)
      
      #Obstenir observacions de la quadricula 10x10 seleccionada
      
      quadricula_10x10_selecc <-Quadricules_10x10_sf[index[1],]
      
      
      
      observacio_quadricula_10 <- rv$obs_10x10 %>% dplyr::select("id","taxon_name","COORD_10K","user_id","observed_on",
                                                                 "user_login","longitude","latitude","uri","month","year","url_picture") %>% dplyr::filter(COORD_10K ==input$quadricula) %>% arrange(observed_on,user_login)
      
      #Titol especie encapçalament 1x1
      
      output$titol_espec_catalunya_1x1 <-renderText({req(rv$quad_actual)
        paste("ANALISIS OBSERVACIONS ",rv$especie_actual, "PER UTM ",rv$quad_actual)})
      
      #Sortida de text del n d observacion de la quadricula 10x10
      
      output$espec_quadricula <-renderText({req(rv$quad_actual)
        paste("El numero d observacions de" ,rv$especie_actual,"dins les quadricula 10x10 ", rv$quad_actual," és de : ",nrow(observacio_quadricula_10))})
      
      
      #---------------------------------------------------------------------------------------------------------
      
      #--------------------Tractament per capes a 1x1-------------------------------------
      
      
      municipis_litorals_sf_1x1 <- st_intersection(quadricula_10x10_selecc,municipis_litorals_sf)
      
      batimetria_sf_1x1 <- st_intersection(quadricula_10x10_selecc, batimetria_sf)
      
      Quadricules_1x1_en_10x10_sf_ <- st_intersection (quadricula_10x10_selecc,Quadricules_1x1_sf)
      
      Quadricules_1x1_en_10x10_sf <- distinct (Quadricules_1x1_en_10x10_sf_,COD1X1,.keep_all = TRUE)
      
      #-------------------Observacions en quadricules 1x1 dins de la quadricula 10x10 objectiu
      
      
      #Observacions dins de les quadricules 1x1                                                                                        )
      
      Observacions_especie1x1_<- st_join(Quadricules_1x1_en_10x10_sf, rv$capa_especie, join=st_intersects,largest=FALSE)
      
      
      Observacions_especie1x1 <- subset(Observacions_especie1x1_,COORD_10K = input$quadricula)
      
      
      Observacions_especie_NA1x1 <- subset(Observacions_especie1x1, !is.na(taxon_name))
      
      #Reconte d observacions per quadricula 1x1
      
      Especies_Minka_quadricula1x1<-  Observacions_especie_NA1x1 %>%
        
        group_by(COD1X1)  %>%
        
        summarise ( n_observacions1x1=n())
      
      
      #--------Total suma observ quadricules 1x1----------------------
      
      observ_quadr_1x1 <- sum(Especies_Minka_quadricula1x1$n_observacions1x1)
      
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
      
      
      
      
      
      #=======================================================================================
      #------------------Taula de les observacions individuals de la quadricula 10x10
      #====================================================================================
      
      #Creem link d imatges visibles al DT
      
      contingut <- sapply(observacio_quadricula_10$url_picture, function(x){paste0("data:image/",tools::file_ext(x), ";base64,",
                                                                                   
                                                                                   base64enc::base64encode(x))})
      
      observacio_quadricula_10 <- cbind(observacio_quadricula_10,contingut)
      
      imatge <- sapply(observacio_quadricula_10$contingut,function(x){HTML(paste0("<img src='",x,"' width='75' height='75'>"))})
      
      observacio_quadricula_10 <- cbind(observacio_quadricula_10,imatge)
      
      
      #Dataframe a mostrar en el datatable
      
      df<- data.frame("ID"= observacio_quadricula_10$id,
                      "Imatge"=observacio_quadricula_10$imatge,
                      "Data"= observacio_quadricula_10$observed_on,
                      "Observador"=observacio_quadricula_10$user_login,
                      #"Quadricula 1x1" = Observacions_especie_NA1x1$COD1X1,
                      "URL"=observacio_quadricula_10$uri)
      
      #--------------------DataTable de les observadions de la quadricula 1x1---------------------------
      
      output$valors_seleccionats <- renderDT({
        datatable(
          {
            
            {
              
              # Nova columna amb enllços
              
              df$ID <- mapply(
                
                function(nom, uri) {
                  
                  as.character(htmltools::a(nom, href = uri,target="_blank"))
                  
                }, df$ID,df$URL) # apliquem al ID el URL amb purrr
              
              # No mostrem URL sense enllaç
              
              df_final <- subset(df, select = -URL)
              
            }
            
          },     #Caracteristiques amb les que es presenta el datatable
          escape = FALSE,
          filter = 'top',  # Filtres en la part superior de la taula
          options = list(
            pageLength = 25, # Mostrar 10 files per pag
            lengthMenu = c(10,25,50), # Opcions de files per pag
            dom = 'lfrtip'  # Ordre dels elements de la taula (filtrat, longitut, informacio,...)
          ),
          rownames = FALSE # No mostrar  num fila
        )
      }, sanitize.text.function = function(x) x)
      
      
      
      #---------------Mapa de la quadricula observacions en quedricula amb chincheta
      
      
      output$map3 <- renderLeaflet({
        
        leaflet()%>%
          
          addProviderTiles(providers$CartoDB.Positron, group = "CARTO",
                           options = providerTileOptions(crossOrigin = TRUE)) %>%
          addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
          
          
          addPolygons(data = Quadricules_10x10_sf[index[1],],
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      fillColor = ,  # Aplicar la paleta al valor
                      weight = 1,
                      color = "black",
                      popup =  paste ("Codi quadricula: ",Quadricules_10x10_sf$COD10X10),
                      group ="Quadricula 10x10") %>%
          
          addPolygons(data =  Quadricules_1x1_en_10x10_sf,
                      stroke = TRUE,
                      smoothFactor = 0.2,
                      fillOpacity = 0,
                      weight = 1,
                      color = "red",
                      popup =  paste ("Codi quadricula: ",Quadricules_1x1_sf$COD1X1),
                      group ="Quadricula 1x1") %>%
          
          
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
          
          
          addLayersControl(baseGroups = c("CARTO", "Satel.lit"),overlayGroups = c("Quadricula 1x1","batimetria")) %>%
          
          addMarkers(data = observacio_quadricula_10,
                     group = "dades_finals",
                     lng = observacio_quadricula_10$longitude,
                     lat =  observacio_quadricula_10$latitude,
                     popup = ~paste0("Link observació: ",'<a href= "',observacio_quadricula_10$uri,'" target="_blank">', 'ID ',observacio_quadricula_10$id,'</a>',
                                     '<br>',"Observador: ",observacio_quadricula_10$user_login,
                                     '<br>',"Data: ",observacio_quadricula_10$observed_on,'<br>'),
                     
                     
                     options = markerOptions(draggable = FALSE)) %>%
          
          addPopupImages( observacio_quadricula_10$url_picture,group = "dades_finals", width = 150)
        
        
      })
      
    } # tancament del condicional del token
    
    #tancament del waiter
    session$onFlushed(function() {
      w$hide()
    }, once = TRUE)
    
    
  }) # tancament button 1x1
  
  #================================================================================================
  
  #--------------------Execucio Heatmap-------------------------------------------------------
  
  #===================================================================================================
  
  observeEvent(input$button_heatmap, {
    
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
    
    dat <-as.data.table(cbind(rv$obs_10x10$longitude,rv$obs_10x10$latitude))
    
    #Cal treballar amb matrius i cal que els noms de la latitut i long estiguin definits
    
    colnames(dat)<-c("longitude","latitude")
    
    kde <- bkde2D (dat[ , list(longitude, latitude)],
                   bandwidth=c(.0045, .0068), gridsize = c(750,750))
    
    #En la funcio quan mes gran es la grid size mes definit esta
    
    #Converitm a raster la funcio de densitat del kernel delspunts
    
    KernelDensityRaster <-raster::raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))
    
    #Paleta del heatmap
    
    palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)
    
    #Carreguem els valosr petits com NA per despres fer-los transparents
    
    KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA
    
    palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")
    
    ##Text heatmap
    
    output$espec_heatmap <-renderText(paste("El mapa de desitat del kernel de observacions de",
                                            rv$especie_actual,"a Catalunya del",rv$rango[1]," al ",rv$rango[2]))
    
    
    
    ## Heatmap amb leaflet
    
    output$heatmap <- renderLeaflet({
      
      leaflet() %>%
        
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(crossOrigin = TRUE)) %>%
        
        addRasterImage(KernelDensityRaster,
                       colors = palRaster,
                       opacity = .8) %>%
        
        addLegend(pal = palRaster,
                  values = KernelDensityRaster@data@values,
                  title = "Dens. Kernel Obs",
                  bins = 7,
                  position = "bottomleft")
    })
    
    #Tancament del waiter
    session$onFlushed(function(){ w$hide() }, once = TRUE)
    
  }) #Tancament button heatmap
  
  
  
  
  #==================================================================================================
  
  #Boto descarrega de quadricula 10x10
  
  #=======================================================================================================
  
  observeEvent(input$pdfBtn, {
    session$sendCustomMessage(
      "html2pdf",
      list(id = "contingut_a_imprimir_1", filename = "report_10x10.pdf")
    )
  })
  
  observeEvent(input$pdfBtn_1x1, {
    session$sendCustomMessage(
      "html2pdf",
      list(id = "contingut_a_imprimir_1x1", filename = "report_1x1.pdf")
    )
  })
  
  
  
}  # Tancament del server



# Run the application

shinyApp(ui = ui, server = server)

#Acabat
