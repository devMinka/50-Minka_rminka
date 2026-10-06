library(tidyverse); library(leaflet); library(sf); library(DT); library(bslib)
library(rminka); library(shiny); library(shinyjs); library(waiter)
library(KernSmooth); library(data.table); library(raster)

sf_use_s2(FALSE)
options(shiny.maxRequestSize = 1000*1024^2)

Quadricules_10x10_sf <- readRDS("./Quadricules/Quadricules_10x10_lit_30m_.rds")
Quadricules_1x1_sf <- readRDS("./Quadricules/Quadricules_1x1_lit_30m_.rds")
municipis_litorals_sf <- readRDS("./Capes/Municipis_costaners_1.rds")
extensio_catalunya <- readRDS("./Capes/Extensio_cat_1.rds")

lookup_path <- "./Quadricules/lookup_10x10_1x1.rds"
if(file.exists(lookup_path)){ quad_lookup <- readRDS(lookup_path) } else {
  idx <- st_intersects(Quadricules_10x10_sf, Quadricules_1x1_sf)
  quad_lookup <- setNames(lapply(seq_along(idx), function(i) Quadricules_1x1_sf$COD1X1[idx[[i]]]), Quadricules_10x10_sf$COORD_10K)
  saveRDS(quad_lookup, lookup_path)
}
if(!dir.exists("./cache")) dir.create("./cache")

Observ_Minka <- function(nom, y1, y2){
  cache_file <- sprintf("./cache/%s_%s_%s.rds", gsub(" ","_",nom), y1, y2)
  if(file.exists(cache_file)) return(readRDS(cache_file))
  out_list <- list()
  for(yr in y1:y2){
    tmp <- tryCatch(rminka::mnk_obs(query=nom, year=yr, quality="research", quiet=TRUE), error=function(e) NULL)
    if(!is.null(tmp) && nrow(tmp)>0) out_list[[as.character(yr)]] <- tmp
    Sys.sleep(0.2)
  }
  out <- dplyr::bind_rows(out_list)
  if(is.null(out) || nrow(out)==0) return(NULL)
  out <- out %>% filter(taxon_name==nom) %>% mutate(latitut=as.numeric(latitude), longitut=as.numeric(longitude)) %>% filter(!is.na(latitut))
  saveRDS(out, cache_file)
  out
}

ui <- fluidPage(use_waiter(), useShinyjs(), theme=bs_theme(bootswatch="cosmo"),
                h1("Anàlisi Observacións Marines ",img(src="logo_minka.png", width=120,height=40)),
                navlistPanel(id="tabset","MENU",well=TRUE,
                             tabPanel("10x10", sidebarLayout(sidebarPanel(
                               sliderInput("rango","Anys:",2017,lubridate::year(Sys.Date()),c(2017,lubridate::year(Sys.Date()))),
                               textInput("especie","Nom cientific","Diplodus sargus"), actionButton("mi_boton","Generar mapa")),
                               mainPanel(h4(textOutput("espec_catalunya")), h5(textOutput("espec_quadricula10x10")),
                                         leafletOutput('map1', height=500), plotOutput("grafica_mes"), plotOutput("grafica_any")))),
                             tabPanel("1x1", sidebarLayout(sidebarPanel(
                               selectInput("quadricula","Quadricula 10x10",choices=Quadricules_10x10_sf$COORD_10K),
                               actionButton("quadricula_button","Generar mapa")),
                               mainPanel(h4(textOutput("espec_quadricula")), h5(textOutput("total_quadr1x1")),
                                         leafletOutput('map2', height=300), leafletOutput('map3', height=400), DTOutput("valors_seleccionats")))),
                             tabPanel("Heatmap", actionButton("button_heatmap","Crea mapa densitats"), leafletOutput('heatmap', height=600))
                )
)

server <- function(input, output, session){
  w <- Waiter$new(html=tagList(spin_timer(),h3("Baixant Minka...")), color="rgba(30,40,50,0.9)")
  rv <- reactiveValues(obs_10x10=NULL, quad_10x10=NULL, capa_especie=NULL, listo=FALSE)
  
  observeEvent(input$mi_boton, {
    t0 <- Sys.time(); w$show()
    dades_df <- Observ_Minka(input$especie, input$rango[1], input$rango[2])
    if(is.null(dades_df)){ w$hide(); showModal(modalDialog("No hi ha dades")); return() }
    dades_df$year<-lubridate::year(dades_df$observed_on)
    dades_df$month<-factor(lubridate::month(dades_df$observed_on),1:12,labels=c("Gener","Febrer","Març","Abril","Maig","Juny","Juliol","Agost","Setembre","Octubre","Novembre","Decembre"))
    capa_especie<-st_as_sf(dades_df, coords=c("longitut","latitut"), crs=4326)
    Observacions_especie10x10<-st_join(Quadricules_10x10_sf, capa_especie, join=st_intersects, left=FALSE)
    tmp<-st_drop_geometry(Observacions_especie10x10) %>% group_by(COORD_10K) %>% summarise(n=n(),.groups="drop")
    Especies_Minka_quadricula10x10<-left_join(Quadricules_10x10_sf, tmp, by="COORD_10K") %>% filter(!is.na(n))
    output$espec_catalunya<-renderText(paste("Total",input$especie,":",nrow(dades_df),"obs"))
    output$espec_quadricula10x10<-renderText(paste("22? ->",nrow(Especies_Minka_quadricula10x10),"cuadriculas con presencia"))
    pal<-colorBin(c("yellow","red"), domain=Especies_Minka_quadricula10x10$n, bins=4)
    output$map1<-renderLeaflet({ leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addPolygons(data=Especies_Minka_quadricula10x10, fillColor=~pal(n), fillOpacity=0.7, popup=~COORD_10K) })
    output$grafica_mes<-renderPlot({ ggplot(dades_df,aes(month))+geom_bar()+coord_flip()+labs(title="Per mes") })
    output$grafica_any<-renderPlot({ ggplot(dades_df,aes(as.factor(year)))+geom_bar()+labs(title="Per any") })
    rv$obs_10x10<-Observacions_especie10x10; rv$quad_10x10<-Especies_Minka_quadricula10x10; rv$capa_especie<-capa_especie; rv$listo<-TRUE
    updateSelectInput(session,"quadricula",choices=rv$quad_10x10$COORD_10K)
    print(paste("TEMPS:", round(difftime(Sys.time(),t0,units="secs"),2),"seg")); w$hide()
  })
  
  observeEvent(input$quadricula_button, {
    req(rv$listo); w$show()
    quad_sel<-Quadricules_10x10_sf[Quadricules_10x10_sf$COORD_10K==input$quadricula,]
    obs_10<-rv$obs_10x10 %>% filter(COORD_10K==input$quadricula)
    Quadricules_1x1_en_10x10_sf<-Quadricules_1x1_sf[Quadricules_1x1_sf$COD1X1 %in% quad_lookup[[input$quadricula]],]
    Especies_Minka_quadricula1x1<-Quadricules_1x1_en_10x10_sf %>% st_join(rv$capa_especie, join=st_intersects, left=FALSE) %>% group_by(COD1X1) %>% summarise(n=n(),.groups="drop")
    output$espec_quadricula<-renderText(paste("Obs a",input$quadricula,":",nrow(obs_10)))
    output$total_quadr1x1<-renderText(paste("Cuadriculas 1x1 con presencia:", nrow(Especies_Minka_quadricula1x1)))
    output$map2<-renderLeaflet({ leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addPolygons(data=Especies_Minka_quadricula1x1, fillColor=~colorBin(c("yellow","red"), domain=n, bins=4)(n), fillOpacity=0.7) })
    output$map3<-renderLeaflet({ leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addProviderTiles(providers$Esri.WorldImagery,group="Satel.lit")%>%addPolygons(data=quad_sel, fillOpacity=0, weight=2)%>%addMarkers(data=obs_10, lng=~longitude, lat=~latitude, clusterOptions=markerClusterOptions(), popup=~paste0("<a href='",uri,"' target=_blank>",id,"</a><br><img src='",url_picture,"' width='120'>")) })
    df<-data.frame(ID=obs_10$id, Data=obs_10$observed_on, Foto=paste0("<img src='",obs_10$url_picture,"' width='75'>"), URL=obs_10$uri)
    output$valors_seleccionats<-renderDT({ df$ID<-mapply(function(a,b) as.character(htmltools::a(a, href=b, target="_blank")), df$ID, df$URL); datatable(subset(df, select=-URL), escape=FALSE, options=list(pageLength=25)) })
    w$hide()
  })
  
  observeEvent(input$button_heatmap, {
    req(rv$obs_10x10); dat<-as.data.table(cbind(rv$obs_10x10$longitude, rv$obs_10x10$latitude)); colnames(dat)<-c("longitude","latitude")
    kde<-bkde2D(dat[,list(longitude,latitude)], bandwidth=c(.0045,.0068), gridsize=c(500,500))
    r<-raster(list(x=kde$x1,y=kde$x2,z=kde$fhat)); r@data@values[r@data@values<1]<-NA
    pal<-colorNumeric("Spectral", domain=r@data@values, na.color="transparent")
    output$heatmap<-renderLeaflet({ leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addRasterImage(r, colors=pal, opacity=.8) })
  })
}
shinyApp(ui, server)