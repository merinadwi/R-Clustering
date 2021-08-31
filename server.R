
 function(input, output, session) {
    output$data <- renderDataTable({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        if(input$disp =="head") {
            return(head(df))
        }
        else{
            return(df)
        }
    })
    
    output$std <- renderDataTable({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                round(scale(df),3)
            })
    })
    
    output$mult <- renderDataTable({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                round(cor(scale(df), method = "pearson"),3)
            })
    })
    
    output$prin <- renderDataTable({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                datatable(round(dta_pca$x,3))
            })
    })
    
    output$sumpca <- renderPrint({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                summary(dta_pca)
            })
    })
    
    output$scrplt <- renderPlot({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                fviz_eig(dta_pca, addlabels = TRUE, ncp= 13,ylim = c(0, 50))
            })
    })
    
    output$hasil <- renderPlot({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                pcadata <- round(as.data.frame(dta_pca$x),3)
                if (length(input$inp)==0) print("Pilih Variabel")
                selectedData <- pcadata %>% dplyr::select(!!!input$inp)
                set.seed(123)
                km.res <-eclust(selectedData, "kmeans", k = input$clusters, nstart = 25, graph = TRUE)
                plotcluster <- fviz_cluster(km.res, data = pca.dat, ggtheme = theme_light())
                plotcluster
            }, colnames=TRUE)
        
    })
    
    output$info <- renderPrint({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                pcadata <- round(as.data.frame(dta_pca$x),3)
                if (length(input$inp)==0) print("Pilih Variabel")
                selectedData <- pcadata %>% dplyr::select(!!!input$inp)
                set.seed(123)
                km.res <-eclust(selectedData, "kmeans", k = input$clusters, nstart = 25, graph = TRUE)
                print(km.res)
            }, colnames=TRUE)
        
    })
    
    output$sumsc <- renderPrint({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                pcadata <- round(as.data.frame(dta_pca$x),3)
                if (length(input$inp)==0) print("Pilih Variabel")
                selectedData <- pcadata %>% dplyr::select(!!!input$inp)
                set.seed(123)
                km.res <-eclust(selectedData, "kmeans", k = input$clusters, nstart = 25, graph = TRUE)
                km.res$silinfo
            }, colnames=TRUE)
        
    })
    
    output$plotsc <- renderPlot({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                pcadata <- round(as.data.frame(dta_pca$x),3)
                if (length(input$inp)==0) print("Pilih Variabel")
                selectedData <- pcadata %>% dplyr::select(!!!input$inp)
                set.seed(123)
                km.res <-eclust(selectedData, "kmeans", k = input$clusters, nstart = 25, graph = TRUE)
                fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())
            }, colnames=TRUE)
    })
    
    output$pta <- renderLeaflet({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               quote=input$quote,  row.names = 1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                setwd("D:/GUI/GUI1/gadmn")
                Indonesia <- st_read("gadm36_IDN_2.shp")
                PapuaBarat <- Indonesia %>%
                    subset(Indonesia$NAME_1 == "Papua Barat")      #Mengambil Wilayah papua Barat
                PB=PapuaBarat[,-(1:6)]
                PB1=PB[,-(2:7)]
                Indonesia1 <- st_read("gadm36_IDN_3.shp")        #Mengambil Wilayah Manokwari Selatan
                M <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Dataran Isim")
                M1 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Momi Waren")
                M2 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Nenei")
                M3 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Oransbari")
                M4 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Ransiki")
                M5 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Tahosta")
                MS <- rbind(M,M1,M2,M3,M4,M5)
                names(MS)
                MS1 <- MS[,-(1:9)]
                names(MS1)
                MS2 <- MS1[,-(2:7)]
                colnames(MS2)[1]<-"NAME_2"
                PA <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Anggi")        #Mengambil Wilayah Pegunungan Arfak
                PA1 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Anggi Gida")
                PA2 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Didohu")
                PA3 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Menyambouw")
                PA4 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Sururey")
                PA5 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Taige")
                PA6 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Testega")
                PA7 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Catubouw")
                PA8 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Hingk")
                PA9 <- Indonesia1 %>%
                    subset(Indonesia1$NAME_3 == "Membey")
                PArfak <- rbind(PA,PA1,PA2,PA3,PA4,PA5,PA6,PA7,PA8,PA9)
                PArfak1 <- PArfak[,-(1:9)]
                PArfak2<- PArfak1[,-(2:7)]
                colnames(PArfak2)[1]<-"NAME_2"
                #Menggabungkan dataset papuabarat.manokwari selatan, dan pegunungan arfak
                ProvPapuaBarat <- rbind(PB1,MS2,PArfak2)
                dta_pca <- prcomp(x = scale(df), center = TRUE)
                pcadata <- round(as.data.frame(dta_pca$x),3)
                if (length(input$inp)==0) print("Pilih Variabel")
                selectedData <- pcadata %>% dplyr::select(!!!input$inp)
                set.seed(123)
                km.res <-eclust(selectedData, "kmeans", k = input$clusters, nstart = 25, graph = TRUE)
                c1 =as.matrix(km.res$cluster)
                cluster=c(c1[1,1],c1[2,1],c1[13,1],c1[5,1],c1[10,1],c1[8,1],c1[7,1],c1[6,1],c1[9,1],
                          c1[4,1],c1[3,1],c1[11,1],c1[11,1],c1[11,1],c1[11,1],c1[11,1],c1[11,1],
                          c1[12,1],c1[12,1],c1[12,1],c1[12,1],c1[12,1],c1[12,1],c1[12,1],c1[12,1],
                          c1[12,1],c1[12,1])
                ProvinsiPapuaBarat <- ProvPapuaBarat %>% add_column(cluster)
                #MembuatPeta
                get_long = c(132.2962792, 133.69589235385, 131.247547, 132.984880, 132.3806652, 130.5166646, 131.54328360, 132.218691, 132.48962, 133.898095411607, 134.3911216, 134.225215,133.714248 )
                get_lat = c(-2.9298442, -3.6444524, -0.876026, -0.936647, -1.324408, -0.2333324, -1.300731236, -1.780556, -0.60515, -2.27199715, -2.4570202,-1.326174,-1.155456)
                kota_papua =c("Fakfak","Kaimana","Kota Sorong","Manokwari","Maybrat","Raja Ampat","Sorong","Sorong Selatan","Tambrauw","Teluk Bintuni","Teluk Wondama","Manokwari Selatan","Pegunungan Arfak")
                penamaan <-data.frame(get_long,get_lat,kota_papua)
                pal <- colorNumeric(palette = "YlGnBu",domain = ProvinsiPapuaBarat$cluster)
                Peta <- leaflet(ProvinsiPapuaBarat) %>%
                    addTiles() %>% 
                    addPolygons(
                        color = ~pal(cluster),
                        weight = 2,
                        opacity = 1,
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = FALSE),
                        label = paste0(ProvinsiPapuaBarat$NAME_2, " ", ProvinsiPapuaBarat$cluster),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
                    )%>%addMarkers(get_long, get_lat, 
                                   label = paste0(penamaan$kota_papua, " ", penamaan$cluster) ,
                                   labelOptions = labelOptions(noHide = T, textsize = "1px", direction = "bottom",
                                                               style = list(
                                                                   "color" = "black",
                                                                   "font-family" = "serif",
                                                                   "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                                                   "font-size" = "9px",
                                                                   "border-color" = "rgba(0,0,0,0.5)"
                                                               )))
                
                Peta
            }, colnames=TRUE)
        
    })
    
    getPage<-function() {
        return(includeHTML("file:///D:/GUI/GUI1/tentang.html"))
    }
    output$inc<-renderUI({getPage()})
    
}

