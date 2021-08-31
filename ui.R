library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)     
library(factoextra) 
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(scales)
library(ggsn)
library(plotly)
library(dbscan)
library(rgdal)
library(spatialreg)
library(spatial)
library(mapview)
library(dplyr)
library(tidyselect)
library(tidyr)
library(rgeos)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "K-Means Clustering Kesejahteraan Rakyat Provinsi Papua Barat"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Import Data",icon=icon("folder-open"),
                         menuSubItem("Import Data",tabName = "dash",icon =icon("angle-right")), 
                         menuSubItem("Standarisasi Data",tabName = "Standarisasi",icon =icon("angle-right")),
                         menuSubItem("Uji Multikolinearitas",tabName = "Multikolinearitas",icon =icon("angle-right"))),
                menuItem("Principal Component Analysis", tabName = "pca", icon=icon("filter")),
                menuItem("K-Means Clustering", tabName = "kmeans", icon=icon("line-chart")),
                menuItem("SC Validity", tabName = "sc", icon=icon("file-alt")),
                menuItem("Mapping", tabName = "peta",icon=icon("map")),
                menuItem("About", tabName = "about", icon = icon("question"))
            )),
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "dash", 
                        fluidRow(
                            box(
                                title="Import Data",
                                width = 4,
                                fileInput("file1","Choose CSV file", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                checkboxInput("header", "Header", TRUE),
                                radioButtons("disp", "Display", choices = c(Head = "head", All = "all")),
                                radioButtons("sep", "Separator", choices = c(Comma = ",",Semicolon = ";",Tab= "\t"), selected = ","),
                                radioButtons("quote", "Quote", choices = c(None="", "Double Quote"='"', "Single Quote"="'"), selected = '"')
                            ),
                            box(
                                title="Data",
                                width = 8,
                                dataTableOutput(outputId = "data")
                            )
                        )),
                tabItem(tabName = "Standarisasi",
                        fluidRow(
                            box(
                                title = "Data Hasil Standarisasi",
                                width = 12,
                                dataTableOutput(outputId = "std")
                            )
                        )),
                tabItem(tabName = "Multikolinearitas",
                        fluidRow(
                            box(
                                title = "Pearson's Product Moment Coefficient",
                                width = 12,
                                dataTableOutput(outputId = "mult")
                            )
                        )),
                
                tabItem(tabName = "pca",
                        fluidRow(
                            tabBox(width = NULL,
                                   tabPanel(
                                       h5("Principal Component (PC)"),
                                       fluidRow(
                                           box(
                                               title = "Principal Component Analysis",
                                               width = 12,
                                               dataTableOutput(outputId = "prin")
                                           )
                                       )
                                   ),
                                   tabPanel(
                                       h5("Summary"),
                                       fluidRow(
                                           box(
                                               title = "Summary PCA",
                                               width = 12,
                                               verbatimTextOutput(outputId = "sumpca")
                                           )
                                       )
                                   ),
                                   tabPanel(
                                       h5("Plot"),
                                       fluidRow(
                                           box(
                                               title = "Scree Plot Proportion of Variance",
                                               width = 12,
                                               plotOutput(outputId = "scrplt")
                                           )
                                       )
                                   )
                            )
                        )),
                
                tabItem(tabName = "kmeans", 
                        fluidRow(
                            box(
                                title="Pilih Variabel",
                                width = 4,
                                helpText("Instruksi :
                                         Pilih komponen utama yang akan digunakan sebagai variabel clustering"),
                                tags$hr(),
                                selectizeInput("inp","Masukkan Komponen Utama", 
                                               choices= c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13"), 
                                               multiple = TRUE),
                                tags$hr(),
                                numericInput("clusters", "Jumlah Cluster", 1, min = 1, max = 9),
                                tags$hr()
                            ),
                            box(
                                title="",
                                width = 8,
                                plotOutput(outputId = "hasil")
                            ),
                            box(
                                title = "Info",
                                width = 12,
                                verbatimTextOutput(outputId = "info")
                            )
                        )),
                
                tabItem(tabName = "sc", 
                        fluidRow(
                            box(
                                title = "Sillohuette Coefficient",
                                width = 6,
                                verbatimTextOutput(outputId = "sumsc")
                            ),
                            box(
                                title = "Plot",
                                width=6,
                                plotOutput(outputId = "plotsc")
                            )
                        )),
                
                tabItem(tabName = "peta", 
                        fluidRow(
                            box(
                                title = "Peta Hasil Cluster",
                                width = 12,
                                leafletOutput(outputId = "pta")
                            )
                        )),
                
                tabItem(tabName = "about", 
                        fluidRow(
                            box(width=12,
                                htmlOutput("inc", width="100%", height = '800px',
                                       frameborder = 0, scrolling = 'auto'))
                        ))
                
                
                
                
            )
        )
    )
)

