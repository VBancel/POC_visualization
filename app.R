#
# App Shiny to visualize and/or download the POC global dataset
#
#    https://shiny.posit.co/
#




#plotOutput et 
library(shiny)
library(bslib)
library(shinycssloaders)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tools)
library(lubridate)
source("plot_functions.R")
library(leaflet)
library(leaflet.extras)
library(sf)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # application title
    dashboardHeader(title = "Visualisation des flux de carbone"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Plot of variables", tabName = "plot_variable"),
        menuItem("Download data", tabName = "download_data")
      )),
      
    dashboardBody(
      tabItems(
        tabItem(tabName = "plot_variable",
               fluidRow(
                 box(withSpinner(plotOutput("histogram"))),
                 box(selectInput("selected_var",
                                 "Variable x",
                                 choice = c("latitude", "longitude", "profondeur" = "depth", "année" = "timestamp"),
                                 selected = "latitude"), 
                     uiOutput("dynamic_input"))
               ) 
        ), # end of Plot variables
        
        tabItem(tabName = "download_data",
                fluidRow(
                  box(leafletOutput("map"), height = 500),
                  box(h3("Entrer les coordonées du rectangle :"),
                      numericInput("N", "Lat. max", value = 40),
                      numericInput("S", "Lat. min", value = -40),
                      numericInput("O", "Long. min", value = -40),
                      numericInput("E", "Long. max", value = 40),
                      hr(),
                      h3("Or trace rectangle on the map")),
                  box(
                    actionButton("download_selected", "Download data in the selected area"),
                    downloadButton("downloadData", "Download data"))
                  
                  )
                ) # end of Download data
        
            )
        ) # end of the body
)





# Define server logic 

server <- function(input, output, session) {
  
  ###---- load data flux carbone ------
  
  db_carbon <- read.csv("~/complex/flux_carbone/my_code/Results/Global_Poc_Database_Cathryn.csv")
  db_carbon$timestamp <- as.POSIXct(db_carbon$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  
  
  ###---- plot variable window -------
  
  
  # modif of 2nd choice according to first variable
  
  output$dynamic_input <- renderUI({
    if (input$selected_var == "depth"){
      sliderInput("bins", "Largeurs tranches", min = 100, max = 1000, value = 500, step = 100)
    }
    else if (input$selected_var %in% c("latitude", "longitude")) {
      sliderInput("bins", "Largeurs tranches", min = 10, max = 50, value = 10, step = 10)      
    }
    else if (input$selected_var %in% c("timestamp")){
      selectInput("date_var",
                  "Variable de temps",
                  choices = c("year", "month"),
                  selected = "year")
    }
  })
  
  
  ###----- creation du plot -----
  
  output$histogram <- renderPlot({
    
    if (!(input$selected_var %in% names(db_carbon))) {
      showNotification(paste("Erreur : La colonne", input$selected_var, "n'existe pas dans les données"), type = "error")
      return(NULL)
    }
    
    # seq of x axis depending on bins and variable we want to represent
    
    if (input$selected_var == "latitude") {
      bin_range <- seq(-90, 90, by = input$bins)  # Latitude ranges from -90 to 90
    }
    if (input$selected_var == "longitude") {
      bin_range <- seq(-180, 180, by = input$bins)  # Longitude ranges from -180 to 180
    }
    if (input$selected_var == "depth") {
      bin_range <- seq(0, 6000, by = input$bins)
    }
        
    ###---- if we want to show a variable cut by bins ==> cut data and count them by bins -----
    
    if (input$selected_var %in% c("latitude", "longitude", "depth")){
      db_carbon <- db_carbon %>%
        mutate(var_bins = cut(db_carbon[[input$selected_var]], 
                              breaks = bin_range, 
                              include.lowest = TRUE, right = FALSE))
      
      count_data <- db_carbon %>%
        group_by(var_bins) %>%
        summarise(count = n())
      
      all_bins <- data.frame(var_bins = cut(seq(bin_range[1], bin_range[length(bin_range)], by = 10),
                                            breaks = bin_range,
                                            include.lowest = TRUE,
                                            right = FALSE))  # Créer tous les bins avec cut
      all_bins$count <- 0
      all_bins <- unique(all_bins)
      
      count_data <- left_join(all_bins, count_data, by = "var_bins") %>%
        mutate(count = coalesce(count.y, 0)) %>%  # Utiliser coalesce pour remplacer NA par 0
        select(var_bins, count) 
      
      # Calculer les limites des bins pour les étiquettes
      count_data$bin_labels <- paste0("(", bin_range[-length(bin_range)], ", ", bin_range[-1], "]")
      
      # create the plot
      plot <- plot_lat_lon_depth(count_data, input$selected_var)

    }
    
  ###--- if we want to show by year/month : just count nbr measure by year/month ----
    
    if (input$selected_var %in% c("timestamp")){

      # group by year/or month depending of user choice
      db_carbon <- db_carbon %>%  mutate(date_value = case_when(
        input$date_var == "year" ~ year(timestamp),
        input$date_var == "month" ~month(timestamp)))
      
      count_data <- db_carbon %>%
        group_by(date_value) %>%
        summarise(count = n())
      
      # create the plot
      plot <- plot_other(count_data, input$date_var)
      
    }
    
    return(plot)
    })
  
  
  ### ----- plot map to choose area -----
  
  
  #initialize map
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addDrawToolbar(
        targetGroup = 'drawItems',
        rectangleOptions = drawRectangleOptions(),
        polylineOptions = FALSE,  # Désactiver certaines options
        polygonOptions = FALSE,   
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)
        
        # polyline = FALSE,
        # polygon = FALSE,
        # rectangle = TRUE,
        # circle = FALSE,
        # marker = FALSE,
        # editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        ) %>% 
      addLayersControl(
        overlayGroups = c("drawItems"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  # if user draw rectangle
  
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$properties$feature_type == "rectangle") {
      lat1 <- feature$geometry$coordinates[[1]][[2]][2]  # Coin supérieur gauche
      lon1 <- feature$geometry$coordinates[[1]][[1]][1]  # Coin supérieur gauche
      lat2 <- feature$geometry$coordinates[[1]][[4]][2]  # Coin inférieur droit
      lon2 <- feature$geometry$coordinates[[1]][[2]][1]  # Coin inférieur droit
      print(paste("Rectangle tracé :", lat1, lon1, lat2, lon2))
    }
  })
  
  
  ###--- modify data to download based on choice of user---- 
  
  output$dowloadData <- downloadHandler(
    filename = function(){
      paste0(input$dataset, ".csv")},
    content= function(file){
      write.csv()
    }
  )
  
  
    
}



# Run the application 
shinyApp(ui = ui, server = server)
