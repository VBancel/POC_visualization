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



# Define UI for application that draws a histogram
ui <- dashboardPage(

    # application title
    dashboardHeader(title = "Visualisation des flux de carbone"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Plot of variables", tabName = "plot_variable"),
        menuItem("Dowload data", tabName = "download_data")
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
                     conditionalPanel(
                       condition = "input.selected_var != 'timestamp'",
                       sliderInput("bins",
                                     "Largeurs tranches",
                                     min = 10,
                                     max = 50,
                                     step = 10,
                                     value = 10)),
                       conditionalPanel(
                         condition = "input.selected_var == 'timestamp'",
                         selectInput("date_var",
                                     "Variable de temps",
                                     choices = c("year", "month"),
                                     selected = "year")))))
        )
      )
)

# Define server logic 

server <- function(input, output, session) {
  
  # load flux carbon
  db_carbon <- read.csv("~/complex/flux_carbone/my_code/Results/Global_Poc_Database_Cathryn.csv")
  
  #si la variable selectionnee est depth ou ..., on change la selection des bins
  
  observeEvent(input$selected_var, {
    if (input$selected_var == "depth"){
      updateSliderInput(session, "bins", min = 100, max = 1000, value = 500, step = 100)
    }
  })
  
  
  output$histogram <- renderPlot({
    
    if (input$selected_var == "") return(NULL)
    
    if (!(input$selected_var %in% names(db_carbon))) {
      showNotification(paste("Erreur : La colonne", input$selected_var, "n'existe pas dans les données"), type = "error")
      return(NULL)
    }
    
    if (input$selected_var == "latitude") {
      bin_range <- seq(-90, 90, by = input$bins)  # Latitude ranges from -90 to 90
    }
    if (input$selected_var == "longitude") {
      bin_range <- seq(-180, 180, by = input$bins)  # Longitude ranges from -180 to 180
    }
    if (input$selected_var == "depth") {
      bin_range <- seq(0, 6000, by = input$bins)
    }
        
    ###---- if we want to show a variable cut by bins
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
      
      
      plot <- ggplot(count_data, aes(x = var_bins, y = count, fill = count)) +
        geom_bar(stat = "identity", color = "white", show.legend = FALSE) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Palette de couleurs plus esthétique
        labs(
          title = paste0("Nombre de mesures par tranche de ", input$selected_var),
          subtitle = "Distribution des mesures POC en fonction de la latitude",
          x = toTitleCase(input$selected_var), 
          y = "Nombre de mesures"
        ) +
        scale_x_discrete(labels = function(x) gsub("\\[|\\)", "", x)) +
        theme_minimal() +  # Utilisation d'un thème minimaliste
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12)
        )
    }
    
    
    if (input$selected_var %in% c("timestamp")){

      # group by year/or month depending of user choice
      db_carbon <- db_carbon %>%  mutate(date_value = case_when(
        input$date_var == "year" ~ year(timestamp),
        input$date_var == "month" ~month(timestamp)))
      count_data <- db_carbon %>%
        group_by(date_value) %>%
        summarise(count = n())
      
      # create ggplot
      
      plot <- ggplot(count_data, aes(x = date_value, y = count, fill = count)) +
        geom_bar(stat = "identity", color = "white", show.legend = FALSE) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Palette de couleurs plus esthétique
        labs(
          title = paste0("Nombre de mesures en fonction de ", input$date_var),
          x = toTitleCase(input$date_var), 
          y = "Nombre de mesures"
        ) +
        theme_minimal() +  # Utilisation d'un thème minimaliste
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12)
        )
      
    }
    
    return(plot)
      
      
  })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
