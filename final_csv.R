if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(httr)) {
  install.packages("httr")
}
if (!require(jsonlite)) {
  install.packages("jsonlite")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(scales)) {
  install.packages("scales")
}
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(ggplot2)
library(scales)  # Pour formater les pourcentages

data_list <- read.csv("C:/Users/helly/Downloads/r-shiny-Gauthier/r-shiny-Gauthier/data.csv")
# Données pour le graphique circulaire page 2
data_page2 <- data.frame(
  Category = c("Total Mechanical Bikes",
               "Total Electrical Bikes",
               "Total Electrical Removable Battery Bikes"),
  Value = c(sum(data_list$totalStands.availabilities.mechanicalBikes),
            sum(data_list$totalStands.availabilities.electricalBikes),
            sum(data_list$totalStands.availabilities.electricalRemovableBatteryBikes))
)
# Données pour le graphique en barre page 2
data_page2_2 <- data.frame(
  Category = c("Total Bikes",
               "Total Mechanical Bikes",
               "Total Electrical Bikes",
               "Total Electrical Removable Battery Bikes"),
  Value = c(sum(data_list$totalStands.availabilities.bikes),
            sum(data_list$totalStands.availabilities.mechanicalBikes),
            sum(data_list$totalStands.availabilities.electricalBikes),
            sum(data_list$totalStands.availabilities.electricalRemovableBatteryBikes))
)


ui <- shinyUI(
  navbarPage("My Velov'V App",
             tabPanel("Component 1", uiOutput('page1')),
             tabPanel("Component 2", uiOutput('page2')),  # Nouvelle page avec un graphique circulaire (Pie Chart)
             tabPanel("Component 3")
  )
)

server <- shinyServer(function(input, output, session) {
  filteredPoints <- eventReactive(input$sector, {
    filtered_data <- data_list[data_list$region == input$sector, ]
    cbind(filtered_data$position.longitude, filtered_data$position.latitude)
  }, ignoreNULL = FALSE)
  
  output$nb <- renderText({
    filtered_data2 <- data_list[data_list$region == input$sector, ]
    paste(nrow(filtered_data2))
  })
  
  output$total_capacity <- renderText({
    filtered_data2 <- data_list[data_list$region == input$sector, ]
    paste(sum(filtered_data2$totalStands.capacity))
  })
  output$percent_dispo <- renderText({
    filtered_data2 <- data_list[data_list$region == input$sector, ]
    paste(sprintf("%.3f", sum(filtered_data2$totalStands.availabilities.bikes) / sum(filtered_data2$totalStands.capacity)))
  })
 
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = filteredPoints())
  })
  
  output$page1 <- renderUI({
    fluidPage(
      titlePanel("Répartition des vélos par secteur", windowTitle = "VéloV App"),
      sidebarPanel(
        selectInput("sector", "Sélectionnez le secteur", choices = unique(data_list$region)),
        p(),
        "Informations sur le secteur :",
        "Nombre de stands : ", textOutput("nb"),
        "Capacité totale de VéloV : ", textOutput("total_capacity"),
        "Pourcentage vélos disponibles : ", textOutput("percent_dispo")
      ),
      mainPanel(
        leafletOutput("mymap"),
        p()
      ),
      # Style CSS pour personnaliser les couleurs
      tags$style(HTML("
        body {background-color: #f7f7f7;}
        .navbar {background-color: #343a40;}
        .navbar-default .navbar-nav>li>a {color: #ffffff;}
        .navbar-default .navbar-nav>li>a:hover {color: #dc3545;}
        .leaflet-container {background-color: #ffffff;}
      "))
    )
  })
  
  output$page2 <- renderUI({
    fluidPage(
      titlePanel("Répartition des vélos disponibles par type"),
      # Vous pouvez ajouter d'autres éléments ici, tels que des boutons ou du texte
      plotOutput("pieChart_2"),
      paste("total vélo dispo",sum(data_list$totalStands.availabilities.bikes)),
      br(),
      paste(sprintf("Pourcentage de vélo mécanique dispo/ total capacity : %.3f", 
      sum(data_list$totalStands.availabilities.mechanicalBikes) / sum(data_list$totalStands.capacity)),"(",sum(data_list$totalStands.availabilities.mechanicalBikes),")"),
      br(),
      paste(sprintf("Pourcentage de vélo éléctrique dispo/ total capacity : %.3f", 
      sum(data_list$totalStands.availabilities.electricalBikes) / sum(data_list$totalStands.capacity)),"(",sum(data_list$totalStands.availabilities.electricalBikes),")"),
      br(),
      paste(sprintf("Pourcentage de vélo éléctrique avec batterie changeable dispo/ total capacity : %.3f", 
      sum(data_list$totalStands.availabilities.electricalRemovableBatteryBikes) / sum(data_list$totalStands.capacity)),"(", sum(data_list$totalStands.availabilities.electricalRemovableBatteryBikes),")"),
      plotOutput("barChart2")
    )
  })
  
  output$pieChart_2 <- renderPlot({
    ggplot(data_page2, aes(x = "", y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = "Répartition des vélos disponibles par type") +
      scale_fill_brewer(palette = "Set3") +
      geom_text(aes(label = percent(Value / sum(Value))), position = position_stack(vjust = 0.5))
  })
  output$barChart2 <- renderPlot({
    ggplot(data_page2_2, aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Value), vjust = -0.5) +
      labs(title = "Répartition des vélos disponibles par type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Faire pivoter les étiquettes de l'axe x
  })
})

shinyApp(ui, server)
  
  