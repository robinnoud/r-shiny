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
if (!require(shinyjs)) {
  install.packages("shinyjs")
}
if (!require(webshot)) {
  install.packages("webshot")
}


library(dplyr)
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(ggplot2)
library(scales)  # Pour formater les pourcentages
library(shinyjs)
library(webshot)
library(htmlwidgets)


load_data <- function(){
data <- GET("https://api.jcdecaux.com/vls/v3/stations?contract=Lyon&apiKey=f584928eea4d8fdf00e5a98059f92b70724f917b")
data_list  <- fromJSON(rawToChar(data$content), flatten = TRUE)
# une station est aberrante situé à Paris
data_list <- data_list[data_list$position.longitude >4, ]
# Données pour le graphique circulaire page 2

data_page2 <- data.frame(
  Category = c("Total Mechanical Bikes",
               "Total Electrical Bikes"
  ),
  Value = c(sum(data_list$totalStands.availabilities.mechanicalBikes),
            sum(data_list$totalStands.availabilities.electricalBikes)
  )
)
# Données pour le graphique en barre page 2
data_page2_2 <- data.frame(
  Category = c("Total Bikes",
               "Total Mechanical Bikes",
               "Total Electrical Bikes"
  ),
  Value = c(sum(data_list$totalStands.availabilities.bikes),
            sum(data_list$totalStands.availabilities.mechanicalBikes),
            sum(data_list$totalStands.availabilities.electricalBikes)
  )
)
return(list(data_list=data_list,data_page2 = data_page2, data_page2_2 = data_page2_2))
}
data_loaded <- load_data()
data_list <- data_loaded$data_list
data_page2 <- data_loaded$data_page2
data_page2_2 <- data_loaded$data_page2_2
ui <- shinyUI(
  navbarPage("My Velov'V App",
             tabPanel("Main", uiOutput('page1')),
             tabPanel("Disponibilité", uiOutput('page2')),  # Nouvelle page avec un graphique circulaire (Pie Chart)
             tabPanel("Status",  uiOutput('page3')),
             tabPanel("Station",  uiOutput('page4'))
  )
)

server <- shinyServer(function(input, output, session) {
  
  observeEvent({
    input$totalStands.capacity
  }, {
    filtered_data <- data_list[data_list$totalStands.capacity >= input$totalStands.capacity,]
    
    output$nb <- renderText({
      
      paste(nrow(filtered_data))
    })
    
    output$total_capacity <- renderText({
      
      paste(sum(filtered_data$totalStands.capacity))
    })
    output$percent_dispo <- renderText({
      paste(sprintf("%.3f", sum(filtered_data$totalStands.availabilities.bikes) / sum(filtered_data$totalStands.capacity)),"%")
    })
    output$mean_dispo <- renderText({
      paste(sprintf("%.3f", mean(filtered_data$totalStands.capacity) ),'vélos')
    })
    
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = cbind(filtered_data$position.longitude, filtered_data$position.latitude))
    })
    })
                            
  
  
  output$page1 <- renderUI({
    fluidPage(
      titlePanel("Stations velov Lyon", windowTitle = "VéloV App"),
      sidebarPanel(
        p(),
        "Informations sur la ville :",
        br(),
        br(),
        "Nombre de stands : ", textOutput("nb"),
        br(),
        "Capacité totale de VéloV : ", textOutput("total_capacity"),
        br(),
        "Pourcentage vélos disponibles : ", textOutput("percent_dispo"),
        br(),
        "Capacity moyenne des stands : ", textOutput("mean_dispo")
      ),
      mainPanel(
        leafletOutput("mymap"),
        p(),
        actionButton("refreshButton", "Rafraîchir les Données")
      ),sliderInput("totalStands.capacity", "Capacité minimale du stand", min = 0,max=max(data_list$totalStands.capacity),value=0),
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
      downloadButton("exportButton1", "Export Graph"),
      br(),
      paste(sprintf("Pourcentage de vélo mécanique dispo/ total capacity : %.3f", 
                    sum(data_list$totalStands.availabilities.mechanicalBikes) / sum(data_list$totalStands.capacity)),"(",sum(data_list$totalStands.availabilities.mechanicalBikes),")"),
      br(),
      paste(sprintf("Pourcentage de vélo éléctrique dispo/ total capacity : %.3f", 
                    sum(data_list$totalStands.availabilities.electricalBikes) / sum(data_list$totalStands.capacity)),"(",sum(data_list$totalStands.availabilities.electricalBikes),")"),
      downloadButton("exportButton2", "Export Graph"),
      plotOutput("barChart2")
    )
  })
  
  observeEvent(input$refreshButton, {
    data_loaded <- load_data()
    data_list <- data_loaded$data_list
    data_page2 <- data_loaded$data_page2
    data_page2_2 <- data_loaded$data_page2_2
  })
  # Fonction d'export du graphique
  
  output$exportButton1 <- downloadHandler(
    filename = function() {
      "mon_graphique.png"  # Nom du fichier de sortie
    },
    content = function(file) {
      my_plot <- ( ggplot(data_page2, aes(x = "", y = Value, fill = Category)) +
                     geom_bar(stat = "identity") +
                     coord_polar(theta = "y") +
                     scale_fill_brewer(palette = "Set3") +
                     geom_text(aes(label = percent(Value / sum(Value))), position = position_stack(vjust = 0.5)) +
                     labs(x = NULL, y = NULL)  # Supprimez les titres des axes x et y
      )
      ggsave(file, plot = my_plot, device = "png")
    }
  )
  # Fonction d'export du graphique
  
  output$exportButton2 <- downloadHandler(
    filename = function() {
      "mon_graphique.png"  # Nom du fichier de sortie
    },
    content = function(file) {
      my_plot <- (ggplot(data_page2_2, aes(x = Category, y = Value, fill = Category)) +
                    geom_bar(stat = "identity") +
                    geom_text(aes(label = Value), vjust = -0.5) +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                    labs(x = 'Category', y = 'Somme'))
      ggsave(file, plot = my_plot, device = "png")
    }
  )
  
  output$pieChart_2 <- renderPlot({
    ggplot(data_page2, aes(x = "", y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +
      geom_text(aes(label = percent(Value / sum(Value))), position = position_stack(vjust = 0.5)) +
      labs(x = NULL, y = NULL)  # Supprimez les titres des axes x et y
    
  })
  output$barChart2 <- renderPlot({
    ggplot(data_page2_2, aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Value), vjust = -0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(x = 'Category', y = 'Somme')# Faire pivoter les étiquettes de l'axe x
  })
  
  observeEvent({
    input$banking
    input$bonus
    input$status
    input$connected
    input$overflow
   
  }, {filtered_data3 <- data_list[data_list$banking == input$banking &
                                    data_list$bonus == input$bonus &
                                    data_list$status == input$status &
                                    data_list$connected == input$connected &
                                    data_list$overflow == input$overflow ,]
  output$totalStands <- renderText({
    paste("Total de stations remplissant les critères : ", nrow(filtered_data3))
  })
  output$mymap3 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = cbind(filtered_data3$position.longitude, filtered_data3$position.latitude))
  })
  output$capa3 <- renderPrint({
    capa3 <- sum(filtered_data3$totalStands.capacity)
    cat("capacité : ", capa3)
  })
  output$dispo3 <- renderPrint({
    dispo3 <- sum(filtered_data3$totalStands.availabilities.bikes)
    cat("disponibilité : ", dispo3)
  })
  
  })
  output$page3 <- renderUI({
    fluidPage(
      titlePanel("Status"),
      sidebarPanel(
      selectInput("banking", "Type banking", choices = unique(data_list$banking)),
      selectInput("bonus", "Type bonus", choices = unique(data_list$bonus)),
      selectInput("status", "Type status", choices = unique(data_list$status)),
      selectInput("connected", "Type connected", choices = unique(data_list$connected)),
      selectInput("overflow", "Type overflow", choices = unique(data_list$overflow)),
      textOutput("totalStands")
    ),
    mainPanel(verbatimTextOutput("capa3"),
              verbatimTextOutput("dispo3"),
              leafletOutput("mymap3"))
    )
  })

 
  output$page4 <- renderUI({
    fluidPage(
      titlePanel("Informations statitions velov"),
      # Vous pouvez ajouter d'autres éléments ici, tels que des boutons ou du texte
      sidebarPanel(
        selectInput("nom_station", "Choisissez la station ", 
                    choices = sort(unique(data_list$number)), multiple = F)
        ,verbatimTextOutput("status")
      ),
      mainPanel(
        verbatimTextOutput("numero"),
        verbatimTextOutput("name"),
        verbatimTextOutput("adress"),
        verbatimTextOutput("update"),
        verbatimTextOutput("capacity"),
        verbatimTextOutput("dispo"),
        leafletOutput("mymap2")
        )
     
    )
  })
  filtered_data4 <- reactive({
    data_list %>%
      filter(number %in% input$nom_station)
  })
  output$mymap2 <- renderLeaflet({
    data <- filtered_data4()
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = cbind(data$position.longitude, data$position.latitude))
  })
  output$numero <- renderPrint({
    data <- filtered_data4()
    numero <- data$number
    cat("numéro : ", numero)
  })
  output$name <- renderPrint({
    data <- filtered_data4()
    name <- elements <- unlist(strsplit(data$name, " - "))
    name <- name[2]
    if (is.na(name) || name == "") {
      cat("Nom : Non renseigné")
    } else {
      cat("Nom : ", name)
    }
  })
  output$adress <- renderPrint({
    data <- filtered_data4()
    adress <- data$address
    if (is.na(adress) || adress == "") {
      cat("Adresse : Non renseigné")
    } else {
      cat("Adresse : ", adress)
    }
  })
  output$update <- renderPrint({
    data <- filtered_data4()
    updt <- data$lastUpdate
    cat("update : ", updt)
  })
  output$capacity <- renderPrint({
    data <- filtered_data4()
    capa <- data$totalStands.capacity
    cat("capacity : ", capa)
  })
  output$dispo <- renderPrint({
    data <- filtered_data4()
    dispo <- data$totalStands.availabilities.bikes
    cat("dispo : ", dispo)
  })
  output$status <- renderPrint({
    data <- filtered_data4()
    status <- data$status
    cat("status : ", status)
  })
 
})

shinyApp(ui, server)