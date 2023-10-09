if(!require(shiny))
{
  install.packages("shiny")
}
if(!require(httr))
{
  install.packages("httr")
}
if(!require(jsonlite))
{
  install.packages("jsonlite")
}
if(!require(leaflet))
{
  install.packages("leaflet")
}
if(!require(ggplot2))
{
  install.packages("ggplot2")
}
if(!require(scales))
{
  install.packages("scales")
}
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(ggplot2)
library(scales)  # Pour formater les pourcentages
#dataa <- GET("https://api.jcdecaux.com/vls/v3/stations?contract=Lyon&apiKey=f584928eea4d8fdf00e5a98059f92b70724f917b")
# dataframe
#data_list  <- fromJSON(rawToChar(dataa$content), flatten = TRUE)
data_list <- read.csv("C:/Users/rhelly/Documents/data.csv")
#adrs = reverse_geo(lat = 45.77624, long = 4.871634, method = "osm")[3]
# Exemple de données fictives pour la deuxième page

data_page2 <- data.frame(
  Category = c("Total Mechanical Bikes", 
               "Total Electrical Bikes", 
               "Total Electrical Removable Battery Bikes"),
  Value = c(sum(data_list$totalStands.availabilities.mechanicalBikes),
            sum(data_list$totalStands.availabilities.electricalBikes), 
            sum(data_list$totalStands.availabilities.electricalRemovableBatteryBikes))
)

# Assuming data_list is a data frame with columns 'region', 'position.longitude', 'position.latitude', and 'totalStands.capacity'

ui <- shinyUI(
  navbarPage("My Application",
             tabPanel("Component 1", uiOutput('page1')),
             tabPanel("Component 2", uiOutput('page2')),  # Nouvelle page avec un graphique circulaire (Pie Chart)
             tabPanel("Component 3")
  )
)

server <- shinyServer(function(input, output, session) {
  filteredPoints <- eventReactive(input$recalc, {
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
    paste(sprintf("%.3f", sum(filtered_data2$totalStands.availabilities.bikes)/sum(filtered_data2$totalStands.capacity)))
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
      titlePanel("Répartition des velov par secteur"),
      sidebarPanel(
        selectInput("sector", "Select Sector", choices = unique(data_list$region))
      ),
      sidebarLayout(position = "right",
                    sidebarPanel("nb stands: ", textOutput("nb"), 
                                 "total velov capacity: ", textOutput("total_capacity"),
                                 "pourcentage de velo dispo: ",textOutput("percent_dispo")),
                    mainPanel("map",
                              leafletOutput("mymap"),
                              p(),
                              actionButton("recalc", "Valider")
                    )
      )
    )
  })
  output$page2 <- renderUI({
    fluidPage(
      titlePanel("Répartition des vélos disponible par type"),
      # Vous pouvez ajouter d'autres éléments ici, tels que des boutons ou du texte
      plotOutput("pieChart_2")
    )
  })
  # Code pour la deuxième page avec le graphique circulaire (Pie Chart) et les pourcentages
  output$pieChart_2 <- renderPlot({
    ggplot(data_page2, aes(x = "", y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = "Exemple de graphique circulaire (Pie Chart)") +
      scale_y_continuous(labels = scales::comma) +  # Formater les étiquettes en valeurs brutes
      geom_text(aes(label = percent(Value / sum(Value))), position = position_stack(vjust = 0.5))
  })
})

shinyApp(ui, server)
