if(!require(shiny))
{
  install.packages("shiny")
}
library(httr)
library(jsonlite)
library(shiny)
library(leaflet)
library(tidygeocoder)
options(tidygeocoder.progress_bar = FALSE)

#dataa <- GET("https://api.jcdecaux.com/vls/v3/stations?contract=Lyon&apiKey=f584928eea4d8fdf00e5a98059f92b70724f917b")
# dataframe
#data_list  <- fromJSON(rawToChar(dataa$content), flatten = TRUE)

region <- read.csv("C:/Users/hellyr.CEGIDGROUP/OneDrive - CEGID/Documents/R/data.csv")
#region <- region[1:nrow(data_list),]

#data_list["region"] <- region
#data_list$region <- ifelse(is.na(data_list$region), "unknown", data_list$region)

#adrs = reverse_geo(lat = 45.77624, long = 4.871634, method = "osm")[3]

# prendre le 3 ème 
#strsplit(adrs, ",")
# Division de la chaîne par la virgule

# Récupération de la dixième valeur

# Création d'un nouveau data frame avec le code postal
#nouveau_point <- data.frame(position.lat = 45.77624, position.lng = 4.871634, code_postal = code_postal)
#nouveau_point <- nouveau_point[3]

# Define UI ----
ui <- fluidPage(
  #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  titlePanel("Répartition des velov par secteur"),
  sidebarPanel(
    #secteur
    selectInput("sector", "Select Sector", choices = unique(data_list$region))
  ),
  sidebarLayout(position = "right",
                sidebarPanel("nb velov: ",textOutput("resultat") ),
                mainPanel("map",
                leafletOutput("mymap"),
                p(),
                actionButton("recalc", "Valider"))
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  filteredPoints <- eventReactive(input$recalc, {
    filtered_data <- data_list[data_list$region == input$sector, ]
    cbind( filtered_data$position.longitude,filtered_data$position.latitude)
  }, ignoreNULL = FALSE)
  output$resultat <- renderText({
    filtered_data2 <- data_list[data_list$region == input$sector, ]
    paste( nrow(filtered_data2))
  })
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = filteredPoints())
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
