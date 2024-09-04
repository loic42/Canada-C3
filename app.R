# Load packages ----
#library(shiny)
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflegend)
library(maps)
library(mapproj)
library(tidyverse)
library(leaflegend)
library(bslib)
library(rsconnect)
library(readr)
library(ggplot2)

# Load data ----
edna_data = suppressWarnings(read.csv("Data/01_eDNA_index_C3_shiny_REVIEW.csv", header = T, sep = ","))

ui = navbarPage("Canada C3",
                  theme = bs_theme(
    # Controls the default grayscale palette
    bg = "#FFFFFF",
    fg = "#FFFFFF", # font color
    "navbar-bg" = "#E12C21",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = "#E12C21",
    secondary = "#F0F0F0",
    "input-border-color" = "#EA80FC"),
                  
                  
                  
                  #############################
                  # Setup the information panel
                  #############################
                  tabPanel(title = "Project information",
                           # style of the text
                           # padding-left control left margin
                               tags$style(type='text/css', 'body { overflow-y: scroll;}'), # make scrolling possible
                               tags$style(type="text/css", "h1 {width: 100%; text-align:center; font-size: 30px; padding-left:30px; padding-right:30px; color: #b10026}"),
                               tags$style(type="text/css", "h2 {width: 100%; text-align:center; font-size: 25px; padding-left:30px; padding-right:30px; color: #b10026}"),
                               tags$style(type="text/css", "h3 {width: 100%; text-align:center; font-size: 20px; padding-left:30px; padding-right:30px; color: black}"),
                               h1("Welcome to the Canada C3 eDNA shiny app!"),
                           
                               h2("Canada C3: A coast to coast to coast expedition"),
                               h3("In 2017, as an initiative of the Students on Ice Foundation (SOI Foundation), and with support from the Government of Canada and
                                  a variety of partners, the Canada C3 expedition travelled on the MV Polar Prince icebreaker along Canada's Atlantic, Arctic and 
                                  Pacific coastlines, from Toronto to Victoria via the Northwest passage. This expedition brought together a diverse group of 
                                  Canadians to explore Canada's coastline while reflecting on the journey's core themes of Diversity and Inclusion, Reconciliation,
                                  Youth Engagement and the Environment (Canada C3, 2017). During the 150-day expedition, opportunistic surface water samples were 
                                  collected along the journey by the crew and project participants over 15 legs. The final pan-Canadian dataset resulted in one 
                                  hundred eDNA samples collected along the Atlantic, Arctic and Pacific coasts of Canada. To learn more about the expedition,
                                  please visit https://canadac3.ca/en/homepage/"),
                           
                               img(src = "landing-map.jpg", height = 500, width = 800, style="display: block; margin-left: auto; margin-right: auto;"),
                           
                               h2("What is eDNA?"),
                               h3("Marine eDNA is organism DNA in the water from microbial cells, organisms' tissues, skin and scales, metabolic waste,
                                  or dissolved molecules. A sample of eDNA simply requires a collection of fixed volume of water and collection of
                                  associated eDNA onto a filter. Extraction and sequencing of this DNA can be used to investigate the taxonomic composition
                                  of whole marine communities ranging from invertebrates to fish and marine mammals."),
                           
                               h2("How to interpret the interactive map?"),
                               h3("The map shows the eDNA index value associated with each species detected by eDNA during our sampling events. This species-specific
                                  index ranges from 0 (no DNA detected for the species) to 1 (maximum proportion of DNA detected for the species across
                                  all samples). The index reflects changes in the relative biomass (or proportion) of the species
                                  but do not reflect absolute changes in biomass. The index values can be used to assess spatial variations
                                  of biomass for a given species.")

                               #h2("Do you want to learn more?"),
                               #h3("Please don't hesitate to contact us for more informations:
                               #   Loic Jacquemot (l.jacquemot@oceans.ubc.ca)")
                  ),
                  
                  ##############################
                  # Setup the map panel
                  ##############################
                  tabPanel("Interactive map",
                           tags$style(type = "text/css", "#map {height: calc(100vh - 53px) !important;}"), # control height of the map
                           fillPage(
                           leafletOutput('map')),
                           tags$style(type = "text/css", ".container-fluid {padding-left:0px; padding-right:0px;}"), #control left and right margins of the map
                           tags$style(type = "text/css", ".navbar {margin-bottom: -20px}"), # control space between the navbar and the map
                           
                           ##########################################
                           # add a left panel with selective panel
                           ##########################################
                             absolutePanel(bottom = 1, left = 20,
                                           style = "color: white ; padding: 1px 1px 300px 1px",
                
      # add a picture
      img(src = "canada-c3-vector-logo.svg", height = 140, width = 180),    
      img(src = "SOI.png", height = 60, width = 180),    
      
      # Select a dataset to use
      selectizeInput('dataset', 'Select a dataset', 
                     choices = unique(edna_data$dataset), 
                     selected = 'edna_data'),
                
      # select species panel          
      selectInput("species", 
                  label = "Choose a species to display",
                  choices = levels(factor(unique(edna_data$species))),
                  selected = "Oncorhynchus gorbuscha"))
      )
    )


  
######################################################################################################################
# Server logic ----
######################################################################################################################
  
  
server = function(input, output, session){
  
  # create the reactive for the map
  map_filter = reactive({
    edna_data %>% 
      filter(dataset %in% input$dataset)%>% 
      filter(species %in% input$species)
  })
  
  # create a color gradient
  pal <- colorNumeric(palette = c("#feb24c", "#fc4e2a", "#b10026")
                      , domain = c(0,1)
                      )
  
  # create the map
  output$map <- renderLeaflet({
    leaflet(edna_data) %>% 
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery)%>% # suppose to add depth
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))%>%
    addCircles(data = map_filter(),
               lat =  ~lat,
               lng =  ~lon,
               color = ~pal(indexvalue),
               weight = ~indexvalue_100,
               label = ~paste("station:", ShortName, "/ index value:", indexvalue),
               popup = ~indexvalue_100)%>%
      addLegend("bottomright", 
                pal = pal,
                values = edna_data$indexvalue, 
                title = "eDNA index value",
                opacity = 1)
  })
}

# Run app ----
shinyApp(ui, server)

# see here to deploy on a html page : https://www.shinyapps.io/admin/#/dashboard
# NB: does not work well when integrated into OneDrive
#rsconnect::deployApp('C:/Users/loicj/Documents/03_Shiny_C3/CanadaC3_Shiny_map/census-app-C3_V3', appName="CanadaC3_shiny")