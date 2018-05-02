## app.R ##

library(tidyverse)
library(plyr)
library(shiny)
library(rsconnect)
library(ggmap)
library(maps)
library(sp)
library(plotly)
library(scales)
library(gridExtra)
library(shinydashboard)

load("shiny.RData")


# SHINY APP CODE
# ===


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Graph 2", tabName = "graph2", icon = icon("th")),
      menuItem("Graph 3", tabName = "graph3", icon = icon("th")),
      menuItem("Graph 4", tabName = "graph4", icon = icon("th")),
      menuItem("Graph 5", tabName = "graph5", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        fluidPage(
          inputPanel(
            radioButtons("variable", label = "Fill Variable:",
              choiceNames = c("Number of Streams", "Number of Tracks", 
                              "Number of Artists"),
              choiceValues = c("Total_Streams", "N_Tracks", "N_Artists"))
          ),
          plotlyOutput("world_map")
        )
      ),
      
      # Second tab content
      tabItem(tabName = "graph2",
        fluidPage(
          inputPanel(
            selectInput("country", label = "Country:",
              choices = sort(names(countries)), selected = "Asia")
          ),
          plotOutput("cplot")
        )
      ),
      
      # Third tab content
      tabItem(tabName = "graph3",
        fluidPage(
          inputPanel(
            selectInput("country1", label = "Country",
                         choices = unique(data$Country), 
                         selected = "USA"),
                  
            selectInput("country2", label = "Country 2",
                        choices = unique(data$Country),
                        selected = "Canada"),
                  
            textInput("songTime", label = "Song Name", 
                      value = "rockstar"),
                  
            textInput("artistTime", label = "Artist Name", 
                      value = "Post Malone")
          ),
          plotOutput("streams_over_time")
        )
      ),
      
      # Fourth tab content
      tabItem(tabName = "graph4",
        fluidPage(
          inputPanel(
            selectInput("region", label = "Region:",
                choices = c("North America", "South America", "Central America",
                      "Asia", "Europe", "Oceania"), selected = "North America"),
                  
            sliderInput("top_n", label = "Top n songs by days on top 5",
                         min = 5, max = 50, value = 10, step = 5)
          ),
          plotlyOutput("top_songs_plot")
        )
      ),
      
      # Fifth tab content
      tabItem(tabName = "graph5",
        fluidPage(
          inputPanel(
            selectInput("region5", label = "Region:",
                  choices = c("North America" = "na", "South America" = "sa", 
                              "Central America" = "ca", "Asia" = "asia", 
                              "Europe" = "europe", "Oceania" = "oceania"), 
                  selected = "na"),
                  
            selectInput("artist5", label = "Artist:",
                  choices = unique(top.songs.na.grouped$Artist), 
                  selected = "Ed Sheeran")
                  
          ),
          plotlyOutput("top_artist_prop_plot")
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  options(warn = -1)
  
  # Graph 1
  output$world_map <- renderPlotly({
    
    rplot = ggplot() + 
      geom_polygon(data = coords, aes(x = long, y = lat, group = group), 
                   fill = "white") +
      geom_polygon(data = country.data, aes_string(x = "long", y = "lat", 
                                                   group = "group",
                                                   fill = input$variable)) + 
      scale_fill_gradient2(high = "brown", low = "purple", mid = "white") +
      labs(title = "Top 200 ranking Spotify tracks worldwide",
           x = "", 
           y = "") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    if(input$variable == "Total_Streams") 
      rplot = rplot + labs(fill = "Number of Streams")
    if(input$variable == "N_Tracks") 
      rplot = rplot + labs(fill = "Number of Tracks")
    if(input$variable == "N_Artists") 
      rplot = rplot + labs(fill = "Number of Artists")
    
    ggplotly(rplot)
    
  })
  
  # Graph 2
  output$cplot <- renderPlot({
    
    cy = top15(input$country, "country")
    cy$artists = factor(cy$artists, levels = cy$artists)
    
    rplot1 = ggplot(cy, aes(x = artists, y = appearances)) + 
      geom_bar(stat = "identity", fill = "lightcoral") + 
      coord_flip() +
      labs(title = input$country,
           x = "Artist", y = "Number of Appearances")
    
    cont = country.to.continent(input$country)
    ct = top15(cont, "cont")
    ct$artists = factor(ct$artists, levels = ct$artists)
    
    rplot2 = ggplot(ct, aes(x = artists, y = appearances)) +
      geom_bar(stat = "identity", fill = "lightcoral") + 
      coord_flip() +
      labs(title = cont,
           x = "Artist", y = "Number of Appearances")
    
    p <- grid.arrange(grobs = list(rplot1, rplot2), nrow = 1, 
                 top = "Top 15 Artists Spotify by Country\ncompared with continent")
    print(p)
  })
  
  # Graph 3
  dataSub <- reactive({
    if (input$songTime == "") {
      chosenSong <- " "
    }
    else {
      chosenSong <- input$songTime 
    }
    subset(data, Track.Name == chosenSong &
             Artist == input$artistTime &
             (Country == input$country1 | Country == input$country2))
  })
  
  output$streams_over_time <- renderPlot({
    subsetOfData <- dataSub()
    if (nrow(subsetOfData) == 0) {
      ggplot() + 
        annotate("text", 
                 x = 4, y = 25, size = 8, label = "Song/Artist Not Found") + 
        theme_void() +
        labs(x = NULL, y = NULL)
    }
    else {
      ggplot(subsetOfData, aes(x = as.Date(Date), y = Streams, 
                               color = Country, group = Country)) +
        scale_x_date(date_labels = "%b %y") +
        scale_y_continuous(labels = comma) +
        geom_point() +
        geom_line() +
        labs(title = "Song Streams by Country Over Time",
             x = "Date",
             y = "Number of Streams") +
        theme(axis.title.y = 
                element_text(margin = margin(r = 20)),
              axis.title.x =
                element_text(margin = margin(t = 20)))
    }
  })
  
  # Graph 4
  output$top_songs_plot <- renderPlotly({
    p <- ggplot(eval(parse(text = paste("cum.table$", "'", input$region, "'",
                sep = "")))[1:input$top_n,], aes(x = Track.Name, y = freq)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Amount of Days Song has been in Top 5 Rankings in 2017", 
           x = "Song Name", y = "Number of Days", 
           caption = "Source: Spotify's Worldwide Daily Song Ranking") + 
      theme(axis.text.x = element_text(angle = 25))
    
    ggplotly(p)
  })
  
  # Graph 5
  observe({
    x <- input$region5
    updateSelectInput(session, "artist5", 
      choices = unique(eval(parse(text = paste("top.songs.", 
              input$region5, ".grouped$Artist", sep = "")))), 
              selected = "Ed Sheeran")
  })
  
  output$top_artist_prop_plot <- renderPlotly({
    p <- ggplot(eval(parse(text = paste("top.songs.", input$region5, ".grouped",
            sep = "")))[eval(parse(text = paste("top.songs.", 
              input$region5, ".grouped$Artist", sep = ""))) == input$artist5,], 
            aes(x = Track, y = Streams)) + 
      geom_bar(aes(x = factor(1), fill = Track), stat = "identity",
               width = .5)
    ggplotly(p)
  })
  
}

shinyApp(ui, server)