## app.R ##
library(tidyverse)
library(dplyr)
library(plyr)
library(shiny)
library(rsconnect)
library(ggplot2)
library(igraph)
library(ggraph)
library(ggmap)
library(maps)
library(plotly)
library(scales)
library(dygraphs)

# + Load data from `data.csv`
# + Check dimensions of the data, expecting a 3441197 x 7 dataframe
# + Return the first 5 rows
data = read.csv("data.csv")
dim(data)
head(data, 5)

# Data Manipulation
# ===
#   
# + Write a `date.to.month()` helper function, which takes in a date in the form YEAR-MONTH-DAY and converts it to a month
# + E.g. date.to.month(2017-01-26) returns January
# + Apply `date.to.month()` across the `Date` column of our data, creating a new column `Month`
date.to.month = function(date) {
  date = as.character(date)
  month.numeric = as.numeric(unlist(strsplit(date, "-")))[2]
  return(month.name[month.numeric])
}

data$Month = sapply(data$Date, date.to.month)

# + Create a vector of countries based off of the country codes that we have in our `Region` column
# + Write a `code.to.country()` function that takes in a country code and returns the country
# + E.g. `code.to.country("ec")` returns "Ecuador"
# + Apply `code.to.country()`across the `Region` column of our data, creating a new column `Country`

countries = unique(data$Region)
names(countries) = c("Ecuador", "France", "Argentina", "Finland", "Norway", 
                     "Italy", "Lithuania", "Philippines", "Taiwan", 
                     "New Zealand", "Estonia", "Turkey", 
                     "USA", "El Salvador", "Costa Rica", 
                     "Germany", "Chile", "Japan", "Brazil", "Honduras", 
                     "Guatemala", "Switzerland", "Hungary", "Canada", "Peru", 
                     "Belgium", "Malaysia", "Denmark", "Bolivia", "Poland", 
                     "Austria", "Portugal", "Sweden", "Mexico", "Panama", 
                     "Uruguay", "Iceland", "Spain", "Czech Republic", "Ireland", 
                     "Netherlands", "Slovakia", "Colombia", "Singapore", 
                     "Indonesia", "Dominican Republic", "Luxembourg", 
                     "UK", "World", "Paraguay", "Australia", 
                     "Latvia", "Greece", "Hong Kong")

code.to.country = function(code) {
  code = as.character(code)
  return(names(countries)[which(countries == code)])
}

data$Country = sapply(data$Region, code.to.country)


# + Create a vector of continents based off of the countries that we have in our `Country` column
# + Write a `country.to.continent()` function that takes in a country code and returns the continent
# + E.g. `country.to.continent("Ecuador")` returns "South America"
# + Apply `country.to.continent()`across the `Country` column of our data, replacing the column `Region`

asia = c("Philippines", "Taiwan", "Japan", "Malaysia", "Singapore", "Indonesia", 
         "Hong Kong")
europe = c("France", "Finland", "Norway", "Italy", "Lithuania", "Estonia", 
           "Turkey", "Germany", "Switzerland", "Hungary", "Belgium", "Denmark", 
           "Poland", "Austria", "Portugal", "Sweden", "Iceland", "Spain", 
           "Czech Republic", "Ireland", "Netherlands", "Slovakia", "Luxembourg", 
           "United Kingdom", "Latvia", "Greece")
northamerica = c("USA", "Canada", "Mexico")
centralamerica = c("El Salvador", "Costa Rica", "Honduras", "Guatemala", 
                   "Panama", "Dominican Republic")
southamerica = c("Ecuador", "Argentina", "Chile", "Brazil", "Peru", "Bolivia", 
                 "Uruguay", "Colombia", "Paraguay")
oceania = c("New Zealand", "Australia")

country.to.continent = function(country) {
  country = as.character(country)
  if (country %in% asia) return("Asia")
  if (country %in% europe) return("Europe")
  if (country %in% northamerica) return("North America")
  if (country %in% centralamerica) return("Central America")
  if (country %in% southamerica) return("South America")
  if (country %in% oceania) return("Oceania")
  return(NA)
}

data$Region = sapply(data$Country, country.to.continent)


# + Statistical summaries to get a sense of the data

# Summary statistics on top 200 streams, world wide
streams = summary(data$Streams)
# Summary statistics on top 200 streams, by country
streams.by.country = ddply(data, .(Country), function(x) summary(x$Streams))
# Summary statistics on top 200 streams, by continent
streams.by.cont = ddply(data, .(Region), function(x) summary(x$Streams))

# Total streams, world wide
tot.streams = sum(as.numeric(data$Streams))
# Summary statistics on top 200 streams, by country
tot.streams.by.country = ddply(data, .(Country), 
                               function(x) sum(as.numeric(x$Streams)))
# Summary statistics on top 200 streams, by continent
tot.streams.by.cont = ddply(data, .(Region), 
                            function(x) sum(as.numeric(x$Streams)))

# How many times did Artists end up in the top 200 rankings, world wide?
artists = as.data.frame(table(data$Artist))
# How many times did Artists end up in the top 200 rankings, by country?
artists.by.country = ddply(data, .(Country), function(x) table(x$Artist))
# How many times did Artists end up in the top 200 rankings, by continent?
artists.by.cont = ddply(data, .(Region), function(x) table(x$Artist))

# How many unique Artists are there in the top 200 rankings, world wide?
n.artists = nrow(artists)
# How many times did Artists end up in the top 200 rankings, by country?
n.artists.by.country = ddply(artists.by.country, .(Country), 
                             function(x) sum(x != 0))
# How many times did Artists end up in the top 200 rankings, by continent?
n.artists.by.cont = ddply(artists.by.cont, .(Region), 
                          function(x) sum(x != 0))

# How many times did tracks end up in the top 200 rankings, world wide?
tracks = as.data.frame(table(data$Track.Name))
# How many times did tracks end up in the top 200 rankings, by country?
tracks.by.country = ddply(data, .(Country), function(x) table(x$Track.Name))
# How many times did tracks end up in the top 200 rankings, by continent?
tracks.by.cont = ddply(data, .(Region), function(x) table(x$Track.Name))

# How many unique Artists are there in the top 200 rankings, world wide?
n.tracks = nrow(tracks)
# How many times did Artists end up in the top 200 rankings, by country?
n.tracks.by.country = ddply(tracks.by.country, .(Country), 
                            function(x) sum(x != 0))
# How many times did Artists end up in the top 200 rankings, by continent?
n.tracks.by.cont = ddply(tracks.by.cont, .(Region), 
                         function(x) sum(x != 0))


# Set Up for Graph 1
# ---
#   + Create the `country.data` dataframe that consolidates the number of streams, tracks and artists of each country into one table
# + Use `map_data` to add goegraphical data for our map graphic 

data.asia = data[which(data$Region == "Asia"),]
data.europe = data[which(data$Region == "Europe"),]
data.northamerica = data[which(data$Region == "North America"),]
data.centralamerica = data[which(data$Region == "Central America"),] 
data.southamerica = data[which(data$Region == "South America"),] 
data.oceania = data[which(data$Region == "Oceania"),] 

colnames(tot.streams.by.country)[2] = "Total_Streams"
country.data = tot.streams.by.country

colnames(n.artists.by.country)[2] = "N_Artists"
country.data = left_join(country.data, n.artists.by.country, 
                         by = "Country")

colnames(n.tracks.by.country)[2] = "N_Tracks"
country.data = left_join(country.data, n.tracks.by.country, 
                         by = "Country")

country.data = left_join(country.data, artists.by.country, 
                         by = "Country")

coords = map_data("world")
coords$Country = coords$region
coords = coords[,c("Country", "long", "lat", "group")]

country.data = left_join(country.data, coords, by = c("Country"))


# SHINY APP CODE
# ===


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
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
      )
      
      # tabItem(tabName = "widgets",
      #         fluidRow(
      #           box(plotOutput("plot2", height = 250)),
      #           
      #           box(
      #             title = "Controls",
      #             sliderInput("slider", "Number of observations:", 1, 100, 50)
      #           )
      #         )     
      # )

      # # Second tab content
      # tabItem(tabName = "widgets",
      #   h2("Widgets tab content")
      # )
    )
  )
)

server <- function(input, output) {

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

}

shinyApp(ui, server)