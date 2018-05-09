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

load("draft.RData")
# data = read.csv("data4.csv")
# date.to.month = function(date) {
#   date = as.character(date)
#   month.numeric = as.numeric(unlist(strsplit(date, "-")))[2]
#   return(month.name[month.numeric])
# }
# 
# data$Month = sapply(data$Date, date.to.month)
# 
# countries = unique(data$Region)
# names(countries) = c("Ecuador", "France", "Argentina", "Finland", "Norway",
#                      "Italy", "Lithuania", "Philippines", "Taiwan",
#                      "New Zealand", "Estonia", "Turkey",
#                      "USA", "El Salvador", "Costa Rica",
#                      "Germany", "Chile", "Japan", "Brazil", "Honduras",
#                      "Guatemala", "Switzerland", "Hungary", "Canada", "Peru",
#                      "Belgium", "Malaysia", "Denmark", "Bolivia", "Poland",
#                      "Austria", "Portugal", "Sweden", "Mexico", "Panama",
#                      "Uruguay", "Iceland", "Spain", "Czech Republic", "Ireland",
#                      "Netherlands", "Slovakia", "Colombia", "Singapore",
#                      "Indonesia", "Dominican Republic", "Luxembourg",
#                      "UK", "World", "Paraguay", "Australia",
#                      "Latvia", "Greece", "Hong Kong")
# 
# code.to.country = function(code) {
#   code = as.character(code)
#   return(names(countries)[which(countries == code)])
# }
# 
# data$Country = sapply(data$Region, code.to.country)
# 
# asia = c("Philippines", "Taiwan", "Japan", "Malaysia", "Singapore", "Indonesia",
#          "Hong Kong")
# europe = c("France", "Finland", "Norway", "Italy", "Lithuania", "Estonia",
#            "Turkey", "Germany", "Switzerland", "Hungary", "Belgium", "Denmark",
#            "Poland", "Austria", "Portugal", "Sweden", "Iceland", "Spain",
#            "Czech Republic", "Ireland", "Netherlands", "Slovakia", "Luxembourg",
#            "UK", "Latvia", "Greece")
# northamerica = c("USA", "Canada", "Mexico")
# centralamerica = c("El Salvador", "Costa Rica", "Honduras", "Guatemala",
#                    "Panama", "Dominican Republic")
# southamerica = c("Ecuador", "Argentina", "Chile", "Brazil", "Peru", "Bolivia",
#                  "Uruguay", "Colombia", "Paraguay")
# oceania = c("New Zealand", "Australia")
# 
# country.to.continent = function(country) {
#   country = as.character(country)
#   if (country %in% asia) return("Asia")
#   if (country %in% europe) return("Europe")
#   if (country %in% northamerica) return("North America")
#   if (country %in% centralamerica) return("Central America")
#   if (country %in% southamerica) return("South America")
#   if (country %in% oceania) return("Oceania")
#   return(NA)
# }
# 
# data$Region = sapply(data$Country, country.to.continent)
# 
# 
# # Summary statistics on top 200 streams, world wide
# streams = summary(data$Streams)
# # Summary statistics on top 200 streams, by country
# streams.by.country = ddply(data, .(Country), function(x) summary(x$Streams))
# # Summary statistics on top 200 streams, by continent
# streams.by.cont = ddply(data, .(Region), function(x) summary(x$Streams))
# 
# # Total streams, world wide
# tot.streams = sum(as.numeric(data$Streams))
# # Summary statistics on top 200 streams, by country
# tot.streams.by.country = ddply(data, .(Country),
#                                function(x) sum(as.numeric(x$Streams)))
# # Summary statistics on top 200 streams, by continent
# tot.streams.by.cont = ddply(data, .(Region),
#                             function(x) sum(as.numeric(x$Streams)))
# 
# # How many times did Artists end up in the top 200 rankings, world wide?
# artists = as.data.frame(table(data$Artist))
# # How many times did Artists end up in the top 200 rankings, by country?
# artists.by.country = ddply(data, .(Country), function(x) table(x$Artist))
# # How many times did Artists end up in the top 200 rankings, by continent?
# artists.by.cont = ddply(data, .(Region), function(x) table(x$Artist))
# 
# # How many unique Artists are there in the top 200 rankings, world wide?
# n.artists = nrow(artists)
# # How many times did Artists end up in the top 200 rankings, by country?
# n.artists.by.country = ddply(artists.by.country, .(Country),
#                              function(x) sum(x != 0))
# # How many times did Artists end up in the top 200 rankings, by continent?
# n.artists.by.cont = ddply(artists.by.cont, .(Region),
#                           function(x) sum(x != 0))
# 
# # How many times did tracks end up in the top 200 rankings, world wide?
# tracks = as.data.frame(table(data$Track.Name))
# # How many times did tracks end up in the top 200 rankings, by country?
# tracks.by.country = ddply(data, .(Country), function(x) table(x$Track.Name))
# # How many times did tracks end up in the top 200 rankings, by continent?
# tracks.by.cont = ddply(data, .(Region), function(x) table(x$Track.Name))
# 
# # How many unique Artists are there in the top 200 rankings, world wide?
# n.tracks = nrow(tracks)
# # How many times did Artists end up in the top 200 rankings, by country?
# n.tracks.by.country = ddply(tracks.by.country, .(Country),
#                             function(x) sum(x != 0))
# # How many times did Artists end up in the top 200 rankings, by continent?
# n.tracks.by.cont = ddply(tracks.by.cont, .(Region),
#                          function(x) sum(x != 0))
# 
# data.asia = data[which(data$Region == "Asia"),]
# data.europe = data[which(data$Region == "Europe"),]
# data.northamerica = data[which(data$Region == "North America"),]
# data.centralamerica = data[which(data$Region == "Central America"),]
# data.southamerica = data[which(data$Region == "South America"),]
# data.oceania = data[which(data$Region == "Oceania"),]
# 
# colnames(tot.streams.by.country)[2] = "Total_Streams"
# country.data = tot.streams.by.country
# 
# colnames(n.artists.by.country)[2] = "N_Artists"
# country.data = left_join(country.data, n.artists.by.country,
#                          by = "Country")
# 
# colnames(n.tracks.by.country)[2] = "N_Tracks"
# country.data = left_join(country.data, n.tracks.by.country,
#                          by = "Country")
# 
# country.data = left_join(country.data, artists.by.country,
#                          by = "Country")
# 
# coords = map_data("world")
# coords$Country = coords$region
# coords = coords[,c("Country", "long", "lat", "group")]
# 
# country.data = left_join(country.data, coords, by = c("Country"))
# 
# top15 = function(area, type) {
#   if(type == "country")
#     cy = artists.by.country[which(artists.by.country$Country == area),]
#   if(type == "cont")
#     cy = artists.by.cont[which(artists.by.cont$Region == area),]
#   flipped.cy = data.frame(t(cy[-1]))
#   colnames(flipped.cy) = cy[, 1]
# 
#   artists = rev(rownames(flipped.cy)[order(flipped.cy[,1], decreasing = TRUE)][1:15])
#   appearances = rev(flipped.cy[order(flipped.cy[,1], decreasing = TRUE),][1:15])
# 
#   return(data.frame(artists, appearances))
# }
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~
# asia.data <- data[data$Region == "Asia",]
# europe.data <- data[data$Region == "Europe",]
# north.america.data <- data[data$Region == "North America",]
# south.america.data <- data[data$Region == "South America",]
# central.america.data <- data[data$Region == "Central America",]
# oceania.data <- data[data$Region == "Oceania",]
# 
# top.songs.asia <- asia.data[asia.data[["Position"]] %in% 1:5,]
# top.songs.asia$Artist.Track = paste(top.songs.asia$Artist,
#                                     top.songs.asia$Track.Name, sep = ".")
# top.songs.europe <- europe.data[europe.data[["Position"]] %in% 1:5,]
# top.songs.europe$Artist.Track = paste(top.songs.europe$Artist,
#                                       top.songs.europe$Track.Name, sep = ".")
# top.songs.na <- north.america.data[north.america.data[["Position"]] %in% 1:5,]
# top.songs.na$Artist.Track = paste(top.songs.na$Artist,
#                                   top.songs.na$Track.Name, sep = ".")
# top.songs.sa <- south.america.data[south.america.data[["Position"]] %in% 1:5,]
# top.songs.sa$Artist.Track = paste(top.songs.sa$Artist,
#                                   top.songs.sa$Track.Name, sep = ".")
# top.songs.ca <- central.america.data[central.america.data[["Position"]] %in% 1:5,]
# top.songs.ca$Artist.Track = paste(top.songs.ca$Artist,
#                                   top.songs.ca$Track.Name, sep = ".")
# top.songs.oceania <- oceania.data[oceania.data[["Position"]] %in% 1:5,]
# top.songs.oceania$Artist.Track = paste(top.songs.oceania$Artist,
#                                        top.songs.oceania$Track.Name, sep = ".")
# 
# table.asia <- arrange(count(top.songs.asia, "Track.Name"), desc(freq))[1:50,]
# table.europe <- arrange(count(top.songs.europe, "Track.Name"), desc(freq))[1:50,]
# table.na <- arrange(count(top.songs.na, "Track.Name"), desc(freq))[1:50,]
# table.sa <- arrange(count(top.songs.sa, "Track.Name"), desc(freq))[1:50,]
# table.ca <- arrange(count(top.songs.ca, "Track.Name"), desc(freq))[1:50,]
# table.oceania <- arrange(count(top.songs.oceania, "Track.Name"), desc(freq))[1:50,]
# 
# cum.table <- vector(mode = "list", length = 6)
# names(cum.table) <- c("Asia", "Europe", "North America", "South America",
#                       "Central America", "Oceania")
# cum.table$'Asia' <- table.asia
# cum.table$'Europe' <- table.europe
# cum.table$'North America' <- table.na
# cum.table$'South America' <- table.sa
# cum.table$'Central America' <- table.ca
# cum.table$'Oceania' <- table.oceania
# #~~~~~~~~~~~~~~~~~~~~~~
# top.artists.asia <- arrange(ddply(top.songs.asia, .(Artist),
#                                   function(x) sum(x$Streams))[-1,], desc(V1))[1:25,]
# top.songs.asia.filtered <- top.songs.asia[top.songs.asia$Artist %in%
#                                             top.artists.asia$Artist,]
# top.songs.asia.grouped <- ddply(top.songs.asia.filtered, .(Artist.Track),
#                                 function(x) sum(x$Streams))
# top.songs.asia.grouped$Artist <- vector(mode = "character",
#                                         length = nrow(top.songs.asia.grouped))
# top.songs.asia.grouped$Track <- vector(mode = "character",
#                                        length = nrow(top.songs.asia.grouped))
# for (i in 1:nrow(top.songs.asia.grouped)) {
#   top.songs.asia.grouped[i, "Artist"] =
#     strsplit(top.songs.asia.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][1]
#   top.songs.asia.grouped[i, "Track"] =
#     strsplit(top.songs.asia.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][2]
# }
# colnames(top.songs.asia.grouped)[2] = "Streams"
# 
# top.artists.europe <- arrange(ddply(top.songs.europe, .(Artist),
#                                     function(x) sum(x$Streams))[-1,], desc(V1))[1:25,]
# top.songs.europe.filtered <- top.songs.europe[top.songs.europe$Artist %in%
#                                                 top.artists.europe$Artist,]
# top.songs.europe.grouped <- ddply(top.songs.europe.filtered, .(Artist.Track),
#                                   function(x) sum(x$Streams))
# top.songs.europe.grouped$Artist <- vector(mode = "character",
#                                           length = nrow(top.songs.europe.grouped))
# top.songs.europe.grouped$Track <- vector(mode = "character",
#                                          length = nrow(top.songs.europe.grouped))
# for (i in 1:nrow(top.songs.europe.grouped)) {
#   top.songs.europe.grouped[i, "Artist"] =
#     strsplit(top.songs.europe.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][1]
#   top.songs.europe.grouped[i, "Track"] =
#     strsplit(top.songs.europe.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][2]
# }
# colnames(top.songs.europe.grouped)[2] = "Streams"
# 
# top.artists.na <- arrange(ddply(top.songs.na, .(Artist),
#                                 function(x) sum(x$Streams))[-1,], desc(V1))[1:25,]
# top.songs.na.filtered <- top.songs.na[top.songs.na$Artist %in%
#                                         top.artists.na$Artist,]
# top.songs.na.grouped <- ddply(top.songs.na.filtered, .(Artist.Track),
#                               function(x) sum(x$Streams))
# top.songs.na.grouped$Artist <- vector(mode = "character",
#                                       length = nrow(top.songs.na.grouped))
# top.songs.na.grouped$Track <- vector(mode = "character",
#                                      length = nrow(top.songs.na.grouped))
# for (i in 1:nrow(top.songs.na.grouped)) {
#   top.songs.na.grouped[i, "Artist"] =
#     strsplit(top.songs.na.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][1]
#   top.songs.na.grouped[i, "Track"] =
#     strsplit(top.songs.na.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][2]
# }
# colnames(top.songs.na.grouped)[2] = "Streams"
# 
# top.artists.sa <- arrange(ddply(top.songs.sa, .(Artist),
#                                 function(x) sum(x$Streams))[-1,], desc(V1))[1:25,]
# top.songs.sa.filtered <- top.songs.sa[top.songs.sa$Artist %in%
#                                         top.artists.sa$Artist,]
# top.songs.sa.grouped <- ddply(top.songs.sa.filtered, .(Artist.Track),
#                               function(x) sum(x$Streams))
# top.songs.sa.grouped$Artist <- vector(mode = "character",
#                                       length = nrow(top.songs.sa.grouped))
# top.songs.sa.grouped$Track <- vector(mode = "character",
#                                      length = nrow(top.songs.sa.grouped))
# for (i in 1:nrow(top.songs.sa.grouped)) {
#   top.songs.sa.grouped[i, "Artist"] =
#     strsplit(top.songs.sa.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][1]
#   top.songs.sa.grouped[i, "Track"] =
#     strsplit(top.songs.sa.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][2]
# }
# colnames(top.songs.sa.grouped)[2] = "Streams"
# 
# top.artists.ca <- arrange(ddply(top.songs.ca, .(Artist),
#                                 function(x) sum(x$Streams))[-1,], desc(V1))[1:25,]
# top.songs.ca.filtered <- top.songs.ca[top.songs.ca$Artist %in%
#                                         top.artists.ca$Artist,]
# top.songs.ca.grouped <- ddply(top.songs.ca.filtered, .(Artist.Track),
#                               function(x) sum(x$Streams))
# top.songs.ca.grouped$Artist <- vector(mode = "character",
#                                       length = nrow(top.songs.ca.grouped))
# top.songs.ca.grouped$Track <- vector(mode = "character",
#                                      length = nrow(top.songs.ca.grouped))
# for (i in 1:nrow(top.songs.ca.grouped)) {
#   top.songs.ca.grouped[i, "Artist"] =
#     strsplit(top.songs.ca.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][1]
#   top.songs.ca.grouped[i, "Track"] =
#     strsplit(top.songs.ca.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][2]
# }
# colnames(top.songs.ca.grouped)[2] = "Streams"
# 
# top.artists.oceania <- arrange(ddply(top.songs.oceania, .(Artist),
#                                      function(x) sum(x$Streams))[-1,], desc(V1))[1:25,]
# top.songs.oceania.filtered <- top.songs.oceania[top.songs.oceania$Artist %in%
#                                                   top.artists.oceania$Artist,]
# top.songs.oceania.grouped <- ddply(top.songs.oceania.filtered, .(Artist.Track),
#                                    function(x) sum(x$Streams))
# top.songs.oceania.grouped$Artist <- vector(mode = "character",
#                                            length = nrow(top.songs.oceania.grouped))
# top.songs.oceania.grouped$Track <- vector(mode = "character",
#                                           length = nrow(top.songs.oceania.grouped))
# for (i in 1:nrow(top.songs.oceania.grouped)) {
#   top.songs.oceania.grouped[i, "Artist"] =
#     strsplit(top.songs.oceania.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][1]
#   top.songs.oceania.grouped[i, "Track"] =
#     strsplit(top.songs.oceania.grouped[i, "Artist.Track"],
#              split = ".", fixed = TRUE)[[1]][2]
# }
# colnames(top.songs.oceania.grouped)[2] = "Streams"

#~~~~~~~~~~~~~~~~~~~~~

# SHINY APP CODE
# ===


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Spotify Music Streaming"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("th")),
      menuItem("Top Songs", icon = icon("th"),
               menuSubItem("Based on Streams", tabName = "top_song_stream"),
               menuSubItem("Based on Days in Top 5", tabName = "top_song_day")),
      menuItem("Country vs Region", tabName = "country_v_region", icon = icon("th")),
      menuItem("Streaming Over Time", icon = icon("th"),
               menuSubItem("By Region", tabName = "time_by_region"),
               menuSubItem("Compare Two Countries", tabName = "time_by_country"),
               menuSubItem("Artist on Top 100 Daily List", tabName = "time_200_list")),
      menuItem("Artist Song Proportions", tabName = "artist_prop_song", 
               icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "introduction",
              h2("Introduction"),
              br(),
              p("Music streaming has become the new way of listening to music 
                and Spotify has become one of the most successful platforms 
                for such a service. With so many people on a single platform 
                comes the possibility to analyze listening behaviors to study
                the spread of music."),
              br(),
              p("In this project, we attempt to answer some questions such 
                as:"),
              p("  - How do artists do in a particular country as compared
                to that continent?"),
              p("  - How does the stream amount of a particular artist's song 
                changed over time between two countries?"),
              p("  - In a specific region, how many days have the top songs
                stayed at the top?"),
              p("  - In a specific region, what songs comprise of the artist's
                total stream and what proportion does each song take?"),
              p("  - In a specific region, what are the top songs by stream
                count?"),
              p("  - How has the total amount of streams changed over time in a 
                particular region."),
              p("  - How has an artist's top songs changed in their position
                throughout the year."),
              br(),
              p("Dataset from: ", 
                a("https://www.kaggle.com/edumucelli/spotifys-worldwide-daily-song-ranking")),
              br(),
              p("By: Julie Kim, Austin Yu, Joshua Huang, Bryan Yan")
              ),
      
      # First tab content
      tabItem(tabName = "overview",
              h2("Overview"),
              p("A map overview of the countries that we have data for and
                the amount of streams, tracks, and artists for those 
                countries"),
              p("Countries that have larger numbers of
                artists and tracks have more variety in their top Spotify 
                charts, while countries with smaller numbers tend listen to
                similar songs and artists"),
              
              fluidPage(
                inputPanel(
                  radioButtons("variable", label = "Fill Variable:",
                               choiceNames = c("Number of Streams", 
                                               "Number of Tracks", 
                                               "Number of Artists"),
                               choiceValues = c("Total_Streams", "N_Tracks", 
                                                "N_Artists"))
                  ),
                plotlyOutput("world_map")
                  )
              ),
      
      # Second tab content
      tabItem(tabName = "country_v_region",
              p("A comparison of top 15 artists in selected country compared
                with their respective region."),
              p("The purpose of this graphic is to investigate how much
                countries tend to differentiate from their regions in music
                ratings"),
              p("Top 15 artists are calculated by number of appearances on
                the rankings database"),
              fluidPage(
                inputPanel(
                  selectInput("country", label = "Country:",
                              choices = sort(names(countries)), 
                              selected = "Asia")
                ),
                plotOutput("cplot")
              )
      ),
      
      # Third tab content
      tabItem(tabName = "time_by_country",
              h2("Song Streams Over Time by Country"),
              p("Now that you've seen how song streaming changes by region, we
                also wanted to see how song streams change by country and for
                specific songs. This gives us some insight as to what
                songs different countries are listening to and how this compares
                to other countries."),
              br(),
              p("Selecting an artist will change which songs you can
                select from. Depending on the song/artist selection along with the
                countries selected, you may only get results for 1 country 
                (if no results for the other country were found) or no results at 
                all for both countries."),
              fluidPage(
                inputPanel(
                  selectInput("country1", label = "Country",
                              choices = sort(unique(data$Country)),
                              selected = "USA"),
                  
                  selectInput("country2", label = "Country 2",
                              choices = sort(unique(data$Country)),
                              selected = "Canada"),
                  
                  # textInput("songTime", label = "Song Name",
                  #           value = "rockstar"),
                  # selectInput("artistTime", label = "Artist Name",
                  #             choices = unique(artists),
                  #             selected = "Post Malone"),
                  htmlOutput("artistTime"),
                  htmlOutput("songTime"),
                  br()
                ),
                plotlyOutput("streams_over_time")
              )
              ),
      
      # Fourth tab content
      tabItem(tabName = "top_song_day",
              h2("Top Songs by Number of Days in Top 5"),
              p("Here we see the top songs of a region in terms of how many 
                days they remained in the top 5."),
              br(),
              p("You can pick the region that you are interested in and adjust
                the top n songs that you would like displayed."),
              fluidPage(
                inputPanel(
                  selectInput("region", label = "Region:",
                              choices = c("North America", "South America", "Central America",
                                          "Asia", "Europe", "Oceania"), selected = "North America"),
                  
                  sliderInput("top_n", label = "Top n songs by days on top 5",
                              min = 5, max = 50, value = 10, step = 5)
                ),
                plotOutput("top_songs_plot")
              )
              ),
      
      # Fifth tab content
      tabItem(tabName = "artist_prop_song",
              h2("Artist Song Proportions"),
              p("On this page we can view what songs comprise of the artist's
                total stream and what proportion does each song take"),
              br(),
              p("When changing the region, the dropdown options for artists
                will automatically change to only include those that are in that
                subset of the data."),
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
              ),
      
      # Sixth tab content
      tabItem(tabName = "top_song_stream",
              fluidPage(
                inputPanel(
                  selectInput("region6", label = "Region",
                              choices = c("North America", "South America",
                                          "Central America",
                                          "Asia", "Europe", "Oceania"),
                              selected = "North America")),
                plotOutput("top_songs")
              )
      ),
      
      # Seventh tab content
      tabItem(tabName = "time_by_region",
              fluidPage(
                inputPanel(
                  selectInput("region7", label = "Region",
                              choices = c("North America", "South America",
                                          "Central America",
                                          "Asia", "Europe", "Oceania"),
                              selected = "North America")),
                plotOutput("songs_time")
              )
      ),
      
      # Eighth tab content
      tabItem(tabName = "time_200_list",
              h2("Artist Performance Over Time"),
              p("Now that we've seen how overall streaming numbers were, let's focus
                on the artists. How long did specific songs stay on the Top 100
                list? We'll look at how artists have performed on this list and
                maybe see if we can find reasons for spikes during specific
                times on the charts. If the artist was never on the Top 100 list,
                then you'll receive no visual results."),
              fluidPage(
                inputPanel(
                  selectInput("country8", label = "Country",
                              choices = sort(unique(data$Country)),
                              selected = "USA"),
                  
                  selectInput("artist8", label = "Artist Name",
                              choices = sort(unique(data$Artist)),
                              selected = "Ed Sheeran")
                ),
                plotlyOutput("artistSpec")
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
      scale_fill_distiller(palette = "Spectral") +
      labs(title = "Top 30 Ranking Spotify Tracks Worldwide",
           x = "", 
           y = "") +
      theme(panel.background = element_rect(fill = "darkgrey"),
            panel.grid.major = element_line(color = "darkgrey"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
    
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
    
    top15 = function(area, type) {
      if(type == "country") 
        cy = artists.by.country[which(artists.by.country$Country == area),]
      if(type == "cont") 
        cy = artists.by.cont[which(artists.by.cont$Region == area),]
      flipped.cy = data.frame(t(cy[-1]))
      colnames(flipped.cy) = cy[, 1]
      
      artists = rev(rownames(flipped.cy)[order(flipped.cy[,1], decreasing = TRUE)][1:15])
      appearances = rev(flipped.cy[order(flipped.cy[,1], decreasing = TRUE),][1:15])
      
      return(data.frame(artists, appearances))
    }
    
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
                      top = "Top 15 Artists Spotify by Country\ncompared with region")
    print(p)
  })
  
  # Graph 3
  
  output$artistTime <- renderUI({
    selectInput(
      inputId = "artistTime", 
      label = "Artist: ",
      choices = artists,
      selected = "Post Malone")
  })
  
  
  output$songTime <- renderUI({
    available <- unique(data[data$Artist == input$artistTime, "Track.Name"])
    selectInput(
      inputId = "songTime", 
      label = "Song: ",
      choices = available,
      selected = available[1])
    
  })
  
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
  
  output$streams_over_time <- renderPlotly({
    subsetOfData <- dataSub()
    if (nrow(subsetOfData) == 0) {
      p <- ggplot() +
        annotate("text",
                 x = 4, y = 25, size = 8, label = "No Results for 
                 Input Combination") +
        theme_void() +
        labs(x = NULL, y = NULL)
    }
    else {
      p <- ggplot(subsetOfData, aes(x = as.Date(Date), y = Streams,
                                    color = Country, group = Country)) +
        scale_x_date(date_labels = "%b %d %y") +
        scale_y_continuous(labels = comma) +
        geom_point() +
        geom_line() +
        labs(title = "Song Streams by Country Over Time",
             x = "Date",
             y = "Number of Streams") +
        theme(axis.title.y =
                element_text(margin = margin(r = 50)),
              axis.title.x =
                element_text(margin = margin(t = 20)))
    }
    ggplotly(p)
  })
  
  # Graph 4
  output$top_songs_plot <- renderPlot({
    p <- ggplot(eval(parse(text = paste("cum.table$", "'", input$region, "'",
                                        sep = "")))[1:input$top_n,], aes(x = Track.Name, y = freq)) +
      geom_bar(stat = "identity", fill = "green") +
      labs(title = "Amount of Days Song has been in Top 5 Rankings in 2017",
           x = "Song Name", y = "Number of Days") + 
      coord_flip()
    
    p
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
               width = .5) + scale_y_continuous(labels = comma)
    ggplotly(p)
  })
  
  
  # Graph 6
  dataSub6 <- reactive({
    data.c = data[which(data$Region == input$region6),]
    data.c = data.c = aggregate(data.c$Streams, by=list(Name=data.c$Track.Name), FUN=sum)
    head(arrange(data.c,desc(data.c$x)), 10)
  })
  output$top_songs <- renderPlot({
    ggplot(dataSub6(), aes(x = Name, y = x)) +
      geom_bar(stat = 'identity') +
      #scale_y_continuous(labels = comma) +
      labs(title = "Top Songs by Country",
           x = "Song",
           y = "Number of Streams")  +
      theme(axis.title.y =
              element_text(margin = margin(r = 20)),
            axis.title.x =
              element_text(margin = margin(t = 20)))
  })
  
  # Graph 7
  dataSub7 <- reactive({
    data.d = data[which(data$Region == input$region7),]
    aggregate(data.d$Streams, by=list(Category=data.d$Date), FUN=sum)
  })
  output$songs_time <- renderPlot({
    ggplot(dataSub7(), aes(x = as.Date(Category), y = x)) + geom_point() +
      geom_line() +
      labs(title = "Song Streams by Region Over Time",
           x = "Date",
           y = "Number of Streams") +
      theme(axis.title.y =
              element_text(margin = margin(r = 20)),
            axis.title.x =
              element_text(margin = margin(t = 20)))
  })
  
  # Graph 8
  
  artistData8 <- reactive({
    artist_daily <- data %>%
      filter(Country == input$country8, Artist == input$artist8, Position <= 100)
    artist_20 <- artist_daily %>%
      group_by(`Track.Name`) %>%
      summarise(n_daily = n()) %>%
      filter(n_daily >= 20) %>%
      select(`Track.Name`)
    artist_20 <- artist_20 %>% collect %>% .[["Track.Name"]]
    artist_daily %>% filter(`Track.Name` %in% artist_20)
  })
  
  output$artistSpec <- renderPlotly({
    subData <- artistData8()
    if (nrow(subData) == 0) {
      p <- ggplot() +
        annotate("text",
                 x = 4, y = 25, size = 8, label = "No Results") +
        theme_void() +
        labs(x = NULL, y = NULL)
    }
    else {
      p <- ggplot(subData, aes(x = as.Date(Date), y = Position, col = `Track.Name`)) +
        geom_point(alpha = 0.7, size = 3) +
        geom_line() +
        scale_y_reverse(breaks = seq(0, 100, 10)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
        labs(title = "Artist on Top 100 Daily List in Country",
             x = "Date",
             col = "Track Name")
    }
    ggplotly(p)
  })
}

shinyApp(ui, server)