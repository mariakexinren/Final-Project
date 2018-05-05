

library(shiny)
library(devtools)
library(spotifyr)
library(tidyverse)
library(dplyr)
library(billboard)
library(httr)
library(miscTools)
library(ggplot2)
library(car)
library(ggvis)
library(plotly)

top <- read.csv("Billboard_Top_1.csv")

for (i in 1:nrow(top)) { 
  if (top$year[i] < 1970) {
    top$year[i] <- str_sub("1960-1969") 
    
  }else if (top$year[i] < 1980){
    top$year[i] <- str_sub("1970-1979") 
    
  }else if (top$year[i] < 1990){
    top$year[i] <- str_sub("1980-1989") 
    
  }else if (top$year[i] < 2000){
    top$year[i] <- str_sub("1990-1999")
    
  }else if (top$year[i] < 2010){
    top$year[i] <- str_sub("2000-2009")
    
  }else if (top$year[i] < 2019){
    top$year[i] <- str_sub("2009-2018")
  }
}

danceability <- ggplot(top, aes(top$danceability,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Danceability vs. Valence")

energy <- ggplot(top, aes(top$energy,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Energy vs. Valence")

loudness <- ggplot(top, aes(top$loudness,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Loudness vs. Valence")

speechiness <- ggplot(top, aes(top$speechiness,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Speechiness vs. Valence")

acousticness <- ggplot(top, aes(top$acousticness,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Acousticness vs. Valence")

instrumentalness <- ggplot(top, aes(top$instrumentalness,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Instrumentalness vs. Valence")

tempo <- ggplot(top, aes(top$tempo,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Tempo vs. Valence")

duration <- ggplot(top, aes(top$duration_ms,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Duration vs. Valence")



top_orig <- read.csv("Billboard_Top_1.csv")

danceability_year <- ggplot(top, aes(fill=top$year, y=top$danceability, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$danceability, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")


energy_year <- ggplot(top, aes(fill=top$year, y=top$energy, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$energy, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")


loudness_year <- ggplot(top, aes(fill=top$year, y=top$loudness, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$loudness, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")


speechiness_year <- ggplot(top, aes(fill=top$year, y=top$speechiness, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$speechiness, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")

acousticness_year <- ggplot(top, aes(fill=top$year, y=top$acousticness, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$acousticness, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")

instrumentalness_year <- ggplot(top, aes(fill=top$year, y=top$instrumentalness, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$instrumentalness, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")

valence_year <- ggplot(top, aes(fill=top$year, y=top$valence, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$valence, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")

tempo_year <- ggplot(top, aes(fill=top$year, y=top$tempo, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$tempo, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")

duration_year <- ggplot(top, aes(fill=top$year, y=top$duration_ms, x=top$year)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(top, aes(fill=as.factor(top_orig$year), y=top_orig$duration_ms, x=as.factor(top_orig$year))) + 
  geom_bar(position="dodge", stat="identity")










## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Spotify Music Comparisons"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("dashboard 1", tabName = "firstplot"),
      menuItem("dashboard 2", tabName = "secondplot"),
      menuItem("dashboard 3", tabName = "thirdplot"),
      menuItem("dashboard 4", tabName = "fourthplot"),
      menuItem("dashboard 5", tabName = "fifthplot")
      
    )
  ),

  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "firstplot",
              fluidPage(    
                
                # Give the page a title
                titlePanel("Spotify Audio Features"),
                
                # Generate a row with a sidebar
                sidebarLayout(      
                  
                  # Define the sidebar with one input
                  sidebarPanel(
                    
                    selectInput("Features_X", "Different X Features:", 
                                c("danceability"= "danceability",
                                  "energy"="energy",
                                  "loudness"="loudness",
                                  "speechiness"="speechiness",
                                  "acousticness"="acousticness",
                                  "instrumentalness"="instrumentalness",
                                  "valence"="valence",
                                  "tempo"="tempo",
                                  "duration"="duration_ms")),
                    hr(),
                    helpText("Data from top hot 100 billboard.")
                  ),
                  
                  # Create a spot for the barplot
                  mainPanel(
                    plotOutput("featurePlot")  
                  )
                )
              )
            ),
      
      # Second tab content
      tabItem(tabName = "secondplot",

            fluidRow(
              box(plotOutput("plot1", height = 250)),
            box(
              selectInput("Features_X1", "Different X Features:", 
                          c("danceability"= "danceability",
                            "energy"="energy",
                            "loudness"="loudness",
                            "speechiness"="speechiness",
                            "acousticness"="acousticness",
                            "instrumentalness"="instrumentalness",
                            "tempo"="tempo",
                            "duration"="duration_ms")),
              hr(),
              helpText("Data from top hot 100 billboard.")
              )
            )
          ),
      
 
    # third tab content
      tabItem(tabName = "thirdplot",
          fluidRow(
              box(
                  width = 10,
                  plotOutput("plot2",height = 250)),
                
                box(
                  selectInput("Features_X2", "Different X Features:", 
                              c("danceability"= "danceability",
                                "energy"="energy",
                                "loudness"="loudness",
                                "speechiness"="speechiness",
                                "acousticness"="acousticness",
                                "instrumentalness"="instrumentalness",
                                "tempo"="tempo",
                                "duration"="duration_ms")
                              )
                  
                       )
                
                )
         ),
      
      # fourth tab content
      tabItem(tabName = "fourthplot",
            # fluidPage(
            #     titlePanel("Music Data"),
                fluidRow(
                
                  column(5,
                         wellPanel(
                           h4("Filter"),
                           sliderInput("valence_value", "Happiness level", 0, 1, value = c(0, 1))
                        
                         ),
                         wellPanel(
                           selectInput("Features_X3", "Different X Features:", 
                                       c("danceability"= "danceability",
                                         "energy"="energy",
                                         "loudness"="loudness",
                                         "speechiness"="speechiness",
                                         "acousticness"="acousticness",
                                         "instrumentalness"="instrumentalness",
                                         "tempo"="tempo",
                                         "duration"="duration_ms"
                          )
                        )
                      )
                    ),
                  
                  box(
                      width = 10,
                      plotOutput("plot3"))
                            
                           
                      )
                    
                  
                ),
              
  
  
  
  # fifth tab content
    tabItem(tabName = "fifthplot",
      
              fluidRow(
                  plotlyOutput("plot5")
                     
                     ),
               
                tags$blockquote("Hover over time series chart to fix a specific date. Correlation chart will update with historical 
                          correlations (time span will be hover date +/- selected window length)")
              )
            )
      ) 
  )
  
      
            
         
       
    


















server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$featurePlot <- renderPlot({
    
    # Render a plot
    barplot(top[,input$Features_X],
            main = "Features vs. Year",
            xlab = "Year", 
            ylab = "Feature Frequency")
  })

  output$plot1 <- renderPlot({
    
    # Render a plot
    if (input$Features_X1 == "danceability") {
      danceability
    } else if (input$Features_X1 == "energy") {
      energy
    } else if (input$Features_X1 == "loudness") {
      loudness
    } else if (input$Features_X1 == "speechiness") {
      speechiness
    } else if (input$Features_X1 == "acousticness") {
      acousticness
    } else if (input$Features_X1 == "instrumentalness") {
      instrumentalness
    } else if (input$Features_X1 == "tempo") {
      tempo
    } else if (input$Features_X1 == "duration_ms") {
      duration
    }
  
    })
  
  
  
  output$plot2 <- renderPlot({
    

      # Render a plot
    if (input$Features_X2 == "danceability") {
      danceability_year
    } else if (input$Features_X2 == "energy") {
      energy_year
    } else if (input$Features_X2 == "loudness") {
      loudness_year
    } else if (input$Features_X2 == "speechiness") {
      speechiness_year
    } else if (input$Features_X2 == "acousticness") {
      acousticness_year
    } else if (input$Features_X2 == "instrumentalness") {
      instrumentalness_year
    } else if (input$Features_X2 == "tempo") {
      tempo_year
    } else if (input$Features_X2 == "duration_ms") {
      duration_year
    }
      
    
  })
  
  
  
  
  # The fourth Section
  
    # Define a reactive expression for the document term matrix

  output$plot3 <- renderPlot({
      plot(top[,input$Features_X3],top$valence,
           main = "Scatter Plot",
           xlab = "features", 
           ylab = "Valence",
           ylim = input$valence_value)
      
    })
  
  
  # The fifth Plot 
  
  # Set some colors
  plotcolor <- "#F5F1DA"
  papercolor <- "#E3DFC8"
  
  # Plot time series chart 
  output$plot5 <- renderPlotly({
    p <- plot_ly(source = "source") %>% 
      add_lines(data = top_orig, x = top_orig$year, y = top$valence, 
                color = top_orig$year, mode = "lines", line = list(width = 3),
                text = ~paste('</br>Year: ', top_orig$year,
                              '</br>Valence: ', top$valence,
                              '</br></br>Track: ', top$track_name, 
                              '</br>Artist: ', top$artist,
                              '</br>Danceability: ', top$danceability,
                              '</br>Energy: ', top$energy,
                              '</br>Loudness: ', top$loudness,
                              '</br>Speechiness: ', top$speechiness,
                              '</br>Acousticness: ', top$acousticness,
                              '</br>Instrumentalness: ', top$instrumentalness,
                              '</br>Tempo: ', top$tempo,
                              '</br>Duration: ', top$duration_ms))
      
   
 p
  
  })
  
  
  
}

      
    
  


shinyApp(ui, server)

