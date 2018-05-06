# Final Project
# Maria Ren

# The project libraries are listed below

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

# install.packages("devtools")
# install.packages("spotifyr")
# install.packages("billboard")
# install.packages("httr")
# install.packages("miscTools")
# install.packages("ggplot2")
# install.packages("car")
# install.packages("ggvis")
# install.packages("plotly")

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

liveness <- ggplot(top, aes(top$liveness,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Liveness vs. Valence")

tempo <- ggplot(top, aes(top$tempo,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Tempo vs. Valence")

duration <- ggplot(top, aes(top$duration_ms,top$valence)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(se = FALSE) +
  labs(title = "Duration vs. Valence")



top_orig <- read.csv("Billboard_Top_1.csv")

danceability_year <- ggplot(top, aes(fill=top$year, y=top_orig$danceability, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()


energy_year <- ggplot(top, aes(fill=top$year, y=top_orig$energy, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()


loudness_year <- ggplot(top, aes(fill=top$year, y=top_orig$loudness, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()


speechiness_year <- ggplot(top, aes(fill=top$year, y=top_orig$speechiness, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()

acousticness_year <- ggplot(top, aes(fill=top$year, y=top_orig$acousticness, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()


instrumentalness_year <- ggplot(top, aes(fill=top$year, y=top_orig$instrumentalness, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()


liveness_year <- ggplot(top, aes(fill=top$year, y=top_orig$liveness, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()

valence_year <- ggplot(top, aes(fill=top$year, y=top_orig$valence, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()


tempo_year <- ggplot(top, aes(fill=top$year, y=top_orig$tempo, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()

duration_year <- ggplot(top, aes(fill=top$year, y=top_orig$duration_ms, x=top_orig$year)) + 
  geom_line(color="purple") +
  geom_point()



top_reduced <- subset(top_orig,select=c("track_name","artist","year","danceability",
                                   "energy","loudness","speechiness","acousticness",
                                   "instrumentalness","liveness","valence","tempo","duration_ms"))






## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Music, Emotions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "intro"),
      menuItem("Dataset Keys",tabName = "key"),
      menuItem("Spotify Audio Features", tabName = "firstplot"),
      menuItem("Audio Features vs. Valence", tabName = "secondplot"),
      menuItem("Individual Features Selections", tabName = "fourthplot"),
      menuItem("Individual Features Time Series", tabName = "thirdplot"),
      menuItem("In Depth Valence Time Series", tabName = "fifthplot"),
      menuItem("Reference Data Table", tabName = "sixthplot"),
      menuItem("Conclusion", tabName = "conclusion")
      
    )
  ),

  dashboardBody(
    tabItems(
      # Intro
      tabItem(tabName = "intro",
              # Give the page a title
              titlePanel("Music, Emotions - Project Introduction"),
              fluidRow(
                box(
                  width = 40,
                  height = 450,
                  h4(
                  p("We often say, 'I’m in the mood for a happy tune.' But what exactly makes a song happy? Or sad?"), 
                  br(),
                  p("In the twenty-first century, music has become one of the most important parts of our lives. 
                  Whether it’s classical, jazz, funk, pop, or country, the demand for music has created a billion dollar
                  music industry worldwide. Music industry revenue in turn became a significant component in the global 
                  economy, which also led to fierce competitions across record companies and other music platforms. These 
                  companies strives to satisfy the listener’s taste by creating music that fits individual tastes."), 
                  br(),
                  p("Among these music platforms, Spotify is one of the most popular digital music services that provides 
                  its users with access to millions of songs. It also gives users the ability to rate music according to their 
                  own personal preference and taste."),
                  br(),
                  p("This project uses real time and historical data extracted from the Spotify API (Developer Portal), and analyzes 
                  the different elements of a soundtrack that changes human emotions. The musical data consists of musical features 
                  and ratings for the number one songs from “Billboard Hot 100 Chart” from 1960 to 2018.")  
                  
                  )
                )
              )
           ),
      # Key
      tabItem(tabName = "key",
              # Give the page a title
              titlePanel("Dataset Keys"),
              fluidRow(
                box(
                  width = 40,
                  height = 800,
                  h4(
                    p("The Dataset extracts information from Spotify API. The following audio feature data values are 
                      needed for the project analysis.")), 
                    br(),
                   h5(
                     p(strong("Danceability:"), "Describes how a song track is suitable for dancing. Values ranges from 0 to 1,
                      with 0 being the least danceable, and 1 being the most danceable. Danceability is calculated based on music 
                      tempo, rhythm stability, beat strength. "), 
                    br(),
                    p(strong("Energy:"), "Describes level of music intensity and activity. For example, the most energetic music 
                    feels fast and loud (like death metal), while a quiet and peaceful classical music (like Chopin) would score 
                    lower on the energy scale. Calculated from loudness, onset rate, tempo, and dynamic ranges. Values ranges from 0 to 1, 
                    with 0 being least energetic, and 1 being most energetic. "),
                    br(),
                    p(strong("Loudness:"),"Describes the decibel (dB) level of a score. Values ranges from -60 to 0. With -60 being the loudest,
                    and 0 being quietest."),
                    br(),
                    p(strong("Speechiness:"),"Describes the amount of spoken words in a track. Values ranges from 0 to 1, with 0 being music having 
                    no words, and 1 being completely spoken song track. (For example, a talk show would have closer to 1, song with rap with having 
                    values above 0.66, values between 0.33 and 0.66 would be regular songs with lyrics, and values below 0.33 are most likely classical 
                    music that is completely instrumental)."),
                    br(),
                    p(strong("Acousticness:"),"Describes the level of acoustic sound (as opposed to electronic sound) detected from the soundtrack. Values 
                    ranges from 0 to 1, with 0 being entirely electronic sound, and 1 being acoustic sound."),
                    br(),
                    p(strong("Instrumentalness:"), "Describes whether the track contains vocals. ( “Ooh” and “aah” sound are treated as instrumental). Values 
                    ranges from 0 to 1, with 0 being the soundtrack having no instrumentation, and 1 being instrumental music. (For example, a rap song would 
                    have score closer to 0, and classical symphonic music would have value closer to 1). "),
                    br(),
                    p(strong("Liveness:"), "Describes whether the song track was recorded live or in a studio. Calculated from detection of audience sound in the sound 
                    track. Values ranges from 0 to 1, with values closer to 0 indicating music which were recorded in a studio setting, and with values 
                    above 0.8 indicating music recorded live. "),
                    br(),
                    p(strong("Valence:"), "Describes the happiness level of a music soundtrack. Values ranges from 0 to 1, with 0 being music tracks that are 
                    negative (sad, depressed, and angry), while 1 being music that are positive (happy, exciting and cheerful). Calculated from audience 
                    reviews and ratings. "),
                    br(),
                    p(strong("Tempo:"), "Describes the tempo of a track in beats per minute (BPM). Values ranges from 0 to around 200. With values around 50 being 
                    very slow, and around 200 being very fast."),
                    br(),
                    p(strong("Duration(Duration_ms):"),"Describes the length of the soundtrack. Calculated in milliseconds.") 
                      
                            
                           
                    
                        )
                      )
                    )
                ),
                
      # First tab content
      tabItem(tabName = "firstplot",
              fluidPage(    
                
                # Give the page a title
                titlePanel("Spotify Audio Features"),
                
                # Generate a row with a sidebar
                sidebarLayout(      
                  
                  # Define the sidebar with one input
                  sidebarPanel(
                    
                    
                    selectInput("Features_X", "*Select different audio features to view basic statistical values", 
                                c("danceability"= "danceability",
                                  "energy"="energy",
                                  "loudness"="loudness",
                                  "speechiness"="speechiness",
                                  "acousticness"="acousticness",
                                  "instrumentalness"="instrumentalness",
                                  "liveness"="liveness",
                                  "valence"="valence",
                                  "tempo"="tempo",
                                  "duration"="duration_ms")),
                    hr(),
                    helpText("Data from Spotify."),
                    
                  
                    box(
                      width = 20,
                      verbatimTextOutput("basic_stats")  
                    )
                  
                  
                    
                  ),
                  
                  # Create a spot for the plot
                  mainPanel(
                    plotOutput("featurePlot"),  
                  
                    box(
                      width = 40,
                      height = 800,
                      h4(
                        p("Description:")), 
                        br(),
                       h5(
                        p("Initial data analysis include plotting individual histograms of individual features and looking at basic statistical 
                          analysis (min, median, mean, max). "), 
                        br(),
                        h4(
                          p("Analysis: ")),
                        h5(
                        br(),
                        p("Danceability has average around 0.64, which is pretty high on the scale. This proves that on average, music that are 
                          rhythmic, upbeat- suitable for dancing - tends to be more popular among people. Similarly, energy has average around 0.59, 
                          which shows that music lovers prefer songs that consist faster tempo. The average value for tempo is 116, which in music 
                          is Allegretto - moderately fast. This also points to the same result as people tend to like more energetic/faster songs. 
                          Loudness has average -8.5, this is considerably low on the decibel level, proving that popular music tends to be not too loud. 
                          Speechiness and acousticness both has average value that are low on the scale, showing that people prefer songs that include less 
                          electronic sounds and more vocal lines. Instrumentalness and liveness both have pretty low values. This makes sense as most of 
                          the songs on the list are recorded in a studio, and are songs instead of symphonic works. The mean and median duration of songs are 
                          pretty close together- both have values around 230000 milliseconds (around 3.83 min)."),
                        br(),

                        p("The most interesting analytics comes with the feature “valence”. This is the only feature of the entire dataset that involves 
                          user input data. The other features are calculated from the tempo, sound quality of the songs themselves, while valence is calculated 
                          through user ratings (labeling whether the song feels happy or sad), user ratings do depends on all the previous audio features ( i.e. 
                          danceability, loudness, energy etc). On the histogram for valence, the max value is 0.97, and the min value is 0.1, with average value 0.60. 
                          Valence values varies a lot through the 59 songs, and the max and min each stands at the opposite ends of the spectrum."),
                        br(),
                          
                          p("The rest of the project focus on deeper analysis of valence values.") 
                           
                          )
                        )
                 
                  
                    )
                  ) 
                )
              )
            ),
      
      # Second tab content
      tabItem(tabName = "secondplot",
              # Give the page a title
              titlePanel("Audio Features vs. Valence"),
              
            fluidPage(
             
            box(plotOutput("plot1", height = 500)),
            br(),
            box(
              selectInput("Features_X1", "*Select different audio features to view plots", 
                          c("danceability"= "danceability",
                            "energy"="energy",
                            "loudness"="loudness",
                            "speechiness"="speechiness",
                            "acousticness"="acousticness",
                            "instrumentalness"="instrumentalness",
                            "liveness"="liveness",
                            "tempo"="tempo",
                            "duration"="duration_ms")),
              hr(),
              helpText("Data from Spotify.")
              ),
            
            box(
             
              h4(
                p("Description:")), 
              br(),
              h5(
                p("Focusing on analysis of valence level. Plots consist of each individual audio feature graphed on the x-axis, 
                and valence graphed on the y-axis."), 
                br(),
                h4(
                  p("Analysis: ")),
                h5(
                  br(),
                  p("When danceability, energy, tempo increases, valence increases. While for loudness, speechiness, 
                    acousticness, and duration of songs, the higher they are on the scale, the lower the valence score."),
                  br(),
                  p("According to the graphs above, songs that are determined as “happy” are generally songs that are fast, 
                    upbeat, and not so loud. While songs that fits the “sad” or more negative mood are songs that are slower, 
                  longer, and includes more spoken words. ")
                  
                      )
                  )
                )
            )
          ),
      
 
    # third tab content
      tabItem(tabName = "thirdplot",
              # Give the page a title
              titlePanel("Individual Features Time Series"),
              fluidRow(
              box(
                  
                  h4(
                    p("*Select audio feature to view individual audio feature time series plot",style = "color:blue") 
                  )
                ),
              box(
                  width = 10,
                  plotOutput("plot2",height = 250)),
                
                box(
                  selectInput("Features_X2", "Different Audio Features:", 
                              c("danceability"= "danceability",
                                "energy"="energy",
                                "loudness"="loudness",
                                "speechiness"="speechiness",
                                "acousticness"="acousticness",
                                "instrumentalness"="instrumentalness",
                                "liveness"="liveness",
                                "valence"="valence",
                                "tempo"="tempo",
                                "duration"="duration_ms")
                              )
                  
                       ),
              box(
                
                h4(
                  width = 40,
                  p("Description:")), 
                br(),
                h5(
                  p("Focusing on analysis of individual audio features. Time series plot shows the change of values
                    across time."), 
                  br(),
                  h4(
                    p("Analysis: ")),
                  h5(
                    br(),
                    p("The features that seems to follow an increasing trend is loudness. This shows that as time goes
                  on, popular music tends to be quieter (as the y-axis goes from -60 to 0, the closer loudness to 0, 
                    the quieter the songs are)."),
                    br(),
                    p("Similarly with speechiness. The songs between 2000 and 2018 seems to have many spikes in  speechiness
                      graphs. One reason could be the increasing popularity in rap music."),
                    br(),
                    p("Duration is another feature that slightly increased with time. As time went on, music got a 
                      little longer (although not by a lot)."),
                    br(),
                    p("Instrumentalness seems to decrease over time. Between 1960 to 1965, music tends to have more 
                      instrumental preludes and interludes, while after 1965, popular music tends to have more lyrics 
                      and vocal lines."),
                    br(),
                    p("All the other features seems to follow a pretty constant trend over time. ")
                    )
                 )
              
              )
            )
                
         ),
      
      # fourth tab content
      tabItem(tabName = "fourthplot",
              # Give the page a title
              titlePanel("Individual Features Selections"),  
              fluidRow(
                 
                  column(4,
                         wellPanel(
                           h4("Filter"),
                           sliderInput("valence_value", "Valence/Happiness level", 0, 1, value = c(0, 1))
                         )
                        ),
                  column(4,
                         wellPanel(
                           h4("Filter"),
                           selectInput("Features_X3", "Different Audio Features:", 
                                       c("danceability"= "danceability",
                                         "energy"="energy",
                                         "loudness"="loudness",
                                         "speechiness"="speechiness",
                                         "acousticness"="acousticness",
                                         "instrumentalness"="instrumentalness",
                                         "liveness"="liveness",
                                         "tempo"="tempo",
                                         "duration"="duration_ms"
                              )
                              )
                           )
                        ),
                    
                        box(
                          
                          h4(
                              p("*Please select valence/happiness level and audio feature, then click on 
                                Individual points on scatter plot below to view track information",style = "color:blue")) 
                          ),
                              
                              
                        
                      
                    
                  
                  
                  
                  box(
                
                  
                      
                      width = 10,
                      plotOutput("plot3",click = "plot_click"),
                      verbatimTextOutput("info")      
                           
                      
              
                        )
                       )
                    ),
                  
              
  
  
  
  # fifth tab content
    tabItem(tabName = "fifthplot",
            # Give the page a title
            titlePanel("In Depth Valence Time Series"),
              fluidRow(
                  
                  plotlyOutput("plot5")
                     
                     ),
               
                tags$blockquote("*Hover over time series chart to see information of specific tracks.",style = "color:blue"),
              
    box(
    
      h4(
        width = 40,
      p("Description:")), 
    br(),
    h5(
      p("Focusing on analysis of valence level. Time series plot shows the change of valence 
    values across time. "), 
      br(),
      h4(
        p("Analysis: ")),
      h5(
        br(),
        p("While all the songs are listed as number one sound tracks from each year’s hot billboard 
          song ratings. Valence scores varies a lot between each year for each song track."),
        br(),
        p("The happiest song (highest valence score) belongs to the song track “Joy to the World” 
          by Three Dog Night in 1971. The score has a valence score of 0.97, with danceability 0.591, 
          energy 0.726, tempo around 130. So overall, the song is fast, energetic, and suitable for dancing.  "),
        br(),
        p("The saddest song (lowest valence score) belongs to the song track “I Will Always Love You” by 
        Whitney Houston in 1993. The score has a valence score of 0.108, with danceability 0.306, energy 0.214, tempo 67.
        A significant drop in feature values from “Joy to the World”. It is a slower song, with less energy ratings. 
        Since it’s valence rating is almost 0, this song expresses a sad emotion. ")
            )
          )
        )
      ),
      
  
  
  
  
  # sixth tab content
  tabItem(tabName = "sixthplot",
          # Give the page a title
          titlePanel("Reference Data Table"),
          fluidRow(
            column(4,
                   selectInput("year",
                               "Year:",
                               c("All",
                                 unique(as.character(top_reduced$year))))
            ),
            column(4,
                   selectInput("artist",
                               "Artist:",
                               c("All",
                                 unique(as.character(top_reduced$artist))))
            ),
            column(4,
                   selectInput("track",
                               "Track:",
                               c("All",
                                 unique(as.character(top_reduced$track_name))))
              )
            ),
          
          # Create a new row for the table.
          fluidRow(
            column(5,
              DT::dataTableOutput("table",width = "100%")
            )
          )
        ),
     
  
  # Conclusion
  tabItem(tabName = "conclusion",
          # Give the page a title
          titlePanel("Project Conclusion"),
          fluidRow(
            box(
              width = 40,
              height = 500,
              h4(
                p("After filtering out and analyzing songs with valence score above 0.95 (happy) and valence
                  score below 0.15 (sad), the project concludes with the following analysis: "), 
                br(),
                p("A happy song generally has danceability rating above 0.6, energy above 0.7, loudness around -10, 
                  speechiness around 0.06, acousticness around 0.23, and tempo around 130. So if a song is fast, not 
                  too loud, have a good balance between instrumentation and vocal lyrics, it can usually stimulate 
                  happiness within human emotions, and would make people want to dance to the tune."), 
                br(),
                p("A sad song generally has danceability rating below 0.3, energy below 0.2, loudness around -15, 
                  speechiness around 0.03, acousticness around 0.80, and tempo around 60. So when we hear a song that 
                  generally involves slower tempo, filled with sounds that are more acoustic (as oppose to electronic), 
                  and with less spoken words/lyrics, it usually feels like a sad song. "),
                br(),
                p("Interestingly, sad songs seems to score higher in loudness than happy songs. Intuitively, sad songs 
                  should be quieter as they generally expresses melancholy and sorrow. Yet, some songs (like “I will always 
                  love you” Whitney Houston, which scored the lowest on valence level) gets loud during its climatic 
                  section, expressing heartfelt passions and emotions. "),
                br(),
                p("So next time, when you need to select a song that fits a certain mood, be sure to check out its audio 
                  features and valence scores! ")
                
                )
              )
            )
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
  
  output$basic_stats <- renderPrint({
    summary(top[input$Features_X])
  
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
    } else if (input$Features_X1 == "liveness") {
      liveness
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
    } else if (input$Features_X2 == "liveness") {
      liveness_year
    } else if (input$Features_X2 == "valence") {
      valence_year
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
           main = ("Valence Scatter Plot"),
           xlab = "features", 
           ylab = "Valence",
           ylim = input$valence_value)
      
    })
  
  
  
  output$info <- renderPrint({
    nearPoints(top_reduced, 
               input$plot_click, 
               xvar = input$Features_X3, 
               yvar = "valence"
              )
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
                              '</br>Liveness: ', top$liveness,
                              '</br>Tempo: ', top$tempo,
                              '</br>Duration: ', top$duration_ms))
      
   
 p
  
  })
  
 
  
    
    # The Sixth Plot
  
  
  # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      data <- top_reduced
      if (input$year != "All") {
        data <- data[data$year == input$year,]
      }
      if (input$artist != "All") {
        data <- data[data$artist == input$artist,]
      }
      if (input$track != "All") {
        data <- data[data$track_name == input$track,]
      }
      data
    }))
    
  } 
  


      
    
  


shinyApp(ui, server)

