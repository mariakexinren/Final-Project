library(devtools)
library(spotifyr)
library(tidyverse)
library(dplyr)
library(billboard)
library(httr)
library(miscTools)


# Obtain Spotify access token
# Set Spotify client ID and client secret
# Obtain real time data from Spotify
Sys.setenv(SPOTIFY_CLIENT_ID = '0da3921447fa4e3099913296e99b43d6')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '216f8762ba494bb7a626becb67d6a0ab')
access_token <- get_spotify_access_token()


# Obtain historical data set: Billboards Hot 100 list (containing top 100 most 
# popular songs from 1960 to 2016. This dataset comes from the R library "billboard",
# and the data was scraped from 
# 'https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_' and
# https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_1960.)

historical <- wiki_hot_100s %>% 
  rename(track_name = "title")

# Filter out top song from each year from 1960-2016
top <- as.tibble(historical) %>% filter(no=="1") %>% 
  select(track_name, artist, year)


# Form a complete list of top song tracks from 1960 to 2017
top_complete <- top %>% 
  add_row(track_name = "Shape of You", artist = "Ed Sheeran", year = "2017") %>% 
  as.tibble()


# Clean up and tidying data
# Include track features into the table
track <- spotify_track_data %>% rename(artist  = 'artist_name')
update_track_name <- gsub("\\-.*","",track$track_name) %>%
  as.tibble() 
update_track_name[1,] <- "Theme From A Summer Place"
track$track_name <- update_track_name
track$artist[1] <- "Percy Faith"
track$track_name <- as.tibble(track$track_name)

join_list <- left_join(top_complete, track, by = c("artist","year"))
list <- distinct(join_list,year, .keep_all=TRUE)
list <- subset(list, select = -c(track_name.y,artist_id,explicit,type,uri,track_href,analysis_url,key,mode,time_signature)) %>% 
  rename(track_name = "track_name.x",track_uri = "track_id")







# Still need data for the following artist and song track 
incomplete_index <- which(is.na(list$track_uri), arr.ind=TRUE)
list[incomplete_index,] %>% select(track_name,artist,year)


# Pull data from Spotify API 
# Write functions get_track and get_features 

get_track <- function(artist) {
  artists <- get_artists(artist)
  albums <- get_albums(artists$artist_uri[1])
  return(get_album_tracks(albums))
}

get_features <- function(artist) {
  artists <- get_artists(artist)
  albums <- get_albums(artists$artist_uri[1])
  tracks <- get_album_tracks(albums)
  artists_audio_features <- get_track_audio_features(tracks)
  return(artists_audio_features)
}




# track name: Sugar Shack
# artist: The Fireballs 
# year: 1963

song_list <- get_track("The Fireballs")
feature_list <- get_features("The Fireballs") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Sugar Shack")
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data)
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Sugar Shack"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}


# track name: Wooly Bully
# artist: Sam the Sham and the Pharaohs
# year: 1965 

song_list <- get_track("Sam the Sham and the Pharaohs")
feature_list <- get_features("Sam the Sham and the Pharaohs") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Wooly Bully")
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri) 
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Wooly Bully"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}





# track name: Ballad of the Green Berets  
# artist: SSgt. Barry Sadler 
# year: 1966 

song_list <- get_track("Barry Sadler")
feature_list <- get_features("Barry Sadler") 
song_data <- as.tibble(song_list) %>% filter(track_name=="The Ballad Of The Green Berets")
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data)
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Ballad of the Green Berets"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}




# track name: Sugar, Sugar 
# artist: The Archies       
# year: 1969 

song_list <- get_track("The Archies")
feature_list <- get_features("The Archies") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Sugar, Sugar")
song_data
# the song does not exist in the Spotify API with the band name "The Archies"
# But Spotify has features from Ron Dante (the lead singer)
song_list <- get_track("Ron Dante")
feature_list <- get_features("Ron Dante") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Sugar Sugar") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data)
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Sugar, Sugar"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}

# track name: Tie a Yellow Ribbon Round the Ole Oak Tree
# artist:Tony Orlando and Dawn
# year: 1973

song_list <- get_track("Tony Orlando and Dawn")
feature_list <- get_features("Tony Orlando and Dawn") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Tie a Yellow Ribbon Round the Ole Oak Tree")
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data)
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Tie a Yellow Ribbon Round the Ole Oak Tree"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}


# track name: My Sharona
# artist:The Knack
# year: 1979

song_list <- get_track("The Knack")
feature_list <- get_features("The Knack") 
song_data <- as.tibble(song_list) %>% filter(track_name=="My Sharona")
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,]) %>% slice(1)
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "My Sharona"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}


# track name: Careless Whisper
# artist: Wham! featuring George Michael
# year: 1985

song_list <- get_track("Wham")
feature_list <- get_features("Wham") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Careless Whisper") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Careless Whisper"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}

# track name: That's What Friends Are For
# artist: Dionne and Friends (Dionne Warwick, Gladys Knight, Elton John)
# year: 1986

song_list <- get_track("Dionne Warwick")
feature_list <- get_features("Dionne Warwick") 
song_data <- as.tibble(song_list) %>% filter(track_name=="That's What Friends Are For") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data)
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "That's What Friends Are For"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}



# track name: Gangsta's Paradise
# artist: Coolio featuring L.V.
# year:  1995 

song_list <- get_track("Coolio")
feature_list <- get_features("Coolio") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Gangsta's Paradise") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Gangsta's Paradise"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}






# track name: Macarena (Bayside Boys Mix) 
# artist:Los del Río  
# year: 1996

song_data <- as.tibble(spotify_track_data) %>% 
  filter(track_name=="Macarena - Bayside Boys Remix") %>% 
  slice(1)
get_feature_data <- song_data
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Macarena (Bayside Boys Mix)"){
    list$track_uri[i] <- str_sub(get_feature_data$track_id)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}






# track name: Yeah!
# artist:Usher featuring Lil Jon and Ludacris 
# year: 2004

song_list <- get_track("Usher")
feature_list <- get_features("Usher") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Yeah!") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Yeah!"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}



# track name: Low
# artist: Flo Rida featuring T-Pain   
# year: 2008

song_list <- get_track("Flo Rida")
feature_list <- get_features("Flo Rida") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Low (feat T-Pain)")
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri) %>% slice(1)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Low"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}




# track name: Somebody That I Used to Know   
# artist:Gotye featuring Kimbra 
# year: 2012 

song_list <- get_track("Gotye")
feature_list <- get_features("Gotye") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Somebody That I Used To Know") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Somebody That I Used to Know"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}



# track name: Thrift Shop 
# artist:Macklemore and Ryan Lewis featuring Wanz 
# year: 2015 

song_list <- get_track("Ryan Lewis")
feature_list <- get_features("Ryan Lewis") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Thrift Shop (feat. Wanz)") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Thrift Shop"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}


# track name: Uptown Funk
# artist: Mark Ronson featuring Bruno Mars  
# year: 2015 

song_list <- get_track("Mark Ronson")
feature_list <- get_features("Mark Ronson") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Uptown Funk") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Uptown Funk"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}



# track name: Love Yourself 
# artist:Justin Bieber
# year: 2016


song_list <- get_track("Justin Bieber")
feature_list <- get_features("Justin Bieber") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Love Yourself") %>% slice(1)
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Love Yourself"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}




# track name:Shape of You 
# artist:Ed Sheeran   
# year: 2017

song_list <- get_track("Ed Sheeran")
feature_list <- get_features("Ed Sheeran") 
song_data <- as.tibble(song_list) %>% filter(track_name=="Shape of You") 
feature_data <- as.tibble(feature_list) %>% filter(track_uri==song_data$track_uri)
get_feature_data <- cbind(song_data,feature_data[1,])
for (i in 1:nrow(list)) {
  if (list$track_name[i] == "Shape of You"){
    list$track_uri[i] <- str_sub(get_feature_data$track_uri)
    list$danceability[i] <- str_sub(get_feature_data$danceability)
    list$energy[i] <- str_sub(get_feature_data$energy)
    list$loudness[i] <- str_sub(get_feature_data$loudness)
    list$speechiness[i] <- str_sub(get_feature_data$speechiness)
    list$acousticness[i] <- str_sub(get_feature_data$acousticness)
    list$instrumentalness[i] <- str_sub(get_feature_data$instrumentalness)
    list$liveness[i] <- str_sub(get_feature_data$liveness)
    list$valence[i] <- str_sub(get_feature_data$valence)
    list$tempo[i] <- str_sub(get_feature_data$tempo)
    list$duration_ms[i] <- str_sub(get_feature_data$duration_ms)
  }
}


# Final dataset including all songs and features from top 1 song of the 
# Billboard Hot 100 song list from 1960 to 2017. 
List <- write.csv(list,"Billboard_Top_1.csv")
