library(spotifyr)
library(purrr)
library(knitr)
library(tuneR)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(httpuv)
library(ggridges)

id <- "01800c0e4d034cecbe39ba1b2d6d254a"
secret <- "17a6a2f2b3e844a5b23723bc825de380"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

get_spotify_authorization_code()

my_id <- 'ccsushi417' 
my_plists <- get_user_playlists(my_id)

#Data Collection/Manipulation
  #collecting the entire discographies of the ten artists and getting the song features
    #ABBA
      abba <- my_plists %>%
        filter(name %in% c('ABBA'))
      abba_ID <- abba[1, "id"]
      
      abba_tracks <-c(get_playlist_tracks(abba_ID)[,"track.id"], 
                      get_playlist_tracks(abba_ID, offset = 100)[,"track.id"])
      
      abba_features <- rbind(get_track_audio_features(abba_tracks[1:100]), 
                             get_track_audio_features(abba_tracks[101:length(abba_tracks)]))
      abba_features$Artist <- "ABBA"
    
    
    #Adele
      adele <- my_plists %>%
        filter(name %in% c('Adele'))
      adele_ID <- adele[1, "id"]
      
      adele_tracks <-get_playlist_tracks(adele_ID)[,"track.id"]
      adele_features <- get_track_audio_features(adele_tracks)
      adele_features$Artist <- "Adele"
    
    #Avicii
      avicii <- my_plists %>%
        filter(name %in% c('Avicii'))
      avicii_ID <- avicii[1, "id"]
      
      avicii_tracks <-get_playlist_tracks(avicii_ID)[,"track.id"]
      avicii_features <- get_track_audio_features(avicii_tracks)
      avicii_features$Artist <- "Avicii"
    
    #Bruno Mars
      bruno <- my_plists %>%
        filter(name %in% c('Bruno Mars'))
      bruno_ID <- bruno[1, "id"]
      
      bruno_tracks <-get_playlist_tracks(bruno_ID)[,"track.id"]
      bruno_features <- get_track_audio_features(bruno_tracks)
      bruno_features$Artist <- "Bruno Mars"
    
    #Doja Cat
      doja <- my_plists %>%
        filter(name %in% c('Doja Cat'))
      doja_ID <- doja[1, "id"]
      
      doja_tracks <-get_playlist_tracks(doja_ID)[,"track.id"]
      doja_features <- get_track_audio_features(doja_tracks)
      doja_features$Artist <- "Doja Cat"
    
    #Fall Out Boy
      falloutboy <- my_plists %>%
        filter(name %in% c('Fall Out Boy'))
      falloutboy_ID <- falloutboy[1, "id"]
      
      falloutboy_tracks <-c(get_playlist_tracks(falloutboy_ID)[,"track.id"], 
                            get_playlist_tracks(falloutboy_ID, offset = 100)[,"track.id"])
      falloutboy_features <- rbind(get_track_audio_features(falloutboy_tracks[1:100]), 
                                   get_track_audio_features(falloutboy_tracks[101:length(falloutboy_tracks)]))
      falloutboy_features$Artist <- "Fall Out Boy"
    
    #Kanye West
      kanye <- my_plists %>%
        filter(name %in% c('Kanye West'))
      kanye_ID <- kanye[1, "id"]
      
      kanye_tracks <-c(get_playlist_tracks(kanye_ID)[,"track.id"],
                       get_playlist_tracks(kanye_ID, offset = 100)[,"track.id"])
      kanye_features <- rbind(get_track_audio_features(kanye_tracks[1:100]), 
                              get_track_audio_features(kanye_tracks[101:length(kanye_tracks)]))
      kanye_features$Artist <- "Kanye West"
    
    #Led Zepplin
      zepplin <- my_plists %>%
        filter(name %in% c('Led Zepplin'))
      zepplin_ID <- zepplin[1, "id"]
      
      zepplin_tracks <-get_playlist_tracks(zepplin_ID)[,"track.id"]
      zepplin_features <- get_track_audio_features(zepplin_tracks)
      zepplin_features$Artist <- "Led Zepplin"
    
    #The Beatles
      beatles <- my_plists %>%
        filter(name %in% c('The Beatles'))
      beatles_ID <- beatles[1, "id"]
      
      beatles_tracks <-c(get_playlist_tracks(beatles_ID)[,"track.id"],
                         get_playlist_tracks(beatles_ID, offset = 100)[,"track.id"],
                         get_playlist_tracks(beatles_ID, offset = 200)[,"track.id"])
      beatles_features <- rbind(get_track_audio_features(beatles_tracks[1:100]), 
                                get_track_audio_features(beatles_tracks[101:200]),
                                get_track_audio_features(beatles_tracks[201:length(beatles_tracks)]))
      beatles_features$Artist <- "The Beatles"
                                
    
    #The Weeknd
      weeknd <- my_plists %>%
        filter(name %in% c('The Weeknd'))
      weeknd_ID <- weeknd[1, "id"]
      
      weeknd_tracks <-c(get_playlist_tracks(weeknd_ID)[,"track.id"],
                        get_playlist_tracks(weeknd_ID, offset = 100)[,"track.id"])
      weeknd_features <- rbind(get_track_audio_features(weeknd_tracks[1:100]), 
                              get_track_audio_features(weeknd_tracks[101:length(weeknd_tracks)]))
      weeknd_features$Artist <- "The Weeknd"
    
    
    #All Artists and All Features
      allfeatures <- rbind(abba_features, adele_features, avicii_features, beatles_features,
                           bruno_features, doja_features, falloutboy_features, kanye_features,
                           weeknd_features, zepplin_features)


#Plots/Analysis
  #visualizing the distribution of track values for each track feature for each artist 
      
      
  #Danceability
  dance <-
  ggplot(allfeatures, aes(y = Artist, x = danceability, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Danceability")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#F37362", "#05A8AA", "#61D62F")) +
    theme(axis.title.y = element_blank())
  ggsave("danceability.png", 
         dance, height = 9, width = 8)
  
  #Energy
  energy <-
  ggplot(allfeatures, aes(y = Artist, x = energy, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Energy")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#db4c39", "#f25c41", "#fc9b3a")) +
    theme(axis.title.y = element_blank())
  ggsave("energy.png", 
         energy, height = 9, width = 8)
  
  #Key
    #numbers pitch class notation 
    #(From Spotify: 0 = C, 1 = C♯/D♭, 2 = D, and so on. If no key was detected, the value is -1.)
   key <-
   ggplot(allfeatures, aes(y = Artist, x = key, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Key")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#E2D4B7", "#9C9583", "#A1A499")) +
    theme(axis.title.y = element_blank())
   ggsave("key.png", 
          key, height = 9, width = 8)
   
  #Loudness
  loudness <-
  ggplot(allfeatures, aes(y = Artist, x =loudness, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Loudness")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#D93636", "#00F0B5", "#889696")) +
    theme(axis.title.y = element_blank())
  ggsave("loudness.png", 
         loudness, height = 9, width = 8)  
  
  #Speechiness
  speech <-
  ggplot(allfeatures, aes(y = Artist, x = speechiness, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Speechiness")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#CEC2FF", "#B3B3F1", "#DCB6D5")) +
    theme(axis.title.y = element_blank())
  ggsave("speechiness.png", 
         speech, height = 9, width = 8)  
  
  #Acousticness
  acoustic <-
  ggplot(allfeatures, aes(y = Artist, x = acousticness, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Acousticness")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#091540", "#7692ff", "#abd2fa")) +
    theme(axis.title.y = element_blank())
  ggsave("acousticness.png", 
         acoustic, height = 9, width = 8)  
  
  #Valence
  valence <-
  ggplot(allfeatures, aes(y = Artist, x = valence, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Valence")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#B31AFF", "#218380", "#FFBC42")) +
    theme(axis.title.y = element_blank())
  ggsave("valence.png", 
         valence, height = 9, width = 8)  
  
  #Tempo
  tempo <-
  ggplot(allfeatures, aes(y = Artist, x = tempo, fill = Artist))+ 
    geom_density_ridges()+
    xlab("Tempo")+
    scale_y_discrete(limits = rev)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#ed6a5a", "#f4f1bb", "#9bc1bc")) +
    theme(axis.title.y = element_blank())
  ggsave("tempo.png", 
         tempo, height = 9, width = 8)  
  
  
#Individual Artist Top 10 Songs Loudness Analysis 
  
  #get playlist and tracks for top10 songs of each artist (100 total songs)
  top <- my_plists %>%
    filter(name %in% c('Top'))
  top_ID <- top[1, "id"]
  
  top_tracks <-c(get_playlist_tracks(top_ID)[,"track.id"])
  
  #create dataframe
  songs <- tibble("Loudness" = 0.000, "Name" = "")

#ABBA 
  
  #fill in data frame for loudness values for the top 10 songs 
  for (i in 1:10) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
     
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[1:10])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  abba_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#44C6DA", "#1E3888", "#F5E663"))+
    theme(axis.title.y = element_blank())
  
  ggsave("abba top10 loudness.png", 
         abba_top, height = 9, width = 8)
  
#Adele 
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 11:20) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[11:20])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  adele_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#2c7f85", "#1a4c50", "#96bfc2"))+
    theme(axis.title.y = element_blank())
  
  ggsave("adele top10 loudness.png", 
         adele_top, height = 9, width = 8)

#Avicii
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 21:30) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[21:30])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  avicii_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#EF6461", "#D5FF3B", "#B9C0DA"))+
    theme(axis.title.y = element_blank()) 
  
  ggsave("avicii top10 loudness.png", 
         avicii_top, height = 9, width = 8)
  
#Bruno Mars
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 31:40) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[31:40])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  bruno_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#AC867A", "#DCEDB9", "#C5D98D"))+
    theme(axis.title.y = element_blank()) 
  
  ggsave("bruno mars top10 loudness.png", 
         bruno_top, height = 9, width = 8)

#Doja Cat
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 41:50) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[41:50])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  doja_top <- 
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#48A9A6", "#E546AD", "#7E36B6"))+
    theme(axis.title.y = element_blank()) 
  
  ggsave("doja cat top10 loudness.png", 
         doja_top, height = 9, width = 8)
  
#Fall Out Boy
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 51:60) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[51:60])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  falloutboy_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#752828", "#355070", "#DDD8C4"))+
    theme(axis.title.y = element_blank()) 
  
  ggsave("fall out boy top10 loudness.png", 
         falloutboy_top, height = 9, width = 8)
  
#Kanye West  
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 61:70) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[61:70])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  kanye_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#301818", "#F2783B", "#33658A"))+
    theme(axis.title.y = element_blank()) 

  ggsave("kanye west top10 loudness.png", 
         kanye_top, height = 9, width = 8)
  
#Led Zepplin
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 71:80) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[71:80])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  zepplin_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#D77436", "#578A42", "#2B5075"))+
    theme(axis.title.y = element_blank()) 
  
  ggsave("led zepplin top10 loudness.png", 
         zepplin_top, height = 9, width = 8)
  
#The Beatles
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 81:90) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[81:90])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  beatles_top <- 
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#66BFDF", "#C4A69D", "#355B2D"))+
    theme(axis.title.y = element_blank()) 
  
  ggsave("the beatles top10 loudness.png", 
         beatles_top, height = 9, width = 8)
   
#The Weeknd
  songs <- tibble("Loudness" = 0.000, "Name" = "")
  for (i in 91:100) {
    
    #the only function to get loudness value at multiple time points
    a <- get_track_audio_analysis(top_tracks[i]) 
    b<-a[["sections"]][["loudness"]]
    
    #this function does not sample the same number of points for each track 
    #(different number of loudness values obtained from each track) so must account that
    names <- rep(get_tracks(top_tracks[i])["name"], length(b))
    
    songs <- songs %>% add_row("Loudness"= b, "Name"= array(unlist(names)))
  }
  
  orders <- get_tracks(top_tracks[91:100])["name"]
  level_order <- rev(array(unlist(orders))) 
  #reverse of desired order since ggridges appears to 
  #flip y axis (first assigned level on bottom)
  
  weeknd_top <-
  ggplot(songs[-1,], aes(y = factor(Name, level=(level_order)), x = Loudness, fill = Name))+ 
    geom_density_ridges()+
    scale_y_discrete(breaks=level_order)+
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
    scale_fill_cyclical(values = c("#B51F1F", "#F2C628", "#220F55"))+
    theme(axis.title.y = element_blank()) 
  
  ggsave("the weeknd top10 loudness.png", 
         weeknd_top, height = 9, width = 8)
  
