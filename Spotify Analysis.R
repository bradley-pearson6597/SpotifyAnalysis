library(spotifyr)
library(dplyr)
library(ggplot2)
library(cluster)
library(forcats)

## Need to set api keys in Rprofile file.
## Use file.edit(".Rprofile)
## In this file do 
## Sys.setenv(SPOTIFY_CLIENT_SECRET = '')

access_token <- get_spotify_access_token()
get_recommendations()

categories <- get_categories()
category.playlists <- data.frame()
for(i in 1:20){
  j = ifelse(i == 1, 0, 20*(i-1))
  temp.playlists <- get_category_playlists(category_id = "workout", offset = j, limit = 50)
  category.playlists <- rbind(category.playlists, temp.playlists)
  # Sys.sleep(3)
}
  
category.track.data <- data.frame()
for(p in 1:nrow(category.playlists)){
  playlist.id = gsub(".*playlist\\:", "", category.playlists$uri[p])
  temp.playlists <- get_playlist_audio_features(playlist_uris = playlist.id)
  category.track.data <- rbind(category.track.data, temp.playlists)
  
}

category.track.data2 <- category.track.data %>% 
  dplyr::group_by(playlist_name) %>%
  dplyr::mutate(track_no = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(track_no <=  50)

min.tempo <- category.track.data %>% dplyr::group_by(playlist_name) %>%
  dplyr::summarise(mintempo = min(tempo)) %>%
  dplyr::filter(mintempo > 85) %>% dplyr::ungroup()

category.track.data3 <- category.track.data2 %>%
  dplyr::filter(playlist_name %in% min.tempo$playlist_name)

ggplot(data = category.track.data3, aes(y = tempo, x = track_no,  colour = playlist_name)) + 
  geom_line()

high.tempo.playlist <- category.track.data2 %>%
  dplyr::filter(tempo > 150) %>%
  dplyr::arrange(tempo) %>%
  dplyr::mutate(track_no = 1:n())

sequence.playlist <- round(nrow(high.tempo.playlist)/50)
sequence.playlist2 <- seq(from = 1, to = nrow(high.tempo.playlist), by = sequence.playlist)
high.tempo.playlist <- high.tempo.playlist[sequence.playlist2,]

ggplot(data = high.tempo.playlist, aes(y = tempo, x = track_no)) + 
  geom_line()

artist <- c("kanye west")
artist <- c("james blunt")

artist.data.all <- data.frame()
for(a in artist){
  artist.data <- get_artist_audio_features(a) %>%
    dplyr::select(-c(track_href, is_local, available_markets, artists, analysis_url, album_images))
  artist.data.all <- rbind(artist.data.all, artist.data)
}

artist.data.all <- artist.data.all %>% dplyr::group_by(album_name) %>%
  dplyr::mutate(track_no = 1:n(),
                album_release_date = as.Date(album_release_date)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(album_release_date) %>%
  dplyr::mutate(album_name = as.factor(album_name))

artist.data.all$album_name <- reorder(artist.data.all$album_name, artist.data.all$album_release_date)

anim = ggplot(data = artist.data.all, aes(x = track_no, y = tempo, colour = album_name)) + 
  geom_col() + transition_states(album_name, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
       renderer = gifski_renderer("gganim.gif"))

artist.data.all2 <- artist.data.all %>%
  dplyr::group_by(album_name) %>%
  dplyr::summarise(av_duration = mean(duration_ms),
                   av_danceability = mean(danceability),
                   av_energy = mean(energy),
                   av_loudness = mean(loudness),
                   av_speechiness = mean(speechiness),
                   av_acousticness = mean(acousticness),
                   av_instrumentalness = mean(instrumentalness),
                   av_liveness = mean(liveness),
                   av_valence = mean(valence),
                   av_tempo = mean(tempo),
                   release_date = dplyr::first(album_release_date),
                   no_tracks = n()) %>%
  dplyr::mutate(release_date = as.Date(release_date)) %>%
  dplyr::arrange(release_date) %>%
  dplyr::ungroup() 


ggplot(data = artist.data.all2, aes(x = release_date, y = av_energy)) + 
  geom_line() + ggplot2::ylim(0, 1)

ggplot(data = artist.data.all2, aes(album_name, av_energy, fill = album_name)) + geom_col() +
  # ylim(0, 1) +
  xlab("Album Name") + ylab("Average Danceability") + labs(fill = "Album Name") + theme(axis.title.x=element_blank(),
                                                                                 axis.text.x=element_blank(),
                                                                                 axis.ticks.x=element_blank())


artist.data.all.numdata <- artist.data.all %>% dplyr::select(duration_ms, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>%
  scale(.) %>% as.data.frame()
k.max = 10
wss <- sapply(1:k.max, 
              function(k){kmeans(artist.data.all.numdata, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

artist.clustered <-  kmeans(artist.data.all.numdata, centers = 4)
artist.clustered2 <- cbind(artist.data.all, cluster = artist.clustered$cluster)

ggplot(data = artist.clustered2, aes(x = duration_ms, y = danceability, color = cluster)) + geom_point()
artist.clustered2 %>% dplyr::filter(track_name == "Closed on Sunday")
artist.clustered2 %>% dplyr::filter(cluster == 3) %>% dplyr::select(track_name) %>% unique()


artist.top.tracks.all <- data.frame()
for(t in unique(artist.data.all$artist_id)){
  artist.top.tracks <- get_artist_top_tracks(id = t) %>%
    dplyr::select(name, popularity)
  artist.top.tracks.all <- rbind(artist.top.tracks.all, artist.top.tracks)
  
}

artist.data.toptracks <- artist.data.all %>% dplyr::filter(track_name %in% c(artist.top.tracks.all$name)) %>%
  dplyr::mutate(release_date = as.Date(album_release_date),
                track = 1:n())

ggplot(data = artist.data.toptracks, aes(x = track, y = danceability)) + 
  geom_line() + ggplot2::ylim(0, 1)

ggplot(data = artist.data.toptracks, aes(track_name, duration_ms)) +
  geom_col()


jason.playlist <- get_playlist_audio_features(playlist_uris = "0c3nwXmJU1yQOLCSsUvKV4")
jason.playlist2 <- get_playlist_audio_features(playlist_uris = "6JVyFSEEa7i8ypgZlaDOYP")
jason.playlist.all <- rbind(jason.playlist, jason.playlist2)

jason.playlist.all <- jason.playlist.all %>% 
  dplyr::group_by(playlist_name) %>%
  dplyr::mutate(track_no = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(track_no <=  50)

jason.playlist.fast <- jason.playlist.all %>%
  dplyr::filter(tempo > 125)  %>%
  dplyr::arrange(tempo) %>%
  dplyr::distinct(track.name)

jason.playlist.fast$track.name

ggplot(data = jason.playlist.fast, aes(y = tempo, x = track_no,  colour = playlist_name)) + 
  geom_line() + ylim(0, 200)

get_user_playlists()
  

