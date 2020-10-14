# SpotifyAnalysis
Analysing Spotify API Data

The aim of this project has been to connect to the Spotify API and analyse the data.
In the R Shiny app created has 2 main features Albums & Playlists

Albums:
This feature allows users to search for their favourite artists and the albums of this artist will be returned. As the user goes through each album the background colour of the app will be altered through colour quantization. This colour quantization clusters the RGB values found in each album and finds the center. The cluster with the most points is then used as the background colour.
Playlists:
The playlist features can be used by users to search for their playlist using the Spotify URI of the playlist. The playlist will then be analysed based on each song's attributes. These attributes are then clustered to find similar songs in the playlist and this rearranges the songs into a different order. The distance between the clusters is also used so there is a smoother transition from one cluster to the next. 
