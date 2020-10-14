shinyServer(
  function(input, output, session){
    
    # Search for artists and get their info
    artist.search <- eventReactive(input$artistsearch, {
      show_modal_spinner(spin = "atom", color = "#192a67",
                         text = "Retrieving Albums", session = shiny::getDefaultReactiveDomain()) 
      # Get audio features for artist
      artist.data <- get_artist_audio_features(input$artist) %>%
          dplyr::select(-c(track_href, is_local, available_markets, artists, analysis_url, album_images))
      artist.data
      
    })
    
    # Get ablum images and make sure there aren't replicates
    album.images <- eventReactive(input$artistsearch, {
      album.id <- artist.search() %>% dplyr::group_by(album_name) %>%
        dplyr::slice(1) %>%
        dplyr::filter(grepl(pattern = "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d", album_release_date)) %>%
        dplyr::mutate(album_release_date = as.Date(album_release_date)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(album_release_date))
      
      album.infoall <- data.frame()
      for(alb in album.id$album_id){
        album.info <- get_album(id = alb)
        album.info <- data.frame(url = album.info$images$url[1],
                                 name = album.info$name,
                                 id = alb)
        album.infoall <- rbind(album.infoall, album.info) 
      }
      
      album.infoall
      
    })
    
    # Cluster the songs in a playlist based on the song's attributes
    smart.playlist <- eventReactive(input$playlistsearch, {
      
      playlist.link <- gsub("spotify:playlist:", "", input$playlist)
      
      playlist <- get_playlist(playlist.link)
      playlist.songs <- playlist$tracks
      playlist.songs2 <- playlist.songs$items %>%
        dplyr::distinct(track.id) %>%
        dplyr::slice(1:100)
      audio.features <- get_track_audio_features(ids = c(playlist.songs2$track.id))
      numeric.features <- audio.features %>%
        dplyr::select(-c(type, id, uri, track_href, analysis_url, time_signature))
      playlist.kmeans <- kmeans(numeric.features, centers =  5)
      cluster.dist <- data.frame(cluster = 1:length(playlist.kmeans$withinss), cluster.dist = playlist.kmeans$withinss)
      audio.features2 <- cbind(audio.features, cluster = playlist.kmeans$cluster) %>%
        dplyr::left_join(., cluster.dist, by = c("cluster")) %>%
        dplyr::arrange(cluster.dist)
      
      playlist.songnames <- playlist.songs$items 
      playlist.songnames <- playlist.songnames %>% dplyr::select(track.name, track.id)
      new.playlist <- audio.features2 %>% dplyr::select(id, cluster, cluster.dist) %>%
        dplyr::left_join(., playlist.songnames, by = c("id" = "track.id")) %>%
        dplyr::group_by(id, track.name) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(cluster.dist)
      
      new.playlist %>% dplyr::select(track.name, cluster) %>%
        dplyr::rename(Track = track.name)
      
    })
    
    playlist.create <- reactive({
      
      playlist.link <- gsub("spotify:playlist:", "", input$playlist)
      
      playlist <- get_playlist(playlist.link)
      
      # httr::POST()
      
    })
    
    # Create a HTML object allowing users to click through each album
    # The background of the page will be decided through colour quantization
    # This clusters the RGB colours in each album and returns the most common
    albumart <- reactive({
      ai <- album.images()
      
      # html.full <- '<div class="row">' 
      html.full <-'<br>
      <button class="w3-button w3-display-left  w3-green w3-half" id="btn-1" onclick="plusDivs(-1)">&#10094;</button>
      <button class="w3-button w3-display-right w3-green w3-half" id="btn-2" onclick="plusDivs(1)">&#10095;</button>
      <br><br>'
      
      html.full <- paste0(html.full, '<div class="w3-content w3-display-container">')
      
      for(ur in 1:nrow(ai)){
        tmp <- paste0('<div class="w3-display-container w3-half mySlides" >
                      <img src="', 
                      ai$url[ur],
                      '"style="width:100%">
                      <div class="w3-display-bottommiddle w3-medium w3-container w3-padding-16 w3-black">',
                      ai$name[ur],
                      '</div>
                      </div>')
        
        html.full <- paste(html.full, tmp)
      }
      
      album.songs <- artist.search() %>% dplyr::group_by(album_name) %>%
        dplyr::filter(grepl(pattern = "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d", album_release_date)) %>%
        dplyr::mutate(album_release_date = as.Date(album_release_date)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(album_release_date))
      
      song.lists <- data.frame()
      for(al in unique(album.songs$album_id)){
        tmp <- album.songs %>% dplyr::filter(album_id == al) %>%
          dplyr::mutate(track_number = as.numeric(track_number)) %>%
          dplyr::arrange(track_number) %>%
          dplyr::select(track_name) %>%
          dplyr::rename("Track" = track_name)
        tmp2 <- tableHTML(tmp, 
                          class = "mytable",
                          rownames = FALSE, 
                          border = 0 ) %>%
          add_css_row(css = list(c('background-color', 'text-align'), c('#1DB954', 'center'))) %>%
          as.data.frame()
        tmp2$album_id <- al
        song.lists <- rbind(song.lists, tmp2)
        
      }
      
      
      song.lists <- dplyr::inner_join(ai, song.lists, by = c("id" = "album_id"))
      
      html.full <- paste0(html.full,
                          '<div class="w3-content w3-display-container w3-half">',
                          paste(song.lists$., collapse = ""),
                          "</div>
                          </div>"
                          )


      # Colour Quantisation code. Could be used to colour backgrounds of images
      
      # Read data in
      album.data <- dbGetQuery(conn, "SELECT * FROM album_art") %>%
        dplyr::filter(hex.col != "")
      
      ai2 <- ai %>%
        dplyr::filter(!url %in% unique(c(album.data$albumart)))
      
      all.col <- data.frame()
      
      if(nrow(ai2) > 0){
        for(albumart in 1:nrow(ai2)){
          myurl <- ai2$url[albumart]
          tmp1 = image_read(myurl)
          image_write(tmp1, path = 'temp.png', format = 'png')
          tmp2 = imager::load.image("temp.png")
          tmp3 <- readPNG("temp.png")
          dim.length <- length(dim(tmp3))
          if(dim.length == 3){
            img.df <- data.frame(
              red = matrix(tmp3[,,1], ncol = 1),
              green = matrix(tmp3[,,2], ncol = 1),
              blue = matrix(tmp3[,,3], ncol = 1))
          } else{
            img.df <- data.frame(
              red = matrix(tmp3[,1], ncol = 1),
              green = matrix(tmp3[,2], ncol = 1),
              blue = matrix(tmp3[,3], ncol = 1))
          }
          
          
          if(identical(img.df[,1], img.df[,2]) & identical(img.df[,2], img.df[,3])){
            hex.col <- rgb(red = img.df[1,1],
                           green = img.df[1,2],
                           blue = img.df[1,3])
          }else{
            img.kmeans <- kmeans(img.df, 2)
            com.color = which.max(img.kmeans$size)
            img.kmeans.red <- img.kmeans$centers[com.color,1]
            img.kmeans.green <- img.kmeans$centers[com.color,2]
            img.kmeans.blue <- img.kmeans$centers[com.color,3]
            
            hex.col = rgb(red = img.kmeans.red, 
                          green = img.kmeans.green, 
                          blue = img.kmeans.blue)
            
          }
          
          hex.col <- cbind(hex.col,  ai2$url[albumart], ai2$name[albumart]) %>% as.data.frame()
          names(hex.col) <- c("hex.col", "albumart", "name")
          
          all.col <- rbind(all.col, hex.col)
          
        }
        
        RSQLite::dbWriteTable(conn, "album_art", all.col, append = TRUE)
        
      }
      
      all.col <- rbind(all.col, album.data)
      
      all.col <- dplyr::inner_join(ai, all.col, by = c("url" = "albumart", "name"))
      
      count = 0
      hex.col.array <- ""
      # print(all.col)
      
      hex.col.array <- paste0("var cols = ['", 
                        paste0(all.col$hex.col, collapse = "','"), "'];")
      
      # for(i in unique(all.col$albumart)){
        
        # count = count + 1
        # all.col2 <- all.col %>% dplyr::filter(albumart == i)
        # if(count == 1){
        #   hex.col.array <- paste0('const cols = [ { col1 :"', all.col2$hex.col[1],
        #                           # '", col2 :"', all.col2$hex.col[2],
        #                           # '", col3 :"', all.col2$hex.col[3],
        #                           '"},')
        # } else{
        #   hex.col.array <- paste0(hex.col.array, '{ col1 :"', all.col2$hex.col[1],
        #                           # '", col2 :"', all.col2$hex.col[2],
        #                           # '", col3 :"', all.col2$hex.col[3],
        #                           '"},')
        # }
        # 
      # }
      # print(hex.col.array)
      
      # hex.col.array <- paste0(hex.col.array, "];")
      # print(hex.col.array)
      
      html.full <- paste0(html.full, 
                          '
      <script>  
      
      // Get the input field
      var input = document.getElementById("btn-1");
      // Execute a function when the user releases a key on the keyboard
      
      document.addEventListener("keydown", function(event) {
      if (event.keyCode === 37) {
      // Trigger the button element with a click
      document.getElementById("btn-1").click();
      }
      if (event.keyCode === 39) {
      // Trigger the button element with a click
      document.getElementById("btn-2").click();
      }
      });
      
      var slideIndex = 1;', 
                          hex.col.array,
                          'showDivs(slideIndex);
      function plusDivs(n) {
      showDivs(slideIndex += n);
      }
      function showDivs(n) {
      var i;
      var x = document.getElementsByClassName("mySlides");
      var y = document.getElementsByClassName("mytable");
      var elements = document.getElementsByClassName("content-wrapper")
      if (n > x.length) {slideIndex = 1}
      if (n < 1) {slideIndex = x.length}
      for (i = 0; i < x.length; i++) {
      x[i].style.display = "none";
      y[i].style.display = "none";
        for(var j = 0; j < elements.length; j++){
          elements[j].style.background = cols[slideIndex-1]
      }
      }
      x[slideIndex-1].style.display = "block";
      y[slideIndex-1].style.display = "block";
      }
      // var elements = document.getElementsByClassName("content-wrapper"); // get all elements
      // for(var i = 0; i < elements.length; i++){
      // elements[i].style.background = "red";
      // }
      </script>')
      remove_modal_spinner(session = getDefaultReactiveDomain())
      writeClipboard(html.full)
      shiny::HTML(html.full)
      
    })
    
    
    output$albumimage <- renderUI({
      albumart()
      })
    
    shinyjs::onclick("artistsearch", albumart())
    
    output$playlistupdated <- renderTable({
      smart.playlist()
    })
    
  }
)