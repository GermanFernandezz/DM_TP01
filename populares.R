library(mongolite)

features_avg_mejorPosition_sumStreams_red = mongo(collection = "features_avg_mejorPosition_sumStreams_red", db = "Spotify_DM_TP" )
features_avg_mejorPosition_sumStreams_red <- features_avg_mejorPosition_sumStreams_red$find()
charts_mejorPosition1$count()
names(features_avg_mejorPosition_sumStreams_red)
library(tidyverse)
canciones_por_artista <- features_avg_mejorPosition_sumStreams_red %>% count(artist_name)
print(canciones_por_artista)
hist(x = canciones_por_artista$n, main = "Cantidad de canciones en chart por artista", 
     xlab = "Cant. de canciones en chart por artista", ylab = "Frecuencia",
     col = "pink", border= "pink")
boxplot(canciones_por_artista$n,
        col = "Pink",
        ylab = "Cantidad de canciones en el chart", 
        main = "Canciones por artista")

streams_por_artistas <- aggregate(features_avg_mejorPosition_sumStreams_red$streams, by=list(artist_name=features_avg_mejorPosition_sumStreams_red$artist_name), FUN=sum)

artistas_canciones_streams <-merge(streams_por_artistas, canciones_por_artista, by.x="artist_name", by.y="artist_name")

names(artistas_canciones_streams)
colnames(artistas_canciones_streams) <- c("Artista", "Streams", "Temas")

artistas_canciones_streams$temas_log2 <- log(artistas_canciones_streams$Temas, 2)
artistas_canciones_streams$streams_log10 <- log(artistas_canciones_streams$Streams, 10)


plot(artistas_canciones_streams$Temas, artistas_canciones_streams$streams_log10)

library(isotree)
set.seed(1)

#Ajusto un modelo #agrego la columna
artistas_canciones_streams$iso <- isolation.forest(artistas_canciones_streams[,c(3,5)], ntrees = 7, output_score=TRUE)$score

#armo tabla
top_iso <- head(artistas_canciones_streams[order(artistas_canciones_streams$iso, decreasing = TRUE),],5)

artistas_canciones_streams %>% mutate(Color = ifelse(iso>0.7, "blue", "pink")) %>%
  ggplot(aes(x = Temas, y= streams_log10, color = Color))+
  geom_point()+
  scale_color_identity()


#DISTANCIA DE MAHALANOBIS

vector_medias_streams = colMeans(artistas_canciones_streams[c(3,5)])
matriz_var_cov_streams = cov(artistas_canciones_streams[c(3,5)])
#creamos una variable con la distancia
artistas_canciones_streams$maha = sqrt(mahalanobis(artistas_canciones_streams[c(3,5)],vector_medias_streams,matriz_var_cov_streams))
top_maha <- head(artistas_canciones_streams[order(artistas_canciones_streams$maha,decreasing = TRUE),],5)
print(top_maha)
artistas_canciones_streams %>% mutate(Color = ifelse(maha>5.6, "blue", "pink")) %>%
  ggplot(aes(x = Temas, y= streams_log10, color = Color))+
  geom_point()+
  scale_color_identity()


features_avg_mejorPosition_sumStreams_red$pop_artist <- "Resto"
features_avg_mejorPosition_sumStreams_red$pop_artist[features_avg_mejorPosition_sumStreams_red$artist_name == "Taylor Swift"] <- "Taylor Swift"
features_avg_mejorPosition_sumStreams_red$pop_artist[features_avg_mejorPosition_sumStreams_red$artist_name == "Bad Bunny"] <- "Bad Bunny"
features_avg_mejorPosition_sumStreams_red$pop_artist[features_avg_mejorPosition_sumStreams_red$artist_name == "BTS"] <- "BTS"
features_avg_mejorPosition_sumStreams_red$pop_artist[features_avg_mejorPosition_sumStreams_red$artist_name == "Drake"] <- "Drake"
features_avg_mejorPosition_sumStreams_red$pop_artist[features_avg_mejorPosition_sumStreams_red$artist_name == "Ariana Grande"] <- "Ariana Grande"

boxplot(features_avg_mejorPosition_sumStreams_red$avg_danceability ~ features_avg_mejorPosition_sumStreams_red$pop_artist,
        col = c("light blue", "pink", "light green", "grey", "yellow", "violet"), 
        ylab = "Rango",
        las = 2, 
        xlab = "",
        main = "Danceability"
)
stripchart(features_avg_mejorPosition_sumStreams_red$avg_danceability ~ features_avg_mejorPosition_sumStreams_red$pop_artist, vertical = T ,
           method = "jitter", add = TRUE, pch = 4, col = "orange")

