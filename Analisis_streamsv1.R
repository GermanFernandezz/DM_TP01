library(mongolite)
library(fmsb)
library(dplyr)

charts1 = mongo(collection = "charts", db = "DMUBA_Spotify" )
charts1$count()

features_avg = mongo(collection = "features_avg1", db = "DMUBA_Spotify" )
features_avg$count()


#Paso a data frame
df_charts1 <- charts1$find()
names(df_charts1)
str(df_charts1)

df_audio <- features_avg$find()
names(df_audio)
str(df_audio)

#Elimino valores repetidos
df_charts1 <- df_charts1[!duplicated(df_charts1), ]
df_audio <- df_audio[!duplicated(df_audio), ]

df_charts2 <- df_charts1[c(2,3,4,8)] #me quedo solo con las columnas que me interesan
df_charts2 <- df_charts2[!duplicated(df_charts2), ]

#Merge de ambas bases
charts_audio_features <- merge(df_charts2, df_audio, by.x="artist_track", by.y="artist_track")
charts_audio_features <- charts_audio_features[!duplicated(charts_audio_features), ]

# Armo un df filtrado por 
charts_audio_features1 <- aggregate(charts_audio_features$Streams, by=list(artist_track=charts_audio_features$artist_track,
                                                                           artist=charts_audio_features$Artist,
                                                                           track_name=charts_audio_features$Track_Name,
                                                                           avg_danceability=charts_audio_features$avg_danceability,
                                                                           avg_energy=charts_audio_features$avg_energy,
                                                                           avg_loudness=charts_audio_features$avg_loudness,
                                                                           avg_speechiness=charts_audio_features$avg_speechiness,
                                                                           avg_acousticness=charts_audio_features$avg_acousticness,
                                                                           avg_instrumentalness=charts_audio_features$avg_instrumentalness,
                                                                           avg_liveness=charts_audio_features$avg_liveness,
                                                                           avg_valence=charts_audio_features$avg_valence,
                                                                           avg_tempo=charts_audio_features$avg_tempo,
                                                                           avg_duration=charts_audio_features$avg_duration), FUN=sum)

#emprolijo columnas
colnames(charts_audio_features1)[4] <- "Danceability"
colnames(charts_audio_features1)[5] <- "Energy"
colnames(charts_audio_features1)[6] <- "Loudness"
colnames(charts_audio_features1)[7] <- "Speechiness"
colnames(charts_audio_features1)[8] <- "Acousticness"
colnames(charts_audio_features1)[9] <- "Instrumentalness"
colnames(charts_audio_features1)[10] <- "Liveness"
colnames(charts_audio_features1)[11] <- "Valence"
colnames(charts_audio_features1)[12] <- "Tempo"
colnames(charts_audio_features1)[13] <- "Duration"
colnames(charts_audio_features1)[14] <- "Streams"

#Busco correlacion entre atributos
library(corrplot)
x <- cor(charts_audio_features1[4:12])
corrplot(x, type="upper", order="hclust")

#plot(charts_audio_features1[,4:8])

#No inclui duracion, ni streams en el analisis (no los considero audio features)
#Loudness y Energy tiene una correlacion muy alta (0,8) con lo cual voy a eliminar Loudness del analisis.
#Tempo pareciera utilizarse para medir otros features, con lo cual tambien lo elimino del analisis.

##ANALISIS DE CANCIONES POR STREAMS

#Elijo el top 10 de canciones mas escuchadas en el periodo y analizo sus audio features.

top10_escuchadas <- charts_audio_features1[with(charts_audio_features1,order(-Streams)),]
top10_escuchadas <- top10_escuchadas[1:10,]

#Analizo sus caracteristicas

radar <- top10_escuchadas[c(4,5,7:11)]

#1
radar_TonesAndI <- radar %>% slice(1)
radar_TonesAndI <- rbind(rep(1) , rep(0) , radar_TonesAndI)
create_beautiful_radarchart(radar_TonesAndI)
#2
radar_BlindingLights <- radar %>% slice(2)
radar_BlindingLights <- rbind(rep(1) , rep(0) , radar_BlindingLights)
create_beautiful_radarchart(radar_BlindingLights)
#3
radar_Sunflower <- radar %>% slice(3)
radar_Sunflower <- rbind(rep(1) , rep(0) , radar_Sunflower)
create_beautiful_radarchart(radar_Sunflower)
#4
radar_Senorita <- radar %>% slice(4)
radar_Senorita <- rbind(rep(1) , rep(0) , radar_Senorita)
create_beautiful_radarchart(radar_Senorita)
#5
radar_SomeoneYouLoved <- radar %>% slice(5)
radar_SomeoneYouLoved <- rbind(rep(1) , rep(0) , radar_SomeoneYouLoved)
create_beautiful_radarchart(radar_SomeoneYouLoved)
#6
radar_BadGuy <- radar %>% slice(6)
radar_BadGuy <- rbind(rep(1) , rep(0) , radar_BadGuy)
create_beautiful_radarchart(radar_BadGuy)
#7
radar_LucidDreams <- radar %>% slice(7)
radar_LucidDreams <- rbind(rep(1) , rep(0) , radar_LucidDreams)
create_beautiful_radarchart(radar_LucidDreams)
#8
radar_SAD <- radar %>% slice(8)
radar_SAD <- rbind(rep(1) , rep(0) , radar_SAD)
create_beautiful_radarchart(radar_SAD)
#9
radar_GodsPlan <- radar %>% slice(9)
radar_GodsPlan <- rbind(rep(1) , rep(0) , radar_GodsPlan)
create_beautiful_radarchart(radar_GodsPlan)
#10
radar_Shallow <- radar %>% slice(10)
radar_Shallow <- rbind(rep(1) , rep(0) , radar_Shallow)
create_beautiful_radarchart(radar_Shallow)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


boxplot(top10_escuchadas$Danceability)
boxplot(top10_escuchadas$Energy)
boxplot(top10_escuchadas$Loudness) #hay outlier inferior - Billie Eilish - Bad Guy
boxplot(top10_escuchadas$Speechiness) #hay outlier superior - Billie Eilish - Bad Guy
boxplot(top10_escuchadas$Acousticness)
boxplot(top10_escuchadas$Instrumentalness) #hay 2 outliers superiores - Billie Eilish - Bad Guy y SAD! - XXXTENTACION
boxplot(top10_escuchadas$Liveness) #hay outlier superior #Drake - God's Plan
boxplot(top10_escuchadas$Valence) #hay outlier superior - Post Malone - Sunflower
boxplot(top10_escuchadas$avg_tempo) #hay outlier superior - The Weeknd - Blinding Lights
boxplot(top10_escuchadas$avg_duration)


####ANALISIS DE POPULARIDAD POR CANTIDAD DE TRACKS Y DE STREAMS EN EL CHART DURANTE EL PERIODO DEL DATASET

library(tidyverse)

canciones_por_artista <- charts_audio_features1 %>% count(artist)
hist(x = canciones_por_artista$n, main = "Cantidad de canciones en chart por artista", 
     xlab = "Cant. de canciones en chart por artista", ylab = "Frecuencia",
     col = "pink", border= "pink")

boxplot(canciones_por_artista$n,
        col = "Pink",
        ylab = "Cantidad de canciones en el chart", 
        main = "Canciones por artista")

boxplots_outliers <- canciones_por_artista["artist"][canciones_por_artista$n > boxplot(canciones_por_artista[2])$stats[5],]
sort(boxplots_outliers)

#En el boxplot se observan 50 outliers.

outliers_canciones_por_artista <- data.frame(boxplots_outliers)
colnames(outliers_canciones_por_artista)[1] <- "artist" #corrijo nombre de columna
#junto los outliers con la cantidad de canciones
outliers_canciones_por_artista_merge2 <-merge(outliers_canciones_por_artista, canciones_por_artista, by.x="artist", by.y="artist")

#hago una copia de un df completo y dejo solo la columna de artista y de streams
#(quise hacerlo distinto pero me cambiaba el tipo de variable de streams y no pude convertirla de nuevo a numeric)
charts_audio_features11 <- data.frame(charts_audio_features1)
charts_audio_features11[1] <- NULL
charts_audio_features11[2] <- NULL
charts_audio_features11[2:10] <- list(NULL)

#agrupo por artista y sumo streams
charts_audio_features_streams_sum <- aggregate(charts_audio_features11$Streams, by=list(artist=charts_audio_features11$artist), FUN=sum)

#junto todo en un dataframe con artista, tracks y streams
artistas_canciones_streams <-merge(charts_audio_features_streams_sum, outliers_canciones_por_artista_merge2, by.x="artist", by.y="artist")

artistas_canciones_streams$streams <- log(artistas_canciones_streams$x, 2)
artistas_canciones_streams$log_streams <- log(artistas_canciones_streams$x, 10)

#cambio nombre de columna
colnames(artistas_canciones_streams)[3] <- "Tracks"

#Hago un scatter plot para ver como se distribuyen los artistas segun tracks y streams
library(ggplot2)
ggplot(artistas_canciones_streams, aes(x=Tracks, y=log_streams))+
  geom_point()

#9 mas alejados:Eminem, XXXTENTACION, Post Malone, Juice WRLD, Drake, BTS, Ariana Grande, Bad Bunny, Taylor Swift.

#Analisis multvariado

library(isotree)
set.seed(1)

#Ajusto un modelo #agrego la columna
artistas_canciones_streams$iso <- isolation.forest(artistas_canciones_streams[,c(3:4)], ntrees = 10, output_score=TRUE)$score

#armo tabla
top_iso_streams <- head(artistas_canciones_streams[order(artistas_canciones_streams$iso, decreasing = TRUE),],3)

artistas_canciones_streams %>% mutate(Color = ifelse(iso > 0.6, "blue", "pink")) %>%
  ggplot(aes(x = Tracks, y= log_streams, color = Color))+
  geom_point()+
  scale_color_identity()

#Outliers: BTS, Post Malone, Taylor Swift, Bad Bunny

##ANALIZO LAS CANCIONES 


