library(mongolite)
library(fmsb)
library(dplyr)
library(ggplot2)

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

#No inclui duracion, ni streams en el analisis (no los considero audio features)
#Loudness y Energy tiene una correlacion muy alta (0,8+) con lo cual voy a eliminar Loudness del analisis.
#Tempo pareciera utilizarse para medir otros features, con lo cual tambien lo elimino del analisis.


#Elijo el top 10 de canciones mas escuchadas en el periodo y analizo sus audio features.

top10_escuchadas <- charts_audio_features1[with(charts_audio_features1,order(-Streams)),]
top10_escuchadas <- top10_escuchadas[1:10,]

#Visualizo sus caracteristicas en un chart radar

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
  )}


#Esto no lo usé
#boxplot(top10_escuchadas$Danceability)
#boxplot(top10_escuchadas$Energy)
#boxplot(top10_escuchadas$Loudness) #hay outlier inferior - Billie Eilish - Bad Guy
#boxplot(top10_escuchadas$Speechiness) #hay outlier superior - Billie Eilish - Bad Guy
#boxplot(top10_escuchadas$Acousticness)
#boxplot(top10_escuchadas$Instrumentalness) #hay 2 outliers superiores - Billie Eilish - Bad Guy y SAD! - XXXTENTACION
#boxplot(top10_escuchadas$Liveness) #hay outlier superior #Drake - God's Plan
#boxplot(top10_escuchadas$Valence) #hay outlier superior - Post Malone - Sunflower
#boxplot(top10_escuchadas$avg_tempo) #hay outlier superior - The Weeknd - Blinding Lights
#boxplot(top10_escuchadas$avg_duration)


####ANALISIS DE POPULARIDAD POR CANTIDAD DE TRACKS EN EL CHART DURANTE EL PERIODO DEL DATASET

library(tidyverse)

canciones_por_artista <- charts_audio_features1 %>% count(artist)
hist(x = canciones_por_artista$n, main = "Cantidad de canciones en chart por artista", 
     xlab = "Cant. de canciones en chart por artista", ylab = "Frecuencia",
     col = "pink", border= "pink")

#Esta parte no la supe interpretar aun.
boxplot(canciones_por_artista$n)
boxplots_outliers <- canciones_por_artista["artist"][canciones_por_artista$n > boxplot(canciones_por_artista[2])$stats[5],]
sort(boxplots_outliers)

#Conclusiones:
#La mayoria de los artistas que aparecen en el chart de los tres anos cubiertos por le dataset,
#tuvieron entre 1 y 5 canciones dentro del ranking. Por otro lado, hay un puñado de artistas que superan las 30 canciones
#dentro del chart durante el periodo analizado. En este punto, consideramos que este dato es definitorio sobre la popularidad
# de un artista. Nos enfocaremos en el top 10 de artistas mas populares segun la cantidad de canciones que aparecen en el chart.

# 1.Taylor Swift
# 2.Bad Bunny 
# 3.Ariana Grande
# 4.BTS
# 5.Drake
# 6.Juice WRLD
# 7.Post Malone
# 8.XXXTENTACION
# 9.Eminem
# 10.Khalid

#Viendo los artistas que entraron en este top 10, notamos que solo algunos de ellos lograron una cancion que entrara en el top 10
#de canciones con mas streams de todo el periodo: Post Malone, XXXTENTACION, Drake, Juice WRLD.

#ANALISIS DE POPULARIDAD POR CANTIDAD DE STREAMS TOTALES POR ARTISTA EN EL PERIODO:

artista_mas_escuchado <- aggregate(charts_audio_features$Streams, by=list(artist=charts_audio_features$Artist),FUN=sum)

#Post Malone
#Billie Eilish
#Ariana Grande
#Drake
#EdSheeran
#XXXTENTACION
#BadBunny
#Juice WRLD
#Dua Lipa
#Travis Scott

#CONCLUSIONES:

#Estan dentro de las tres categorias (top 10 de hits, top 10 mas escuchados, top 10 mas canciones en chart):
#XXXTENTACION
#Post Malone
#Juice WRLD
#Drake

#Consideramos a estos 4 artistas como los mas populares del dataset.

#Analizo sus canciones por separado: 

XXXTENTACION <- subset(charts_audio_features1, artist=="XXXTENTACION") #40 canciones
Post_Malone <- subset(charts_audio_features1, artist=="Post Malone") #41 canciones
Juice_WRLD <- subset(charts_audio_features1, artist=="Juice WRLD") #42 canciones
Drake <- subset(charts_audio_features1, artist=="Drake") #47 canciones

#XXXTENTACION:

boxplot(XXXTENTACION$Danceability)
boxplot(XXXTENTACION$Energy)
boxplot(XXXTENTACION$Speechiness) #3 outliers: Train Food | Guardian Angel | Carry on
boxplot(XXXTENTACION$Acousticness)
boxplot(XXXTENTACION$Instrumentalness) #hay outliers (varios): 
boxplot(XXXTENTACION$Liveness) #3 outliers: Love Yourself (Interlude) | Floor 555 | Train Food
boxplot(XXXTENTACION$Valence)

#Post Malone:

boxplot(Post_Malone$Danceability)
boxplot(Post_Malone$Energy)
boxplot(Post_Malone$Speechiness)# hay outliers  (varios):
boxplot(Post_Malone$Acousticness)
boxplot(Post_Malone$Instrumentalness) # hay outliers (varios): 
boxplot(Post_Malone$Liveness) # 3 outliers (varios)
boxplot(Post_Malone$Valence) # hay 2 outliers: Sunflower (Spider Man) | 92 Explorer

#Juice WRLD:

boxplot(Juice_WRLD$Danceability)#hay 3 outliers para abajo: Speaks from heaven (outro) | Anxiety - Intro | I Want It
boxplot(Juice_WRLD$Energy) 
boxplot(Juice_WRLD$Speechiness) #hay outliers (varios):
boxplot(Juice_WRLD$Acousticness) #3 outliers: Whising Well | Man of The Year | Flaws and Sins (arriba)
boxplot(Juice_WRLD$Instrumentalness) #hay outliers
boxplot(Juice_WRLD$Liveness) #varios outliers
boxplot(Juice_WRLD$Valence)

#Drake:

boxplot(Drake$Danceability)
boxplot(Drake$Energy) #hay outlier arriba: Nice for what (es minimo!)
boxplot(Drake$Speechiness) # hay outlier: 8 out of 10
boxplot(Drake$Acousticness)
boxplot(Drake$Instrumentalness) #hay outliers
boxplot(Drake$Liveness) #varios outliers
boxplot(Drake$Valence)
