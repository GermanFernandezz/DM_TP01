library(mongolite)

# Cargo las bases de datos y verifico que tenga todos los objetos
artist = mongo(collection = "artist", db = "Spotify_UBA" )
artist$count()

artist_audio_features = mongo(collection = "artist_audio_features", db = "Spotify_UBA" )
artist_audio_features$count()


df_features <- artist_audio_features$find()
df_features <- df_features[!duplicated(df_features), ]
pie(table(df_features$album_type),main="Cantidad de Tipos de album")



charts = mongo(collection = "charts", db = "Spotify_UBA" )
charts$count()

#Empiezo analizando charts. Paso a data frame
df_charts <- charts$find()
names(df_charts)
str(df_charts)

#Elimino valores repetidos en charts
df_charts <- df_charts[!duplicated(df_charts), ]

prueba <- df_charts[!duplicated(df_charts$Track_Name), ]


#Boxplot e histogramas de posiciones
boxplot(df_charts$Position)
hist(df_charts$Position)

#Calculo log2 se streams para tener mejores gráficos
df_charts$logStreams <- log(df_charts$Streams, 2)

#Boxplot e histogramas de streams
boxplot(df_charts$Streams)
hist(df_charts$Streams)
boxplot(df_charts$logStreams)
hist(df_charts$logStreams)

#Scatter plot posicion vs Streams
plot(df_charts$Position, df_charts$Streams)

tracks_por_artista <- data.frame(table(df_charts$Artist))
names(tracks_por_artista)[1] <- "artist"
names(tracks_por_artista)[2] <- "# tracks"
str(tracks_por_artista)

#Cargo los charts pero con la mejor posición por tema
charts_mejorPosicion = mongo(collection = "charts_mejorPosicion", db = "Spotify_UBA" )
charts_mejorPosicion$count()

#Paso a Data Frame
df_charts_mejorPosicion <- charts_mejorPosicion$find()
names(df_charts_mejorPosicion)


#Grafico posiciones
boxplot(df_charts_mejorPosicion_2$mejor_position)
hist(df_charts_mejorPosicion_2$mejor_position)

#Transformo valores de streams
df_charts_mejorPosicion$log_avg_streams <- log(df_charts_mejorPosicion$avg_streams, 2)
df_charts_mejorPosicion$log_sum_streams <- log(df_charts_mejorPosicion$sum_streams, 2)

#Grafico streams
boxplot(df_charts_mejorPosicion$log_avg_streams)
hist(df_charts_mejorPosicion$log_avg_streams)
boxplot(df_charts_mejorPosicion$log_sum_streams)
hist(df_charts_mejorPosicion$log_sum_streams)

#Sccater plot mejor posicion vs streams
plot(df_charts_mejorPosicion$mejor_position, df_charts_mejorPosicion$avg_streams)

tracks_mejorPosicion_por_artista <- data.frame(table(df_charts_mejorPosicion$track_artist$artist_name))
names(tracks_mejorPosicion_por_artista)[1] <- "artist"
names(tracks_mejorPosicion_por_artista)[2] <- "# tracks"
str(tracks_por_artista)

#Analizo features_red Paso a data frame
df_features_red <- artist_audio_features_red$find()

boxplot(df_features_red$danceability)
hist(df_features_red$danceability)
boxplot(df_features_red$energy)
hist(df_features_red$energy)
boxplot(df_features_red$loudness)
hist(df_features_red$loudness)
boxplot(df_features_red$speechiness)
hist(df_features_red$speechiness)
boxplot(df_features_red$acousticness)
hist(df_features_red$acousticness)
boxplot(df_features_red$instrumentalness)
hist(df_features_red$instrumentalness)
boxplot(df_features_red$valence)
hist(df_features_red$valence)
boxplot(df_features_red$liveness)
hist(df_features_red$liveness)


features_avg_mejorPosition = mongo(collection = "features_avg_mejorPosition", db = "Spotify_UBA" )
features_avg_mejorPosition$count()
df_features_avg_mejorPosition <- features_avg_mejorPosition$find()

plot(df_features_avg_mejorPosition$mejor_position, df_features_avg_mejorPosition$f_danceability)
plot(df_features_avg_mejorPosition$mejor_position, df_features_avg_mejorPosition$f_energy)
plot(df_features_avg_mejorPosition$mejor_position, df_features_avg_mejorPosition$f_loudness)
plot(df_features_avg_mejorPosition$mejor_position, df_features_avg_mejorPosition$f_speechiness)
plot(df_features_avg_mejorPosition$mejor_position, df_features_avg_mejorPosition$f_liveness)

#Test de chi-cuadrado para atriutos nominales
#Creamos una tabla de contingencia
library(MASS)
tbl_cont = table(df_features_avg_mejorPosition$track_artist$artist_name, df_features_avg_mejorPosition$track_artist$track_name)
print(tbl_cont)
#Test de Chi-cuadrado
chisq.test(tbl_cont)

tbl_cont_features = table(df_features$artist_name, df_features$track_name)
print(tbl_cont)
#Test de Chi-cuadrado
chisq.test(tbl_cont_features)

#TECNICA DE BINNING PARA LA GESTION DE RUIDO EN R
library(infotheo)

# Discretize recibe el atributo, el método de binning y la cantidad de bins
bin_eq_freq_speechiness <- discretize(df_features_avg_mejorPosition$f_speechiness,"equalfreq", 5)

# Incorporo atributo al dataframe
speechiness_disc <- data.frame(df_features_avg_mejorPosition$f_speechiness, bin_eq_freq_speechiness)

# Cambio los nombres de los atributos
names(speechiness_disc) <- c('speechiness', 'E.Freq.Bin')

# Muestro los primeros 3 datos
speechiness_disc[1:3,]

#Suavizado de datos por la media
#Por cada bin calculamos la media y reemplazamos en el atributo suavizado
#Calculo de media de los Sepal.Width que pertencen al mismo Freq.Bin, y luego genero una nueva columna donde le asigno a cada dato su media 
#por bin correspondinete
for(bin in 1:5){
  media_bin = mean(speechiness_disc$speechiness[speechiness_disc$E.Freq.Bin==bin])
  speechiness_disc$E.Freq.Suav[speechiness_disc$E.Freq.Bin==bin] = media_bin
}

speechiness_disc[1:3,]

#Grafico donde se observa como evoluciona la variable desde la original hasta la suavizada
# grafico Sepal.Width ordenado de menor a mayor
plot(speechiness_disc$speechiness , type = "l", col="red", ylab = "speechiness", xlab = "Observaciones", main = "Dato original vs suavizado")
# Agrego la serie de la variable media 
lines(speechiness_disc$E.Freq.Suav,
      type = "l", col="blue")
legend("topleft", legend=c("Original", "Suavizado Freq"), col=c("red", "blue"), lty=1)



