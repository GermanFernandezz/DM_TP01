library(mongolite)

#ANALISIS DE CHARTS Y POSICIONES
# Cargo las bases de datos y verifico que tenga todos los objetos
charts_artist_track_position = mongo(collection = "charts_artist_track_position", db = "Spotify_DM_TP" )
charts_artist_track_position$count()

df_charts <- charts_artist_track_position$find()
df_charts <- df_charts[!duplicated(df_charts), ]

#Algunos gráficos
plot(df_charts$position, df_charts$avg_streams) #Se observa una correlación negativa
boxplot(df_charts$avg_streams) #Hay varios outliers que se pueden analizar
hist(df_charts$avg_streams) #No se observa una distribucion normal
boxplot(df_charts$position) #No hay mucho para analizar
hist(df_charts$position) #La cantidad de posiciones es la misma, salvo cuando se aproxima a las primeras posiciones que disminuye


df_charts$logStreams <- log(df_charts$avg_streams, 2) #cambio de variable para que sea normal        
plot(df_charts$position, df_charts$logStreams) #correlacion negativa, mas stream, menor posicion
boxplot(df_charts$logStreams) #se siguen observando muchos outliers
hist(df_charts$logStreams) #la distribucion es mas normal

cor(df_charts$position, df_charts$logStreams) #-0,92


boxplot(df_charts$avg_streams)$stats 

charts_outliers <- df_charts[1:5][df_charts$avg_streams > boxplot(df_charts$avg_streams)$stats[5],] #data frame con los valores outliers que se ven en el boxplot
charts_outliers <- charts_outliers[!duplicated(charts_outliers$artist_track), ] #elimino duplicados segun artist_track

##############
#ANALISIS DE DATOS DE FEATURES
features_avg_pos = mongo(collection = "features_avg_mejorPosition_red", db = "Spotify_DM_TP" )
features_avg_pos$count() # 1997

features_avg_pos <- features_avg_pos$find() #paso a data frame

features_charts_outliers <- merge(charts_outliers, features_avg_pos, by.x="artist_track", by.y="artist_track") #integro features con los charts_outliers
names(features_charts_outliers)

boxplot(features_charts_outliers$avg_danceability) #se ven dos outliers
danceability_features_charts_outliers <- features_charts_outliers[c(1:4)][features_charts_outliers$avg_danceability < boxplot(features_charts_outliers$avg_danceability)$stats[1],] #identifico esos dos outliers 
head(danceability_features_charts_outliers) #"Bing Crosby/White Christmas" "The Weeknd/Alone Again"


features = mongo(collection = "features_avg_mejorPosition", db = "Spotify_DM_TP" ) #cargo base de datos mas completa de features
features$count()

features <- features$find() #hago data frame de features
features <- features[!duplicated(features), ] #elimino duplicados


charts_entera = mongo(collection = "charts", db = "Spotify_DM_TP" ) #base de datos charts completa
charts_entera$count() # 63600
charts_entera <- charts_entera$find() #hago data frame
charts_entera <- charts_entera[!duplicated(charts_entera), ] #elimino duplicados
charts_features <- merge(charts_entera, features, by.x="artist_track", by.y="artist_track") #integro features con los charts
names(charts_features)
charts_features <- charts_features[c(1,2,7,8, 11,13, 12)] #me quedo solo con las columnas que me interesan de "charts_features"
charts_features <- charts_features[!duplicated(charts_features), ] # elimino duplicados
danceability_outliers <- charts_features[charts_features$artist_track == danceability_features_charts_outliers$artist_track, ] #nuevo data frame con los resultados de los outliers detectados
danceability_outliers <- danceability_outliers[!duplicated(danceability_outliers$week_start), ] # elimino duplicados de week_start para limpiar el data frame 

#White Christmas: todos los años aparece en los charts en epocas navideñas
#Alone Again coincide con el lanzamiento del album al que pertenece, solo estuvo una semana en los charts




boxplot(features_charts_outliers$avg_energy)
energy_features_charts_outliers <- features_charts_outliers[c(1,5)]
head(energy_features_charts_outliers)



boxplot(features_charts_outliers$avg_loudness)
boxplot(features_charts_outliers$avg_speechiness)
boxplot(features_charts_outliers$avg_acousticness)
boxplot(features_charts_outliers$avg_instrumentalness)
boxplot(features_charts_outliers$avg_liveness)
boxplot(features_charts_outliers$avg_duration)
boxplot(features_charts_outliers$avg_valence)
boxplot(features_charts_outliers$avg_tempo)



plot(features_charts_outliers$position, features_charts_outliers$avg_acousticness)
plot(features_charts_outliers$position, features_charts_outliers$avg_danceability)
plot(features_charts_outliers$position, features_charts_outliers$avg_energy)



