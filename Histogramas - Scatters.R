library(mongolite)

features_avg_pos = mongo(collection = "features_avg_mejorPosition_red", db = "Spotify_DM_TP" )
features_avg_pos <- features_avg_pos$find()
names(features_avg_pos)

features_avg_pos$top <- features_avg_pos$mejorPosition
features_avg_pos$top[features_avg_pos$mejorPosition == 1] <- "Top 1"
features_avg_pos$top[features_avg_pos$mejorPosition > 1] <- "Top 2-10"
features_avg_pos$top[features_avg_pos$mejorPosition > 10] <- "Top 11-200"

par(mfrow=c(3, 1), mar=c(5, 10, 4, 2) )
hist(features_avg_pos$avg_danceability, xlab="Danceability", main= "Histograma de Danceability")

par(mfrow=c(3, 1), mar=c(5, 10, 4, 2) )
hist(features_avg_pos$avg_danceability[features_avg_pos$top == "Top 1"])
hist(features_avg_pos$avg_danceability[features_avg_pos$top == "Top 2-10"])
hist(features_avg_pos$avg_danceability[features_avg_pos$top == "Top 11-200"])

hist(features_avg_pos$avg_energy, xlab="Energy", main= "Histograma de Energy")
hist(features_avg_pos$avg_loudness, xlab="Loudness", main= "Histograma de Loudness")
hist(features_avg_pos$avg_speechiness, xlab="Speechiness", main= "Histograma de Speechiness")
hist(features_avg_pos$avg_acousticness, xlab="Acousticness", main= "Histograma de Acousticness")
hist(features_avg_pos$avg_instrumentalness, xlab="Instrumentalness", main= "Histograma de Instrumentalness")
hist(features_avg_pos$avg_liveness, xlab="Livenss", main= "Histograma de Liveness")
hist(features_avg_pos$avg_valence, xlab="Valence", main= "Histograma de Valence")
hist(features_avg_pos$avg_tempo, xlab="Tempo", main= "Histograma de Tempo")
hist(features_avg_pos$avg_duration, xlab="Duration", main= "Histograma de Duration")

dev.off()
plot(features_avg_pos[4:13], labels = names(features_avg_pos)[4:13])

features_avg_pos_top1 <- features_avg_pos[features_avg_pos$top == 'Top 1', c(1:14)]
plot(features_avg_pos_top1[4:13], labels = names(features_avg_pos_top1)[4:13])


features_avg_pos_top10 <- features_avg_pos[features_avg_pos$top == 'Top 2-10', c(1:14)]
plot(features_avg_pos_top10[4:13], labels = names(features_avg_pos_top10)[4:13])


plot(features_avg_pos_top10$avg_energy, features_avg_pos_top10$avg_loudness)
plot(features_avg_pos_top1$avg_energy, features_avg_pos_top1$avg_loudness)
plot(features_avg_pos$avg_energy, features_avg_pos$avg_loudness)
