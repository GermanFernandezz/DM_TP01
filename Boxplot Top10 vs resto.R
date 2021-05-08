library(mongolite)
features_avg_pos = mongo(collection = "features_avg_mejorPosition_red", db = "Spotify_DM_TP" )
features_avg_pos <- features_avg_pos$find()
names(features_avg_pos)
features_avg_pos$top10 <- features_avg_pos$mejorPosition <= 10

boxplot(features_avg_pos$avg_danceability ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Danceability", 
        main = "Boxplot de Danceabilty según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(4)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(4)])

boxplot(features_avg_pos$avg_acousticness ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Acousticness", 
        main = "Boxplot de Acousticness según posición en chart")


boxplot(features_avg_pos$avg_energy ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Energy", 
        main = "Boxplot de Energy según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(5)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(5)])

boxplot(features_avg_pos$avg_loudness ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Loudness", 
        main = "Boxplot de Loudness según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(6)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(6)])

boxplot(features_avg_pos$avg_speechiness ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Speechiness", 
        main = "Boxplot de Speechiness según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(7)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(7)])

boxplot(features_avg_pos$avg_instrumentalness ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Instrumentalness", 
        main = "Boxplot de Instrumentalness según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(9)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(9)])

boxplot(features_avg_pos$avg_liveness ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Liveness", 
        main = "Boxplot de Liveness según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(10)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(10)])

boxplot(features_avg_pos$avg_valence ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Valence", 
        main = "Boxplot de Valence según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(11)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(11)])

boxplot(features_avg_pos$avg_duration ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Duration", 
        main = "Boxplot de Duration según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(13)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(13)])

boxplot(features_avg_pos$avg_tempo ~ features_avg_pos$top10, 
        names = c("Posiciones del 11 al 200", "Top 10"), 
        col = c("blue", "light blue"), 
        xlab = "Posicion en el chart", 
        ylab = "Tempo", 
        main = "Boxplot de Tempo según posición en chart")
summary(features_avg_pos[features_avg_pos$mejorPosition<=10,c(12)])
summary(features_avg_pos[features_avg_pos$mejorPosition>10,c(12)])

