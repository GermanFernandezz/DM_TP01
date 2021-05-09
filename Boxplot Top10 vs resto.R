library(mongolite)

features_avg_pos = mongo(collection = "features_avg_mejorPosition_red", db = "Spotify_DM_TP" )
features_avg_pos <- features_avg_pos$find()
names(features_avg_pos)
features_avg_pos$top <- features_avg_pos$mejorPosition
features_avg_pos$top[features_avg_pos$mejorPosition == 1] <- "Top 1"
features_avg_pos$top[features_avg_pos$mejorPosition > 1] <- "Top 2-10"
features_avg_pos$top[features_avg_pos$mejorPosition > 10] <- "Top 11-200"

pie(table(features_avg_pos$top),  main="Cantidad de temas por rango de posición")

boxplot(features_avg_pos$avg_danceability ~ features_avg_pos$top,
        col = c("blue", "red", "green"), 
        xlab = "Posicion en el chart", 
        ylab = "Danceability", 
        main = "Boxplot de Danceabilty según posición en chart")

boxplot(features_avg_pos$avg_energy ~ features_avg_pos$top,
        col = c("blue", "red", "green"), 
        xlab = "Posicion en el chart", 
        ylab = "Energy", 
        main = "Boxplot de Energy según posición en chart")

boxplot(features_avg_pos$avg_loudness ~ features_avg_pos$top,
        col = c("blue", "red", "green"),
        xlab = "Posicion en el chart", 
        ylab = "Loudness", 
        main = "Boxplot de Loudness según posición en chart",
        ylim = c(-15,0))

boxplot(features_avg_pos$avg_speechiness ~ features_avg_pos$top,
        col = c("blue", "red", "green"), 
        xlab = "Posicion en el chart", 
        ylab = "Speechiness", 
        main = "Boxplot de Speechiness según posición en chart",
        ylim = c(0, 0.5))

boxplot(features_avg_pos$avg_instrumentalness ~ features_avg_pos$top,
        col = c("blue", "red", "green"),
        xlab = "Posicion en el chart", 
        ylab = "Instrumentalness", 
        main = "Boxplot de Instrumentalness según posición en chart",
        ylim = c(0,0.0000004))

boxplot(features_avg_pos$avg_acousticness ~ features_avg_pos$top,
        col = c("blue", "red", "green"),
        xlab = "Posicion en el chart", 
        ylab = "Acousticness", 
        main = "Boxplot de Acousticnes según posición en chart",
        ylim = c(0,0.8))

boxplot(features_avg_pos$avg_liveness ~ features_avg_pos$top,
        col = c("blue", "red", "green"),
        xlab = "Posicion en el chart", 
        ylab = "Liveness", 
        main = "Boxplot de Liveness según posición en chart",
        ylim = c(0,0.6))

boxplot(features_avg_pos$avg_valence ~ features_avg_pos$top,
        col = c("blue", "red", "green"),
        xlab = "Posicion en el chart", 
        ylab = "Valence", 
        main = "Boxplot de Valence según posición en chart")

boxplot(features_avg_pos$avg_duration ~ features_avg_pos$top,
        col = c("blue", "red", "green"),
        xlab = "Posicion en el chart", 
        ylab = "Duration", 
        main = "Boxplot de Duration según posición en chart",
        ylim = c(1e+05,3.75e+05))

boxplot(features_avg_pos$avg_tempo ~ features_avg_pos$top,
        col = c("blue", "red", "green"),
        xlab = "Posicion en el chart", 
        ylab = "Tempo", 
        main = "Boxplot de Tempo según posición en chart")


library(modeest)
means_TOP1 <- apply(features_avg_pos[features_avg_pos$top == "Top 1",c(4:13)], 2, mean)
means_TOP10 <- apply(features_avg_pos[features_avg_pos$top == "Top 2-10",c(4:13)], 2, mean)
means_TOP200 <- apply(features_avg_pos[features_avg_pos$top == "Top 11-200",c(4:13)], 2, mean)
medians_TOP1 <- apply(features_avg_pos[features_avg_pos$top == "Top 1",c(4:13)], 2, median)
medians_TOP10 <- apply(features_avg_pos[features_avg_pos$top == "Top 2-10",c(4:13)], 2, median)
medians_TOP200 <- apply(features_avg_pos[features_avg_pos$top == "Top 11-200",c(4:13)], 2, median)
sd_TOP1 <- apply(features_avg_pos[features_avg_pos$top == "Top 1",c(4:13)], 2, sd)
sd_TOP10 <- apply(features_avg_pos[features_avg_pos$top == "Top 2-10",c(4:13)], 2, sd)
sd_TOP200 <- apply(features_avg_pos[features_avg_pos$top == "Top 11-200",c(4:13)], 2, sd)

medidas_tendencia_central <- data.frame(cbind(means_TOP1, means_TOP10, means_TOP200, medians_TOP1, medians_TOP10, medians_TOP200, sd_TOP1, sd_TOP10, sd_TOP200))
names(medidas_tendencia_central) <- c("Promedio Top 1", "Promedio Top 2-10", "Promedio Top 11-200", "Mediana Top 1", "Mediana Top 2-10", "Mediana 11-200", "Des. Std. Top 1", "Des. Std. Top 2-10", "Des. Std. 11-200")
rownames(medidas_tendencia_central) <- c("Danceability", "Energy", "Loudness", "Speechiness", "Acousticnes", "Instrumentalness", "Liveness", "Valence", "Tempo", "Duration")
