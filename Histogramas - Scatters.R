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
dev.off()
par(mfrow=c(3, 1), mar=c(5, 10, 4, 2) )
hist(features_avg_pos$avg_danceability[features_avg_pos$top == "Top 1"])
hist(features_avg_pos$avg_danceability[features_avg_pos$top == "Top 2-10"])
hist(features_avg_pos$avg_danceability[features_avg_pos$top == "Top 11-200"])

par(mfrow=c(4, 2), mar=c(5, 10, 4, 2) )
hist(features_avg_pos$avg_danceability, xlab="Danceability", main= "Histograma de Danceability")
hist(features_avg_pos$avg_energy, xlab="Energy", main= "Histograma de Energy")
hist(features_avg_pos$avg_loudness, xlab="Loudness", main= "Histograma de Loudness")
hist(features_avg_pos$avg_speechiness, xlab="Speechiness", main= "Histograma de Speechiness")
hist(features_avg_pos$avg_acousticness, xlab="Acousticness", main= "Histograma de Acousticness")
hist(features_avg_pos$avg_instrumentalness, xlab="Instrumentalness", main= "Histograma de Instrumentalness")
hist(features_avg_pos$avg_liveness, xlab="Livenss", main= "Histograma de Liveness")
hist(features_avg_pos$avg_valence, xlab="Valence", main= "Histograma de Valence")
hist(features_avg_pos$avg_tempo, xlab="Tempo", main= "Histograma de Tempo")
hist(features_avg_pos$avg_duration, xlab="Duration", main= "Histograma de Duration")


media <- apply(features_avg_pos[,4:13], 2, mean)
mediana <- apply(features_avg_pos[,4:13], 2, median)
varianza <- apply(features_avg_pos[,4:13], 2, var)
desvio <- apply(features_avg_pos[,4:13], 2, sd)
medidas_tendencia_central <- data.frame(cbind(media, mediana, varianza, desvio))
names(medidas_tendencia_central) <- c("Promedio Top 1", "Promedio Top 2-10", "Promedio Top 11-200", "Mediana Top 1", "Mediana Top 2-10", "Mediana 11-200", "Des. Std. Top 1", "Des. Std. Top 2-10", "Des. Std. 11-200")
rownames(medidas_tendencia_central) <- c("Danceability", "Energy", "Loudness", "Speechiness", "Acousticnes", "Instrumentalness", "Liveness", "Valence", "Tempo", "Duration")




plot(features_avg_pos$avg_danceability)
plot(sort(features_avg_pos$avg_danceability))
plot(features_avg_pos$avg_energy)
plot(sort(features_avg_pos$avg_energy))





dev.off()
plot(features_avg_pos[4:11], labels = names(features_avg_pos)[4:11])
cor_top200 <- cor(features_avg_pos[,4:13])
library(gplots)
library(RColorBrewer)

dev.off()
par(mar=c(3,3,3,3))
my_palette <- colorRampPalette(c("green", "black", "red"))(n = 100)
heatmap.2(cor_top200,
          cellnote = round(cor_top200,2), 
          notecol="white",     
          trace="none",        
          margins = c(9,15), # 1er numero: margen inferior, 2do: margen izquierdo
          col=my_palette,  
          cexCol=0.75,
          cexRow=0.75,
          dendrogram="none",
          symm= T,
          Rowv=F) 

features_avg_pos_top1 <- features_avg_pos[features_avg_pos$top == 'Top 1', c(1:14)]
plot(features_avg_pos_top1[4:13], labels = names(features_avg_pos_top1)[4:13])
cor_top1 <- cor(features_avg_pos_top1[,4:13])
heatmap.2(cor_top1,
          cellnote = round(cor_top1,2), 
          notecol="white",     
          trace="none",        
          margins = c(9,15), # 1er numero: margen inferior, 2do: margen izquierdo
          col=my_palette,  
          cexCol=0.75,
          cexRow=0.75,
          dendrogram="none",
          symm= T,
          Rowv=F) 

features_avg_pos_top2_10 <- features_avg_pos[features_avg_pos$top == 'Top 2-10', c(1:14)]
plot(features_avg_pos_top10[4:13], labels = names(features_avg_pos_top10)[4:13])
cor_top2_10 <- cor(features_avg_pos_top2_10[,4:13])
heatmap.2(cor_top2_10,
          cellnote = round(cor_top2_10,2), 
          notecol="white",     
          trace="none",        
          margins = c(9,15), # 1er numero: margen inferior, 2do: margen izquierdo
          col=my_palette,  
          cexCol=0.75,
          cexRow=0.75,
          dendrogram="none",
          symm= T,
          Rowv=F) 

features_avg_pos_top10 <- features_avg_pos[features_avg_pos$mejorPosition < 11, c(1:14)]

plot(features_avg_pos_top10$avg_energy, features_avg_pos_top10$avg_loudness)
plot(features_avg_pos_top1$avg_energy, features_avg_pos_top1$avg_loudness)
plot(features_avg_pos$avg_energy, features_avg_pos$avg_loudness)
