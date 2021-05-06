###ANALISIS DE VALORES ATIPICOS MULTIVARIADO: ACOUSTICNESS, DANCEABILITY Y POSITION
charts_mejorPosicion = mongo(collection = "charts_mejorPosicion", db = "DMUBA_Spotify" )
charts_mejorPosicion$count()

charts_mejorPosicion <- charts_mejorPosicion$find()
charts_mejorPosicion <- charts_mejorPosicion[!duplicated(charts_mejorPosicion), ]

charts_features1 <- merge(charts_mejorPosicion, features_avg_pos, by.x="artist_track", by.y="artist_track")
charts_features1_red <- charts_features1[c(1,5,8,9,10,11,12,13,14,15,16,17)] #me quedo solo con las columnas que me interesan
charts_features1_numbers <- charts_features1[c(5,8,12)] #me quedo solo con las columnas que me interesan (en numeros)
charts_features1_numbers <- charts_features1_numbers[!duplicated(charts_features1_numbers), ] # elimino duplicados

#DISTANCIA DE MAHALANOBIS

vector_medias = colMeans(charts_features1_numbers)
matriz_var_cov = cov(charts_features1_numbers)

#creamos una variable con la distancia
charts_features1_numbers$maha = sqrt(mahalanobis(charts_features1_numbers,vector_medias,matriz_var_cov))

#Los 3 registros mas distantes

top_maha <- head(charts_features1_numbers[order(charts_features1_numbers$maha,decreasing = TRUE),],3)
#1 (871) Juice WRLD/Juice WRLD Speaks From Heaven - Outro
#2 (278) Billie Eilish/goodbye
#3 (1781) The Weeknd/Alone Again

#LOF Local Outlier Factor
library(Rlof)

#Score para K vecinos
charts_features1_numbers$LOF_score <-lof(charts_features1_numbers[,c(1:3)], k=5)

#Los 3 registros mas distantes
top_LOF <- head(charts_features1_numbers[order(charts_features1_numbers$LOF_score, decreasing = TRUE),],3)
# 1 (291) Bing Crosby/White Christmas 
# 2 (1939) XXXTENTACION/Guardian angel
# 3 (1011) KIDS SEE GHOSTS/Freeee (Ghost Town Pt. 2)

#grafico donde se ubican
library(scatterplot3d)

lof_3d <- scatterplot3d(x = charts_features1_numbers[,1], y = charts_features1_numbers[,2], z = charts_features1_numbers[,3],
                        color=ifelse(charts_features1_numbers$LOF_score>=min(top_LOF$LOF_score),"red","black"), xlab="Posicion",ylab="Danceability",zlab="Acousticness" , main="3D Scatterplot LOF Scores")

library(isotree)
set.seed(1)

#Ajusto un modelo
#agrego la columna
charts_features1_numbers$iso <- isolation.forest(charts_features1_numbers[,c(1:3)], ntrees = 3, output_score=TRUE)$score

#armo tabla
top_iso <- head(charts_features1_numbers[order(charts_features1_numbers$iso, decreasing = TRUE),],3)
# 1 (1781) The Weeknd/Alone Again
# 2 (1243) Mac Miller/Once A Day
# 3 (1273) Manuel Turizo/Culpables