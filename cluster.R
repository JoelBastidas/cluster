#### Cargando la base de datos ####
democr_latam <- as.data.frame(read_sav(file.choose())) 


#### Verificando si hay valores perdidos en el conjunto de datos ####
datos_perdidos <- apply(is.na(democr_latam), 2, any)

if (any(datos_perdidos)) {
  cat("Columnas con valores perdidos:\n")
  print(names(democr_latam)[missing_values])
} else {
  cat("No hay valores perdidos en el conjunto de datos.\n")
}


#### Seleccionando las variables para el análisis de conglomerados ####
datos_cluster <- democr_latam[, c("ppelec", "libciv", "fdelgob", "partpk", "cultpk")]

#### Escalar los datos ####
datos_cluster_scale <- scale(datos_cluster)


#### Calculando los coeficientes de aglomeración ####
res_single=agnes(scale(datos_cluster_scale),
          method="single")
res_single
#single = 0.6592985 

res_complete=agnes(scale(datos_cluster_scale),
                 method="complete")
res_complete
#completo = 0.7737342

#### comparando entre el método complete y ward.D ####

cluster_completo <- agnes(datos_cluster_scale, method = "complete")
cluster_ward <- hclust(dist(datos_cluster_scale), method = "ward.D")

par(mfrow=c(1,2))
plot(cluster_completo, main = "Método Completo", xlab = "")
rect.hclust(cluster_completo,2,border = "red4")
plot(cluster_ward, main = "Método de Ward.D", xlab = "")
rect.hclust(cluster_ward,2,border = "red4")



#### Calculando el número óptimo de clúster ####
diss.datos_cluster <- daisy(scale(datos_cluster_scale))  
res <- agnes(diss.datos_cluster, method = "complete")  

par(mfrow = c(1, 3))
for (h in 2:4) {
  conglomerados <- cutree(as.dendrogram(res), h)
  plot(silhouette(conglomerados, diss.datos_cluster))
}



#nos quedamos con 4 cluster porque averange=0.30 


#### Gráfico de los 4 cluster ####
par(mfrow=c(1,1))

#Gráfico radial
colors = c("red", "blue", "green", "black")
clus4 = cutree(as.dendrogram(res), 4)
plot(as.phylo(as.dendrogram(res)), type = "fan", tip.color = colors[clus4],
     label.offset = 0.3, cex = 0.7)# tam etiq y letra

#Gráfico del árbol
plot(as.phylo(as.dendrogram(res)), type = "cladogram", cex = 0.6,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "steelblue")


#### Calculando los grupos utilizando el número óptimo de clústeres####
mejor_modelo <- agnes(diss.datos_cluster, method = "complete")
grupos <- cutree(mejor_modelo, k = 4)

df_grupos <- data.frame(grupo = grupos)

#### Uniendo las dos tablas ####
datos_cluster_con_grupos <- cbind(datos_cluster, df_grupos)

#### Hallando la media y desviación estándar por grupo ####

resultados <- lapply(datos_cluster_con_grupos, function(col) {
  tapply(col, grupos, function(x) c(media = mean(x), desviacion = sd(x)))
})

# En el grupo 4 es imposible hallar la desviación estándar porque contiene solo 1 dato.


