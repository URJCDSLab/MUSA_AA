library(readr)

url <- "https://raw.githubusercontent.com/URJCDSLab/athletics_shiny_app/refs/heads/main/_aux/get_all_1500m/race_012_Olympic_Games_4_8_2024.csv?token=GHSAT0AAAAAADTBMIXY76EUQG36NHTHZCW42MXACMA"

datos <- read_csv(url)

print(datos)

library(dplyr)

split_cols <- grep("^\\d+m$", names(datos), value = TRUE)

# Euclídea sobre splits tal cual

X <- datos %>%
  select(all_of(split_cols)) %>%
  as.matrix()

# Si hay NA, dist() no los tolera bien; opción simple: imputar con la media de la columna
if (anyNA(X)) {
  col_means <- colMeans(X, na.rm = TRUE)
  for (j in seq_len(ncol(X))) {
    X[is.na(X[, j]), j] <- col_means[j]
  }
}

D <- as.matrix(dist(X, method = "euclidean"))
rownames(D) <- datos$Competitor
colnames(D) <- datos$Competitor

D[1:5, 1:5]

# Distancia comparando “forma de pacing” (z-score por corredor)
# Esto ignora si uno es globalmente más rápido y compara patrón (salida/ritmo/kick).

row_z <- function(v) {
  s <- sd(v, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(v)))
  (v - mean(v, na.rm = TRUE)) / s
}

Xshape <- t(apply(X, 1, row_z))

D_shape <- as.matrix(dist(Xshape, method = "euclidean"))
rownames(D_shape) <- datos$Competitor
colnames(D_shape) <- datos$Competitor

D_shape[1:5, 1:5]

# Distancia basada en correlación (muy usada para “perfiles”)
# Si dos perfiles tienen forma parecida aunque uno sea más rápido, saldrá cerca.

D_cor <- as.matrix(as.dist(1 - cor(t(X), use = "pairwise.complete.obs")))
rownames(D_cor) <- datos$Competitor
colnames(D_cor) <- datos$Competitor

D_cor[1:5, 1:5]


------
  
  library(dplyr)

# 1) Matriz de splits
split_cols <- grep("^\\d+m$", names(datos), value = TRUE)

X <- datos %>%
  select(all_of(split_cols)) %>%
  as.matrix()

# (si hubiera NA, imputación simple por columna)
if (anyNA(X)) {
  cm <- colMeans(X, na.rm = TRUE)
  for (j in seq_len(ncol(X))) X[is.na(X[, j]), j] <- cm[j]
}

# 2) Normalizar "estrategia" por corredor: z-score por fila
row_z <- function(v) {
  s <- sd(v)
  if (is.na(s) || s == 0) return(rep(0, length(v)))
  (v - mean(v)) / s
}
Xshape <- t(apply(X, 1, row_z))

# 3) Distancias + clustering jerárquico (Ward)
d <- dist(Xshape, method = "euclidean")
hc <- hclust(d, method = "ward.D2")

# 4) Elegir número de clusters (prueba varios y usa silhouette)
library(cluster)

k_grid <- 2:8
sil <- sapply(k_grid, function(k) {
  cl <- cutree(hc, k = k)
  mean(silhouette(cl, d)[, 3])
})

k_best <- k_grid[which.max(sil)]
k_best
sil

k_best=3

# 5) Cortar el dendrograma y asignar cluster
clusters <- cutree(hc, k = k_best)

res <- datos %>%
  transmute(
    Competitor,
    Place,
    Time,
    cluster = factor(clusters)
  ) %>%
  arrange(cluster, Place)

print(res, n = 50)

# 6) Visualizaciones rápidas
plot(hc, labels = datos$Competitor, main = "Clustering por estrategia (pacing)")
rect.hclust(hc, k = k_best, border = 2:6)

plot(k_grid, sil, type = "b", xlab = "k", ylab = "Mean silhouette",
     main = "Selección de k por silhouette")

----
  
  library(tidyr)

profile <- as.data.frame(Xshape)
colnames(profile) <- split_cols
profile$cluster <- factor(clusters)

cluster_mean <- profile %>%
  group_by(cluster) %>%
  summarise(across(all_of(split_cols), mean), .groups = "drop")

print(cluster_mean)


# Calcular métricas de estrategia

split_cols <- grep("^\\d+m$", names(datos), value = TRUE)

X <- datos[, split_cols] |> as.matrix()

# Normalización por corredor (estrategia pura)
row_z <- function(v) {
  s <- sd(v)
  if (is.na(s) || s == 0) return(rep(0, length(v)))
  (v - mean(v)) / s
}
Xshape <- t(apply(X, 1, row_z))

colnames(Xshape) <- split_cols

# Índices de tramos
early_idx  <- split_cols %in% c("100m","200m","300m","400m","500m")
late_idx   <- split_cols %in% c("1100m","1200m","1300m","1400m","1500m")
kick_idx   <- split_cols %in% c("1300m","1400m","1500m")
start_idx  <- split_cols %in% c("100m","200m","300m")

strategy_metrics <- data.frame(
  Competitor = datos$Competitor,
  slope      = rowMeans(Xshape[, late_idx]) - rowMeans(Xshape[, early_idx]),
  kick       = rowMeans(Xshape[, kick_idx]),
  fast_start = rowMeans(Xshape[, start_idx]),
  variability = apply(Xshape, 1, sd)
)

head(strategy_metrics)


# Reglas automáticas de etiquetado
label_strategy <- function(slope, kick, fast_start, variability) {
  
  if (kick < -0.5 & slope < 0) {
    return("Kick final fuerte")
  }
  
  if (fast_start < -0.5 & slope > 0.3) {
    return("Salida rápida, se apaga")
  }
  
  if (abs(slope) < 0.2 & variability < 0.6) {
    return("Ritmo muy constante")
  }
  
  if (slope > 0.5) {
    return("Desaceleración progresiva")
  }
  
  if (slope < -0.5) {
    return("Negativa progresiva")
  }
  
  return("Estrategia mixta")
}

strategy_metrics$label <- mapply(
  label_strategy,
  strategy_metrics$slope,
  strategy_metrics$kick,
  strategy_metrics$fast_start,
  strategy_metrics$variability
)

strategy_metrics |> 
  dplyr::arrange(label, slope) |> 
  print(n = 50)

