# Clustering Global Top 200
# Fakhri Ihsan Ramadhan
# Load data
path <- file.path("C:","Users","Fakhri Ihsan","Documents","SBM","Semester 2","1 - Advanced Statistics","Spotify chart analysis")
top_gl_df <- read.csv(paste(path,"gl_combine.csv", sep = "/"), header= TRUE)

# Load library
library(tidyverse)
library(tidyselect)
library(cluster)
library(plotly)

# Select relevant attributes
top_gl_att <- top_gl_df %>%
  select("acousticness","energy","speechiness") %>%
  .[complete.cases(.),]

# Clustering
hc_top_gl <- hclust(dist(top_gl_att))

# Plot
plot(hc_top_gl)

# K-Means
km_top_gl <- kmeans(top_gl_att, 4)


# Cluster plot
clusplot(top_gl_att, km_top_gl$cluster, color = TRUE, shade = TRUE)

# Make 3D plot
plot_ly(top_gl_df, x= ~acousticness, y= ~energy, z= ~Streams,
        color= ~factor(km_top_gl$cluster)) %>%
  add_markers()

# Additional steps
# Elbow check
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(top_gl_att, nc=6)

# PCA
pca_spotify <- princomp(top_gl_att, cor = TRUE)
summary(pca_spotify)
pca_spotify$loadings

# Make distribution
ggplot(top_gl_df, aes(x=acousticness)) + geom_density()
ggplot(top_gl_df, aes(x=danceability)) + geom_density()
ggplot(top_gl_df, aes(x=energy)) + geom_density()
ggplot(top_gl_df, aes(x=instrumentalness)) + geom_density()
ggplot(top_gl_df, aes(x=liveness)) + geom_density()
ggplot(top_gl_df, aes(x=loudness)) + geom_density()
ggplot(top_gl_df, aes(x=speechiness)) + geom_density()
ggplot(top_gl_df, aes(x=valence)) + geom_density()
