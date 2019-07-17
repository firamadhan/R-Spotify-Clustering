# Clustering Indonesia Top 200
# Fakhri Ihsan Ramadhan
# Load data
path <- file.path("C:","Users","Fakhri Ihsan","Documents","SBM","Semester 2","1 - Advanced Statistics","Spotify chart analysis")
top_id_df <- read.csv(paste(path,"id_combine.csv", sep = "/"), header= TRUE)

# Load library
library(tidyverse)
library(tidyselect)
library(cluster)
library(plotly)

# Select relevant attributes
top_id_att <- top_id_df %>%
  select("acousticness","energy","speechiness") %>%
  .[complete.cases(.),]

# Clustering
hc_top_id <- hclust(dist(top_id_att))

# Plot
plot(hc_top_id)

# K-Means
km_top_id <- kmeans(top_id_att, 3)

# Cluster plot
clusplot(top_id_att, km_top_id$cluster, color = TRUE, shade = TRUE)

# Make 3D plot
plot_ly(top_id_df, x= ~acousticness, y= ~energy, z= ~Streams,
        color= ~factor(km_top_id$cluster)) %>%
  add_markers()

# Elbow
wssplot(top_id_att, nc=6)
