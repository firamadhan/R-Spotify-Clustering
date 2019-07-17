# Collect data with Spotify API
# Fakhri Ihsan Ramadhan

# Load library
library(Rspotify)
library(tidyverse)

# Set timezone to avoid tz error
Sys.setenv(TZ='GMT')

# Set authorization keys provided by Spotify
keys <- spotifyOAuth("song_analysis","YOURTOKENHERE","YOURTOKENHERE")

# Load data
path <- file.path("C:","Users","Fakhri Ihsan","Documents","SBM","Semester 2","1 - Advanced Statistics","Spotify chart analysis")
top_id <- read.csv(paste(path,"regional-id-weekly-latest.csv", sep = "/"), header= TRUE)
top_us <- read.csv(paste(path,"regional-us-weekly-latest.csv", sep = "/"), header= TRUE)
top_gl <- read.csv(paste(path,"regional-global-weekly-latest.csv", sep = "/"), header= TRUE)

# Here I used advanced codes from medium.freecodecamp.org
# Formula (getFeatures) that extracts the audio features for any specific ID stored as a vector
getFeatures <- function (vector_id, token) 
{
  link <- httr::GET(paste0("https://api.spotify.com/v1/audio-features/?ids=", 
                           vector_id), httr::config(token = token))
  list <- httr::content(link)
  return(list)
}

# get_features formula extracts the audio features for the track ID's vector, and returns them in a data.frame
get_features <- function (x) 
{
  getFeatures2 <- getFeatures(vector_id = x, token = spotifyToken)
  features_output <- do.call(rbind, lapply(getFeatures2$audio_features, data.frame, stringsAsFactors=FALSE))
}

# Using the formula created above, I was able to extract the audio features for each track. In order to do so, I needed a vector containing each track ID.
# Define vector for Track ID of Indonesian Top 200 Chart; since Spotify API limit is 100
v_id_1 <- paste(as.character(top_id$URL[1:100]), sep ="", collapse = ",")
v_id_2 <- paste(as.character(top_id$URL[101:200]), sep ="", collapse = ",")

# Define vector for Track ID of Global Top 200 Chart; since Spotify API limit is 100
v_gl_1 <- paste(as.character(top_gl$URL[1:100]), sep ="", collapse = ",")
v_gl_2 <- paste(as.character(top_gl$URL[101:200]), sep ="", collapse = ",")

# Get song features for Indonesian Top 200 Chart
id_1 <- get_features(v_id_1)
id_2 <- get_features(v_id_2)

# Get song features for Global Top 200 Chart
gl_1 <- get_features(v_gl_1)
gl_2 <- get_features(v_gl_2)

# Combine the result into single data frame respectively
gl_combine <- rbind(gl_1,gl_2)
id_combine <- rbind(id_1,id_2)

# Export to CSV, then merge with initial data frame with Excel
write.csv(gl_combine, "gl_combine.csv")
write.csv(id_combine, "id_combine.csv")