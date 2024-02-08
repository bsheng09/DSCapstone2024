#----------DS Capstone Project---------#
#Spring2024
#BridgetSheng 

#--------Part1-------#
#Read into the original data 
s2023 <- read.csv(file =  "/Users/yutongsheng/Downloads/spotify2023.csv", header = TRUE)
nrow(s2023)
ncol(s2023)
head(s2023)
summary(s2023$streams)


#create the subset includeing variables I want to explore 
s2023sub <- s2023[c("track_name", "artist.s._name", "streams","bpm", "danceability_.", 
                    "valence_.", "energy_.", "speechiness_.")]
dim(s2023sub)
head(s2023sub)

#check missing values
any(is.na(s2023sub$streams))
any(is.na(s2023sub$bpm))
any(is.na(s2023sub$danceability_.))
any(is.na(s2023sub$valence_.))
any(is.na(s2023sub$energy_.))
any(is.na(s2023sub$speechiness_.))

s2023sub$streamsClean <- as.numeric(s2023sub$streams)
head(s2023sub)
is.na(s2023sub$streamsClean)

#change variable names
names(s2023sub)[5] <- "danceability"
names(s2023sub)[6] <- "valence"
names(s2023sub)[7] <- "energy"
names(s2023sub)[8] <- "speechiness"
head(s2023sub)

# merge variables 
s2023sub$song <- paste(s2023sub$track_name, s2023sub$artist.s._name, sep = "-")
head(s2023sub)
dim(s2023sub)

#initial visualization
par(mfrow = c(2, 3))
plot(s2023sub$bpm, s2023sub$streamsClean, pch = 18, col = "lightblue")
plot(s2023sub$danceability, s2023sub$streamsClean, pch = 18, col = "lightgreen")
plot(s2023sub$valence, s2023sub$streamsClean, pch = 18, col = "yellow")
plot(s2023sub$energy, s2023sub$streamsClean, pch = 18, col = "lightpink")
plot(s2023sub$speechiness, s2023sub$streamsClean, pch = 18, col = "purple")

par(mfrow = c(2, 3))
boxplot(s2023sub$bpm, main = "Distribution of BPM", col="lightblue")
boxplot(s2023sub$danceability, main = "Distribution of Dancebility", col="lightgreen")
boxplot(s2023sub$valence, main = "Distribution of Valence", col="yellow")
boxplot(s2023sub$energy, main = "Distribution of Energy", col="lightpink")
boxplot(s2023sub$speechiness, main = "Distribution of Speechiness", col="purple")


