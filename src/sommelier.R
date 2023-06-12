# Papa Emanuele - 844888
# Furini Andrea - 845118


# ---------------------------------------------------------------------------- #
### Dipendenze ###

library('dplyr')      # count
library("groupdata2") # downsample


# ---------------------------------------------------------------------------- #
### Gestione datasets ###

# Join dei due datasets
wine.red <- read.csv('./datasets/WineQT-Red.csv')
wine.red <- unique(wine.red)
wine.red['target.type'] <- 'red'
wine.red$target.type <- as.factor(wine.red$target.type)

count(wine.red, quality)
png(filename = "./img/distribution-quality-red.png", width = 600, height = 600)
barplot(table(wine.red$quality), main = "Distribuzione Quality - Rosso")
dev.off()


wine.white <-read.csv('./datasets/WineQT-White.csv')
wine.white <- unique(wine.white)
wine.white['target.type'] <- 'white'
wine.white$target.type <- as.factor(wine.white$target.type)

count(wine.white, quality)
png(filename = "./img/distribution-quality-white.png", width = 600, height = 600)
barplot(table(wine.white$quality), main = "Distribuzione Quality - Bianco")
dev.off()


wine <- merge(wine.red, wine.white, all = TRUE)

count(wine, quality)
png(filename = "./img/distribution-quality-all.png", width = 600, height = 600)
barplot(table(wine$quality), main = "Distribuzione Quality")
dev.off()


# creo la colonna di target della qualità
target.quality <- c(wine$quality)
target.quality[target.quality > 5] <- 'good'
target.quality[target.quality < 6] <- 'bad'
wine['target.quality'] <- target.quality
wine$target.quality <- as.factor(wine$target.quality)


count(wine, target.quality)
png(filename = "./img/distribution-target-quality.png", width = 600, height = 600)
barplot(table(wine$target.quality), main = "Distribuzione Target - Quality")
dev.off()

count(wine, target.type)
png(filename = "./img/distribution-target-type.png", width = 600, height = 600)
barplot(table(wine$target.type), main = "Distribuzione Target - Type")
dev.off()

# creo due downsample del dataset per lo studio
wine.type <- downsample(wine, cat_col = "target.type")
count(wine.type, wine.type$target.type)
png(filename = "./img/ds-distribution-target-type.png", width = 600, height = 600)
barplot(table(wine.type$target.type), main = "Distribuzione Downsample Target - Type")
dev.off()

wine.quality <- downsample(wine, cat_col = "target.quality")
count(wine.quality, wine.quality$target.quality)
png(filename = "./img/ds-distribution-target-quality.png", width = 600, height = 600)
barplot(table(wine.quality$target.quality), main = "Distribuzione Downsample Target - Quality")
dev.off()


rm(wine.red, wine.white, target.quality)