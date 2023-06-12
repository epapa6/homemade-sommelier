# Papa Emanuele - 844888
# Furini Andrea - 845118


# ---------------------------------------------------------------------------- #
### Dipendenze ###

library('dplyr')      # count
library("groupdata2") # downsample
library('corrplot')   # corrplot
library('FactoMineR') # PCA
library('factoextra') # eigenvalue
library('caret')      # train


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


# ---------------------------------------------------------------------------- #
### Analisi esplorativa ###

wine.analysis <- subset(wine, select = -c(target.quality, target.type))

summary(wine.analysis)

# boxplot per visualizzare la distribuzione dei valori nel dataset
png(filename = "./img/boxplot-attributes.png", width = 1500, height = 800)
par(mfrow = c(2, (ncol(wine.analysis)) / 2))
invisible(lapply(1:ncol(wine.analysis),
                 function(i) boxplot(wine.analysis[, i],
                                     cex.lab = 2,
                                     xlab = colnames(wine.analysis[i]))))
dev.off()

# matrice di correlazione degli attributi
wine.correlation <- wine
wine.correlation$target.quality <- as.numeric(wine$target.quality)
wine.correlation$target.type <- as.numeric(wine$target.type)

correlation <- cor(wine.correlation)

png(filename = "./img/correlation.png", width = 1000, height = 1000)
corrplot(correlation, method = "color", tl.col = "black")
dev.off()

rm(wine.analysis, wine.correlation, correlation)


# ---------------------------------------------------------------------------- #
### PCA ###

wine.PCA <- PCA(subset(wine, select = -c(target.quality, target.type, quality)), 
                scale.unit = TRUE, graph = FALSE)

eig.val <- get_eigenvalue(wine.PCA)
eig.val

rm(eig.val)

png(filename="./img/PCA-eigenvalues.png", width = 1480, height = 550)
fviz_eig(wine.PCA, addlabels = TRUE)
dev.off()

png(filename="./img/PCA-variables.png",  width = 550, height = 550)
fviz_pca_var(wine.PCA, col.var = "contrib")
dev.off()

png(filename="./img/PCA-individuals-group-type.png", width = 1000, height = 1000)
fviz_pca_ind(wine.PCA, label = "none", col.ind = wine$target.type)
dev.off()

png(filename="./img/PCA-individuals-group-quality.png", width = 1000, height = 1000)
fviz_pca_ind(wine.PCA, label = "none", col.ind = wine$target.quality)
dev.off()

png(filename="./img/PCA-individuals-quality.png", width = 1000, height = 1000)
fviz_pca_ind(wine.PCA, label = "none", col.ind = wine$quality)
dev.off()

png(filename="./img/PCA-individuals.png", width = 1000, height = 1000)
fviz_pca_ind(wine.PCA, label = "none", col.ind = "cos2")
dev.off()

png(filename="./img/PCA-qualities.png", width = 1480, height = 550)
fviz_cos2(wine.PCA, choice = "var")
dev.off()

rm(wine.PCA)


# ---------------------------------------------------------------------------- #
### Selezione Feature e Splitting ###

# selezione attributi a seguito delle PCA
attributes <- c(
  "target.quality",
  "target.type",
  
  "total.sulfur.dioxide",
  "volatile.acidity",
  "density",
  "chlorides",
  "sulphates"
)

# funzione di split dei dataset
split.data = function(data, p = 0.7, s = 1) {
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)],]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]],]
  return(list(train = train, test = test))
}

wine.quality.all <- split.data(select(wine.quality, all_of(attributes)), p = 0.7)
wine.quality.train <- wine.quality.all$train 
wine.quality.test <- wine.quality.all$test

wine.type.all <- split.data(select(wine.type, all_of(attributes)), p = 0.7)
wine.type.train <- wine.type.all$train 
wine.type.test <- wine.type.all$test

rm(split.data, attributes, wine.quality.all, wine.type.all)


# ---------------------------------------------------------------------------- #
### Training & Testing ###


# Quality - naive bayes
nb.q.model <- train(target.quality~ ., data = wine.quality.train, method = "naive_bayes", trace = FALSE)
nb.q.pred <- predict(nb.q.model, wine.quality.test)
nb.q.probs <- predict(nb.q.model, wine.quality.test, type="prob")
result <- confusionMatrix(nb.q.pred, wine.quality.test$target.quality, positive = "good", mode="prec_recall")
result

# Quality - neural network
nn.q.model <- train(target.quality~ ., data = wine.quality.train, method = "nnet", trace = FALSE)
nn.q.pred <- predict(nn.q.model, wine.quality.test)
nn.q.probs <- predict(nn.q.model, wine.quality.test, type="prob")
result <- confusionMatrix(nn.q.pred, wine.quality.test$target.quality, positive = "good", mode="prec_recall")
result

# Type - naive bayes
nb.t.model <- train(target.type~ ., data = wine.type.train, method = "naive_bayes", trace = FALSE)
nb.t.pred <- predict(nb.t.model, wine.type.test)
nb.t.probs <- predict(nb.t.model, wine.type.test, type="prob")
result <- confusionMatrix(nb.t.pred, wine.type.test$target.type, positive = "red", mode="prec_recall")
result

# Type - neural network
nn.t.model <- train(target.type~ ., data = wine.type.train, method = "nnet", trace = FALSE)
nn.t.pred <- predict(nn.t.model, wine.type.test)
nn.t.probs <- predict(nn.t.model, wine.type.test, type="prob")
result <- confusionMatrix(nn.t.pred, wine.type.test$target.type, positive = "red", mode="prec_recall")
result


# ---------------------------------------------------------------------------- #
### Training & Testing (10-fold) ###

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                        classProbs= TRUE, savePredictions = "all",
                        summaryFunction= twoClassSummary, verboseIter=FALSE)

# Quality - naive bayes (10-fold)
nb.q10f.model <- train(target.quality~ ., data = wine, method = "naive_bayes", metric= "ROC", trControl = control, trace = FALSE)
result <- confusionMatrix(nb.q10f.model, mode = "prec_recall", positive = "good", norm="none")
result

# Quality - neural network (10-fold)
nn.q10f.model <- train(target.quality~ ., data = wine, method = "nnet", metric = "ROC", trControl = control, trace = FALSE)
result <- confusionMatrix(nn.q10f.model, mode = "prec_recall", positive = "good", norm="none")
result

# Type - naive bayes (10-fold)
nb.t10f.model <- train(target.type~ ., data = wine, method = "naive_bayes", metric= "ROC", trControl = control, trace = FALSE)
result <- confusionMatrix(nb.t10f.model, mode = "prec_recall", positive = "red", norm="none")
result

# Type - neural network (10-fold)
nn.t10f.model <- train(target.type~ ., data = wine, method = "nnet", metric = "ROC", trControl = control, trace = FALSE)
result <- confusionMatrix(nn.t10f.model, mode = "prec_recall", positive = "red", norm="none")
result

rm(control, result)

