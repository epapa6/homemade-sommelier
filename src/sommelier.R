# Papa Emanuele - 844888
# Furini Andrea - 845118

# ---------------------------------------------------------------------------- #

# svuoto memoria
rm(list=ls())
# pulisco la console
cat("\014")


# ---------------------------------------------------------------------------- #
### Dipendenze ###

library('dplyr')      # count, mutate_if
library("groupdata2") # downsample
library('corrplot')   # corrplot
library('FactoMineR') # PCA
library('factoextra') # eigenvalue
library('caret')      # train
library('ROCR')       # ROC curve


# ---------------------------------------------------------------------------- #
### Gestione datasets ###

# join dei due datasets
wine.red <- read.csv('./datasets/WineQT-Red.csv')
wine.red <- unique(wine.red)
wine.red['target.type'] <- 'red'
wine.red$target.type <- as.factor(wine.red$target.type)

count(wine.red, quality)

wine.white <-read.csv('./datasets/WineQT-White.csv')
wine.white <- unique(wine.white)
wine.white['target.type'] <- 'white'
wine.white$target.type <- as.factor(wine.white$target.type)

count(wine.white, quality)

png(filename = "./img/distribution-quality.png", width = 1300, height = 600)
par(mfrow = c(1:2))
barplot(table(wine.red$quality), main = "Distribuzione Quality - Rosso")
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

count(wine, target.type)
png(filename = "./img/distribution-target.png", width = 1250, height = 600)
par(mfrow = c(1:2))
barplot(table(wine$target.quality), main = "Distribuzione Target - Quality")
barplot(table(wine$target.type), main = "Distribuzione Target - Type")
dev.off()

# creo due downsample del dataset per lo studio
wine.type <- downsample(wine, cat_col = "target.type")
count(wine.type, wine.type$target.type)

wine.quality <- downsample(wine, cat_col = "target.quality")
count(wine.quality, wine.quality$target.quality)

png(filename = "./img/ds-distribution-target.png", width = 1250, height = 600)
par(mfrow = c(1:2))
barplot(table(wine.quality$target.quality), main = "Distribuzione Downsample Target - Quality")
barplot(table(wine.type$target.type), main = "Distribuzione Downsample Target - Type")
dev.off()


rm(wine.red, wine.white, target.quality)


# ---------------------------------------------------------------------------- #
### Analisi esplorativa ###

wine.analysis <- subset(wine, select = -c(target.quality, target.type))

summary(wine.analysis)

# boxplot per visualizzare la distribuzione dei valori nel dataset
png(filename = "./img/boxplot-attributes.png", width = 1500, height = 800)
par(mfrow = c(2, 6))
invisible(lapply(1:ncol(wine.analysis),
                 function(i) boxplot(wine.analysis[, i],
                                     cex.lab = 2,
                                     xlab = colnames(wine.analysis[i]))))
dev.off()

wine <- subset(wine, select = -c(quality))

# matrice di correlazione degli attributi
wine.correlation <- wine
wine.correlation$target.quality <- as.numeric(wine$target.quality)
wine.correlation$target.type <- as.numeric(wine$target.type)

correlation <- cor(wine.correlation)

png(filename = "./img/correlation.png", width = 1000, height = 1000)
corrplot(correlation, method = "number", type = 'lower', diag = FALSE, tl.col = "black", number.cex = 1.2)
dev.off()

rm(wine.analysis, wine.correlation, correlation)


# ---------------------------------------------------------------------------- #
### PCA ###

wine.PCA <- PCA(subset(wine, select = -c(target.quality, target.type)), 
                scale.unit = TRUE, graph = FALSE)

eig.val <- get_eigenvalue(wine.PCA)
eig.val

rm(eig.val)

png(filename="./img/PCA-eigenvalues.png", width = 1480, height = 550)
fviz_eig(wine.PCA, title = "", addlabels = TRUE)+theme(axis.text.x = element_text(size = 15))
dev.off()

png(filename="./img/PCA-variables.png",  width = 550, height = 550)
fviz_pca_var(wine.PCA, title = "", col.var = "contrib")
dev.off()

png(filename="./img/PCA-individuals-group-type.png", width = 1000, height = 1000)
fviz_pca_ind(wine.PCA, title = "Individuals - Type", label = "none", col.ind = wine$target.type)
dev.off()

png(filename="./img/PCA-individuals-group-quality.png", width = 1000, height = 1000)
fviz_pca_ind(wine.PCA, title = "Individuals - Quality",label = "none", col.ind = wine$target.quality)
dev.off()

# png(filename="./img/PCA-individuals-quality.png", width = 1000, height = 1000)
# fviz_pca_ind(wine.PCA, label = "none", col.ind = wine$quality)
# dev.off()

png(filename="./img/PCA-individuals.png", width = 1000, height = 1000)
fviz_pca_ind(wine.PCA, title = "", label = "none", col.ind = "cos2")
dev.off()

png(filename="./img/PCA-qualities.png", width = 1480, height = 550)
fviz_cos2(wine.PCA, title = "", choice = "var", axes = 1:3)+theme(axis.text.x = element_text(size = 16,angle = 90,vjust = 0.5))
dev.off()

rm(wine.PCA)


# ---------------------------------------------------------------------------- #
### Selezione Feature e Splitting ###

# selezione attributi a seguito delle PCA
attributes <- c(
  "target.quality",
  "target.type",
  
  "total.sulfur.dioxide",
  "fixed.acidity",
  "density",
  "alcohol"
)

wine.quality <- select(wine.quality, all_of(attributes))
wine.type <- select(wine.type, all_of(attributes))

# scaling dei valori
wine.quality <- mutate_if(wine.quality, is.numeric, scale)
wine.type <- mutate_if(wine.type, is.numeric, scale)

# funzione di split dei dataset
split.data = function(data, p = 0.7, s = 1) {
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)],]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]],]
  return(list(train = train, test = test))
}

wine.quality.all <- split.data(wine.quality, p = 0.7)
wine.quality.train <- wine.quality.all$train 
wine.quality.test <- wine.quality.all$test

wine.type.all <- split.data(wine.type, p = 0.7)
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


# ---------------------------------------------------------------------------- #
### Curva ROC-AUC ###

# Type - naive bayes
png(filename="./img/roc-nb-q.png", width = 600, height = 600)
pred.to.roc = nb.q.probs[,2]
pred.ROCR = prediction(pred.to.roc, wine.quality.test$target.quality)
perf.ROCR = performance(pred.ROCR, measure="auc", x.measure="cutoff")
perf.tpr.ROCR = performance(pred.ROCR, "tpr", "fpr")
plot(perf.tpr.ROCR, colorize=T, main=paste("AUC:",(perf.ROCR@y.values)))
abline(a=0, b=1)
dev.off()

# Type - neural network
png(filename="./img/roc-nn-q.png", width = 600, height = 600)
pred.to.roc = nn.q.probs[,2]
pred.ROCR = prediction(pred.to.roc, wine.quality.test$target.quality)
perf.ROCR = performance(pred.ROCR, measure="auc", x.measure="cutoff")
perf.tpr.ROCR = performance(pred.ROCR, "tpr", "fpr")
plot(perf.tpr.ROCR, colorize=T, main=paste("AUC:",(perf.ROCR@y.values)))
abline(a=0, b=1)
dev.off()

# Type - naive bayes
png(filename="./img/roc-nb-t.png", width = 600, height = 600)
pred.to.roc = nb.t.probs[,2]
pred.ROCR = prediction(pred.to.roc, wine.type.test$target.type)
perf.ROCR = performance(pred.ROCR, measure="auc", x.measure="cutoff")
perf.tpr.ROCR = performance(pred.ROCR, "tpr", "fpr")
plot(perf.tpr.ROCR, colorize=T, main=paste("AUC:",(perf.ROCR@y.values)))
abline(a=0, b=1)
dev.off()

# Type - neural network
png(filename="./img/roc-nn-t.png", width = 600, height = 600)
pred.to.roc = nn.t.probs[,2]
pred.ROCR = prediction(pred.to.roc, wine.type.test$target.type)
perf.ROCR = performance(pred.ROCR, measure="auc", x.measure="cutoff")
perf.tpr.ROCR = performance(pred.ROCR, "tpr", "fpr")
plot(perf.tpr.ROCR, colorize=T, main=paste("AUC:",(perf.ROCR@y.values)))
abline(a=0, b=1)
dev.off()

rm(pred.to.roc, pred.ROCR, perf.ROCR, perf.tpr.ROCR)


cv.values = resamples(list(nb=nb.q10f.model, nn = nn.q10f.model))
summary(cv.values)

png(filename="./img/dotplot-q10f.png", width = 800, height = 400)
dotplot(cv.values, metric = "ROC") 
dev.off()

png(filename="./img/dbwplot-q10f.png", width = 800, height = 400)
bwplot(cv.values, layout = c(3, 1)) 
dev.off()


cv.values = resamples(list(nb=nb.t10f.model, nn = nn.t10f.model))
summary(cv.values)

png(filename="./img/dotplot-t10f.png", width = 800, height = 400)
dotplot(cv.values, metric = "ROC") 
dev.off()

png(filename="./img/dbwplot-t10f.png", width = 800, height = 400)
bwplot(cv.values, layout = c(3, 1)) 
dev.off()

rm(cv.values)
