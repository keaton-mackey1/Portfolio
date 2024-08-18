#Load in Libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tibble)
library(stringr)
library(devtools)
library(ggbiplot)
library(ggcorrplot) # more data visualization tools
library(stats) # contain PCA tools
library(factoextra)
library(caTools)
library(MASS)
library(psych)
library(ggord)
library(scales)
library(gridExtra)
library(janitor)
library(readxl)

#Set rootpaths
rootpath = '/Users/keatonmackey/Desktop/'
setwd(rootpath)

#Load in dataset
innerJoin <- read.csv('SurveyMeansInnerJoin.csv', sep = ',', header = T)
outerJoin <- read.csv("SurveyMeansOuterJoin.csv", sep = ',', header = T)
outDemo <- read.csv("Outdemo.csv", sep = ',', header = T)
testData <- read_xlsx('testdata.xlsx')
WIP <- read_xlsx('WIP.xlsx')
testData <- testData[, -c(8)]
#Fix test data file
testData$csacsa_avg <- WIP$csacsa_avg...18

#Working with phq as the group
testData$phq_avg <- WIP$phq_avg...50





########LDA###########
#Predicting with Train and Test

##Scale data
testData[c(1, 3:11)] <- scale(testData[c(1, 3:11)])
apply(testData[c(1, 3:11)], 2, mean)
apply(testData[c(1, 3:11)], 2, sd)
#Split + train
smp_size <- floor(0.7 * nrow(testData))
train_ind <- sample(nrow(testData), size = smp_size)
train.df <- as.data.frame(testData[train_ind, ])
test.df <- as.data.frame(testData[-train_ind, ])

#Create a model with a MASS Library
practice.lda <- lda(phq_avg ~., data = train.df)
practice.lda.predict <- predict(practice.lda, newdata = test.df)

#Visualize LDA
head(practice.lda.predict$x)

mean(practice.lda.predict$class==test.df$phq_avg)

#Validating model with confusion matrix
perf_indexes = function(cm){
  sensitivity = cm[2,2] / (cm[1,2] + cm[2,2])
  specificity = cm[1,1] / (cm[1,1] + cm[2,1])
  accuracy = sum(diag(cm)) / sum(cm)
  return(c(sens=sensitivity,spec=specificity,acc=accuracy))
}
perf_indexes(table(practice.lda.predict$class, test.df$phq_avg))

#Visualizing LDA
ldahist(data = practice.lda.predict$x[,1], g = test.df$phq_avg)


lda.arrows <- function(x, myscale = 1, tex = 0.75, choices = c(1,2), ...){
  ## adds `biplot` arrows to an lda using the discriminant function values
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], ...)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex)
}

display <- lda(as.matrix(testData[, c(1, 3:11)]), testData$phq_avg)
plot(display, asp = 1)
lda.arrows(display, col = 2, myscale = 2)

library(devtools)
library(ggord)
names(testData) <-c("loneliness", "phq_avg", "stai_state", "stai_trait", "pss", "ERQCog","ERQExp", "CopeAvoidant", "CopeApproach", "csacsa","LOTR")   
ord <- lda(phq_avg ~., testData)
group <- factor(testData$phq_avg)
ggord(ord, group, arrow = 0.4,txt = 3)


#PCA

outerJoinPCA <- row_to_names(outerJoin, 1)
pcaDF <- outerJoinPCA[, -c(1)]

#Converting to numeric

pcaDF[1:40] <- lapply(pcaDF[1:40], as.numeric)

pcaDF %>%
  cor() %>%
  ggcorrplot(hc.order = TRUE, outline.col = "white")

practice_pca <- prcomp(pcaDF, center = TRUE, scale = TRUE)

summary(practice_pca)

#Scree Plot and different ways to visualize PCA
fviz_eig(practice_pca, choice = "variance")

fviz_pca_var(practice_pca,
             axes = c(1, 2), # visualize the first two dimensions
             col.var = "contrib", # color by contributions to principal component
             gradient.cols = c("#2b83ba", "#ffffbf", "#d7191c"),
             repel = "True")

fviz_pca_ind(practice_pca,
             axes = c(1, 2), # visualize the first two dimensions
             col.ind = "cos2",
             gradient.cols = c("#2b83ba", "#ffffbf", "#d7191c"),
             label = "None")

#Extracting Eigenvalue

practice_pca_eigval <- get_eigenvalue(practice_pca)
practice_pca_eigval

#Extract PCA Results at the Variable Level
practice_pca_var <- get_pca_var(practice_pca)
practice_pca_var$coord[, 1:5]
practice_pca_var$contrib[, 1:5]

library(corrplot)
practice_pca_var$cos2[, 1:3] %>%
  corrplot(is.corr = FALSE , method = "circle",
           cl.pos = "r", cl.align.text = "l",
           tl.pos = "t", tl.col = "#000000",
           col = colorRampPalette(c("#fee0d2", "#67000d") )(11) )

library(plotly)
library(ggfortify)
pcaDF$Group <- testData$phq_avg
p <- autoplot(practice_pca, data = pcaDF, colour = 'Group', loadings = TRUE)
ggplotly(p)
