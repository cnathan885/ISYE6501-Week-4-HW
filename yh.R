#-----Question 9.1----#

#load data
data <- read.table('uscrime.txt', header=TRUE)

#perform pca
pca <- prcomp(data[,1:15], center = TRUE, scale.=TRUE)

summary(pca)

#visualize the principal components
plot(pca, type='line')

