###### Correlations
cor(vars)

###### PCA plotting (scaling 2?)
pca <- prcomp(vars, center=TRUE)
biplot(pca)