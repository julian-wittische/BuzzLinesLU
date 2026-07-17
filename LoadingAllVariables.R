lf <- list.files("C:/Users/YNM724/Desktop/BuzzLine variables", full.names = T)

stack <- sapply(lf, FUN=rast)

for (i in 1:(length(stack)-1)){
  for (j in (i+1):length(stack)){
    print(cor(values(stack[[i]]), values(stack[[j]]), use = "na.or.complete"))
  }
}
env.stack <- rast(stack)
names(env.stack)

env.vals <- data.frame(matrix())
for (i in 1:length(stack)){
  env.vals[,i] <- values(stack[[i]])
}




# TO DO LIST
# Check why moisture did not work (previous stack?, na?)
# Rename variables so that "mean" and such dont appear anymore
# Rewrite function to transform into resistance
pca <- prcomp(cbind(unlist(values(stack))), center=T)
