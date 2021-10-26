data <- read.table("seeddata.txt", header=FALSE)
colnames(data) <- c("area","perimeter","compactness","lengthker","widthkern","asymmcoeff","lengthkergrov","variety")
seed.d <- dist(data[,-8])
# using the single method
seed.hc.c <-hclust(seed.d, method = "single")
# create estimated labels for each obs
table(predicted = cutree(seed.hc.c, k = 3), actual = data[,8])
table(cutree(seed.hc.c, k = 3))
plot(seed.hc.c)
rect.hclust(seed.hc.c, k =3, border="red")
pairs(data[,-8], pch = unclass(data[,8]),
col=cutree(seed.hc.c, k = 3))

# using the complete method
seed.hc.c1 <-hclust(seed.d, method = "complete")
# create estimated labels for each obs
table(predicted = cutree(seed.hc.c1, k = 3), actual = data[,8])
table(cutree(seed.hc.c1, k = 3))
plot(seed.hc.c1)
rect.hclust(seed.hc.c1, k =3, border="red")
pairs(data[,-8], pch = unclass(data[,8]),
col=cutree(seed.hc.c1, k = 3))

# using the Average method
seed.hc.c2 <-hclust(seed.d, method = "average")
# create estimated labels for each obs
table(predicted = cutree(seed.hc.c2, k = 3), actual = data[,8])
table(cutree(seed.hc.c2, k = 3))
plot(seed.hc.c2)
rect.hclust(seed.hc.c2, k =3, border="red")
pairs(data[,-8], pch = unclass(data[,8]),
col=cutree(seed.hc.c2, k = 3))

