# Read CSV file
players <- read.csv("player_stats-24-06-2019.txt", sep = " ")

# Normalization
z <- players[, -c(1,1)]
m <- apply(z,2, mean)
s <- apply(z,2, sd)
z <- scale(z,m,s)

#Calculating Euclidian Distance
distance <- dist(z)
print(distance,digits = 3)

# Cluster dendogram with complete linkage
hc.c <- hclust(distance)
plot(hc.c)

# Cluster with average linkage
hc.a <- hclust(distance,method = "average")

# Compare average and complete linkage methods
member.a <- cutree(hc.a,5)
member.c <- cutree(hc.c,5)
table(member.a,member.c)

# Kmeans method
kc <- kmeans(na.omit(z), 7)
kc
z$cluster[which(!is.na(z$x))] <- kc$cluster
