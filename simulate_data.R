# correlation matrix for simulating traits
cormat <- matrix(c(1, -0.89, -0.302, -0.89, 1, 0.599, -0.302, 0.599, 1),3,3)

# simulate correlated traits
X <- MASS::mvrnorm(n=10000,mu=c(Nitrogen=0,LMA=0,Water=0),Sigma = cormat)

# sort by value of first column (Nitrogen)
X <- X[order(X[,1]),]

# create variable for plant Type
Type <- character(nrow(X))

# Restrict Types to specific regions of trait-space
Type[which(X[,"Nitrogen"]>.2 & X[,"Water"]>.3)] <- "Dessert Annual"
Type[which(X[,"Nitrogen"]<(-1) & X[,"Water"]>.3)] <- "Dessert Perennial"
Type[which(X[,"Nitrogen"]<(-.04) & X[,"Water"]<(-.5))] <- "Temperate Perennial"
Type[which(X[,"Nitrogen"]>.3 & X[,"Water"]<(-.05))] <- "Temperate Annual"
X <- X[-which(Type==""),]
Type <- Type[-which(Type=="")]

# Take a random subset of each Type
subset <- sort(as.numeric(sapply(unique(Type),function(X) sample(which(Type == X),50))))
X <- (X[subset,])
Type <- Type[subset]

# Create a data frame dat with data (X) and Type
dat <- data.frame(X,Type=as.factor(Type))

# n = number of data points
n <- nrow(X)

# cols = rainbow gradient of colors for visualization
cols <- rainbow(round(n*1.5))[1:n]
