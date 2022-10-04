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



set.seed(42)
defense <- c(rnorm(50,mean=-2),rnorm(50,mean=0),rnorm(50,mean=2))
defense <- defense - min(defense)
baseline_herbivory <- rnorm(150,sd=1)
baseline_herbivory <- baseline_herbivory - mean(baseline_herbivory)
herbivory <- baseline_herbivory - defense + rnorm(150)
herbivory <- herbivory - min(herbivory)
baseline_growth <- c(rnorm(75,mean = -2),rnorm(75,mean=2))
baseline_growth <- baseline_growth - min(baseline_growth)
TYPE <- character(length(defense))
growth <- baseline_growth - herbivory - defense
growth <- growth - min(growth)
fitness <- growth - herbivory - defense
TYPE[growth < 7 & defense > 3 & fitness < -4] <- "Perennial"
TYPE[!(growth < 7 & defense > 3 & fitness < -4)] <- "Annual"
sim_plant <- data.frame(defense,herbivory,growth,fitness,TYPE)
