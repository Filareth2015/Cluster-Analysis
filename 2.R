library(rattle) # Load weather dataset. Normalise names normVarNames().
library(randomForest) # Impute missing using na.roughfix().
library(ggplot2) # Visualise the data through plots.
library(reshape2) # Reshape data for plotting.
library(fpc)
library(beepr)


ds <- read.csv('weatherAUS.csv', nrows = 1000)

names(ds) <- normVarNames(names(ds))
vars <- names(ds)
target <- "rain_tomorrow"
risk <- "risk_mm"
id <- c("date", "location")

ignore <- union(id, if (exists("risk")) risk)

mvc <- sapply(ds[vars], function(x) sum(is.na(x))) # Missing value count.
mvn <- names(ds)[(which(mvc == nrow(ds)))] # Missing var names.
ignore <- union(ignore, mvn)

vars <- setdiff(vars, ignore)

inputc <- setdiff(vars, target)
inputi <- sapply(inputc, function(x) which(x == names(ds)), USE.NAMES=FALSE)

numi <- intersect(inputi, which(sapply(ds, is.numeric)))
numc <- names(ds)[numi]

cati <- intersect(inputi, which(sapply(ds, is.factor)))
catc <- names(ds)[cati]

if (sum(is.na(ds[vars]))) ds[vars] <- na.roughfix(ds[vars])



nk <- 1:20
model <- kmeansruns(scale(ds[numi]), krange=nk, criterion="asw")
class(model)
model$crit
model$bestk
beep()

model$centers

nclust <- 2
model <- m.kms <- kmeans(scale(ds[numi]), nclust)
dscm <- melt(model$centers)
names(dscm) <- c("Cluster", "Variable", "Value")
dscm$Cluster <- factor(dscm$Cluster)
dscm$Order <- as.vector(sapply(1:length(numi), rep, nclust))
p <- ggplot(dscm,
            aes(x=reorder(Variable, Order),
                y=Value, group=Cluster, colour=Cluster))
p <- p + coord_polar()
p <- p + geom_point()
p <- p + geom_path()
p <- p + labs(x=NULL, y=NULL)
p <- p + ylim(-1, 1)
p
beep()
