library(animation)

cent <- 1.5 * c(1, 1, -1, -1, 1, -1, 1, -1)
x <- NULL
for (i in 1:8) x <- c(x, rnorm(25, mean=cent[i]))
x <- matrix(x, ncol=2)
colnames(x) <- c("X1", "X2")
dim(x)

par(mar=c(3, 3, 1, 1.5), mgp=c(1.5, 0.5, 0), bg="white")
kmeans.ani(x, centers=4, pch=1:4, col=1:4)

library(rattle) # Load weather dataset. Normalise names normVarNames().
library(randomForest) # Impute missing using na.roughfix().
library(ggplot2) # Visualise the data through plots.
library(reshape2) # Reshape data for plotting.
library(animation)

ds <- read.csv('weatherAUS.csv')

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


crit <- vector()
nk <- 1:20
for (k in nk)
  {
    m <- kmeans(scale(ds[numi]), k)
    crit <- c(crit, sum(m$withinss))
  }
crit

dsc <- data.frame(k=nk, crit=scale(crit))
dscm <- melt(dsc, id.vars="k", variable.name="Measure")
p <- ggplot(dscm, aes(x=k, y=value, colour=Measure))
p <- p + geom_point(aes(shape=Measure))
p <- p + geom_line(aes(linetype=Measure))
p <- p + scale_x_continuous(breaks=nk, labels=nk)
p <- p + theme(legend.position="none")
p


library(amap)
model <- hclusterpar(na.omit(ds[numi]),
                     method="euclidean",
                     link="ward",
                     nbproc=1)



set.seed(123456)
model <- kmeans(scale(ds[numi]), 15)

par(mar=c(3, 3, 1, 1.5), mgp=c(1.5, 0.5, 0), bg="white")
kmeans.ani(scale(ds[numi]), centers=15, pch=1:4, col=1:15)

library(dendroextras)
plot(colour_clusters(model, k=10), xlab="")

m = matrix(rnorm(10), 100, 5)
km = kmeans(m, 10)


model2 <- cbind(ds,model$cluster)
orderedModel2 <- order(model2[, 'model$cluster'])
model2 <- model2[orderedModel2, ]
library(pheatmap) # I like esoteric packages!
library(RColorBrewer)
pheatmap(model2[,1:24], cluster_rows=F,cluster_cols=F, col=brewer.pal(10,"Set3"),border_color=NA)









dscm <- melt(model$centers)
names(dscm) <- c("Cluster", "Variable", "Value")
dscm$Cluster <- factor(dscm$Cluster)
dscm$Order <- as.vector(sapply(1:length(numi), rep, 5))
p <- ggplot(subset(dscm, Cluster %in% 1:5),
            aes(x=reorder(Variable, Order),
                y=Value, group=Cluster, colour=Cluster))
p <- p + coord_polar()
p <- p + geom_point()
p <- p + geom_path()
p <- p + labs(x=NULL, y=NULL)
p <- p + theme(axis.ticks.y=element_blank(), axis.text.y = element_blank())
p

image(t(as.matrix(ds))[, order(model$cluster)], yaxt = "n", main = "Clustered Data")