## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4
)
# Legge denne i YAML på toppen for å skrive ut til tex
#output: 
#  pdf_document: 
#    keep_tex: true
# Original:
#  rmarkdown::html_vignette:
#    toc: true

## ----setup--------------------------------------------------------------------
library(multiblock)

## -----------------------------------------------------------------------------
# Random data
n <- 30; p <- 90
X <- matrix(rnorm(n*p), nrow=n)
y <- X %*% rnorm(p) + 10

# Split X into three blocks in a named list
ABC <- list(A = X[,1:20], B = X[,21:50], C = X[,51:90], y = y)

# Model using names of blocks (see below for full SO-PLS example)
so.abc <- sopls(y ~ A + B + C, data = ABC, ncomp = c(4,3,4))

## -----------------------------------------------------------------------------
data(potato)
mb <- mbpls(potato[c('Chemical','Compression')], potato[['Sensory']], ncomp = 10,
            max_comps=10, validation="CV", segments=10)
print(mb)

## -----------------------------------------------------------------------------
Tb1 <- scores(mb, block=1)
plot(Tb1, labels = "names")

Pb2 <- loadings(mb, block=2)
plot(Pb2, main = 'Loadings for block 2')

## -----------------------------------------------------------------------------
# Load potato data and fit SO-PLS model
so.pot <- sopls(Sensory ~ Chemical + Compression, data=potato, 
            ncomp=c(10,10), max_comps=10, validation="CV", segments=10)
print(so.pot)
summary(so.pot)

## -----------------------------------------------------------------------------
# Load Wine data and model with SO-PLS
data(wine)
ncomp <- unlist(lapply(wine, ncol))[-5]
so.wine <- sopls(`Global quality` ~ ., data=wine, ncomp=ncomp, 
             max_comps=6, validation="CV", segments=10)
maage(so.wine)

## -----------------------------------------------------------------------------
# Sequential search for optimal number of components per block
old.par <- par(mfrow=c(2,2), mar=c(3,3,0.5,1), mgp=c(2,0.7,0))
maageSeq(so.wine)
maageSeq(so.wine, 2)
maageSeq(so.wine, c(2,1))
maageSeq(so.wine, c(2,1,1))
par(old.par)

## -----------------------------------------------------------------------------
# Display loadings up to four components for first block
loadings(so.pot, ncomp = c(4,0), block = 1)

## -----------------------------------------------------------------------------
# Plot loadings from block 1 and 2
old.par <- par(mfrow=c(1,2))
loadingplot(so.pot, ncomp = c(4,3), comps = c(2,3), block = 1, scatter = TRUE, main = "Block 1", labels = "names", cex = 0.8)
loadingplot(so.pot, ncomp = c(4,3), block = 2, scatter = TRUE, main = "Block 2", labels = "names", cex = 0.8)
par(old.par)

## -----------------------------------------------------------------------------
# Display scores up to four components for first block
scores(so.pot, ncomp = c(4,0), block = 1)

## -----------------------------------------------------------------------------
# Plot scores from block 1 and 2
old.par <- par(mfrow=c(1,2))
scoreplot(so.pot, ncomp = c(4,3), comps = c(2,3), block = 1, main = "Block 1", labels = "names")
scoreplot(so.pot, ncomp = c(4,3), block = 2, main = "Block 2", labels = "names")
par(old.par)

## -----------------------------------------------------------------------------
# Modify data to contain a single response
potato1 <- potato; potato1$Sensory <- potato1$Sensory[,1]
# Model 20 first objects with SO-PLS
so.pot20 <- sopls(Sensory ~ ., data = potato1[c(1:3,9)], ncomp = 5, subset = 1:20)
# Predict remaining objects
testset <- potato1[-(1:20),]; # testset$Sensory <- NULL
predict(so.pot20, testset, comps=c(2,1,2))


## -----------------------------------------------------------------------------
# Cross-validation
R2(so.pot, ncomp = c(5,5))
R2(so.pot, ncomp = c(5,5), individual = TRUE)
# Training
R2(so.pot, 'train', ncomp = c(5,5))

# Test data
R2(so.pot20, newdata = testset, ncomp = c(2,1,2))

## -----------------------------------------------------------------------------
# Cross-validation
RMSEP(so.pot, ncomp = c(5,5))
RMSEP(so.pot, ncomp = c(5,5), individual = TRUE)
# Training
RMSEP(so.pot, 'train', ncomp = c(5,5))

# Test data
RMSEP(so.pot20, newdata = testset, ncomp = c(2,1,2))

## -----------------------------------------------------------------------------
# Automatic analysis
pot.po.auto <- popls(potato[1:3], potato[['Sensory']][,1], commons = 2)

# Explained variance
pot.po.auto$explVar

## -----------------------------------------------------------------------------
# Manual choice of up to 5 components for each block and 1, 0, and 2 blocks,
# respectively from the (1,2), (1,3) and (2,3) combinations of blocks.
pot.po.man <- popls(potato[1:3], potato[['Sensory']][,1], commons = 2,
                    auto=FALSE, manual.par = list(ncomp=c(5,5,5),
                                                  ncommon=c(1,0,2)))
# Explained variance
pot.po.man$explVar

## -----------------------------------------------------------------------------
# Score plot for local (2,3) components
plot(scores(pot.po.man,3), comps=1:2, labels="names")

# Corresponding loadings
plot(loadings(pot.po.man,3), comps=1:2, labels="names")

## -----------------------------------------------------------------------------
# Model all eight potato blocks with ROSA
ros.pot <- rosa(Sensory ~ ., data = potato1, ncomp = 10, validation = "CV", segments = 5)
print(ros.pot)
summary(ros.pot)

## -----------------------------------------------------------------------------
loads <- loadings(ros.pot)
loadingplot(ros.pot, comps = 1:2)

## -----------------------------------------------------------------------------
sco <- scores(ros.pot)
scoreplot(ros.pot, comps = 1:2, labels = "names")

## -----------------------------------------------------------------------------
# Model 20 first objects of three potato blocks
rosT <- rosa(Sensory ~ ., data = potato1[c(1:3,9)], ncomp = 5, subset = 1:20)
testset <- potato1[-(1:20),]; # testset$Sensory <- NULL
predict(rosT, testset, comps=2)

## -----------------------------------------------------------------------------
# Cross-validation
R2(ros.pot)
# Training
R2(ros.pot, 'train')

# Test data
R2(rosT, 'test', newdata = testset)

## -----------------------------------------------------------------------------
# Cross-validation
RMSEP(ros.pot)
# Training
RMSEP(ros.pot, 'train')

# Test data
RMSEP(rosT, newdata = testset)

## -----------------------------------------------------------------------------
# Correlation to winning scores
image(ros.pot)
# Residual response given candidate scores
image(ros.pot, "residual")

## -----------------------------------------------------------------------------
# Convert data.frame with AsIs objects to list of matrices
potatoList <- lapply(potato, unclass)

# Perform mbRDA with two blocks explaining sensory attributes
mbr <- mbrda(potatoList[c('Chemical','Compression')], potatoList[['Sensory']], ncomp = 5)
print(mbr)

## -----------------------------------------------------------------------------
# Extract and view loadings
lo_mbr <- loadings(mbr)
print(head(lo_mbr))
# Plot scores
scoreplot(mbr, labels = "names")

