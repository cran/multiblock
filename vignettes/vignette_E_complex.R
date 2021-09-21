## ----setup, include=FALSE-----------------------------------------------------
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

## -----------------------------------------------------------------------------
# Start the multiblock R package
library(multiblock)

## -----------------------------------------------------------------------------
set.seed(42)

# Simulate data set
sim <- lplsData(I = 30, N = 20, J = 5, K = 6, ncomp = 2)

# Split into separate blocks
X1  <- sim$X1; X2 <- sim$X2; X3 <- sim$X3

## ----fig.width=5, fig.height=5------------------------------------------------
# exo-L-PLS:
lp.exo  <- lpls(X1,X2,X3, ncomp = 2) # type = "exo" is default

# Predict X1
pred.exo.X2 <- predict(lp.exo, X1new = X1, exo.direction = "X2")

# Predict X3
pred.exo.X2 <- predict(lp.exo, X1new = X1, exo.direction = "X3")

# Correlation loading plot
plot(lp.exo)

## -----------------------------------------------------------------------------
# endo-L-PLS:
lp.endo <- lpls(X1,X2,X3, ncomp = 2, type = "endo")

# Predict X1 from X2 and X3 (in this case fitted values):
pred.endo.X1 <- predict(lp.endo, X2new = X2, X3new = X3)

## -----------------------------------------------------------------------------
# LOO cross-validation horizontally
lp.cv1 <- lplsCV(lp.exo, segments1 = as.list(1:dim(X1)[1]), trace = FALSE)

# LOO cross-validation vertically
lp.cv2 <- lplsCV(lp.exo, segments2 = as.list(1:dim(X1)[2]), trace = FALSE)

# Three-fold CV, horizontal
lp.cv3 <- lplsCV(lp.exo, segments1 = as.list(1:10, 11:20, 21:30), trace = FALSE)

# Three-fold CV, horizontal, inwards model
lp.cv4 <- lplsCV(lp.endo, segments1 = as.list(1:10, 11:20, 21:30), trace = FALSE)

## -----------------------------------------------------------------------------
# Load potato data
data(potato)

# Single path
pot.pm <- sopls_pm(potato[1:3], potato[['Sensory']], c(5,5,5), computeAdditional=TRUE)

# Report of explained variances and optimal number of components .
# Bootstrapping can be enabled to assess stability.
# (LOO cross-validation is default)
pot.pm

## -----------------------------------------------------------------------------
# Load wine data
data(wine)

# All path in the forward direction
pot.pm.multiple <- sopls_pm_multiple(wine, comps = c(4,2,9,8))

# Report of direct, indirect and total explained variance per sub-path.
# Bootstrapping can be enabled to assess stability.
pot.pm.multiple

