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
# Start the multiblock R package
library(multiblock)

## -----------------------------------------------------------------------------
data(potato)
X <- potato$Chemical
y <- potato$Sensory[,1,drop=FALSE]

## -----------------------------------------------------------------------------
# Single block
pot.pca  <- pca(X, ncomp = 2)

# Two blocks, supervised
pot.pcr  <- pcr(y ~ X, ncomp = 2)
pot.pls  <- plsr(y ~ X, ncomp = 2)

# Two blocks, unsupervised
pot.cca  <- cca(potato[1:2])
pot.ifa  <- ifa(potato[1:2])

# Variable linked decomposition
pot.gsvd <- gsvd(lapply(potato[3:4], t))

## -----------------------------------------------------------------------------
# PCA returns loadings and scores:
names(pot.pca)
summary(pot.pca)
# GSVD returns block scores and common loadings:
names(pot.gsvd)
summary(pot.gsvd)

## -----------------------------------------------------------------------------
# Global scores plotted with object labels
scoreplot(pot.pca, labels = "names")

## -----------------------------------------------------------------------------
# Block loadings for Chemical block with variable labels in scatter format
loadingplot(pot.cca, block = "Chemical", labels = "names")

## -----------------------------------------------------------------------------
# Non-existing elements are swapped with existing ones with a warning.
sc <- scores(pot.cca)

