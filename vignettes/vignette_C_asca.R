## ---- include=FALSE-----------------------------------------------------------
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

## ----cars---------------------------------------------------------------------
set.seed(1)
dataset   <- data.frame(y = I(matrix(rnorm(24*10), ncol = 10)), 
                        x = factor(c(rep(2,8), rep(1,8), rep(0,8))), 
                        z = factor(rep(c(1,0), 12)), w = rnorm(24))
colnames(dataset$y) <- paste('Var', 1:10, sep = " ")
rownames(dataset)   <- paste('Obj', 1:24, sep = " ")
str(dataset)

## -----------------------------------------------------------------------------
mod <- asca(y~x+z, data = dataset)
print(mod)

## -----------------------------------------------------------------------------
sc <- scores(mod)
head(sc)

scoreplot(mod, legendpos = "topleft", ellipsoids = "confidence")

## -----------------------------------------------------------------------------
sc <- scores(mod, factor = "z")
head(sc)

scoreplot(mod, factor = "z", ellipsoids = "confidence")

## -----------------------------------------------------------------------------
lo <- loadings(mod)
head(lo)

loadingplot(mod, scatter = TRUE, labels = 'names')

