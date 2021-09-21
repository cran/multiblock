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
# Load potato data
data(potato)
class(potato)
# data.frames can contain matrices as variables, 
# thus becoming object linked lists of blocks.
str(potato[1:3])

# Explicit conversion to a list
potList <- as.list(potato[1:3])
str(potList)

## -----------------------------------------------------------------------------
# Object linked data
data(potato)
potList <- as.list(potato[c(1,2,9)])

suppressWarnings( # FactoMineR <=2.3 uses recycling of length 1 array.
invisible({capture.output({ # DISCOsca in package RegularizedSCA is highly verbose.
pot.sca    <- sca(potList)
pot.gca    <- gca(potList)
pot.gpa    <- gpa(potList)
pot.mfa    <- mfa(potList)
pot.pcagca <- pcagca(potList)
pot.disco  <- disco(potList)
pot.hpca   <- hpca(potList)
pot.mcoa   <- mcoa(potList)
})}))

## -----------------------------------------------------------------------------
# Shared variable mode data
data(candies)
candyList  <- lapply(1:nlevels(candies$candy), function(x)candies$assessment[candies$candy==x,])

invisible({capture.output({ # jive in package r.jive is highly verbose.
can.sca    <- sca(candyList, samplelinked = FALSE)
can.jive   <- jive(candyList)
can.statis <- statis(candyList)
can.hogsvd <- hogsvd(candyList)
})})

## -----------------------------------------------------------------------------
# SCA used with shared variable mode data returns block loadings and common scores:
names(pot.sca)
summary(pot.sca)
# MFA stores individual PCA scores and loadings as block elements:
names(pot.mfa)
summary(pot.mfa)

## -----------------------------------------------------------------------------
# Global scores plotted with object labels
plot(scores(pot.sca), labels = "names")

## -----------------------------------------------------------------------------
# Block loadings for Sensory block with variable labels in scatter format
plot(loadings(pot.sca, "Sensory"), labels = "names", scatter = TRUE)

## -----------------------------------------------------------------------------
# Non-existing elements are swapped with existing ones with a warning.
sc <- scores(pot.sca, block = 1)

## -----------------------------------------------------------------------------
# Apply a plot function from ade4 (no extra import required).
plot(can.statis$statis)

