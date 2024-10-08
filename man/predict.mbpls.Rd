% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mbpls_results.R
\name{predict.mbpls}
\alias{predict.mbpls}
\title{Predict Method for MBPLS}
\usage{
\method{predict}{mbpls}(
  object,
  newdata,
  ncomp = 1:object$ncomp,
  comps,
  type = c("response", "scores"),
  na.action = na.pass,
  ...
)
}
\arguments{
\item{object}{an \code{mvr} object.  The fitted model}

\item{newdata}{a data frame.  The new data.  If missing, the training data
is used.}

\item{ncomp, comps}{vector of positive integers.  The components to use in
the prediction.  See below.}

\item{type}{character.  Whether to predict scores or response values}

\item{na.action}{function determining what should be done with missing
values in \code{newdata}.  The default is to predict \code{NA}.  See
\code{\link{na.omit}} for alternatives.}

\item{\dots}{further arguments.  Currently not used}
}
\value{
When \code{type} is \code{"response"}, a three dimensional array of
predicted response values is returned.  The dimensions correspond to the
observations, the response variables and the model sizes, respectively.

When \code{type} is \code{"scores"}, a score matrix is returned.
}
\description{
Prediction for the mbpls (MBPLS) model. New responses or scores are
predicted using a fitted model and a data.frame or list containing matrices of observations.
}
\details{
When \code{type} is \code{"response"} (default), predicted response values
are returned.  If \code{comps} is missing (or is \code{NULL}), predictions
for \code{length(ncomp)} models with \code{ncomp[1]} components,
\code{ncomp[2]} components, etc., are returned.  Otherwise, predictions for
a single model with the exact components in \code{comps} are returned.
(Note that in both cases, the intercept is always included in the
predictions.  It can be removed by subtracting the \code{Ymeans} component
of the fitted model.)

When \code{type} is \code{"scores"}, predicted score values are returned for
the components given in \code{comps}.  If \code{comps} is missing or
\code{NULL}, \code{ncomps} is used instead.
}
\note{
A warning message like \samp{'newdata' had 10 rows but variable(s)
found have 106 rows} means that not all variables were found in the
\code{newdata} data frame.  This (usually) happens if the formula contains
terms like \code{yarn$NIR}.  Do not use such terms; use the \code{data}
argument instead.  See \code{\link[pls]{mvr}} for details.
}
\examples{
data(potato)
mb <- mbpls(Sensory ~ Chemical+Compression, data=potato, ncomp = 5, subset = 1:26 <= 18)
testdata <- subset(potato, 1:26 > 18)

# Predict response
yhat <- predict(mb, newdata = testdata)

# Predict scores and plot
scores <- predict(mb, newdata = testdata, type = "scores")
scoreplot(mb)
points(scores[,1], scores[,2], col="red")
legend("topright", legend = c("training", "test"), col=1:2, pch = 1)
}
\seealso{
\code{\link{mbpls}}
}
\author{
Kristian Hovde Liland
}
