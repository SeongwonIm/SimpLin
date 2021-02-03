\name{simp_lin_R}
\alias{simp_lin_R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Simple linear regression
}
\description{
The function takes two numeric vectors and performs simple linear regression. The returned object includes the estimated regression coefficients, standard errors, 95\% confidence interval, predicted values, and residuals.
}
\usage{
simp_lin_R(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric vector of predictor(independent) variable
}
  \item{y}{
A numeric vector of response(dependent) variable
}
}

\value{
  \item{Coefficients}{Estimated intercept and slope}
  \item{StandardError}{Standard errors of intercept and slope}
  \item{b0_95CI}{The 95\% confidence interval of the intercept}
  \item{b1_95CI}{The 95\% confidence interval of the slope}
  \item{Residuals}{The difference between the predicted values and the observed values}
  \item{Predicted}{The predicted(fitted) values}

}
\references{
Kutner, M. H., Nachtsheim, C. J., Neter, J., &amp; Li, W. (2013). Applied linear statistical models (5th ed.). New York, NY: McGraw-Hill Education Private Limited.
}
\author{
Seongwon Im
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x = rnorm(100)

y = rnorm(100)

simp_lin_R(x, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
