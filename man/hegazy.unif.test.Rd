\name{hegazy.unif.test}
\alias{hegazy.unif.test}
\title{Hegazy-Green test for uniformity}
\description{
Performs Hegazy-Green test for the hypothesis of uniformity.
}
\usage{
hegazy.unif.test(x, nrepl=2000, p=1)
}
\arguments{
\item{x}{a numeric vector of data values.}
\item{p}{a parameter of the test (see below).}
\item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Hegazy-Green test for uniformity is based on the following statistic:
\deqn{
T_p = \frac{1}{n}\sum_{i=1}^{n}{\left|X_{(i)}-\frac{i}{n+1}\right|^p}.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Hegazy-Green statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Hegazy-Green test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Hegazy, Y. A. S. and Green, J. R. (1975): Some new goodness-of-fit tests using order statistics. --- Journal of the Royal Statistical Society. Series C (Applied Statistics), vol. 24, pp. 299--308.
}

\author{Maxim Melnik and Ruslan Pusev}

\examples{
hegazy.unif.test(runif(100,0,1))
hegazy.unif.test(runif(100,0.1,0.9))
}
\keyword{htest}