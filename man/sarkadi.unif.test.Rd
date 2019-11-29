\name{sarkadi.unif.test}
\alias{sarkadi.unif.test}
\title{Sarkadi-Kosik test for uniformity}
\description{
  Performs Sarkadi-Kosik test for the hypothesis of uniformity.
}
\usage{
sarkadi.unif.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Sarkadi-Kosik test for uniformity is based on the following statistic:
\deqn{
J = n^2\sum_{i=1}^{n}{\left( \frac{x_i-\frac{i}{n+1}}{i\left(n-i+1\right)} \right)^2}-n\left(\sum_{i=1}^{n}{\frac{x_i-\frac{i}{n+1}}{i\left(n-i+1\right)}} \right)^2.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Sarkadi-Kosik statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Sarkadi-Kosik test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Kosik P., Sarkadi K. A new goodness-of-fit test // Proc. of 5-th Pannonian Symp. of Math. Stat., Visegrad, Hungary, 20 24 May, 1985. P. 267 272.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
sarkadi.unif.test(runif(100,0,1))
sarkadi.unif.test(runif(100,0.1,0.9))
}
\keyword{htest}