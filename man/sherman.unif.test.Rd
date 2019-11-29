\name{sherman.unif.test}
\alias{sherman.unif.test}
\title{Sherman test for uniformity}
\description{
  Performs Sherman test for the hypothesis of uniformity, 
  see Sherman (1950).
}
\usage{
sherman.unif.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Sherman test for uniformity is based on the following statistic:
\deqn{
B_n = \frac{1}{2}\sum_{i=1}^{n+1}{\left| X_{(i)} - X_{(i-1)} - \frac{1}{n+1} \right|},
}
where \eqn{X_{(0)}=0}, \eqn{X_{(n+1)}=1}.
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Sherman statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Sherman test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Sherman, B. (1950): A random variable related to the spacing of sample values. --- Ann. Math. Stat., vol. 21, pp. 339--361.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
sherman.unif.test(runif(100,0,1))
sherman.unif.test(runif(100,0.1,0.9))
}
\keyword{htest}