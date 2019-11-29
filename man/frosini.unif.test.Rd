\name{frosini.unif.test}
\alias{frosini.unif.test}
\title{Frosini test for uniformity}
\description{
  Performs Frosini test for the hypothesis of uniformity, 
  see Frosini (1987).
}
\usage{
frosini.unif.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Frosini test for uniformity is based on the following statistic:
\deqn{
B_n = \frac{1}{\sqrt{n}}\sum_{i=1}^n{\left| X_{(i)} - \frac{i-0.5}{n} \right|}.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Frosini statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Frosini test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Frosini, B.V. (1987): On the distribution and power of a goodness-of-fit statistic with parametric and nonparametric applications, "Goodness-of-fit". (Ed. by Revesz P., Sarkadi K., Sen P.K.) --- Amsterdam-Oxford-New York: North-Holland. --- Pp. 133--154.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
frosini.unif.test(runif(100,0,1))
frosini.unif.test(runif(100,0.1,0.9))
}
\keyword{htest}