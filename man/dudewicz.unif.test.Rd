\name{dudewicz.unif.test}
\alias{dudewicz.unif.test}
\title{Dudewicz-van der Meulen test for uniformity}
\description{
  Performs Dudewicz-van der Meulen test for the hypothesis of uniformity.
}
\usage{
dudewicz.unif.test(x, nrepl=2000,m=length(x)/2)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
  \item{m}{a parameter of the test (see below).}
}
\details{The Dudewicz-van der Meulen test for uniformity is based on the following statistic:
\deqn{
H(m, n) = -\frac{1}{n}\sum_{i=1}^{n}{\log_2{\frac{n}{2m}(x_{(i+m)}-x_{(i-m)})}}
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Dudewicz-van der Meulen statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Dudewicz-van der Meulen test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Dudewicz E. J., van der Meulen E. C. (1981): Entropy-based tests of uniformity. --- JASA, vol. 76, pp. 967--974.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
dudewicz.unif.test(runif(100,0,1))
dudewicz.unif.test(runif(100,0.1,0.9))
}
\keyword{htest}