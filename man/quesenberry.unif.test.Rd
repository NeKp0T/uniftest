\name{quesenberry.unif.test}
\alias{quesenberry.unif.test}
\title{Quesenberry--Miller test for uniformity}
\description{
  Performs Quesenberry--Miller test for the hypothesis of uniformity, 
  see Quesenberry and Miller (1977).
}
\usage{
quesenberry.unif.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Quesenberry--Miller test for uniformity is based on the following statistic:
\deqn{
B_n = \sum_{i=1}^{n+1}{\left( X_{(i)} - X_{(i-1)} \right)^2} + \sum_{i=1}^{n}{\left( X_{(i)} - X_{(i-1)} \right)\left( X_{(i+1)} - X_{(i)} \right)},
}
where \eqn{X_{(0)}=0}, \eqn{X_{(n+1)}=1}.
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Quesenberry--Miller statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Quesenberry--Miller test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Quesenberry, C.P. and Miller F.L. (1977): Power studies of some tests for uniformity. --- J. Stat. Comput. Simul., vol. 5, pp. 169--191.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
quesenberry.unif.test(runif(100,0,1))
quesenberry.unif.test(runif(100,0,1.05))
}
\keyword{htest}