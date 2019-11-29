\name{kuiper.unif.test}
\alias{kuiper.unif.test}
\title{Kuiper test for uniformity}
\description{
  Performs Kuiper test for the hypothesis of uniformity, see Kuiper (1960).
}
\usage{
kuiper.unif.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Kuiper test for uniformity is based on the following statistic:
\deqn{
V = \max_i\left(\frac{i}{n}-X_{(i)}\right) + \max_i\left(X_{(i)}-\frac{i-1}{n}\right)
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Kuiper statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Kuiper test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Kuiper, N.H. (1960): Tests concerning random points on a circle. --- Proc. Kon. Ned. Akad. Wetensch., Ser. A, vol. 63, pp. 38--47.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
kuiper.unif.test(runif(100,0,1))
kuiper.unif.test(rbeta(100,0.5,0.5))
}
\keyword{htest}