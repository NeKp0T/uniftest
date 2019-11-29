\name{neyman.unif.test}
\alias{neyman.unif.test}
\title{Neyman-Barton test for uniformity}
\description{
  Performs Neyman-Barton test for the hypothesis of uniformity.
}
\usage{
neyman.unif.test(x, nrepl=2000, k=5)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
  \item{k}{the number of Legendre polynomials.}
}
\details{The Neyman-Barton test for uniformity is based on the following statistic:
\deqn{
N_k = \sum_{j=1}^{k}{\left(\frac{1}{\sqrt{n}}\sum_{i=1}^{n}{\pi_j(x_i)}\right)^2},
}
where \eqn{\pi_j(x_i)} are Legendre polynomials orthogonal on the interval [0,1].

The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Neyman-Barton statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Neyman-Barton test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Neyman J. "Smooth" test for goodness-of-fit // Scand. Aktuarietidsrift. 1937. V. 20. P. 149-199.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
neyman.unif.test(runif(100,0,1))
neyman.unif.test(runif(100,0.1,0.9))
}
\keyword{htest}