\name{kolmogorov.unif.test}
\alias{kolmogorov.unif.test}
\title{Kolmogorov-Smirnov test for uniformity}
\description{
  Performs Kolmogorov-Smirnov test for the hypothesis of uniformity, see Kolmogorov (1933).
}
\usage{
kolmogorov.unif.test(x, nrepl=2000,k=0)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
  \item{k}{variant the criterion.}
}
\details{The Kolmogorov-Smirnov test for uniformity is based on the following statistics:
\deqn{
D^+ = max_i\left(x_i-\frac{i}{n+1}\right),\quad
D^- = max_i\left(\frac{i}{n+1}-x_i\right),\quad
D = max(D^+,D^-).
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Kolmogorov-Smirnov statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Kolmogorov-Smirnov test for uniformity".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Kolmogorov A. (1933): Sulla determinazione empirica di una legge di distribuzione. --- G. Ist. Ital. Attuari, vol. 4, pp. 83--91.
}

\author{Maxim Melnik and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
kolmogorov.unif.test(runif(100,0,1))
kolmogorov.unif.test(runif(100,0.1,0.9))
}
\keyword{htest}