\name{cor.sig}
\alias{cor.sig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cor.sig(x, y = NULL, use = "pairwise", method = "pearson", adjust = "holm", alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{use}{
%%     ~~Describe \code{use} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{adjust}{
%%     ~~Describe \code{adjust} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y = NULL, use = "pairwise", method = "pearson", 
    adjust = "holm", alpha = 0.05) 
{
    mypaste <- function(x, y) {
        paste(x, y, sep = "")
    }
    if (is.null(y)) {
        res.cor1 = psych::corr.test(x, use = use, method = method, 
            adjust = adjust, alpha = 0.05)$r
        res.sig1 = psych::corr.test(x, use = use, method = method, 
            adjust = adjust, alpha = 0.05)$p
        res.sig1[which(res.sig1 <= 0.01)] = "**"
        res.sig1[which(res.sig1 > 0.01 & res.sig1 < 0.05)] = "*"
        res.sig1[which(res.sig1 >= 0.05)] = ""
        cor.sig <- mapply(mypaste, as.data.frame(round(res.cor1, 
            3)), as.data.frame(res.sig1))
        row.names(cor.sig) <- colnames(x)
        as.data.frame(cor.sig)
    }
    else {
        res.cor1 = psych::corr.test(x, y, use = use, method = method, 
            adjust = adjust, alpha = 0.05)$r
        res.sig1 = psych::corr.test(x, y, use = use, method = method, 
            adjust = adjust, alpha = 0.05)$p
        res.sig1[which(res.sig1 <= 0.01)] = "**"
        res.sig1[which(res.sig1 > 0.01 & res.sig1 < 0.05)] = "*"
        res.sig1[which(res.sig1 >= 0.05)] = ""
        cor.sig <- mapply(mypaste, as.data.frame(round(res.cor1, 
            3)), as.data.frame(res.sig1))
        row.names(cor.sig) <- colnames(x)
        as.data.frame(cor.sig)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
