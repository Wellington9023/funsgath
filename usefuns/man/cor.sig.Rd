\name{cor.sig}
\alias{cor.sig}
\title{cor.sig}
\usage{
cor.sig(x,y=NULL,use = "pairwise",method="pearson",adjust="holm", alpha=.05)
}
\description{
cor.sig() function can run correlation analysis for one single dataframe or two dataframes. And the result shows 0.99**.
}
\examples{
library(usefuns)

data(mtcars)
cor.sig(mtcars[,c(1:8)])
cor.sig(mtcars,mtcars[,c(2:4)])
}
