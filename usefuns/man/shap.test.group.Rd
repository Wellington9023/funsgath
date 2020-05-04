\name{shap.test.group}
\alias{shap.test.group}
\title{shap.test.group}
\usage{
shap.test.group(data,var,group = NULL)

data : dataset
var  : a variable name or a vector of variable names
group: a factor to be used for groupig
}
\description{
shap.test.group() function can run shapiro.test for multi-variable by group. And the result was kept in a list.
}
\examples{
library(usefuns)

nm <- colnames(iris[,c(1:4)])

shap.test.group(iris, nm)
shap.test.group(iris, nm,group = "Species")
}
