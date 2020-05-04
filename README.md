# funsgath
shap.test.group() function can run shapiro.test for multi-variable by group. And the result was kept in a list.

# install
```
library(devtools)
install_github("Wellington9023/funsgath/usefuns")
```
# usage
```
library(usefuns)

nm <- colnames(iris[,c(1:4)])

shap.test.group(iris, nm)
shap.test.group(iris, nm,group = "Species")
```
