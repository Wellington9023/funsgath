#多变量 分组检验正态性
shap.test.group <- function(
  data,
  var,
  group = NULL)
{
  xdata <- data

  if (is.null(group)) {
    test.res <- list()
    for (j in nm) {
      t.r <- shapiro.test(xdata[,j])
      test.result <- data.frame(varName=0,
                                Method=0,
                                W=0,
                                p.value=0,
                                norm.test=0)
      test.result[1,1] = j
      test.result[1,2] = "Shapiro-Wilk"
      test.result[1,3] = round(t.r$statistic,4)
      test.result[1,4] = round(t.r$p.value,4)

      if (t.r$p.value > 0.05){
        test.result[1,5] = "Norm"
      }else{
        test.result[1,5] = "Other_situation"  #未通过判断执行的命令
      }
      g <- data.frame(test.result)

      test.res[j] <- list(g)

      print(test.result)
      cat("\n")
    }

    return(test.res)
  }
  else{
    test.res <- list()
    for (j in var) {

      require(magrittr)

      table(xdata[,group]) %>%
        data.frame(.) -> a1

      a2 <- as.vector(a1[,1])

      data = data.frame(group = xdata[,group],
                        j = xdata[,j])

      test.result <- data.frame(varName=0,
                                group=0,
                                Method=0,
                                W=0,
                                p.value=0,
                                norm.test=0)

      for (i in (1:length(a2))){
        subset(data,
               group == a2[i],
               select = j) %>%
          .[,1] %>%
          shapiro.test(.) -> t.r

        test.result[i,1] = j
        test.result[i,2] = a2[i]
        test.result[i,3] = "Shapiro-Wilk"
        test.result[i,4] = round(t.r$statistic,4)
        test.result[i,5] = round(t.r$p.value,4)

        if (t.r$p.value > 0.05)
          test.result[i,6] = "Norm"
        else
          test.result[i,6] = "Other_situation"
      }

      g <- data.frame(test.result)

      test.res[j] <- list(g)

      print(test.result)
      cat("\n")
    }
  }
  return(test.res)
  ## modify from https://blog.csdn.net/yanlingbin/article/details/81194134
}


