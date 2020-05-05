cor.sig = function(x,y=NULL,use = "pairwise",method="pearson",adjust="holm",
                   alpha=.05) {
  #可用于单个或两个数据框
  #use = "pairwise" ("complete" )
  #method="pearson" ("spearman" and "kendall")
  #adjust="holm"("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  #alpha=.05

  #拼接小函数
  mypaste <- function(x,y) {paste(x,  y,  sep="")}
  if (is.null(y)) {
    #获得r值
    res.cor1 = psych::corr.test(x,use = use,method= method,adjust= adjust,
                                alpha=.05)$r
    #获得p值
    res.sig1 = psych::corr.test(x,use = use,method= method,adjust= adjust,
                                alpha=.05)$p
    #星号替换p值
    res.sig1[which(res.sig1 <= 0.01)] = "**"
    res.sig1[which(res.sig1 > 0.01 & res.sig1 < 0.05)] = "*"
    res.sig1[which(res.sig1 >= 0.05)] = ""

    #拼接
    cor.sig<-mapply(mypaste, as.data.frame(round(res.cor1, 3)), as.data.frame(res.sig1))

    #重命名行名
    row.names(cor.sig) <- colnames(x)

    as.data.frame(cor.sig)}
  else{
    res.cor1 = psych::corr.test(x, y,use = use,method= method,adjust= adjust,
                                alpha=.05)$r
    res.sig1 = psych::corr.test(x, y,use = use,method= method,adjust= adjust,
                                alpha=.05)$p

    res.sig1[which(res.sig1 <= 0.01)] = "**"
    res.sig1[which(res.sig1 > 0.01 & res.sig1 < 0.05)] = "*"
    res.sig1[which(res.sig1 >= 0.05)] = ""

    cor.sig<-mapply(mypaste, as.data.frame(round(res.cor1, 3)), as.data.frame(res.sig1))

    row.names(cor.sig) <- colnames(x)

    as.data.frame(cor.sig)}
}
