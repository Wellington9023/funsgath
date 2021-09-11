cons.ord <- function(spe,#物种数据
                     env,#环境数据
                     group = NULL,#分组 仅支持一组
                     col = NULL,#分组点的颜色
                     hull=F,#按分组连接最外围的点
                     coord_fix=F,#固定坐标轴
                     alpha = 1,#点的透明度0-1
                     alpha_el = 0.4,#hull形状透明度
                     vec_ext = 1 #箭头长度倍数
                     ){
  theme_mk <- function(..., bg='transparent'){
    require(grid)
    theme_bw(...) +
      theme(
        panel.background=element_blank(),#背景为空
        panel.border=element_rect(color='black',size = 0.6),#面板边界
        panel.grid=element_blank(),#绘图区网格线
        #legend.position = "none",#无图例
        axis.text.x=element_text(colour = 'black',size = 16),#坐标轴刻度标签
        axis.text.y=element_text(colour = 'black',size = 16),#坐标轴刻度标签
        axis.title.x=element_text(size = 18,face = "bold"),#坐标轴标题
        axis.title.y=element_text(size = 18,face = "bold"),#坐标轴标题
        strip.text.x = element_text(size=16),#分面标签外观
        strip.text.y = element_text(size=16),#分面标签外观
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(size=16),#图例文字大小
        legend.text = element_text(size=16),#图例文字大小
        plot.title = element_text(size = 18),#主标题大小
        axis.ticks.length.y = unit(.25, "cm"),#刻度标签的长度
        axis.ticks.length.x = unit(.25, "cm"))

  }
  require(tidyverse)
  require(vegan)
  require(ggrepel)
  require(ggord)
  spe <-  spe
  #DCA
  dca <- decorana(veg = spe)
  dca1 <- max(dca$rproj[,1])
  print(paste("DCA =",round(dca1,2)))

  if (is.null(group))
  {
    if (3 < dca1 ){
      print("CCA")
      cca <- cca(spe, env, scale = TRUE)
      #CCA plot
      plot_CCA <-ggord(cca,
                       grp_in = NULL,
                       col = col,
                       ellipse =F,
                       hull=hull,
                       labcol = "red",
                       veccol = "red",
                       repel = T,
                       size = 4,
                       coord_fix =coord_fix,
                       ptslab = F,
                       addsize = NA,
                       alpha = alpha,
                       alpha_el = alpha_el,
                       vec_ext = 1)+
        labs(title = "CCA Plot") +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        theme_mk()
      ano <- anova.cca(cca)
      all <- list('cca'=cca,'plot'=plot_CCA,'ano'=ano)
      return(all)
    }
    else{
      print("RDA")
      rda <- rda(spe, env, scale = TRUE)
      #RDA plot
      plot_RDA <-ggord(rda,
                       grp_in = NULL,
                       col = col,
                       ellipse =F,
                       hull=hull,
                       labcol = "red",
                       veccol = "red",
                       repel = T,
                       size = 4,
                       coord_fix =coord_fix,
                       ptslab = F,
                       addsize = NA,
                       alpha = alpha,
                       alpha_el = alpha_el,
                       vec_ext = 1)+
        labs(title = "RDA Plot") +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        theme_mk()
      ano <- anova.cca(rda)
      all <- list('cca'=rda,'plot'=plot_RDA,'ano'=ano)
      return(all)

    }
  }
  else
  {
    if (3 < dca1 ){
      print("CCA")
      cca <- cca(spe, env, scale = TRUE)
      #CCA plot
      plot_CCA <-ggord(cca,
                       grp_in = group,
                       col = col,
                       ellipse =F,
                       hull=hull,
                       labcol = "red",
                       veccol = "red",
                       repel = T,
                       size = 4,
                       coord_fix =coord_fix,
                       ptslab = F,
                       addsize = NA,
                       alpha = alpha,
                       alpha_el = alpha_el,
                       vec_ext = 1)+
        labs(title = "CCA Plot") +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        theme_mk()
      ano <- anova.cca(cca)
      all <- list('cca'=cca,'plot'=plot_CCA,'ano'=ano)
      return(all)
    }
    else{
      print("RDA")
      rda <- rda(spe, env, scale = TRUE)
      #Plot
      plot_RDA <-ggord(rda,
                       grp_in = group,
                       col = col,
                       ellipse =F,
                       hull=hull,
                       labcol = "red",
                       veccol = "red",
                       repel = T,
                       size = 4,
                       coord_fix =coord_fix,
                       ptslab = F,
                       addsize = NA,
                       alpha = alpha,
                       alpha_el = alpha_el,
                       vec_ext = 1)+
        labs(title = "RDA Plot") +
        geom_vline(aes(xintercept = 0), linetype = "dotted") +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        theme_mk()
      ano <- anova.cca(rda)
      all <- list('cca'=rda,'plot'=plot_RDA,'ano'=ano)
      return(all)
    }
  }
}

#data(varespec)
#data(varechem)

#data(dune)
#data(dune.env)

#cons.ord(spe = varespec,env = varechem)#,group
#cons.ord(spe = dune,dune.env[,-3],dune.env$Management,col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),hull = T,alpha_el = 0.2)

