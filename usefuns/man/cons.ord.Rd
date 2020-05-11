\name{cons.ord}
\alias{cons.ord}
\title{cons.ord}
\usage{
cons.ord(spe,
         env,
         group = NULL,
         col = NULL,
         hull=F,
         coord_fix=F,
         alpha = 1,
         alpha_el = 0.4)
}
\description{
cons.ord() function can run first run DCA to choose suitable method (RDA or CCA).
}
\examples{
library(usefuns)
library(vegan)
#data(varespec)
#data(varechem)

#data(dune)
#data(dune.env)

#cons.ord(spe = varespec,env = varechem)#,group
#cons.ord(spe = dune,dune.env[,-3],dune.env$Management,col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),hull = T,alpha_el = 0.2)

}
