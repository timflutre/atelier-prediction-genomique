# author: Friedrich Longin (Univ. Hohenheim)

# install the package
install.packages("selectiongain")

# load the package
library(selectiongain)

# determine the session --> regularly updated

#current version is 2.0.591, avoid older versions, they might have bugs!
sessionInfo()


# First step - define budget, crop and trait via variance components
Budget= 10000
VCGCAandError= c(14.06,22.27,0,0,24.37)
VCSCA=c(0,0,0,0)
Nf = 5



###########################################
# Let's go to breeding schemes
###########################################



#### 1. Phenotypic selection

# Two-stage phenotypic selection with nursery selection
# Breeding scheme PSstandard in Marulanda et al 2016

multistageoptimum.search (maseff=NA,alpha.nursery = 0.25, cost.nursery = c(1,0.3),VGCAandE=VCGCAandError,
                          VSCA=VCSCA, CostProd = c(0,0,0), CostTest = c(2,1,1),
                          Nf = 5, Budget = Budget, N2grid = c(Nf, 6011, 50),
                          N3grid = c(Nf, 1511, 5), L2grid=c(1,5,1), L3grid=c(3,10,1),
                          T2grid=c(1,1,1), T3grid=c(1,1,1), R2=1, R3=1, alg = Miwa(),
                          detail=FALSE, fig=FALSE)



# one-stage phenotypic selection with nursery selection
# Breeding scheme PSrapid in Marulanda et al. 2016

multistageoptimum.search (maseff=NA,alpha.nursery = 0.4,
                          VGCAandE=VCGCAandError, VSCA=VCSCA,
                          cost.nursery = c(1,0.3), CostProd = c(0,4,0), CostTest = c(2,1,0),
                          Nf = 5, Budget = Budget,
                          N2grid = c(Nf, 511, 10), N3grid = c(Nf, 5, 1),
                          L2grid=c(1,5,1), L3grid=c(0,0,1),
                          T2grid=c(1,1,1), T3grid=c(0,0,1),
                          R2=1, R3=1, alg = Miwa(),
                          detail=FALSE, fig=FALSE,
                          t2free = T)


#### 2. Breeding schemes with GS selection

Budget= 10000
VCGCAandError= c(14.06,22.27,0,0,24.37)
VCSCA=c(0,0,0,0)
Nf = 5

# Nursery selection, followed by GS and one-stage phenotypic selection
# Breeding scheme GSrapid in Marulanda et al 2016


multistageoptimum.search (maseff=0.1,alpha.nursery = 0.25,
                          VGCAandE=VCGCAandError, VSCA=VCSCA,
                          cost.nursery = c(1,0.3), CostProd = c(0,0,0), CostTest = c(2,1,0),
                          Nf = 5, Budget = Budget,
                          N2grid = c(Nf, 1011, 10), N3grid = c(Nf, Nf, 1),
                          L2grid=c(1,10,1), L3grid=c(1,1,1),
                          T2grid=c(1,1,1), T3grid=c(1,1,1),
                          R2=1, R3=1, alg = Miwa(),
                          detail=FALSE, fig=F,
                          t2free = T)


