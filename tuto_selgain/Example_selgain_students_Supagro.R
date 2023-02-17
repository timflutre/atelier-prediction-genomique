# install the package
install.packages("selectiongain")

# load the package
library(selectiongain)

# determine the session --> regularly updated

#current version is 2.0.591, avoid older versions, they might have bugs! 
sessionInfo()


# First step - define budget, crop and trait via variance components
Budget= 10000
VCGCAandError= c(0.11,0.22,0,0,0.2)
VCSCA=c(0,0,0,0)
Nf = 5


###########################################
# Let's go to breeding schemes
###########################################



#### 1. Phenotypic selection

# Two-stage phenotypic selection with nursery selection
# Breeding scheme PSstandard in Marulanda et al 2016

multistageoptimum.search (maseff=NA,alpha.nursery = 0.2, cost.nursery = c(1,0.3),VGCAandE=VCGCAandError,
                          VSCA=VCSCA, CostProd = c(0,0,0), CostTest = c(1.5,1,1),
                          Nf = 5, Budget = Budget, N2grid = c(Nf, 3011, 30),
                          N3grid = c(Nf, 511, 5), L2grid=c(2,4,1), L3grid=c(2,8,1),
                          T2grid=c(1,1,1), T3grid=c(1,1,1), R2=1, R3=1, alg = Miwa(),
                          detail=FALSE, fig=FALSE)




#### 2. Breeding schemes with GS selection

# Nursery selection, followed by GS and one-stage phenotypic selection
# Breeding scheme GSrapid in Marulanda et al 2016


multistageoptimum.search (maseff=0.4,alpha.nursery = 0.2, 
                          VGCAandE=VCGCAandError, VSCA=VCSCA, 
                          cost.nursery = c(1,0.3), CostProd = c(0,0,0), CostTest = c(1.5,1,0),
                          Nf = 5, Budget = Budget, 
                          N2grid = c(Nf, 3011, 30), N3grid = c(Nf, Nf, 1), 
                          L2grid=c(2,8,1), L3grid=c(1,1,1),
                          T2grid=c(1,1,1), T3grid=c(1,1,1), 
                          R2=1, R3=1, alg = Miwa(),
                          detail=FALSE, fig=F,
                          t2free = T)


