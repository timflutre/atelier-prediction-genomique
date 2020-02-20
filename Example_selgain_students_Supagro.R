# install the package
install.packages("selectiongain")

# load the package
library(selectiongain)

# determine the session --> regularly updated

#current version is 2.0.591, avoid older versions, they might have bugs! 
sessionInfo()


# First step - define budget, crop and trait via variance components
Budget= 1000

## Variance components : AGC, AGCxLoc, AGCxyear, AGCxLocxyear, error
VCGCAandError= c(14.06,22.27,0,0,24.37)

## Non additive variance : SCA, SCAxLoc , SCAxyear, SCAxLocxYear
VCSCA=c(0,0,0,0)

## Number of final selected lines
Nf = 5


###########################################
# Let's go to breeding schemes
###########################################



#### 1. Phenotypic selection

# Two-stage phenotypic selection with nursery selection
# Breeding scheme PSstandard in Marulanda et al 2016

multistageoptimum.search (maseff=NA, # GS prediction ability
						  alpha.nursery = 0.25, # selected fraction in disease nursery
						  cost.nursery = c(1,0.3), # relative cost of nursery vs. plot
						  VGCAandE=VCGCAandError,
                          VSCA=VCSCA, 
                          CostProd = c(0,0,0), # Seed production cost 
                          CostTest = c(2,1,1),  # costs of Genomic selection, yield plot, yiel plot
                          Nf = Nf , # number of final selected lines
                          Budget = Budget, # total budget
                          N2grid = c(Nf, 6011, 50), # exploration grid for N2 effectives
                          N3grid = c(Nf, 1511, 5),  # exploration grid for N3 effectives 
                          L2grid=c(1,5,1), # exploration grid for the number of locations in first field avaluation
                          L3grid=c(3,10,1), # exploration grid for varietal field evaluation
                          T2grid=c(1,1,1), # evaluation of tester 
                          T3grid=c(1,1,1), # evaluation of tester 
                          R2=1, # number of repets first eval
                          R3=1, # number of repeats varietal eval
                          alg = Miwa(),
                          detail=FALSE, fig=FALSE)




#### 2. Breeding schemes with GS selection

# Nursery selection, followed by GS and one-stage phenotypic selection
# Breeding scheme GSrapid in Marulanda et al 2016


multistageoptimum.search (maseff=0.3,alpha.nursery = 0.25, 
                          VGCAandE=VCGCAandError, 
                          VSCA=VCSCA, 
                          cost.nursery = c(1,0.3), 
                          CostProd = c(0,0,0), 
                          CostTest = c(2,1,0),
                          Nf = 5, 
                          Budget = Budget, 
                          N2grid = c(Nf, 1011, 10),
                           N3grid = c(Nf, Nf, 1), 
                          L2grid=c(1,10,1), 
                          L3grid=c(1,1,1),
                          T2grid=c(1,1,1), 
                          T3grid=c(1,1,1), 
                          R2=1, R3=1, alg = Miwa(),
                          detail=FALSE, fig=F,
                          t2free = T)


