


##' Plot a matrix as a heatmap in its natural orientation, with a colored
##' scale on the right side, and optionally using its dimension names for
##' rows and columns
##'
##' To print all row names, choose idx.rownames=1:nrow(z). To print a subset
##' of 10 row names, choose idx.rownames=floor(seq(1, nrow(z), length.out=10)).
##' Similarly for column names.
##' @param z matrix to be plotted
##' @param main title to appear above the heatmap
##' @param idx.rownames vector giving the indices of the row names of z to be added on the left side of the plot
##' @param idx.colnames vector giving the indices of the column names of z to be added on top of the plot
##' @param breaks vector of breaks (if NULL, will be \code{seq(min(z), max(z), length.out=nb.breaks)})
##' @param nb.breaks number of breaks
##' @param left.text.at vector which names and values will be used to label the left side of the plot; if not NULL, takes precedence over idx.rownames
##' @param cex.txt numeric character expansion factor used with "left.text.at"
##' @param cex.sc numeric character expansion factor used with the scale
##' @param col.pal output of \code{\link[grDevices]{colorRampPalette}}
##' @param custom.mar see \code{\link[graphics]{par}}
##' @author Timothee Flutre
##' @examples
##' \dontrun{set.seed(1859)
##' genomes <- simulCoalescent(nb.inds=200, nb.pops=3, mig.rate=3)
##' X <- genomes$genos
##' A <- estimGenRel(X=X, relationships="additive", method="vanraden1")
##' imageWithScale(z=A, main="Additive genetic relationships", breaks=seq(0,1,length.out=20),
##'                left.text.at=setNames(c(0.83, 0.5, 0.17), c("pop1", "pop2", "pop3")))
##' }
##' @export
imageWithScale <- function(z, main=NULL, idx.rownames=NULL, idx.colnames=NULL,
                           breaks=NULL, nb.breaks=20, left.text.at=NULL,
                           cex.txt=1, cex.sc=1,
                           col.pal=grDevices::colorRampPalette(c("black", "red", "yellow"),
                                                               space="rgb"),
                           custom.mar=NULL){
  stopifnot(is.matrix(z))
  if(! is.null(left.text.at))
    stopifnot(is.null(idx.rownames))
  if(! is.null(idx.rownames) & is.null(rownames(z)))
    stop("non-null idx.rownames requires z to have row names")
  if(! is.null(idx.colnames) & is.null(colnames(z)))
    stop("non-null idx.colnames requires z to have column names")
  if(is.null(breaks))
    breaks <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=nb.breaks)
  
  def.par <- graphics::par(no.readonly=TRUE)
  
  graphics::layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(7,1))
  ## layout.show(2) # for debugging purposes
  
  ## plot the heatmap
  if(is.null(custom.mar)){
    custom.mar <- c(1, 5, 6, 1)
    if(is.null(idx.rownames) & is.null(left.text.at))
      custom.mar[2] <- 1
    if(is.null(idx.colnames))
      custom.mar[3] <- 3
  }
  graphics::par(mar=custom.mar, no.readonly=TRUE)
  graphics::image(t(z)[,nrow(z):1], axes=FALSE, col=col.pal(length(breaks)-1))
  if(! is.null(main))
    graphics::mtext(text=main, side=3, line=ifelse(is.null(idx.colnames), 1, 4),
                    font=2, cex=1.3)
  if(! is.null(idx.colnames))
    graphics::text(x=seq(0,1,length.out=length(idx.colnames)), y=graphics::par("usr")[4]+0.02,
                   srt=45, adj=0, labels=colnames(z)[idx.colnames], xpd=TRUE)
  if(! is.null(idx.rownames))
    graphics::mtext(text=rev(rownames(z)[idx.rownames]), side=2, line=1,
                    at=seq(0,1,length.out=length(idx.rownames)),
                    las=2)
  if(! is.null(left.text.at))
    graphics::mtext(text=names(left.text.at), side=2, line=1,
                    at=left.text.at, las=2, cex=cex.txt)
  
  ## plot the scale
  graphics::par(mar=c(1,0,6,4), no.readonly=TRUE)
  plotWithScale(z, col=col.pal(length(breaks)-1), breaks=breaks, horiz=FALSE,
                yaxt="n")
  coords.ticks <- breaks[seq.int(1, length(breaks), length.out=5)]
  graphics::axis(4, at=coords.ticks, label=format(coords.ticks, digits=2),
                 las=2, lwd=0, lwd.ticks=1, cex.axis=cex.sc)
  
  on.exit(graphics::par(def.par))
}















##' Principal component analysis
##'
##' Plot the two first principal components from a PCA.
##' @param rotation matrix of rotated data which columns corresponds to "principal components" (the first column will be plotted along the x-axis against the second column along the y-axis)
##' @param prop.vars vector with the proportion of variance explained per PC
##' @param idx.x index of the column from "rotation" that will be plotted along the x-axis
##' @param idx.y index of the column from "rotation" that will be plotted along the y-axis
##' @param plot use "points" to show a plot with \code{\link[graphics]{points}} of PC1 versus PC2, and "text" to use \code{\link[graphics]{text}} with row names of \code{rotation} as labels
##' @param main main title of the plot
##' @param cols N-vector of colors
##' @param pchs N-vector of point symbols (used if \code{plot="points"})
##' @param ... arguments to be passed to \code{\link[graphics]{plot}}, such as "xlim", ylim", etc
##' @return nothing
##' @author Timothee Flutre
##' @seealso \code{\link{pca}}, \code{\link{orthoRotate2D}}
##' @export
plotPca <- function(rotation, prop.vars,
                    idx.x=1, idx.y=2,
                    plot="points", main="PCA",
                    cols=rep("black", nrow(rotation)),
                    pchs=rep(20, nrow(rotation)),
                    ...){
  stopifnot(is.matrix(rotation),
            is.vector(prop.vars),
            is.numeric(prop.vars),
            all(prop.vars >= 0),
            all(prop.vars <= 1),
            idx.x %in% 1:ncol(rotation),
            idx.y %in% 1:ncol(rotation),
            idx.x != idx.y,
            plot %in% c("points", "text"),
            is.vector(cols),
            length(cols) == nrow(rotation),
            is.vector(pchs),
            length(pchs) == nrow(rotation))
  
  graphics::plot(x=rotation[,idx.x], y=rotation[,idx.y], las=1,
                 xlab=paste0("PC", idx.x, " (",
                             format(100 * prop.vars[idx.x], digits=3), "%)"),
                 ylab=paste0("PC", idx.y, " (",
                             format(100 * prop.vars[idx.y], digits=3), "%)"),
                 main=main, type="n",
                 ...)
  graphics::abline(h=0, lty=2)
  graphics::abline(v=0, lty=2)
  
  if(plot == "points"){
    graphics::points(x=rotation[,idx.x], y=rotation[,idx.y], col=cols,
                     pch=pchs)
  } else if(plot == "text")
    graphics::text(x=rotation[,idx.x], y=rotation[,idx.y], col=cols,
                   labels=rownames(rotation))
}




##' Principal component analysis
##'
##' Given a data matrix X with N rows and P columns, principal component analysis can be performed using the singular value decomposition (SVD), X = U D V^T, where U is NxN, D is NxN and diagonal (singular values), and V is PxN.
##' Another way to perform it, is to first compute a symmetric matrix, S (e.g. the scatter matrix X X^T, but not necessarily), and then to use the eigendecomposition (EVD) of it, S = Q Delta Q^-1, where Q is NxN and Delta is NxN and diagonal (eigenvalues).
##' TODO: for large matrices, use the \href{https://cran.r-project.org/package=RSpectra}{RSpectra} package which allows to calculate only the k largest singular values and corresponding singular vectors.
##' @param X data matrix with N rows ("units") and P columns ("variables"); P can be equal to N but X shouldn't be symmetric; a data frame will be converted into a matrix; specify X or S, but not both
##' @param S symmetric matrix with N rows and columns; a data frame will be converted into a matrix; specify X or S, but not both
##' @param ct if TRUE, the columns of X will be centered (recommended); a good reason to center the data matrix for PCA is given in \href{http://link.springer.com/10.1007/s11063-007-9069-2}{Miranda et al (2008)}
##' @param sc if TRUE, the columns of X will be scaled/standardized (if different units)
##' @param plot if not NULL, use "points" to show a plot with \code{\link[graphics]{points}} of PC1 versus PC2, and "text" to use \code{\link[graphics]{text}} with row names of \code{X} as labels (use \code{\link{plotPca}} to use other axes)
##' @param main main title of the plot
##' @param cols N-vector of colors (will be \code{"black"} by default)
##' @param pchs N-vector of point symbols; used if \code{plot="points"}; will be \code{20} by default
##' @param ES10 if TRUE (and X is specified), the Lambda (= U) and F (= D V^T) matrices from \href{http://dx.doi.org/10.1371/journal.pgen.1001117}{Engelhart and Stephens (2010)} are also returned
##' @return list with (1) if X is given, the rotated data matrix (= X V) which rows correspond to the original rows after translation towards the sample mean (if center=TRUE) and rotation onto the "principal components" (eigenvectors of the sample covariance matrix), (2) if X is given, the singular values, (3) the eigen values, and (4) the proportions of variance explained per PC
##' @author Timothee Flutre
##' @seealso \code{\link{plotPca}}
##' @examples
##' \dontrun{## simulate genotypes from 3 populations
##' set.seed(1859)
##' genomes <- simulCoalescent(nb.inds=300, nb.pops=3, mig.rate=3)
##' X <- genomes$genos
##' table(inds.per.pop <- kmeans(X, 3)$cluster)
##' A <- estimGenRel(X)
##' imageWithScale(A, main="Additive genetic relationships") # we clearly see 3 clusters
##'
##' ## prcomp() uses svd()
##' out.prcomp <- prcomp(x=X, retx=TRUE, center=TRUE, scale.=FALSE)
##' summary(out.prcomp)$importance[,1:4]
##' out.prcomp$sdev[1:4]
##' (out.prcomp$sdev^2 / sum(out.prcomp$sdev^2))[1:4]
##' head(out.prcomp$rotation[, 1:4]) # first four PCs (i.e. eigenvectors)
##' head(out.prcomp$x[, 1:4]) # rotated data (= data x rotation matrix)
##'
##' ## princomp() uses eigen() and requires more units than variables
##' out.princomp <- princomp(x=X)
##'
##' ## this function fed with the data matrix
##' out.pca.X <- pca(X=X, ct=TRUE, sc=FALSE)
##' out.pca.X$sgl.values[1:4]
##' out.pca.X$eigen.values[1:4]
##' out.pca.X$prop.vars[1:4]
##' head(out.pca.X$rot.dat[, 1:4]) # rotated data
##'
##' ## this function fed with the scatter matrix
##' S <- tcrossprod(scale(X, center=TRUE, scale=FALSE))
##' out.pca.S <- pca(S=S)
##' out.pca.S$eigen.values[1:4]
##' out.pca.S$prop.vars[1:4]
##' head(out.pca.S$rot.dat[, 1:4]) # rotated data
##' }
##' @export
pca <- function(X=NULL, S=NULL, ct=TRUE, sc=FALSE, plot=NULL, main="PCA",
                cols=NULL, pchs=NULL, ES10=FALSE){
  stopifnot(xor(! is.null(X), ! is.null(S)))
  if(! is.null(X)){
    if(! is.matrix(X))
      X <- as.matrix(X)
    stopifnot(is.matrix(X),
              ! isSymmetric(X),
              all(! is.na(X)),
              is.logical(ct),
              is.logical(sc),
              is.logical(ES10))
  } else if(! is.null(S)){
    if(! is.matrix(S))
      S <- as.matrix(S)
    stopifnot(is.matrix(S),
              isSymmetric(S))
  }
  if(! is.null(plot)){
    if(is.null(cols))
      cols <- rep("black", ifelse(is.null(S), nrow(X), nrow(S)))
    if(is.null(pchs))
      pchs <- rep(20, ifelse(is.null(S), nrow(X), nrow(S)))
    stopifnot(plot %in% c("points", "text"),
              is.vector(cols),
              ifelse(is.null(S), length(cols) == nrow(X),
                     length(cols) == nrow(S)),
              is.vector(pchs),
              ifelse(is.null(S), length(pchs) == nrow(X),
                     length(pchs) == nrow(S)))
  }
  
  output <- list()
  
  if(! is.null(X)){
    X <- scale(x=X, center=ct, scale=sc)
    svd.X <- svd(x=X)
    sgl.values <- svd.X$d
    eigen.values <- sgl.values^2
    rot.dat <- X %*% svd.X$v
  } else if(! is.null(S)){
    sgl.values <- NULL
    evd.S <- eigen(S)
    eigen.values <- evd.S$values
    rot.dat <- evd.S$vectors
    rownames(rot.dat) <- rownames(S)
  }
  
  colnames(rot.dat) <- paste0("PC", 1:ncol(rot.dat))
  prop.vars <- eigen.values / sum(eigen.values)
  names(prop.vars) <- colnames(rot.dat)
  
  output$sgl.values <- sgl.values
  output$eigen.values <- eigen.values
  output$prop.vars <- prop.vars
  output$rot.dat <- rot.dat
  
  if(! is.null(plot))
    plotPca(rotation=rot.dat, prop.vars=prop.vars, plot=plot, main=main,
            cols=cols, pchs=pchs)
  
  if(! is.null(X) & ES10){
    ## Lambda = U
    ## F = D V' and is N x P
    output$Lambda <- svd.X$u
    rownames(output$Lambda) <- rownames(X)
    output$F <- diag(svd.X$d) %*% t(svd.X$v)
    colnames(output$F) <- colnames(X)
  }
  
  return(output)
}



##' Genomic relatedness
##'
##' Estimate genetic relationships between genotypes from their SNP genotypes.
##' Note that this function estimates "relationships" and not "coancestries".
##' See \href{http://dx.doi.org/10.1186/1297-9686-43-27}{Toro et al (2011)}: "for diploid individuals, twice the coancestry coefficient is the additive relationship coefficient, which describes the ratio between the genetic covariance between individuals and the genetic variance of the base population".
##' @param X matrix of bi-allelic SNP genotypes encoded in allele doses in {0,1,2}, with genotypes in rows and SNPs in columns; missing values should be encoded as NA
##' @param afs vector of allele frequencies, corresponding to the alleles whose copies are counted in \code{X} (if NULL, will be calculated with \code{\link{estimSnpAf}})
##' @param thresh threshold on minor allele frequencies below which SNPs are ignored (e.g. 0.01; NULL to skip this step)
##' @param relationships relationship to estimate (additive/dominant/gaussian) where "gaussian" corresponds to the Gaussian kernel from \href{http://dx.doi.org/10.3835/plantgenome2011.08.0024}{Endelman (2011)}
##' @param method \itemize{
##' \item if additive relationships, can be "vanraden1" (first method in \href{http://dx.doi.org/10.3168/jds.2007-0980}{VanRaden, 2008}), "toro2011_eq10" (equation 10 using molecular covariance from \href{http://dx.doi.org/10.1186/1297-9686-43-27}{Toro et al, 2011}), "habier" (similar to "vanraden1" but without centering; from \href{http://dx.doi.org/10.1534/genetics.107.081190}{Habier et al, 2007}), "astle-balding" (two times equation 2.2 in \href{http://dx.doi.org/10.1214/09-sts307}{Astle & Balding, 2009}), "yang" (similar to 'astle-balding' but without ignoring sampling error per SNP; from \href{http://dx.doi.org/10.1038/ng.608}{Yang et al, 2010}), "zhou" (centering the genotypes with \code{\link{scale}} and not assuming that rare variants have larger effects; from \href{http://dx.doi.org/10.1371/journal.pgen.1003264}{Zhou et al, 2013}) or "center-std";
##' \item if dominant relationships, can be "vitezica" (classical/statistical parametrization from \href{http://dx.doi.org/10.1534/genetics.113.155176}{Vitezica et al, 2013}) or "su" (from \href{http://dx.doi.org/10.1371/journal.pone.0045293}{Su et al, 2012})
##' }
##' @param theta smoothing parameter for "gauss"
##' @param verbose verbosity level (0/1)
##' @return matrix
##' @author Timothee Flutre
##' @examples
##' \dontrun{set.seed(1859)
##' nb.genos <- 200
##' Ne <- 10^4
##' chrom.len <- 10^5
##' mu <- 10^(-8)
##' c <- 10^(-8)
##' genomes <- simulCoalescent(nb.inds=nb.genos,
##'                            pop.mut.rate=4 * Ne * mu * chrom.len,
##'                            pop.recomb.rate=4 * Ne * c * chrom.len,
##'                            chrom.len=chrom.len)
##' X <- genomes$genos
##'
##' A.vr <- estimGenRel(X=X, relationships="additive", method="vanraden1")
##' mean(diag(A.vr)) # should be 1 in a base population at HWE
##' mean(A.vr[upper.tri(A.vr)]) # should be 0 in a base population at HWE
##'
##' D.v <- estimGenRel(X=X, relationships="dominant", method="vitezica")
##' mean(diag(D.v)) # should be 1 in a base population at HWE
##' mean(D.v[upper.tri(D.v)]) # should be 0 in a base population at HWE
##' }
##' @export
estimGenRel <- function(X, afs=NULL, thresh=NULL, relationships="additive",
                        method="vanraden1", theta=0.5, verbose=1){
  stopIfNotValidGenosDose(X, check.noNA=FALSE)
  stopifnot(relationships %in% c("additive", "dominant", "gaussian"))
  if(relationships == "additive")
    stopifnot(method %in% c("vanraden1", "toro2011_eq10", "habier",
                            "astle-balding", "yang",
                            "zhou", "center-std"))
  if(relationships == "dominant")
    stopifnot(method %in% c("vitezica", "su"))
  if(! is.null(thresh))
    stopifnot(thresh >= 0, thresh <= 0.5)
  if(relationships == "gauss"){
    stopifnot(! is.null(theta),
              is.numeric(theta),
              length(theta) == 1,
              theta > 0,
              theta <= 1)
  }
  if(! is.null(afs)){
    stopifnot(is.vector(afs),
              is.numeric(afs),
              ! is.null(names(afs)),
              all(colnames(X) %in% names(afs)),
              all(names(afs) %in% colnames(X)))
    afs <- afs[colnames(X)] # put in same order
  }
  
  gen.rel <- NULL # to be filled and returned
  
  N <- nrow(X) # nb of genotypes
  P <- ncol(X) # nb of SNPs
  
  if(any(is.na(X))){
    X <- discardMarkersMissGenos(X=X, verbose=verbose)
    P <- ncol(X)
    if(! is.null(afs))
      afs <- afs[colnames(X)]
  }
  
  if(is.null(afs)){
    if(verbose > 0){
      msg <- "estimate allele frequencies ..."
      write(msg, stdout())
    }
    afs <- estimSnpAf(X=X)
  }
  
  if(all(relationships == "additive", method == "center-std",
         is.null(thresh))){
    if(any(afs == 0, afs == 1)){
      msg <- paste0("some SNPs have fixed alleles, ",
                    "a threshold of 1% on MAFs will hence be used")
      write(msg, stdout())
      thresh <- 0.01
    }
  }
  if(! is.null(thresh)){
    mafs <- estimSnpMaf(afs=afs)
    if(any(mafs < thresh)){
      X <- discardSnpsLowMaf(X=X, mafs=mafs, thresh=thresh, verbose=verbose)
      P <- ncol(X)
      afs <- afs[colnames(X)]
    }
  }
  if(method == "center-std" & any(afs == 0.5)){
    idx <- which(afs != 0.5)
    X <- X[, idx]
    P <- ncol(X)
    afs <- afs[colnames(X)]
  }
  
  if(verbose > 0){
    msg <- paste0("estimate relationships with ", ncol(X), " SNPs ...")
    write(msg, stdout())
  }
  if(relationships == "additive"){
    if(method == "vanraden1"){
      ## implementation as in VanRaden (2008)
      ## M <- X - 1 # recode genotypes as {-1,0,1}
      ## Pmat <- matrix(rep(1, N)) %*% (2 * (afs - 0.5))
      ## Z <- M - Pmat
      
      ## implementation as in Vitezica et al (2013)
      tmp <- matrix(rep(1, N)) %*% (2 * afs)
      Z <- X - tmp
      
      gen.rel <- tcrossprod(Z, Z) / (2 * sum(afs * (1 - afs)))
    } else if(method == "toro2011_eq10"){
      var.afs <- stats::var(afs)
      gen.rel <- 2 * (stats::cov(t(X) / 2) - var.afs) /
        (mean(afs) * mean(1 - afs) - var.afs)
    } else if(method == "habier"){
      gen.rel <- tcrossprod(X, X) / (2 * sum(afs * (1 - afs)))
    } else if(method == "astle-balding"){
      tmp1 <- sweep(x=X, MARGIN=2, STATS=2*afs, FUN="-")
      tmp2 <- sweep(x=tmp1, MARGIN=2, STATS=2*afs*(1-afs), FUN="/")
      gen.rel <- (1/P) * tcrossprod(tmp1, tmp2)
    } else if(method == "yang"){
      tmp1 <- sweep(x=X, MARGIN=2, STATS=2*afs, FUN="-")
      tmp2 <- sweep(x=tmp1, MARGIN=2, STATS=2*afs*(1-afs), FUN="/")
      gen.rel <- (1/P) * tcrossprod(tmp1, tmp2)
      tmp3 <- X^2 - sweep(x=X, MARGIN=2, STATS=1+2*afs, FUN="*")
      tmp4 <- sweep(x=tmp3, MARGIN=2, STATS=2*afs^2, FUN="+")
      tmp5 <- sweep(x=tmp4, MARGIN=2, STATS=2*afs*(1-afs), FUN="/")
      diag(gen.rel) <- 1 + (1/P) * rowSums(tmp5)
    } else if(method == "zhou"){
      tmp <- scale(x=X, center=TRUE, scale=FALSE)
      gen.rel <- tcrossprod(tmp, tmp) / P
    } else if(method == "center-std"){
      tmp <- scale(x=X, center=TRUE, scale=TRUE)
      gen.rel <- tcrossprod(tmp, tmp) / P
    }
  } else if(relationships == "dominant"){
    if(method == "vitezica"){
      ## caution, compared to Vitezica et al (2013), the X matrix encodes
      ## genotypes in terms of nb of copies of the A2 allele, not A1
      boundaries <- seq(from=0, to=2, length.out=4)
      is.0 <- (X <= boundaries[2]) # homozygotes for the first allele
      is.1 <- (X > boundaries[2] & X <= boundaries[3]) # heterozygotes
      is.2 <- (X > boundaries[3]) # homozygotes for the second allele
      W <- matrix(NA, nrow=N, ncol=P, dimnames=dimnames(X))
      for(i in 1:N){
        W[i, is.0[i,]] <- - 2 * afs[is.0[i,]]^2
        W[i, is.1[i,]] <- 2 * afs[is.1[i,]] * (1 - afs[is.1[i,]])
        W[i, is.2[i,]] <- - 2 * (1 - afs[is.2[i,]])^2
      }
      gen.rel <- tcrossprod(W, W) / sum((2 * afs * (1 - afs))^2)
    } else if(method == "su"){
      H <- recodeIntoDominant(X=X)
      H <- sweep(x=H, MARGIN=2, STATS=2*afs*(1-afs), FUN="-")
      gen.rel <- tcrossprod(H, H) /
        (2 * sum(afs * (1 - afs) * (1 - 2 * afs * (1 - afs))))
    }
  } else if(relationships == "gaussian"){
    M <- X - 1 # recode genotypes as {-1,0,1}
    gen.dist <- as.matrix(stats::dist(x=M, method="euclidean")) / (2 * sqrt(P))
    gen.rel <- exp(-(gen.dist / theta)^2)
  }
  
  return(gen.rel)
}



##' Pairwise linkage disequilibrium
##'
##' Estimates linkage disequilibrium between pairs of SNPs when the observations are the genotypes of genotypes, not their gametes (i.e. the gametic phases are unknown).
##' When ignoring kinship and population structure, the estimator of Rogers and Huff (Genetics, 2009) can be used.
##' When kinship and/or population structure are controlled for, the estimator of Mangin et al (Heredity, 2012) is used via their LDcorSV package.
##' @param X matrix of bi-allelic SNP genotypes encoded in allele doses in {0,1,2}, with genotypes in rows and SNPs in columns; missing values should be encoded as NA
##' @param snp.coords data.frame with SNP identifiers as row names, and two columns, "chr" and "pos"
##' @param K matrix of "kinship" (additive genetic relationships)
##' @param pops vector of characters indicating the population of each genotype
##' @param only.chr identifier of a given chromosome
##' @param only.pop identifier of a given population
##' @param use.ldcorsv required if K and/or pops are not NULL; otherwise use the square of \code{\link{cor}}
##' @param verbose verbosity level (0/1)
##' @return data frame with at least three columns, "loc1", "loc2" and the LD values
##' @author Timothee Flutre
##' @seealso \code{\link{plotLd}}
##' @examples \dontrun{## make fake data
##' library(scrm)
##' set.seed(1859)
##' nb.genos <- 100
##' Ne <- 10^4
##' nb.chrs <- 1
##' chrom.len <- 10^5
##' mu <- 10^(-8)
##' c.rec <- 10^(-8)
##' genomes <- simulCoalescent(nb.inds=nb.genos, nb.reps=nb.chrs,
##'                            pop.mut.rate=4 * Ne * mu * chrom.len,
##'                            pop.recomb.rate=4 * Ne * c.rec * chrom.len,
##'                            chrom.len=chrom.len)
##'
##' ## checks
##' afs <- estimSnpAf(X=genomes$genos)
##' summary(afs)
##' plotHistAllelFreq(afs=afs)
##' mafs <- estimSnpMaf(afs=afs)
##' plotHistMinAllelFreq(maf=mafs)
##' plotHaplosMatrix(haplos=genomes$haplos$chr1)
##'
##' ## subset SNPs
##' min.maf <- 0.15
##' length(snps.tokeep <- rownames(genomes$snp.coords[mafs >= min.maf,]))
##'
##' ## LD estimator of Rogers and Huff
##' system.time(ld <- estimLd(X=genomes$genos[,snps.tokeep],
##'                           snp.coords=genomes$snp.coords[snps.tokeep,]))
##' dim(ld)
##' head(ld)
##' summary(ld$cor2)
##'
##' ## LD estimator of Mangin et al
##' system.time(ld2 <- estimLd(X=genomes$genos[,snps.tokeep],
##'                            snp.coords=genomes$snp.coords[snps.tokeep,],
##'                            use.ldcorsv=TRUE))
##' dim(ld2)
##' head(ld2)
##'
##' ## physical distance between SNP pairs for which LD was computed
##' dis <- distSnpPairs(snp.pairs=ld[, c("loc1","loc2")],
##'                     snp.coords=genomes$snp.coords[snps.tokeep,])
##'
##' ## plot LD
##' plotLd(x=dis, y=sqrt(ld$cor2), estim="r",
##'        main=paste0(length(snps.tokeep), " SNPs with MAF >= ", min.maf),
##'        sample.size=2*nb.genos, add.ohta.kimura=TRUE, Ne=Ne, c=c.rec)
##'
##' ## physical distance between consecutive SNPs
##' tmp <- distConsecutiveSnps(snp.coords=genomes$snp.coords)
##' hist(tmp[["chr1"]], breaks="FD", xlab="in bp",
##'      las=1, col="grey", border="white",
##'      main="Distances between consecutive SNPs")
##' }
##' @export
estimLd <- function(X, snp.coords, K=NULL, pops=NULL,
                    only.chr=NULL, only.pop=NULL,
                    use.ldcorsv=FALSE, verbose=1){
  if(use.ldcorsv & ! requireNamespace("LDcorSV", quietly=TRUE))
    stop("Pkg 'LDcorSV' needed for this function to work.",
         call.=FALSE)
  stopIfNotValidGenosDose(X)
  stopifnot(.isValidSnpCoords(snp.coords))
  if(! is.null(K))
    stopifnot(use.ldcorsv,
              is.matrix(K),
              nrow(K) == ncol(K),
              nrow(K) == nrow(X),
              ! is.null(dimnames(K)),
              all(rownames(K) == colnames(K)),
              all(rownames(K) == rownames(X)))
  W.s <- NA
  if(! is.null(pops)){
    stopifnot(use.ldcorsv,
              length(pops) == nrow(X),
              ! is.null(names(pops)),
              names(pops) == rownames(X))
    W.s <- stats::model.matrix(~ as.factor(pops))[, -1]
    rownames(W.s) <- names(pops)
  }
  if(! is.null(only.chr))
    if(! only.chr %in% snp.coords$chr)
      stop(paste0("chr '", only.chr, "' absent from snp.coords"))
  if(! is.null(only.pop))
    if(! only.pop %in% pops)
      stop(paste0("pop '", only.pop, "' absent from pops"))
  
  ld <- NULL
  
  subset.snps <- 1:ncol(X)
  subset.inds <- 1:nrow(X)
  if(! is.null(only.chr) | ! is.null(only.pop)){
    if(verbose > 0)
      write("extract relevant genotypes and SNPs...", stdout())
    if(! is.null(only.chr))
      subset.snps <- which(snp.coords$chr == only.chr)
    if(! is.null(only.pop))
      subset.inds <- which(pops == only.pop)
  }
  X <- X[subset.inds, subset.snps]
  if(! is.null(K))
    K <- K[subset.inds, subset.inds]
  
  if(verbose > 0)
    write("estimate pairwise LD...", stdout())
  if(is.null(K)){
    if(is.null(pops)){
      if(use.ldcorsv){
        ld <- LDcorSV::LD.Measures(donnees=X,
                                   V=NA,
                                   S=NA,
                                   data="G", supinfo=FALSE, na.presence=FALSE)
      } else{
        tmp <- stats::cor(X)^2
        tmp[upper.tri(tmp)] <- NA
        diag(tmp) <- NA
        ld <- data.frame(t(utils::combn(colnames(X), 2)),
                         tmp[lower.tri(tmp)],
                         stringsAsFactors=TRUE)
        colnames(ld) <- c("loc1", "loc2", "cor2")
      }
    } else{ # if(is.null(K) & ! is.null(pops))
      if(! is.null(only.pop)){
        ld <- LDcorSV::LD.Measures(donnees=X,
                                   V=NA,
                                   S=NA,
                                   data="G", supinfo=FALSE, na.presence=FALSE)
      } else{
        ld <- LDcorSV::LD.Measures(donnees=X,
                                   V=NA,
                                   S=W.s,
                                   data="G", supinfo=FALSE, na.presence=FALSE)
      }
    }
  } else{ # if(! is.null(K))
    if(is.null(pops)){
      ld <- LDcorSV::LD.Measures(donnees=X,
                                 V=K,
                                 S=NA,
                                 data="G", supinfo=FALSE, na.presence=FALSE)
    } else{ # if(! is.null(K) & ! is.null(pops))
      if(! is.null(only.pop)){
        ld <- LDcorSV::LD.Measures(donnees=X,
                                   V=K,
                                   S=NA,
                                   data="G", supinfo=FALSE, na.presence=FALSE)
      } else{
        ld <- LDcorSV::LD.Measures(donnees=X,
                                   V=K,
                                   S=W.s,
                                   data="G", supinfo=FALSE, na.presence=FALSE)
      }
    }
  }
  
  return(ld)
}






##' Pairwise linkage disequilibrium
##'
##' Plots the linkage disequilibrium between pairs of SNPs, as a blue density or black points, with a red loess.
##' Possibility to add two analytical approximations of E[r^2] at equilibrium (see McVean, Handbook of Stat Gen, 2007): 1 / (1 + 4 Ne c x) by Sved (1971) and (10 + 4 Ne c x) / (22 + 13 * 4 Ne c x + (4 Ne c x)^2) by Ohta and Kimura (1971).
##' @param x vector of distances between SNPs (see \code{\link{distSnpPairs}})
##' @param y vector of LD estimates (see \code{\link{estimLd}})
##' @param main main title
##' @param estim estimator of pairwise LD corresponding to the values in y (r2/r)
##' @param use.density if TRUE, uses smoothScatter; otherwise, use scatter.smooth
##' @param xlab label for the x axis
##' @param ylab label for the y axis
##' @param span the parameter alpha which controls the degree of smoothing (see \code{\link[stats]{loess}})
##' @param degree the degree of the polynomials to be used (see \code{\link[stats]{loess}})
##' @param evaluation number of points at which to evaluate the smooth curve (see \code{\link[stats]{loess.smooth}})
##' @param sample.size nb of sampled haplotypes, n, used to estimate the pairwise LD; if not NULL, n is used to plot the horizontal line at the r value above which the null hypothesis "D=0" is rejected, where r = D / sqrt(f_A f_a f_B f_b) and X^2 = n r^2 is the test statistic asymptotically following a Chi2(df=1) (see McVean, Handbook of Statistical Genetics, 2007, and Pritchard and Przewoski, AJHG, 2001)
##' @param add.ohta.kimura add the analytical approximation by Ohta and Kimura (1971); requires Ne and c
##' @param add.sved add the analytical approximation by Sved (1971); requires Ne and c
##' @param Ne effective population size
##' @param c recomb rate in events per base per generation
##' @param xlim numeric vector of length 2 specifying the x-axis limit (optional)
##' @return invisible list
##' @author Timothee Flutre
##' @seealso \code{\link{estimLd}}, \code{\link{distSnpPairs}}
##' @export
plotLd <- function(x, y, main="", estim="r2",
                   use.density=TRUE,
                   xlab="Physical distance (bp)",
                   ylab=paste0("Linkage disequilibrium (", estim, ")"),
                   span=1/10, degree=1, evaluation=50,
                   sample.size=NULL,
                   add.ohta.kimura=FALSE, add.sved=FALSE, Ne=NULL, c=NULL,
                   xlim){
  stopifnot(is.vector(x),
            is.vector(y),
            estim %in% c("r2","r"))
  if(! is.null(sample.size))
    stopifnot(is.numeric(sample.size))
  if(any(c(add.ohta.kimura, add.sved)))
    stopifnot(! is.null(Ne),
              ! is.null(c))
  
  out <- list()
  
  ## plot the pairwise estimates of LD
  lpars <- list(col="red", cex=2)
  if(use.density){
    graphics::smoothScatter(x, y,
                            main=main,
                            xlab=xlab,
                            ylab=ylab,
                            las=1,
                            xlim=xlim)
    pred <- stats::loess.smooth(x, y, span=span, degree=degree,
                                evaluation=evaluation)
    do.call(graphics::lines, c(list(pred), lpars))
    out$loess <- pred
  } else{
    stats::scatter.smooth(x, y, lpars=lpars,
                          main=main, xlab=xlab, ylab=ylab, las=1,
                          span=span, degree=degree, evaluation=evaluation,
                          xlim=xlim)
  }
  
  ## add the "significance" horizontal line
  ## reject H0:"D=0" at 5% if X2 = n x hat(r^2) >= Chi2(1)
  if(! is.null(sample.size)){
    X2 <- stats::qchisq(p=0.05, df=1, lower.tail=FALSE)
    tmp <- X2 / sample.size
    if(estim == "r")
      tmp <- sqrt(tmp)
    graphics::abline(h=tmp,
                     col=ifelse(use.density, "black", "blue"),
                     lty=2, lwd=2)
    out$X2 <- X2
  }
  
  ## add analytical approximations
  if(any(c(add.ohta.kimura, add.sved)))
    scaled.dist <- 4 * Ne * c * x
  if(add.ohta.kimura){
    ok <- (10 + scaled.dist) / (22 + 13 * scaled.dist + scaled.dist^2)
    if(estim == "r")
      ok <- sqrt(ok)
    graphics::points(x, ok, pch=".", col="purple", cex=1.2)
    out$ohta.kimura <- ok
  }
  if(add.sved){
    sved <- 1 / (1 + scaled.dist)
    if(estim == "r")
      sved <- sqrt(sved)
    graphics::points(x, sved, pch=".", col="green", cex=1.2)
    out$sved <- sved
  }
  
  ## add the legend
  legs <- "loess"
  cols <- "red"
  ltys <- 1
  lwds <- c(2)
  if(! is.null(sample.size)){
    legs <- c(legs,
              as.expression(bquote(paste("r | D=0, n=", .(sample.size),
                                         ", ", alpha, "=5%"))))
    cols <- c(cols, ifelse(use.density, "black", "blue"))
    ltys <- c(ltys, 2)
    lwds <- c(lwds, 2)
  }
  if(add.ohta.kimura){
    legs <- c(legs, "Ohta & Kimura (1971)")
    cols <- c(cols, "purple")
    ltys <- c(ltys, 1)
    lwds <- c(lwds, 2)
  }
  if(add.sved){
    legs <- c(legs, "Sved (1971)")
    cols <- c(cols, "green")
    ltys <- c(ltys, 1)
    lwds <- c(lwds, 2)
  }
  graphics::legend("topright", legend=legs, col=cols, lty=ltys, lwd=lwds, bty="n")
  
  invisible(out)
}



##' Distance between SNP pairs
##'
##' For each SNP pair, return the number of "blocks" (i.e. nucleotides) between both SNPs via the \code{\link[GenomicRanges]{distance}} function.
##' @param snp.pairs data.frame with two columns "loc1" and "loc2"
##' @param snp.coords data.frame with SNP identifiers as row names, and two columns, "chr" and "coord" (or "pos")
##' @param nb.cores the number of cores to use
##' @param verbose verbosity level (0/1)
##' @return vector
##' @author Timothee Flutre
##' @seealso \code{\link{estimLd}}, \code{\link{distConsecutiveSnps}}
##' @export
distSnpPairs <- function(snp.pairs, snp.coords, nb.cores=1, verbose=1){
  requireNamespaces(c("GenomicRanges", "S4Vectors", "IRanges"))
  stopifnot(is.data.frame(snp.pairs),
            ncol(snp.pairs) >= 2,
            all(c("loc1", "loc2") %in% colnames(snp.pairs)),
            .isValidSnpCoords(snp.coords))
  snp.pairs$loc1 <- as.character(snp.pairs$loc1)
  snp.pairs$loc2 <- as.character(snp.pairs$loc2)
  stopifnot(all(unique(unlist(snp.pairs[, c("loc1", "loc2")])) %in%
                  rownames(snp.coords)))
  if(! "coord" %in% colnames(snp.coords))
    colnames(snp.coords)[colnames(snp.coords) == "pos"] <- "coord"
  
  if(verbose > 0)
    message("make GRanges ...")
  snp.granges <- snpCoordsDf2Gr(snp.coords)
  
  if(verbose > 0)
    message("calculate pairwise distances ...")
  dist.loc <- GenomicRanges::distance(x=snp.granges[snp.pairs$loc1],
                                      y=snp.granges[snp.pairs$loc2])
  
  return(dist.loc)
}



##' Site frequency spectrum
##'
##' Make a data.frame of SNP coordinates (1-based) from the SFS of independent replicates.
##' @param seg.sites list of haplotypes returned by \code{scrm}, each component of which corresponds to a matrix with haplotypes in rows and SNP in columns
##' @param snp.ids vector of identifiers (one per SNP)
##' @param chrom.len chromosome length (same for all)
##' @param prefix character string
##' @param verbose verbosity level (0/1)
##' @return data.frame with SNPs in rows and 2 columns (chr, pos)
##' @author Timothee Flutre
##' @export
segSites2snpCoords <- function(seg.sites, snp.ids, chrom.len, prefix="chr",
                               verbose=1){
  stopIfNotValidHaplos(haplos=seg.sites, check.hasColNames=TRUE,
                       check.noNA=FALSE)
  
  if(verbose > 0){
    msg <- "make a data.frame with SNP coordinates ..."
    write(msg, stdout())
  }
  
  nb.chrs <- length(seg.sites)
  nb.snps.per.chr <- sapply(seg.sites, ncol)
  nb.snps <- sum(nb.snps.per.chr)
  
  ## fill up the output data.frame
  snp.coords <- data.frame(chr=rep(NA, nb.snps),
                           pos=-1,
                           row.names=snp.ids)
  snp.coords$chr <- rep(paste0(prefix, 1:nb.chrs), nb.snps.per.chr)
  snp.coords$pos <- as.numeric(do.call(c, lapply(seg.sites, colnames)))
  
  ## convert genomic positions from float to integer
  snp.coords$pos <- ceiling(snp.coords$pos)
  for(chr in unique(snp.coords$chr)){
    ## message(chr) # when debugging
    idx <- which(snp.coords$chr == chr)
    pos <- snp.coords$pos[idx]
    if(any(pos == 0))
      pos <- pos + 1
    if(max(pos) > chrom.len){
      pos[which.max(pos)] <- chrom.len
    }
    while(anyDuplicated(pos)){
      ## message("dedup") # when debugging
      i.dup <- which(duplicated(pos))
      pos[i.dup] <- pos[i.dup] + 1
    }
    stopifnot(min(pos) >= 1,
              max(pos) <= chrom.len,
              anyDuplicated(pos) == 0)
    snp.coords$pos[idx] <- pos
  }
  
  return(snp.coords)
}



##' Haplotypes
##'
##' Permute alleles in haplotypes.
##' In the \href{https://cran.r-project.org/package=scrm}{scrm} package, haplotypes are made of 0's and 1's, the former indicating ancestral alleles and the latter derived alleles.
##' However, when one wants to simulate realistic data, one often converts haplotypes into genotypes encoded as allele dose.
##' At this step, one may not want to always count the number of copies of the derived allele.
##' It hence is useful to permute alleles beforehand.
##' @param haplos list of haplotypes returned by \code{scrm}, each component of which corresponds to a matrix with haplotypes in rows and SNP in columns
##' @param snps.toperm vector of SNP identifiers corresponding to the SNPs to which allele permutation will be performed (column names of \code{haplos})
##' @param verbose verbosity level (0/1)
##' @return list of haplotypes
##' @author Timothee Flutre
##' @seealso \code{\link{simulCoalescent}}, \code{\link{segSites2allDoses}}, \code{\link{haplosAlleles2num}}
##' @export
permuteAllelesInHaplosNum <- function(haplos, snps.toperm, verbose=0){
  stopIfNotValidHaplos(haplos=haplos, check.hasColNames=FALSE, check.noNA=TRUE)
  stopifnot(is.vector(snps.toperm),
            all(snps.toperm %in% do.call(c, lapply(haplos, colnames))))
  
  if(verbose > 0){
    msg <- "permute alleles in haplotypes ..."
    write(msg, stdout())
  }
  
  out <- lapply(haplos, function(mat){
    if(any(colnames(mat) %in% snps.toperm)){
      col.idx <- which(colnames(mat) %in% snps.toperm)
      row.idx.ancestral <- which(mat[, col.idx] == 0)
      row.idx.derived <- which(mat[, col.idx] == 1)
      mat[, col.idx][row.idx.ancestral] <- 1
      mat[, col.idx][row.idx.derived] <- 0
    }
    return(mat)
  })
  
  return(out)
}


##' Crosses
##'
##' Make crosses (fecundation, autofecondation, haplodiploidization).
##' @param haplos list of matrices (one per chromosome)
##' @param crosses data.frame with three columns, parent1, parent2, child (no duplicate); if parent 1 and 2 are the same, it will be an autofecondation; if parent2 is NA, it will be a haplodiploidization
##' @param loc.crossovers list of lists (one per cross, then one per parent, then one per chromosome) whose names are crosses$child, in the same order; if NULL, draw many crossing-overs positions at once (as Poisson with parameter 2, assuming all chromosomes have the same length)
##' @param howto.start.haplo if 0, haplotypes with which to start the gametes will be chosen at random; but one can also specify 1 (always the first haplotype) or 2 (always the second haplotype)
##' @param nb.cores the number of cores to use, i.e. at most how many child processes will be run simultaneously (not on Windows)
##' @param verbose verbosity level (0/1)
##' @return list of matrices (one per chromosome) with child haplotypes in rows and SNPs in columns
##' @author Timothee Flutre
##' @seealso \code{\link{makeCross}}, \code{\link{simulCoalescent}}, \code{\link{getHaplosInds}}, \code{\link{drawLocCrossovers}}
##' @examples
##' \dontrun{set.seed(1859)
##' if(require(scrm)){
##'   ## simulate haplotypes
##'   nb.genos <- 2*10^2
##'   nb.chroms <- 10
##'   Ne <- 10^5
##'   chrom.len <- 10^5
##'   mu <- 10^(-8)
##'   c.rec <- 10^(-8)
##'   genomes <- simulCoalescent(nb.inds=nb.genos,
##'                              nb.reps=nb.chroms,
##'                              pop.mut.rate=4 * Ne * mu * chrom.len,
##'                              pop.recomb.rate=4 * Ne * c.rec * chrom.len,
##'                              chrom.len=chrom.len,
##'                              get.alleles=TRUE,
##'                              permute.alleles=TRUE)
##'
##'   ## pick 2 individuals at random as parents
##'   (idx.parents <- sample.int(n=nb.genos, size=2))
##'   genos.parents <- genomes$genos[idx.parents,]
##'   names.parents <- rownames(genos.parents)
##'   haplos.parents <- getHaplosInds(haplos=genomes$haplos,
##'                                  ind.names=names.parents)
##'
##'   ## cross them several times to make offsprings
##'   nb.offs <- 100
##'   names.offs <- paste0(names.parents[1], "-", names.parents[2], "-",
##'                        sprintf(fmt=paste0("%0", floor(log10(nb.offs))+1, "i"),
##'                                1:nb.offs))
##'   crosses <- data.frame(parent1=rep(names.parents[1], nb.offs),
##'                         parent2=rep(names.parents[2], nb.offs),
##'                         child=names.offs,
##'                         stringsAsFactors=FALSE)
##'   loc.crossovers <- drawLocCrossovers(crosses=crosses,
##'                                       nb.snps=sapply(haplos.parents, ncol))
##'   haplos.offs <- makeCrosses(haplos=haplos.parents, crosses=crosses,
##'                              loc.crossovers=loc.crossovers)
##'   genos.offs <- segSites2allDoses(seg.sites=haplos.offs,
##'                                   ind.ids=getIndNamesFromHaplos(haplos.offs),
##'                                   snp.ids=rownames(genomes$snp.coords))
##'
##'   ## look at the first crossing-over in the first offspring
##'   (snps.co <- colnames(haplos.parents$chr1)[loc.crossovers[[1]][[1]]$chr1])
##'   tmp <- subsetDiffHaplosWithinParent(haplos.chr=haplos.parents$chr1[1:2,],
##'                                       snps.tokeep=snps.co)
##'   (idx1 <- which(colnames(tmp) == snps.co[1]))
##'   (snps.tokeep <- colnames(tmp)[(idx1-2):(idx1+2)])
##'   tmp[, snps.tokeep]
##'   haplos.offs$chr1[1:2, snps.tokeep]
##' }
##' }
##' @export
makeCrosses <- function(haplos, crosses, loc.crossovers=NULL,
                        howto.start.haplo=0,
                        nb.cores=1, verbose=1){
  stopifnot(is.list(haplos),
            all(sapply(haplos, methods::is, "matrix")),
            length(unique(sapply(haplos, nrow))) == 1, # same nb of genotypes
            is.data.frame(crosses),
            ncol(crosses) >= 3,
            all(c("parent1", "parent2", "child") %in% colnames(crosses)),
            sum(is.na(crosses$parent1)) == 0,
            sum(is.na(crosses$child)) == 0,
            anyDuplicated(crosses$child) == 0)
  haplos.ind.names <- getIndNamesFromHaplos(haplos)
  idx <- sapply(crosses, is.factor)
  crosses[idx] <- lapply(crosses[idx], as.character)
  parent.names <- c(crosses$parent1, crosses$parent2)
  parent.names <- parent.names[! is.na(parent.names)]
  stopifnot(all(parent.names %in% haplos.ind.names))
  if(! is.null(loc.crossovers))
    stopifnot(is.list(loc.crossovers),
              all(names(loc.crossovers) == crosses$child))
  
  if(Sys.info()["sysname"] == "Windows")
    nb.cores <- 1
  
  nb.crosses <- nrow(crosses)
  nb.chroms <- length(haplos)
  chrom.names <- names(haplos)
  
  if(is.null(loc.crossovers)){
    if(verbose > 0){
      msg <- "draw locations of crossing-overs ..."
      write(msg, stdout())
    }
    nb.snps <- sapply(haplos, ncol) # per chromosome
    loc.crossovers <- drawLocCrossovers(crosses, nb.snps)
  }
  
  if(verbose > 0){
    msg <- "make all crosses ..."
    write(msg, stdout())
  }
  tmp <- parallel::mclapply(1:nb.crosses, function(i){
    if(is.na(crosses$parent2[i])){ # haplodiploidization
      makeCross(haplos.par1=getHaplosInd(haplos, crosses$parent1[i]),
                loc.crossovers.par1=loc.crossovers[[i]][[crosses$parent1[i]]],
                child.name=crosses$child[i],
                howto.start.haplo=howto.start.haplo,
                verbose=verbose-1)
    } else #(auto)fecondation
      makeCross(haplos.par1=getHaplosInd(haplos, crosses$parent1[i]),
                loc.crossovers.par1=loc.crossovers[[i]][[crosses$parent1[i]]],
                haplos.par2=getHaplosInd(haplos, crosses$parent2[i]),
                loc.crossovers.par2=loc.crossovers[[i]][[crosses$parent2[i]]],
                child.name=crosses$child[i],
                howto.start.haplo=howto.start.haplo,
                verbose=verbose-1)
  }, mc.cores=nb.cores)
  
  if(verbose > 0){
    msg <- "gather all children's haplotypes per chromosome ..."
    write(msg, stdout())
  }
  haplos.children <- lapply(1:nb.chroms, function(c){
    do.call(rbind, parallel::mclapply(tmp, `[[`, c, mc.cores=nb.cores))
  })
  names(haplos.children) <- chrom.names
  
  return(haplos.children)
}


##' SNP alleles
##'
##' Simulate alleles for bi-allelic SNPs.
##' @param nb.snps number of SNPs
##' @param snp.ids SNP identifiers (if \code{NULL}, will be generated by default)
##' @param colnames names of the columns for the output data frame
##' @param verbose verbosity level (0/1)
##' @return data frame with SNPs in rows
##' @author Timothee Flutre
##' @seealso \code{\link{simulCoalescent}}, \code{\link{simulGenosDose}}
##' @export
simulRefAltSnpAlleles <- function(nb.snps=NULL, snp.ids=NULL,
                                  colnames=c("ref", "alt"), verbose=1){
  stopifnot(any(! is.null(nb.snps), ! is.null(snp.ids)),
            length(colnames) == 2)
  if(! is.null(nb.snps))
    stopifnot(nb.snps > 0)
  if(! is.null(snp.ids))
    stopifnot(is.character(snp.ids))
  if(all(! is.null(nb.snps), ! is.null(snp.ids)))
    stopifnot(nb.snps == length(snp.ids))
  
  if(is.null(nb.snps))
    nb.snps <- length(snp.ids)
  if(is.null(snp.ids))
    snp.ids <- sprintf(fmt=paste0("snp%0", floor(log10(nb.snps))+1, "i"),
                       1:nb.snps)
  
  if(verbose > 0){
    msg <- paste0("simulate a data frame of ", nb.snps, " SNP alleles ...")
    write(msg, stdout())
  }
  
  tmp <- replicate(nb.snps, sample(x=c("A","T","G","C"), size=2,
                                   replace=FALSE))
  stopifnot(all(tmp[1,] != tmp[2,]))
  
  alleles <- as.data.frame(t(tmp), row.names=snp.ids,
                           stringsAsFactors=FALSE)
  colnames(alleles) <- colnames
  
  return(alleles)
}




##' Site frequency spectrum
##'
##' Convert the SFS of independent replicates into a matrix of allele doses.
##' @param seg.sites list of haplotypes returned by \code{scrm::scrm}, each component of which corresponds to a matrix with haplotypes in rows and SNP in columns
##' @param ind.ids vector with the identifiers of the genotypes
##' @param snp.ids vector with the identifiers of the SNPs (if NULL, the SNP identifiers from seg.sites will be used if they aren't NULL, too)
##' @param verbose verbosity level (0/1)
##' @return matrix with diploid genotypes in rows and SNPs in columns
##' @author Timothee Flutre
##' @export
segSites2allDoses <- function(seg.sites, ind.ids=NULL, snp.ids=NULL,
                              verbose=0){
  stopIfNotValidHaplos(haplos=seg.sites, check.hasColNames=FALSE,
                       check.noNA=TRUE)
  if(! is.null(ind.ids))
    stopifnot(is.vector(ind.ids),
              is.character(ind.ids),
              length(ind.ids) == nrow(seg.sites[[1]]) / 2)
  if(! is.null(snp.ids))
    stopifnot(is.vector(snp.ids),
              is.character(snp.ids),
              length(snp.ids) == sum(sapply(seg.sites, ncol)))
  
  if(verbose > 0){
    msg <- "convert haplotypes into genotypes encoded as allele dose ..."
    write(msg, stdout())
  }
  
  nb.inds <- nrow(seg.sites[[1]]) / 2 # nb of diploid genotypes
  nb.snps <- sum(sapply(seg.sites, ncol)) # nb of SNPs
  X <- matrix(data=NA, nrow=nb.inds, ncol=nb.snps)
  if(! is.null(ind.ids))
    rownames(X) <- ind.ids
  if(! is.null(snp.ids)){
    colnames(X) <- snp.ids
  } else{
    tmp <- do.call(c, lapply(seg.sites, colnames))
    if(all(! is.null(tmp), length(tmp) == ncol(X)))
      colnames(X) <- tmp
  }
  
  j <- 1
  for(x in seq_along(seg.sites)){ # for loop over "chromosomes"
    X[,j:(j+ncol(seg.sites[[x]])-1)] <-
      do.call(rbind, lapply(seq(1, 2*nb.inds, by=2), function(i){
        colSums(seg.sites[[x]][c(i,i+1),])
      }))
    j <- j + ncol(seg.sites[[x]])
  }
  
  return(X)
}



##' Coalescent with recombination
##'
##' Simulate haplotypes according to an approximation to the coalescent with recombination named the Sequential Coalescent with Recombination Model. Requires the scrm package (Staab et al, 2014).
##' @param nb.inds diploids (thus nb of haplotypes is 2 * nb.inds)
##' @param ind.ids vector of identifiers (one per genotype)
##' @param nb.reps number of independent loci that will be produced (could be seen as distinct chromosomes)
##' @param pop.mut.rate theta = 4 N0 mu
##' @param pop.recomb.rate rho = 4 N0 r
##' @param chrom.len in bp
##' @param other character vector of length 1 with other parameters to the simulator (e.g. time-specific parameters such as "-G 6.93 -eG 0.2 0.0 -eN 0.3 0.5")
##' @param nb.pops number of populations (\code{\link{kmeans}} will then be used to pair haplotypes into diploid genotypes)
##' @param mig.rate migration rate = 4 N0 m (will be symmetric)
##' @param get.trees get gene genealogies in the Newick format
##' @param get.tmrca get time to most recent common ancestor and local tree lengths
##' @param get.alleles get fake alleles sampled in {A,T,G,C}
##' @param permute.alleles if TRUE, the reference alleles are randomly chosen between ancestral and derived alleles
##' @param verbose verbosity level (0/1/2)
##' @return list with haplotypes (list), genotypes as allele doses (matrix) and SNP coordinates (data.frame)
##' @author Timothee Flutre
##' @seealso \code{\link{segSites2snpCoords}}, \code{\link{permuteAllelesInHaplosNum}}, \code{\link{segSites2allDoses}}, \code{\link{simulRefAltSnpAlleles}}, \code{\link{makeCrosses}}
##' @examples
##' \dontrun{## simulate haplotypes and genotypes in a single population
##' nb.genos <- 200
##' Ne <- 10^4
##' chrom.len <- 10^5
##' mu <- 10^(-8)
##' c <- 10^(-8)
##' genomes <- simulCoalescent(nb.inds=nb.genos,
##'                            pop.mut.rate=4 * Ne * mu * chrom.len,
##'                            pop.recomb.rate=4 * Ne * c * chrom.len,
##'                            chrom.len=chrom.len)
##' }
##' @export
simulCoalescent <- function(nb.inds=500,
                            ind.ids=NULL,
                            nb.reps=10,
                            pop.mut.rate=40,
                            pop.recomb.rate=40,
                            chrom.len=5*10^5,
                            other=NULL,
                            nb.pops=1,
                            mig.rate=5,
                            get.trees=FALSE,
                            get.tmrca=FALSE,
                            get.alleles=FALSE,
                            permute.alleles=TRUE,
                            verbose=1){
  requireNamespace("scrm")
  stopifnot(nb.inds > nb.pops,
            is.logical(permute.alleles))
  if(! is.null(other))
    stopifnot(is.character(other),
              length(other) == 1)
  
  out <- list()
  
  if(is.null(ind.ids))
    ind.ids <- sprintf(fmt=paste0("ind%0", floor(log10(nb.inds))+1, "i"),
                       1:nb.inds)
  
  if(verbose > 0){
    msg <- "simulate according to the SCRM ..."
    write(msg, stdout())
  }
  nb.samples <- nb.inds * 2 # e.g. 2 chr1 in ind1, 2 chr1 in ind2, etc
  cmd <- paste0(nb.samples, " ", nb.reps)
  cmd <- paste0(cmd, " -t ", pop.mut.rate)
  cmd <- paste0(cmd, " -r ", pop.recomb.rate, " ", chrom.len)
  if(nb.pops > 1){
    cmd <- paste0(cmd, " -I ", nb.pops)
    nb.inds.per.pop <- rep(0, nb.pops)
    for(p in 1:(nb.pops-1)){
      nb.inds.per.pop[p] <- floor(nb.inds / nb.pops)
      cmd <- paste0(cmd, " ", 2 * nb.inds.per.pop[p])
    }
    nb.inds.per.pop[nb.pops] <- nb.inds - sum(nb.inds.per.pop)
    cmd <- paste0(cmd, " ", 2 * nb.inds.per.pop[nb.pops])
    ## cmd <- paste0(cmd, " ", mig.rate)
    if(is.null(other)){
      cmd <- paste0(cmd, " ", mig.rate)
    } else if(! grepl("-m|-ma", other))
      cmd <- paste0(cmd, " ", mig.rate)
  }
  if(! is.null(other))
    cmd <- paste0(cmd, " ", other)
  if(get.trees)
    cmd <- paste0(cmd, " -T")
  if(get.tmrca)
    cmd <- paste0(cmd, " -L")
  cmd <- paste0(cmd, " -SC abs") # absolute seq positions in bp
  cmd <- paste0(cmd, " -oSFS") # print site freq spectrum, requires -t
  if(verbose > 0){
    msg <- paste0("scrm ", cmd)
    write(msg, stdout())
  }
  out$cmd <- cmd
  sum.stats <- scrm::scrm(cmd)
  if(verbose > 1)
    print(utils::str(sum.stats))
  
  prefix <- "chr"
  names(sum.stats$seg_sites) <- paste0(prefix, 1:nb.reps)
  nb.snps.per.chr <- sapply(sum.stats$seg_sites, ncol)
  nb.snps <- sum(nb.snps.per.chr)
  if(verbose > 0){
    msg <- paste0("nb of SNPs: ", nb.snps)
    write(msg, stdout())
    print(sapply(sum.stats$seg_sites, ncol))
  }
  
  snp.ids <- sprintf(fmt=paste0("snp%0", floor(log10(nb.snps))+1, "i"),
                     1:nb.snps)
  snp.coords <- segSites2snpCoords(seg.sites=sum.stats$seg_sites,
                                   snp.ids=snp.ids, chrom.len=chrom.len,
                                   prefix=prefix, verbose=verbose)
  out[["snp.coords"]] <- snp.coords
  
  if(verbose > 0){
    msg <- "randomize haplotypes to make diploid genotypes ..."
    write(msg, stdout())
  }
  out[["haplos"]] <- list()
  idx <- sample.int(nb.samples)
  if(nb.pops > 1){
    H <- haplosList2Matrix(sum.stats$seg_sites)
    kmH <- stats::kmeans(x=H, centers=nb.pops)
    ## table(kmH$cluster) # to debug
  }
  for(chr in 1:nb.reps){
    if(nb.pops > 1)
      idx <- do.call(c, lapply(1:nb.pops, function(pop.idx){
        sample(x=which(kmH$cluster == pop.idx),
               sum(kmH$cluster == pop.idx))
      }))
    out$haplos[[chr]] <- sum.stats$seg_sites[[chr]][idx,]
    rownames(out$haplos[[chr]]) <- paste0(rep(ind.ids, each=2), "_h", 1:2)
    colnames(out$haplos[[chr]]) <-
      snp.ids[(ifelse(chr == 1, 1, 1 + cumsum(nb.snps.per.chr)[chr-1])):
                (cumsum(nb.snps.per.chr)[chr])]
  }
  names(out$haplos) <- names(sum.stats$seg_sites)
  
  X <- segSites2allDoses(seg.sites=out$haplos, ind.ids=ind.ids,
                         snp.ids=snp.ids, verbose=verbose)
  out[["genos"]] <- X
  
  if(permute.alleles){
    mafs <- estimSnpMaf(X=out$genos)
    snps.toperm <- names(mafs)[sample.int(n=nb.snps, size=floor(0.5*nb.snps),
                                          replace=FALSE, prob=1 - mafs)]
    out$haplos <- permuteAllelesInHaplosNum(haplos=out$haplos,
                                            snps.toperm=snps.toperm,
                                            verbose=verbose)
    out$genos <- permuteAllelesInGenosDose(X=out$genos,
                                           snps.toperm=snps.toperm,
                                           verbose=verbose)
  }
  
  if(get.alleles){
    alleles <- simulRefAltSnpAlleles(snp.ids=snp.ids,
                                     colnames=c("first", "second"),
                                     verbose=verbose)
    out[["alleles"]] <- alleles
  }
  
  if(get.trees)
    out[["trees"]] <- sum.stats$trees
  if(get.tmrca)
    out[["tmrca"]] <- sum.stats$tmrca
  
  return(out)
}

##' Haplotypes
##'
##' Check that the input is a valid list of matrices of bi-allelic marker haplotypes.
##' @param haplos input list
##' @param check.hasColNames logical
##' @param check.noNA logical
##' @author Timothee Flutre
##' @export
stopIfNotValidHaplos <- function(haplos, check.hasColNames=FALSE,
                                 check.noNA=TRUE){
  stopifnot(! missing(haplos),
            ! is.null(haplos),
            is.list(haplos),
            all(sapply(haplos, is.matrix)),
            all(sapply(haplos, is.numeric)),
            length(unique(sapply(haplos, nrow))) == 1,
            ifelse(check.hasColNames,
                   ! any(sapply(haplos, function(x){is.null(colnames(x))})),
                   TRUE),
            ifelse(check.noNA,
                   ! any(sapply(haplos, function(x){any(is.na(x))})),
                   TRUE),
            all(sapply(haplos, function(x){all(x %in% c(0,1))})))
}

##' Minor allele frequencies
##'
##' Estimate minor allele frequencies of bi-allelic SNPs.
##' @param X matrix of bi-allelic SNP genotypes encoded in allele doses in [0,2], with genotypes in rows and SNPs in columns; missing values should be encoded as NA
##' @param afs vector of allele frequencies; if NULL, X should be specified, and the allele frequencies will be estimated with \code{\link{estimSnpAf}}
##' @return vector
##' @seealso \code{\link{estimSnpAf}}
##' @author Timothee Flutre
##' @export
estimSnpMaf <- function(X=NULL, afs=NULL){
  stopifnot(xor(is.null(X), is.null(afs)))
  
  if(is.null(afs))
    afs <- estimSnpAf(X)
  
  mafs <- apply(rbind(afs, 1 - afs), 2, min)
  
  return(mafs)
}

##' Allele frequencies
##'
##' Estimate allele frequencies of bi-allelic SNPs.
##' If previous sample sizes and counts per SNP are provided, it updates them.
##' @param X matrix of bi-allelic SNP genotypes encoded in allele doses in [0,2], with genotypes in rows and SNPs in columns; missing values should be encoded as NA
##' @param allow.updating if TRUE, the sample sizes and counts per SNP will be kept to allow further updating
##' @param prev.nb.genos vector of number of genotypes (sample sizes per SNP); the length and names should correspond to the columns of the X matrix
##' @param prev.nb.refalls vector of number of reference alleles (counts per SNP); the length and names should correspond to the columns of the X matrix
##' @return vector, with sample sizes and counts per SNP as attributes if updating is allowed
##' @seealso \code{\link{estimSnpMaf}}
##' @author Timothee Flutre
##' @export
estimSnpAf <- function(X, allow.updating=FALSE,
                       prev.nb.genos=NULL, prev.nb.refalls=NULL){
  stopIfNotValidGenosDose(X, check.hasColNames=FALSE,
                          check.hasRowNames=FALSE, check.noNA=FALSE)
  stopifnot(is.logical(allow.updating),
            xor(all(is.null(prev.nb.genos),
                    is.null(prev.nb.refalls)),
                all(! is.null(prev.nb.genos),
                    ! is.null(prev.nb.refalls))))
  if(! is.null(prev.nb.genos)){
    stopifnot(is.vector(prev.nb.genos),
              length(prev.nb.genos) == ncol(X),
              ! is.null(colnames(X)),
              ! is.null(names(prev.nb.genos)),
              all(names(prev.nb.genos) == colnames(X)),
              all(! is.na(prev.nb.genos)),
              all(prev.nb.genos >= 0, na.rm=TRUE))
    stopifnot(is.vector(prev.nb.refalls),
              length(prev.nb.refalls) == ncol(X),
              ! is.null(names(prev.nb.refalls)),
              all(names(prev.nb.refalls) == colnames(X)),
              all(! is.na(prev.nb.refalls)),
              all(prev.nb.refalls >= 0, na.rm=TRUE))
  }
  
  ## compute inputs allowing allele frequencies to be easily updated
  tot.nb.genos <- colSums(! is.na(X))
  tot.nb.refalls <- colSums(X, na.rm=TRUE)
  
  ## update inputs if necessary
  if(! is.null(prev.nb.genos)){
    tot.nb.genos <- tot.nb.genos + prev.nb.genos
    tot.nb.refalls <- tot.nb.refalls + prev.nb.refalls
  }
  
  ## compute allele frequencies
  afs <- tot.nb.refalls / (2 * tot.nb.genos)
  
  ## keep inputs to allow update later on
  if(allow.updating){
    attr(x=afs, which="nb.genos") <- tot.nb.genos
    attr(x=afs, which="nb.refalls") <- tot.nb.refalls
  }
  
  return(afs)
}

##' Genotypes
##'
##' Check that the input is a valid matrix of bi-allelic SNP genotypes coded in allele doses.
##' @param X input matrix
##' @param check.hasColNames logical
##' @param check.hasRowNames logical
##' @param check.noNA logical
##' @param check.isDose logical
##' @param check.notImputed logical
##' @author Timothee Flutre
##' @export
stopIfNotValidGenosDose <- function(X, check.hasColNames=TRUE,
                                    check.hasRowNames=TRUE,
                                    check.noNA=TRUE,
                                    check.isDose=TRUE,
                                    check.notImputed=FALSE){
  stopifnot(! missing(X),
            ! is.null(X),
            is.matrix(X),
            is.numeric(X),
            ifelse(check.hasColNames,
                   ! is.null(colnames(X)) & ! anyDuplicated(colnames(X)),
                   TRUE),
            ifelse(check.hasRowNames,
                   ! is.null(rownames(X)),
                   TRUE),
            ifelse(check.noNA, ! any(is.na(X)), TRUE),
            ifelse(check.isDose,
                   sum(X < 0, na.rm=TRUE) == 0,
                   TRUE),
            ifelse(check.isDose,
                   sum(X > 2, na.rm=TRUE) == 0,
                   TRUE),
            ifelse(check.notImputed,
                   sum(X == 0, na.rm=TRUE) + sum(X == 1, na.rm=TRUE) +
                     sum(X == 2, na.rm=TRUE) + sum(is.na(X)) ==
                     length(X),
                   TRUE))
}


##' Genotypes
##'
##' Permute alleles in genotypes once alleles have been permuted in the corresponding haplotypes.
##' @param X matrix of bi-allelic SNP genotypes encoded, for each SNP, in number of copies of its second allele, i.e. as allele doses in {0,1,2}, with genotypes in rows and SNPs in columns; the "second" allele is arbitrary, it can be the minor or the major allele
##' @param snps.toperm vector of SNP identifiers corresponding to the SNPs to which allele permutation will be performed (column names of \code{X})
##' @param verbose verbosity level (0/1)
##' @return matrix of genotypes
##' @author Timothee Flutre
##' @seealso \code{\link{permuteAllelesInHaplosNum}}
##' @export
permuteAllelesInGenosDose <- function(X, snps.toperm, verbose=0){
  stopIfNotValidGenosDose(X=X, check.hasColNames=TRUE, check.noNA=FALSE)
  stopifnot(is.vector(snps.toperm),
            all(snps.toperm %in% colnames(X)))
  
  if(verbose > 0){
    msg <- "permute alleles in genotypes ..."
    write(msg, stdout())
  }
  
  out <- X
  
  if(any(colnames(X) %in% snps.toperm)){
    col.idx <- which(colnames(X) %in% snps.toperm)
    row.idx.0 <- which(X[, col.idx] == 0)
    row.idx.2 <- which(X[, col.idx] == 2)
    out[, col.idx][row.idx.0] <- 2
    out[, col.idx][row.idx.2] <- 0
  }
  
  return(out)
}

##' Convert haplotypes
##'
##' Convert a list of haplotypes into a matrix.
##' @param haplos list of matrices (one per chromosome, with genotypes in rows and SNPs in columns)
##' @return matrix
##' @author Timothee Flutre
##' @export
haplosList2Matrix <- function(haplos){
  stopifnot(is.list(haplos),
            all(sapply(haplos, methods::is, "matrix")))
  
  nb.chroms <- length(haplos)
  nb.haplos <- nrow(haplos[[1]]) # 2 x nb of genotypes
  nb.snps <- sapply(haplos, ncol)
  P <- sum(nb.snps) # nb of SNPs
  
  H <- matrix(data=NA, nrow=nb.haplos, ncol=P)
  rownames(H) <- rownames(haplos[[1]])
  tmp <- lapply(haplos, colnames)
  names(tmp) <- NULL
  colnames(H) <- do.call(c, tmp)
  
  H[, 1:nb.snps[1]] <- haplos[[1]]
  if(nb.chroms > 1)
    for(chr in 2:nb.chroms)
      H[, (cumsum(nb.snps)[chr-1]+1):(cumsum(nb.snps)[chr])] <- haplos[[chr]]
  
  return(H)
}


##' Plot a scale, e.g. to add on the side of image()
##'
##' Takes some time to draw (there is one polygon per break...)
##' http://menugget.blogspot.de/2011/08/adding-scale-to-image-plot.html
##' @param z vector
##' @param zlim lim
##' @param col color
##' @param breaks vector
##' @param horiz boolean
##' @param ylim lim
##' @param xlim lim
##' @param ... arguments to be passed to plot()
##' @author Timothee Flutre
##' @export
plotWithScale <- function(z, zlim, col = grDevices::heat.colors(12),
                          breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(! missing(breaks))
    if(length(breaks) != (length(col)+1))
      stop("must have one more break than colour")
  
  if(missing(breaks) & ! missing(zlim))
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2] + c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1] - c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  
  poly <- vector(mode="list", length(col))
  for(i in seq(poly))
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){
    YLIM <- c(0,1)
    XLIM <- range(breaks)
  } else{
    YLIM <- range(breaks)
    XLIM <- c(0,1)
  }
  if(missing(xlim))
    xlim <- XLIM
  if(missing(ylim))
    ylim <- YLIM
  
  graphics::plot(1, 1, t="n", ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt,
                 xaxs="i", yaxs="i", bty="n", ...)
  
  for(i in seq(poly)){
    if(horiz){
      graphics::polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    } else
      graphics::polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
  }
}


.isValidSnpCoords <- function(snp.coords){
  all(is.data.frame(snp.coords),
      ! is.null(rownames(snp.coords)),
      ncol(snp.coords) >= 2,
      "chr" %in% colnames(snp.coords),
      "coord" %in% colnames(snp.coords) | "pos" %in% colnames(snp.coords))
}

##' Namespaces
##'
##' Stops if any package's namespace can't be loaded.
##' @param packages vector of package names
##' @return nothing
##' @author Timothee Flutre
##' @export
requireNamespaces <- function(packages){
  stopifnot(is.vector(packages),
            is.character(packages))
  for(pkg in packages)
    if(! requireNamespace(pkg, quietly=TRUE))
      stop(paste0("package '", pkg, "' needed for this function to work."),
           call.=FALSE)
}



##' SNP coordinates from data frame to GRanges
##'
##' Convert a data frame of SNP coordinates to GRanges.
##' @param x data frame of SNP coordinates with at least two columns, "chr" and "coord" (or "pos")
##' @param si output from the "seqinfo" function from the GenomeInfoDb package
##' @return GRanges
##' @author Timothee Flutre
##' @export
snpCoordsDf2Gr <- function(x, si=NULL){
  stopifnot(.isValidSnpCoords(x))
  
  if("pos" %in% colnames(x))
    colnames(x)[colnames(x) == "pos"] <- "coord"
  
  out <- df2gr(x=x, seq="chr", start="coord", end="coord", strand=NULL, si=si)
  
  return(out)
}

##' Convert data frame to GRanges
##'
##' Convert a data frame of genomic coordinates to GRanges.
##' @param x data frame of genomic coordinates
##' @param seq name of the column containing the sequence of the intervals
##' @param start name of the column containing the start of the intervals
##' @param end name of the column containing the end of the intervals
##' @param strand name of the column containing the strand of the intervals
##' @param si output from the "seqinfo" function from the GenomeInfoDb package
##' @param keep.all.seqlevels if TRUE, all sequence levels from \code{si} are kept, otherwise, only those present in \code{x}
##' @param names2use if NULL, the output will have no names; if "row", it will have the row names of the input (if any); else, it will have the content of the specified column (if it exists)
##' @return GRanges
##' @author Timothee Flutre
##' @export
df2gr <- function(x, seq="chr", start="start", end="end", strand=NULL,
                  si=NULL, keep.all.seqlevels=FALSE, names2use="row"){
  requireNamespace("GenomicRanges")
  requireNamespace("S4Vectors")
  requireNamespace("IRanges")
  requireNamespace("GenomeInfoDb")
  stopifnot(all(c(seq, start, end, strand) %in% colnames(x)),
            is.logical(keep.all.seqlevels))
  if(! is.null(si))
    stopifnot(methods::is(si, "Seqinfo"))
  
  if(is.null(strand)){
    out.gr <-
      GenomicRanges::GRanges(seqnames=S4Vectors::Rle(x[[seq]]),
                             ranges=IRanges::IRanges(start=x[[start]],
                                                     end=x[[end]]))
  } else
    out.gr <-
    GenomicRanges::GRanges(seqnames=S4Vectors::Rle(x[[seq]]),
                           ranges=IRanges::IRanges(start=x[[start]],
                                                   end=x[[end]]),
                           strand=S4Vectors::Rle(x[[strand]]))
  if(! is.null(names2use)){
    if(names2use == "row"){
      names(out.gr) <- rownames(x)
    } else if(names2use %in% colnames(x))
      names(out.gr) <- x[[names2use]]
  }
  out.gr <- GenomeInfoDb::sortSeqlevels(out.gr)
  
  if(any(! colnames(x) %in% c(seq, start, end, strand))){
    for(coln in colnames(x))
      if(! coln %in% c(seq, start, end, strand))
        S4Vectors::mcols(out.gr)[[coln]] <- x[[coln]]
  }
  
  if(! is.null(si)){
    GenomeInfoDb::seqlevels(si) <-
      GenomeInfoDb::sortSeqlevels(GenomeInfoDb::seqlevels(si))
    if(length(GenomeInfoDb::seqlevels(out.gr)) ==
       length(GenomeInfoDb::seqlevels(si))){
      stopifnot(all(GenomeInfoDb::seqlevels(out.gr) ==
                      GenomeInfoDb::seqlevels(si)))
    } else{
      stopifnot(all(GenomeInfoDb::seqlevels(out.gr) %in%
                      GenomeInfoDb::seqlevels(si)))
      if(keep.all.seqlevels){
        GenomeInfoDb::seqlevels(out.gr, pruning.mode="coarse") <-
          GenomeInfoDb::seqlevels(si)
      } else
        si <- GenomeInfoDb::keepSeqlevels(x=si,
                                          value=GenomeInfoDb::seqlevels(out.gr))
    }
    GenomeInfoDb::seqinfo(out.gr) <- si
  }
  
  return(out.gr)
}

