#### Originally from orphaned package SLmisc
#### (Version: 1.4.1, 2007-04-12, Maintainer: Matthias Kohl <kohl@sirs-lab.com>)
#### License: GPL (version 2 or later)
####
#### which said
####  "function corresponds to function gap in package SAGx"

## MM: SAGx is now in Bioconductor --- 1.10.1{devel} or 1.11.1{release}
##     had gap() *corrected* to re-cluster using FUNcluster --> see ./gap-SAGx.R.~orig~
##
## MM: Package 'lga' -- has gap() and lga and robust lga [-> UBC]
##    - it uses  boot() nicely  [2012-01: ORPHANED because  Justin Harrington is amiss]
## MM: renamed arguments, and changed almost everything

clusGap <- function (x, 
                     FUNcluster, 
                     K.max, 
                     B = 100, 
                     verbose = interactive(),
                     pca = TRUE,
                     isBoot = TRUE,
                     repBoot = 10,
                     nBoot = 500,
                     do_parallel = FALSE, ...)
{
    stopifnot(is.function(FUNcluster), length(dim(x)) == 2, K.max >= 2,
              (n <- nrow(x)) >= 1, (p <- ncol(x)) >= 1)
    if(B != (B. <- as.integer(B)) || (B <- B.) <= 0)
        stop("'B' has to be a positive integer")
    if (isBoot){
      n = min(nrow(x), nBoot)
    } else {
      n = nrow(x)
    }

    apply_fun <- lapply

    if (do_parallel)
    {
        apply_fun <- parallel::mclapply
    }

    W.k <- function(X, kk) {
        clus <- if ((kk > 1) && (nrow(X) > 5*kk)) FUNcluster(X, kk, ...)$cluster 
                else rep.int(1L, nrow(X))
        ##---------- =  =-------- kmeans() has 'cluster'; pam() 'clustering'
        ii <- seq_len(nrow(X))
	      0.5* sum(vapply(split(ii, clus),
			        function(I) { xs <- X[I,, drop=FALSE]
				        sum(dist(xs)/nrow(xs)) }, 0.))
	      }

    E.logW <- SE.sim <- logW <- logW.SE <- numeric(K.max)

    if(verbose) cat("Clustering k = 1,2,..., K.max (= ",K.max,"): .. ", sep='')
    if (!isBoot){
      if(is.data.frame(x))
        x <- as.matrix(x)
      logW <- unlist(apply_fun(1:K.max, function(k) log(W.k(x, k))))
    } else {
      cat("Bootstrapping")
      logWksList <- apply_fun(1:repBoot,
                              function(b)
                              {
                                df = sample_n(x, size = min(nrow(x), nBoot), replace = TRUE)
                                df_m = as.matrix(df)
                                curLogWks <- unlist(lapply(1:K.max, function(k) log(W.k(df_m, k))))
                                if(verbose && !do_parallel) cat(".", if(b %% 10 == 0) paste(b,"\n"))
                                curLogWks
                              })
      logWks <- matrix(unlist(logWksList),
                       byrow = TRUE,
                       nrow = repBoot)
      logW <- colMeans(logWks)
      logW.SE <- sqrt((1 + 1/repBoot) * apply(logWks, 2, var))
    }
    
    if(verbose) cat("done\n")
    if(is.data.frame(x))
      x <- as.matrix(x)
    ## Scale 'x' into "hypercube" -- we later fill with H0-generated data
    xs <- scale(x, center=TRUE, scale=FALSE)
    m.x <- rep(attr(xs,"scaled:center"), each = n)# for back transforming
    if (pca){
      V.sx <- svd(xs)$v
      rng.x1 <- apply(xs %*% V.sx, # = transformed(x)
                      2, range)
    } else {
      rng.x1 <- apply(xs, # = transformed(x)
                      2, range)
    }

    if(verbose) cat("Bootstrapping, b = 1,2,..., B (= ", B,
                    ")  [one \".\" per sample]:\n", sep="")
    nstart = 1;#WARN: disable using multiple start to fasten the process
    logWksList <- apply_fun(1:B,
        function(b)
        {
            ## Generate "H0"-data as "parametric bootstrap sample" :
            z1 <- apply(rng.x1, 2,
                function(M, nn) runif(nn, min=M[1], max=M[2]),
                nn=n)
            if (pca){
              z <- tcrossprod(z1, V.sx) + m.x # back transformed
            } else {
              z <- z1 + m.x # back transformed
            }
            
            curLogWks <- unlist(lapply(1:K.max, function(k) log(W.k(z, k))))
            if(verbose && !do_parallel) cat(".", if(b %% 50 == 0) paste(b,"\n"))

            curLogWks
        })

    logWks <- matrix(unlist(logWksList),
        byrow = TRUE,
        nrow = B)

    if(verbose && (B %% 50 != 0)) cat("",B,"\n")
    print(logWks)
    E.logW <- colMeans(logWks)
    SE.sim <- logW.SE + sqrt((1 + 1/B) * apply(logWks, 2, var))
    structure(class = "clusGap",
              list(Tab = cbind(logW, E.logW, gap = E.logW - logW, SE.sim),
                   ## K.max == nrow(T)
                   n = n, B = B, FUNcluster=FUNcluster))
}

## lga/R/gap.R   --- has for Tibshirani et al (2001):
        ## ElogWks[k,] <- c(mean(BootOutput), sqrt(var(BootOutput)*(1+1/B)))
        ## GAP[k] <- ElogWks[k,1] - logWks[k]
        ## if (k > 1)
        ##     if(GAP[k-1] >= GAP[k]-ElogWks[k,2] & !doall)
        ##         finished <- TRUE
##  so they effectively only look for the *first* (local) maximum which ..
## MM: <==> diff(GAP) = GAP[k] - GAP[k-1] <= +SE.sim[k]


## criteria.DandF() -- Dudoit and Fridlyand (2002)
## ---------------- looks at the *global* maximum and then to the left..
    ## y <- x$data
    ## crit <- diff(y[which.max(y[,"Gap"]), c("Sks", "Gap")])
    ## nclust <- min(which(y[,"Gap"] > crit))
    ## return(ifelse(nclust == nrow(y), NA, nclust))

maxSE <- function(f, SE.f,
		  method = c("firstSEmax", "Tibs2001SEmax",
		  "globalSEmax", "firstmax", "globalmax", "accMax", "accMaxSE", "firstAccMaxSE"),
		  SE.factor = 1)
{
    method <- match.arg(method)
    stopifnot((K <- length(f)) >= 1, K == length(SE.f), SE.f >= 0, SE.factor >= 0)
    fSE <- SE.factor * SE.f
    switch(method,
	   "firstmax" = { ## the first local maximum  (== firstSEmax with SE.factor == 0)
	       decr <- (dg <- diff(f)) <= 0 # length K-1
	       if(any(decr)) which.max(decr) else K # the first TRUE, or K
	   },
	   "globalmax" = {
	       which.max(f)
	   },
	   "Tibs2001SEmax" = { ## The one Tibshirani et al (2001) proposed:
	       ## "the smallest k such that f(k) >= f(k+1) - s_{k+1}"
	       g.s <- f - 1*SE.f ## Always set factor to 1 in Tibs2001
	       if(any(mp <- f[-K] >= g.s[-1])) which.max(mp) else K
	   },
	   "firstSEmax" = { ## M.Maechler(2012): rather ..
	       ## look at the first *local* maximum and then to the left ..:
	       #Always set factor to 1 in firstSEmax
	       decr <- (dg <- diff(f)) <= 0 # length K-1
	       nc <- if(any(decr)) which.max(decr) else K # the first TRUE, or K
	       if(any(mp <- f[seq_len(nc - 1)] >= f[nc] - SE.f[nc]))
		   which(mp)[1]
	       else nc
	   },
	   "globalSEmax" = { ## Dudoit and Fridlyand (2002) *thought* Tibshirani proposed..
	       ## in 'lga', see criteria.DandF():
	       ## looks at the *global* maximum and then to the left..
	       nc <- which.max(f)
	       if(any(mp <- f[seq_len(nc - 1)] >= f[nc] - fSE[nc]))
		   which(mp)[1]
	       else nc
	   },
	   "accMax" = {
	     1+which.min(diff(diff(f)))
	   },
	   "accMaxSE" = {
	     accSE = diff(diff(f)) + head(fSE, -2) + tail(fSE, -2) + head(tail(fSE, -1), -1)
	     if (min(accSE)>=0){
	       1
	     } else {
	       1+which.min(accSE)
	     }
	   },
	   "firstAccMaxSE" = {
	     accSE = diff(diff(f)) + head(fSE, -2) + tail(fSE, -2) + head(tail(fSE, -1), -1)
	     vSE = diff(f) + head(fSE, -1) #+ tail(fSE, 1)#TODO: this is experiment on S1 dataset, remove head(fSE, -1) if not working on other cases
	     print("accSE: ")
	     print(accSE)
	     idx = 0
	     minAccSE = 0
	     for (i in seq(length(accSE))){
  	       if (vSE[i]<0){
  	         break
  	       } else {
  	         if (accSE[i] < minAccSE){
  	           minAccSE = accSE[i]
  	           idx = i
  	         }
  	       }
  	     }
	     idx + 1
	     }
	   )
}

print.clusGap <- function(x, method="firstSEmax", SE.factor = 1, ...)
{
    method <- match.arg(method, choices = eval(formals(maxSE)$method))
    stopifnot((K <- nrow(T <- x$Tab)) >= 1, SE.factor >= 0)
    cat("Clustering Gap statistic [\"clusGap\"].\n",
        sprintf("B=%d simulated reference sets, k = 1..%d\n",x$B, K), sep="")
    nc <- maxSE(f = T[,"gap"], SE.f = T[,"SE.sim"],
                method=method, SE.factor=SE.factor)
    cat(sprintf(" --> Number of clusters (method '%s'%s): %d\n",
		method, if(grepl("SE", method))
		sprintf(", SE.factor=%g",SE.factor) else "", nc))
    print(T, ...)
    invisible(x)
}

plot.clusGap <- function(x, type="b", xlab = "k", ylab = expression(Gap[k]),
                         do.arrows=TRUE,
                         arrowArgs = list(col="red3", length=1/16, angle=90, code=3),
                         ...)
{
    stopifnot(is.matrix(Tab <- x$Tab), is.numeric(Tab))
    K <- nrow(Tab)
    k <- seq_len(K) # == 1,2,... k
    gap <- Tab[, "gap"]
    plot(k, gap, type=type, xlab=xlab, ylab=ylab, ...)
    if(do.arrows)
	do.call(arrows,
		c(list(k, gap+ Tab[, "SE.sim"], k, gap- Tab[, "SE.sim"]), arrowArgs))
    invisible()
}
