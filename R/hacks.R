##' @title nmk.modified
##' @description dfoptim::nmk which outputs the parameter vector of every optimization step
##'
##' @details See \code{\link{dfoptim::nmk}}. This version incorporates a modified step for avoiding the forbidden region. Its activated by setting MODIFIED to TRUE. Since its always about the likelihood function its x argument also will be provided explicitly (much nicer for debugging; modified regions indicated by MOD)
##'
##' @param par Initial parameter set. Default = call of the \code{\link{likelihood.initials}} function with the time.series as argument
##' @param fn Function which is about to optimize. Default = \code{\link{likelihood}}
##' @param x Time series of class 'xts' or 'numeric'.
##' @param MODIFIED Flag specifying if the modified or the original algorithm should be used. Default = TRUE.
##' @param control List of options.
##' @param ... Additional input for the function 'fn'. In case of the likelihood function the time series 'x' must be provided
##'
##' @family hacks
##'
##' @return List of elements see original function. In addition a new element called x.updates is provided containing the parameters at each optimization step. Of class c( "list", "climex.gev.fit" )
##' @author Philipp Mueller
##' @examples
##' nmk.modified( x = block( anomalies( temp.potsdam ) ) )
nmk.modified <- function ( par = likelihood.initials( x ), fn = likelihood, x, MODIFIED = TRUE, control = list(), ...) 
{
    ## Initialization
    ctrl <- list( tol = 1e-06, maxfeval = min( 5000, max( 1500, 20 * length( par )^2 ) ),
                 regsimp = TRUE, maximize = FALSE, restarts.max = 3, trace = FALSE )
    namc <- match.arg( names( control ), choices = names( ctrl ), several.ok = TRUE )
    if ( !all( namc %in% names( ctrl ) ) ) 
        stop( "unknown names in control: ", namc[ !( namc %in% names( ctrl ) ) ] )
    if (!is.null( names( control ) ) ) 
        ctrl[ namc ] <- control
    ftol <- ctrl$tol
    maxfeval <- ctrl$maxfeval
    regsimp <- ctrl$regsimp
    restarts.max <- ctrl$restarts.max
    maximize <- ctrl$maximize
    trace <- ctrl$trace
    ## MOD
    if ( maximize ) {
        fnm <- function( par, x ) -fn( par, x )
    } else
        fnm <- function( par, x ) fn( par, x )
    ## /MOD

    ## Starting values
    x0 <- as.numeric( par ) # initial parameters
    n <- length( par )
    if ( n == 1 ) 
        stop( call. = FALSE, "Use `optimize' for univariate optimization" )
    if ( n > 30 ) 
        warning( "Nelder-Mead should not be used for high-dimensional optimization" )
    V <- cbind( rep( 0, n ), diag( n ) ) # matrix containing the n+1 vertices of the simplex
    f <- rep( 0, n + 1 ) # contains function evaluations
    ## MOD
    f[ 1 ] <- fnm( x0, x )
    if ( is.nan( f[ 1 ] ) )
        warning( "The supplied initial parameter set to nmk.modified can not be evaluated!" )
    nf <- 0
    restarts <- 0
    ## /MOD
    V[ , 1 ] <- x0
    scale <- max( 1, sqrt( sum( x0^2 ) ) )
    ## to obtain the other three initial points for the simplex the provided initial coordinate is shifted by a constant value alpha[ 2 ] and then moved by alpha[ 1 ] in one of the three dimensions (location, scale and shape ) respectively. But this happens to be completely independent of the forbidden region. So maybe one can think of something better for the GEV case.
    if ( regsimp ) {
        ## So here again. the alphas have to be chosen in such a way that there is no evaluation in the forbidden region
        alpha <- scale/( n * sqrt( 2 ) ) * c( sqrt( n + 1 ) + n - 1, sqrt( n + 1 ) - 1 )
        if ( MODIFIED ){
            if ( is.nan( fnm( x0 + alpha[ 2 ], x ) ) ){
                ## The shifted point is in the forbidden region. Since I see no motivation in the literature to have any specific initialization of the simplex I will just figure out another point.
                alpha.2.vec <- seq( -alpha[ 2 ]* .5, alpha[ 2 ]* .5, , 200 )
                alpha.2.v <- as.numeric( lapply( alpha.2.vec, function( y ) fnm( x0 + y, x ) ) )
                if ( all( is.nan( alpha.2.v ) ) ){
                    warning( "In the alpha[2] range in nmk.modified no point can be evaluated. Initialization of the simplex failed." )
                    return ( list( par = c( NaN, NaN, NaN ), value = NaN, feval = 0,
                                  restarts = 0, convergence = conv, message = message,
                                  x.updates = NULL ) )
                }
                alpha.2.v[ is.nan( alpha.2.v ) ] <- Inf
                ## We don't want to be at the very bottom of the parabola in the likelihood with respect to alpha.2.vec but on the lower regions.
                alpha.2.min <- alpha.2.vec[ which.min( abs( alpha.2.v - min( alpha.2.v )* 1.25 ) ) ]
                V[ , - 1 ] <- ( x0 + alpha.2.min )
            } else {
                alpha.2.min <- alpha[ 2 ]
                V[ , -1 ] <- ( x0 + alpha.2.min )
            }
            ## spanning the orthogonal directions.
            diag( V[ , - 1 ] ) <- x0[ 1 : n ] + alpha[ 1 ]
            for ( jj in 2 : ncol( V ) )
                f[ jj ] <- fnm( V[ ,jj ], x )
            if ( any( is.nan( f ) ) ){
                ## At least one point in the initial simplex is in the forbidden region.
                ## In general I don't see any problem in the spanned points having different distances alpha[ 1 ] from the shifted point x0 + alpha[ 2 ]. Sometimes only very close ones can be evaluated. Therefore the distance will start at 0 and the most far away value with an acceptable likelihood will be chosen.
                alpha.1.vec <- seq( 0, alpha[ 1 ]* .5, , 120 )
                for ( cc in 1 : ( ncol( V ) - 1 ) ){
                    alpha.1.v <- as.numeric( lapply( alpha.1.vec, function( y ){
                        x0[ cc ] <- x0[ cc ] + y
                        fnm( x0 + alpha.2.min, x ) } ) )
                    alpha.1.v[ is.nan( alpha.1.v ) ] <- Inf
                    ## Get the most far away value which is lowest
                    alpha.1.min <- length( alpha.1.v ) + 1 -
                        which.min( rev( abs( alpha.1.v - min( alpha.1.v )* 1.25 ) ) )
                    if ( is.nan( alpha.1.v[ alpha.1.min ] ) )
                        alpha.1.min <- which.min( alpha.1.v )
                    V[ cc, cc + 1 ] <- x0[ cc ] + alpha.2.min + alpha.1.vec[ alpha.1.min ]
                }
            for ( jj in 2 : ncol( V ) )
                f[ jj ] <- fnm( V[ ,jj ], x )
            }
        } else {
            V[ , -1 ] <- ( x0 + alpha[ 2 ] )
            diag( V[ , -1 ] ) <- x0[ 1 : n ] + alpha[ 1 ]
            ## MOD
            for ( jj in 2 : ncol( V ) )
                f[ jj ] <- fnm( V[ , jj ], x )
            ## /MOD
        }
    } else {
        V[ , -1 ] <- x0 + scale * V[ , -1 ]
        ## MOD
        for ( jj in 2 : ncol( V ) )
            f[ jj ] <- fnm( V[ , jj ], x )
        ## /MOD
    }
    f[ is.nan( f ) ] <- Inf
    nf <- n + 1 # number of function evaluation
    ord <- order( f ) # order the sample to replace always the last entry
    f <- f[ ord ]
    V <- V[ , ord ]

    ## Setting up the Nelder-Mead optimization
    rho <- 1 # weight for the reflection step
    gamma <- 0.5 # rho*gamma is the weight for the outer contraction step
    chi <- 2 # rho*chi is the weight for the expansion step
    sigma <- 0.5
    conv <- 1
    oshrink <- 0
    restarts <- 0
    orth <- 0
    dist <- f[ n + 1 ] - f[ 1 ]
    v <- V[ , -1 ] - V[ , 1 ] # matrix containing the n simplex directions
    delf <- f[ -1 ] - f[ 1 ] 
    diam <- sqrt( colSums( v^2 ) )
    sgrad <- c( crossprod( t( v ), delf ) ) # simplex gradient
    alpha <- 1e-04 * max( diam )/ sqrt( sum( sgrad^2 ) )
    simplex.size <- sum( abs( v ) )/ max( 1, sum( abs( V[ ,1 ] ) ) )
    itc <- 0
    conv <- 0
    ## MOD (but even beforehand)
    x.new.vector <- data.frame( location = x0[ 1 ], scale = x0[ 2 ], shape = x0[ 3 ], step = 1 )
    xnew <- NULL # to check later on if it was already modified
    ## /MOD
    message <- "Run into error"
    while ( nf < maxfeval & restarts < restarts.max & dist > ftol & simplex.size > 1e-06 ){
        ## Staring the optimization 
        fbc <- mean( f )
        happy <- 0
        itc <- itc + 1
        xbar <- rowMeans( V[ , 1 : n ] ) # centroid of the convex simplex hull
        
        ## Reflection step
        xr <- ( 1 + rho )* xbar - rho* V[ , n + 1 ]
        ## MOD
        fr <- fnm( xr, x )
        ## There is a problem right here. If this evaluation returns NaN the algorithm doesn't know how to handle it. All the following if statements are false and if fr is NaN for the initial value 'xnew' will not be defined and the algorithm throws an error. Therefore a bunch of different rhos will be evaluated
        if ( is.nan( fr ) && MODIFIED ){
            rho.sequence <- seq( .5, 1.5, .1 )
            x.sequence <- lapply( rho.sequence, function( y ) {
                ( 1 + y )* xbar - y* V[ , n + 1 ] } )
            rho.results <- lapply( x.sequence, function( y ) fnm( y, x ) )
            if ( all( is.nan( as.numeric( rho.results ) ) ) ){
                warning( "no allowed points in the reflection step in nmk.modified" )
                ## Well maybe the other methods are of more luck
                xe <- ( 1 + rho* chi )* xbar - rho* chi* V[ , n + 1 ]
                xco <- ( 1 + rho* gamma )* xbar - rho* gamma* V [ , n + 1 ]
                xci <- ( 1 - gamma )* xbar + gamma* V[ , n + 1 ]
                other.results <- as.numeric( lapply( list( xe, xco, xci ),
                                                     function( y ) fnm( y, x ) ) )
                if ( all( is.nan( other.results ) ) && is.null( xnew ) ){
                    warning( "All other steps couldn't produce a valid step either. The provided starting point!" )
                    ## So especially for the parameter region of the shape where the likelihood isn't even defined it makes no sense to force the algorithm to work. Sometimes one has to let things go.
                    fr <- xr <- NaN
                } else {
                    other.results[ is.nan( other.results ) ] <- Inf
                    fr <- other.results[ which.min( other.results ) ] # This is kinda stupid but shouldn't break anything
                    xr <- rbind( xe, xco, xci )[ which.min( other.results ), ]
                }
            } else {
                rho.results <- as.numeric( rho.results )
                rho.results[ is.nan( rho.results ) ] <- Inf
                fr <- rho.results[ which.min( rho.results ) ]
                xr <- x.sequence[[ which.min( rho.results ) ]]
            } }
        ## /MOD
        nf <- nf + 1
        if ( is.nan( fr ) && is.null( xnew ) ){
            ## Is only used when xnew is not defined yet
            return ( list( par = c( NaN, NaN, NaN ), value = NaN, feval = nf,
                          restarts = restarts, convergence = conv, message = message,
                          x.updates = x.new.vector ) )
        } else if ( is.nan( fr ) )
            fr <- Inf
        if ( fr >= f[ 1 ] & fr < f[ n ] ){
            ## Reflection is successful and the update is accepted
            happy <- 1
            xnew <- xr
            fnew <- fr
            ## /Reflection step
        } else if ( fr < f[ 1 ] ){
            
            ## Expansion step
            xe <- ( 1 + rho* chi )* xbar - rho* chi* V[ , n + 1 ]
            ## MOD
            fe <- fnm( xe, x )
            ## /MOD
            if ( is.nan( fe ) ) 
                fe <- Inf
            nf <- nf + 1
            if ( fe < fr ){
                ## Expansion step was successful
                xnew <- xe
                fnew <- fe
                happy <- 1
                ## /Expansion step
            } else {
                xnew <- xr
                fnew <- fr
                happy <- 1
            }
        } else if ( fr >= f[ n ] & fr < f[ n + 1 ] ) {
            
            ## Outer contraction step
            xc <- ( 1 + rho* gamma )* xbar - rho* gamma* V [ , n + 1 ]
            ## MOD
            fc <- fnm( xc, x )
            ## /MOD
            if ( is.nan( fc ) ) 
                fc <- Inf
            nf <- nf + 1
            if ( fc <= fr ) {
                xnew <- xc
                fnew <- fc
                happy <- 1
                ## /Outer contraction step
            }
        } else if ( fr >= f[ n + 1 ] ) {

            ## Inner contraction step
            xc <- ( 1 - gamma )* xbar + gamma* V[ , n + 1 ]
            ## MOD
            fc <- fnm( xc, x )
            ## /MOD
            if ( is.nan( fc ) ) 
                fc <- Inf
            nf <- nf + 1
            if ( fc < f[ n + 1 ] ){
                xnew <- xc
                fnew <- fc
                happy <- 1
                ## /Inner contraction step
            } }
        if ( happy == 1 & oshrink == 1 ) {
            ## Some esoteric stuff. oshrink is set to zero and never touched again
            fbt <- mean(c(f[1:n], fnew))
            delfb <- fbt - fbc
            armtst <- alpha * sum(sgrad^2)
            if (delfb > -armtst/n) {
                if (trace) 
                    cat("Trouble - restarting: \n")
                restarts <- restarts + 1
                orth <- 1
                diams <- min(diam)
                sx <- sign(0.5 * sign(sgrad))
                happy <- 0
                V[, -1] <- V[, 1]
                diag(V[, -1]) <- diag(V[, -1]) - diams * sx[1:n]
            }
        }

        ## Re-initializing of the optimization 
        if ( happy == 1 ) {
            ## Overwriting the worst point with the newly found one
            V[ , n + 1 ] <- xnew
            f[ n + 1 ] <- fnew
            ord <- order( f )
            V <- V[ , ord ]
            f <- f[ ord ]
        } else if ( happy == 0 & restarts < restarts.max ) {
            ## Hmm. But since oshrink is strictly zero there is no way in updating 'restart'
            if ( orth == 0 )
                ## This variable has no influence at all. Quite some bugs in here.
                orth <- 1
            V[ , -1 ] <- V[ , 1 ] - sigma * ( V[ , -1 ] - V[ , 1 ] )
            ## MOD
            for ( jj in 2 : ncol( V ) )
                f[ jj ] <- fnm( V[ , jj ], x )
            ## /MOD
            nf <- nf + n
            ord <- order( f )
            V <- V[ , ord ]
            f <- f[ ord ] }
        v <- V[ , -1 ] - V[ , 1 ]
        delf <- f[ - 1] - f[ 1 ]
        diam <- sqrt( colSums( v^2 ) )
        simplex.size <- sum( abs( v ) )/ max( 1, sum( abs( V[ , 1 ] ) ) )
        f[ is.nan( f ) ] <- Inf
        dist <- f[ n + 1 ] - f[ 1 ]
        sgrad <- c( crossprod( t( v ), delf ) )
        ## MOD
        x.new.vector[ itc, ] <- c( xnew, itc )
        ## /MOD
        if ( trace & !( itc%%2 ) ) 
            cat( "iter: ", itc, "\n", "value: ", f[ 1 ], "\n")
    }
    if ( dist <= ftol | simplex.size <= 1e-06 ){
        conv <- 0
        message <- "Successful convergence"
    }
    else if ( nf >= maxfeval ) {
        conv <- 1
        message <- "Maximum number of fevals exceeded"
    }
    else if ( restarts >= restarts.max ) {
        conv <- 2
        message <- "Stagnation in Nelder-Mead"
    }

    res <- list( par = V[ , 1 ], value = f[ 1 ]* ( -1 )^maximize, feval = nf, 
                 restarts = restarts, convergence = conv, message = message,
                x.updates = x.new.vector )
    class( res ) <- c( "list", "climex.gev.fit" )
    return( res )        
}

##' @title rcgmin
##' @description R version of conjugated gradient algorithm
##'
##' @details Modified for my own needs (e.g. x as an input)
##'
##' @param par Initial parameters.
##' @param fn Function to be optimized (neglog likelihood of the GEV).
##' @param gr Gradient of the function to be optimized.
##' @param control Additional settings for the CG algorithms
##' @param ... Additional arguments for fn
##'
##' @return List. Containing e.g. the fitted parameters in the element 'par'.
##' @author Philipp Mueller 
rcgmin <- function ( par = likelihood.initials( x ), fn = likelihood,
                        gr = likelihood.gradient, x, control = list(), ... ) 
{
    ctrl <- list(maxit = 500, maximize = FALSE, trace = 0, eps = 1e-07, 
        dowarn = TRUE, tol = 0)
    namc <- names(control)
    if (!all(namc %in% names(ctrl))) 
        stop("unknown names in control: ", namc[!(namc %in% names(ctrl))])
    ctrl[namc] <- control
    npar <- length(par)
    if (ctrl$tol == 0) 
        tol <- npar * (npar * .Machine$double.eps)
    else tol <- ctrl$tol
    maxit <- ctrl$maxit
    maximize <- ctrl$maximize
    trace <- ctrl$trace
    if (trace > 2) 
        cat("trace = ", trace, "\n")
    eps <- ctrl$eps
    fargs <- list(...)
    grNULL <- is.null(gr)
    dowarn <- ctrl$dowarn
    if (maximize) {
        warning("Rcgmin no longer supports maximize 111121 -- see documentation")
        msg <- "Rcgmin no longer supports maximize 111121"
        ans <- list(par, NA, c(0, 0), 9999, msg)
        return(ans)
    }
    if (grNULL) {
        if (control$dowarn) 
            warning("A NULL gradient function is being replaced by numDeriv 'grad()'for Rcgmin")
        if (ctrl$trace > 1) {
            cat("Using following function in numDeriv grad()\n")
            print(fn)
        }
        mygr <- function(prm, func = fn, ...) {
            gv <- grad(func = func, x0 = prm, ...)
        }
    }
    else {
        mygr <- gr
    }
    if (trace > 0) {
        cat("Rcgminu -- J C Nash 2009 - unconstrained version CG min\n")
        cat("an R implementation of Alg 22 with Yuan/Dai modification\n")
    }
    bvec <- par
    n <- length(bvec)
    maxfeval <- round(sqrt(n + 1) * maxit)
    ig <- 0
    ifn <- 1
    stepredn <- 0.15
    acctol <- 1e-04
    reltest <- 100
    accpoint <- as.logical(FALSE)
    cyclimit <- min(2.5 * n, 10 + sqrt(n))
    fargs <- list(...)
    if (trace > 2) {
        cat("Extra function arguments:")
        print(fargs)
    }
    if (trace > 2) {
        cat("Try function at initial point:")
        print(bvec)
    }
    f <- try(fn(bvec, ...), silent = TRUE)
    if (trace > 0) {
        cat("Initial function value=", f, "\n")
    }
    if (class(f) == "try-error") {
        msg <- "Initial point is infeasible."
        if (trace > 0) 
            cat(msg, "\n")
        ans <- list(par, NA, c(ifn, 0), 2, msg)
        names(ans) <- c("par", "value", "counts", "convergence", 
            "message")
        return(ans)
    }
    fmin <- f
    if (trace > 0) 
        cat("Initial fn=", f, "\n")
    if (trace > 2) 
        print(bvec)
    keepgoing <- TRUE
    msg <- "not finished"
    oldstep <- 0.8
    fdiff <- NA
    cycle <- 0
    while (keepgoing) {
        t <- as.vector(rep(0, n))
        c <- t
        while (keepgoing && (cycle < cyclimit)) {
            cycle <- cycle + 1
            if (trace > 0) 
                cat(ifn, " ", ig, " ", cycle, " ", fmin, "  last decrease=", 
                  fdiff, "\n")
            if (trace > 2) {
                print(bvec)
                cat("\n")
            }
            if (ifn > maxfeval) {
                msg <- paste("Too many function evaluations (> ", 
                  maxfeval, ") ", sep = "")
                if (trace > 0) 
                  cat(msg, "\n")
                ans <- list(par, fmin, c(ifn, ig), 1, msg)
                names(ans) <- c("par", "value", "counts", "convergence", 
                  "message")
                return(ans)
            }
            par <- bvec
            ig <- ig + 1
            if (ig > maxit) {
                msg <- paste("Too many gradient evaluations (> ", 
                  maxit, ") ", sep = "")
                if (trace > 0) 
                  cat(msg, "\n")
                ans <- list(par, fmin, c(ifn, ig), 1, msg)
                names(ans) <- c("par", "value", "counts", "convergence", 
                  "message")
                return(ans)
            }
            g <- mygr(bvec, ...)
            g1 <- sum(g * (g - c))
            g2 <- sum(t * (g - c))
            gradsqr <- sum(g * g)
            if (trace > 1) {
                cat("Gradsqr = ", gradsqr, " g1, g2 ", g1, " ", 
                  g2, " fmin=", fmin, "\n")
            }
            c <- g
            g3 <- 1
            if (gradsqr > tol * (abs(fmin) + reltest)) {
                if (g2 > 0) {
                  betaDY <- gradsqr/g2
                  betaHS <- g1/g2
                  g3 <- max(0, min(betaHS, betaDY))
                }
            }
            else {
                msg <- paste("Very small gradient -- gradsqr =", 
                  gradsqr, sep = " ")
                if (trace > 0) 
                  cat(msg, "\n")
                keepgoing <- FALSE
                break
            }
            if (trace > 2) 
                cat("Betak = g3 = ", g3, "\n")
            if (g3 == 0 || cycle >= cyclimit) {
                if (trace > 0) {
                  if (cycle < cyclimit) 
                    cat("Yuan/Dai cycle reset\n")
                  else cat("Cycle limit reached -- reset\n")
                }
                fdiff <- NA
                cycle <- 0
                break
            }
            else {
                t <- t * g3 - g
                gradproj <- sum(t * g)
                if (trace > 1) 
                  cat("Gradproj =", gradproj, "\n")
                OKpoint <- FALSE
                if (trace > 2) 
                  cat("Start linesearch with oldstep=", oldstep, 
                    "\n")
                steplength <- oldstep * 1.5
                f <- fmin
                changed <- TRUE
                while ((f >= fmin) && changed) {
                  bvec <- par + steplength * t
                  changed <- (!identical((bvec + reltest), (par + 
                    reltest)))
                  if (changed) {
                    f <- fn(bvec, ...)
                    ifn <- ifn + 1
                    if (is.na(f) || (!is.finite(f))) {
                      warning("Rcgmin - undefined function")
                      f <- .Machine$double.xmax
                    }
                    if (f < fmin) {
                      f1 <- f
                    }
                    else {
                      savestep <- steplength
                      steplength <- steplength * stepredn
                      if (steplength >= savestep) 
                        changed <- FALSE
                      if (trace > 0) 
                        cat("*")
                    }
                  }
                }
                changed1 <- changed
                if (changed1) {
                  newstep <- 2 * (f - fmin - gradproj * steplength)
                  if (newstep > 0) {
                    newstep = -(gradproj * steplength * steplength/newstep)
                  }
                  bvec <- par + newstep * t
                  changed <- (!identical((bvec + reltest), (par + 
                    reltest)))
                  if (changed) {
                    f <- fn(bvec, ...)
                    ifn <- ifn + 1
                  }
                  if (trace > 2) 
                    cat("fmin, f1, f: ", fmin, f1, f, "\n")
                  if (f < min(fmin, f1)) {
                    OKpoint <- TRUE
                    accpoint <- (f <= fmin + gradproj * newstep * 
                      acctol)
                    fdiff <- (fmin - f)
                    fmin <- f
                    oldstep <- newstep
                  }
                  else {
                    if (f1 < fmin) {
                      bvec <- par + steplength * t
                      accpoint <- (f1 <= fmin + gradproj * steplength * 
                        acctol)
                      OKpoint <- TRUE
                      fdiff <- (fmin - f1)
                      fmin <- f1
                      oldstep <- steplength
                    }
                    else {
                      fdiff <- NA
                      accpoint <- FALSE
                    }
                  }
                  if (trace > 1) 
                    cat("accpoint = ", accpoint, " OKpoint = ", 
                      OKpoint, "\n")
                  if (!accpoint) {
                    msg <- "No acceptable point -- exit loop"
                    if (trace > 0) 
                      cat("\n", msg, "\n")
                    keepgoing <- FALSE
                    break
                  }
                }
                else {
                  if (cycle == 1) {
                    msg <- " Converged -- no progress on new CG cycle"
                    if (trace > 0) 
                      cat("\n", msg, "\n")
                    keekpgoing <- FALSE
                    break
                  }
                }
            }
        }
        if (oldstep < acctol) {
            oldstep <- acctol
        }
        if (oldstep > 1) {
            oldstep <- 1
        }
        if (trace > 1) 
            cat("End inner loop, cycle =", cycle, "\n")
    }
    msg <- "Rcgmin seems to have converged"
    if (trace > 0) 
        cat(msg, "\n")
    ans <- list( par = par, fmin, c(ifn, ig), 0, msg)
    names(ans) <- c("par", "value", "counts", "convergence", 
        "message")
    return(ans)
}
