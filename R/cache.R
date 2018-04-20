



cache <- function(model, seed=NA) {

    beta <- getME(model, "fixef")
    names(beta) <- paste0("beta", seq_along(beta))

    theta <- getME(model, "theta")
    names(theta) <- paste0("theta", seq_along(theta))

    sigma <- getME(model, "sigma")

    rval <- c(beta, theta, sigma=sigma)

    rval <- as.data.frame(as.list(rval))
    rval$seed <- list(seed)

    return(rval)
}

uncache1 <- function(model, cache, u) {

    nm <- names(cache)

    beta <- as.numeric(cache[, grep("beta", nm)])
    theta <- as.numeric(cache[, grep("theta", nm)])
    sigma <- cache$sigma

    seed <- cache$seed[[1]]

    .Random.seed <<- seed
    y <- doSim(model)

    rval <- makeMer2(

        y        = y,
        formula  = formula(model),
        family   = family(model),
        fixef    = beta,
        theta    = theta,
        data     = getData(model),
        dataName = as.character(getCall(model)$data)
    )

    rval@beta <- beta
    rval@pp$setBeta0(beta)

    rval@theta <- theta
    rval@pp$setTheta(theta)

    rval@u <- u
    rval@pp$setDelu(u)

    mu <- predict(rval, type="response", newdata=getData(rval))
    rval@resp$updateMu(family(fm0)$linkfun(mu))

    return(rval)
}

uncache2 <- function(model, cache) {

    nm <- names(cache)

    beta <- as.numeric(cache[, grep("beta", nm)])
    theta <- as.numeric(cache[, grep("theta", nm)])
    sigma <- cache$sigma

    seed <- cache$seed[[1]]

    ##### fix this bit ---- needs to have re.form

    .Random.seed <<- seed
    y <- doSim(model)

    ### data frame with y
    data <- getData(model)
    response <- as.character(formula(model)[[2]])
    data[[response]] <- y

    ### call from model
    call <- getCall(model)

    ### modify the call, e.g. opts for 1 iteration, start=...
    call$control <- glmerControl(optCtrl=list(maxfun=1))
    call$start <- list(fixef=beta, theta=theta)
    call$data <- quote(data)

    ### that's all?
    eval(call)

}

print.stuff <- function(fm) {

    message("fixef:")
    print(fixef(fm))

    message("theta:")
    print(fm@theta)

    message("ranef:")
    print(ranef(fm))

    message("predict:")
    print(head(predict(fm)))
}

cf <- function(fm1, fm2) {

    message("fixef:")
    print(all.equal(fixef(fm1), fixef(fm2)))

    message("theta:")
    print(all.equal(fm1@theta, fm2@theta))

    message("ranef:")
    print(all.equal(ranef(fm1), ranef(fm2)))

    message("predict:")
    print(all.equal(predict(fm1), predict(fm2)))
}

if(FALSE) {

fm0 <- glmer(z ~ x + (x|g), family=poisson, data=simdata)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)

s <- .Random.seed

y <- doSim(fm0)
fm1 <- doFit(y, fm0)


z1 <- cache(fm1, s)

fm2 <- uncache(fm0, z1, fm1@u)

opts <- glmerControl(optCtrl=list(maxfun=1))

fm3 <- glmer(z ~ x + (x|g), family=poisson, data=getData(fm1), start=list(fixef=fm1@beta, theta=fm1@theta), control=opts)

}



#
# TO-DO
#

#
# document
#
# check for errors in inputs and give meaningful messages
#  - e.g. sigma missing for lmer
#

makeMer2 <- function(y, formula, family, fixef, theta, data, dataName) {

    if(length(formula) < 3) stop("Formula must have left and right hand side")

    lhs <- make.names(deparse(formula[[2]])); formula[[2]] <- as.name(lhs)
    rhs <- formula[-2]

    p <- list(beta=fixef, theta=theta)

    data[[lhs]] <- y

    environment(formula) <- environment() # https://github.com/lme4/lme4/issues/177

    suppressWarnings({

        rval <- glmer(formula, family=family, data=data, control=glmerSet(theta))
        rval@call$family <- rval@resp$family$family
    })

    fixef(rval) <- fixef
    rval@theta <- theta

    attr(rval, "newData") <- data
    rval@call$data <- parse(text=dataName)[[1]]

    rval@call$control <- NULL

    attr(rval, "simrTag") <- TRUE

    assign(dataName, data)
    return(rval)
}

#
# We need to make merMod objects but we don't need to fit them because we're supplying the parameters
#
nullOpt <- function(fn, par, lower, upper, control) {

    theta <- control$theta
    if(is.null(theta)) theta <- rep(1, length(par))

    rval <- list(
        fval        = fn(theta),
        par         = theta,
        convergence = 0,
        message     = "No optimisation",
        control     = list()
    )

    # calling the deviance function updates its environment
    rval$fval <- fn(rval$par)

    return(rval)
}

class(nullOpt) <- "workaround408" #https://github.com/lme4/lme4/issues/408
#' @export
`==.workaround408` <- function(e1, e2) if(e2=="none") FALSE else `==`(unclass(e1), e2)

lmerSet <- function(theta) lmerControl(

    optimizer=nullOpt,
    optCtrl=list(theta=theta),
    restart_edge=FALSE,
    boundary.tol=0,
    calc.derivs=FALSE
)

glmerSet <- function(theta) glmerControl(

    optimizer=nullOpt,
    optCtrl=list(theta=theta),
    restart_edge=FALSE,
    boundary.tol=0,
    calc.derivs=FALSE
)

# logic from stats::glm
as.family <- function(family) {

    if(is.character(family)) {

        family <- get(family, mode = "function", envir = parent.frame(2))
        family <- family()
    }

    if(is.function(family)) {

        family <- family()
    }

    if(is.null(family$family)) {

        	stop("'family' not recognized")
    }

    return(family)
}