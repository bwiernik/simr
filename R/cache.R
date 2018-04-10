



cache <- function(model, seed=NA) {

    beta <- getME(model, "fixef")
    names(beta) <- paste0("beta", seq_along(beta))

    theta <- getME(model, "theta")
    names(theta) <- paste0("theta", seq_along(theta))

    sigma <- getME(model, "sigma")

    rval <- c(beta, theta, sigma=sigma)

    rval <- as.data.frame(as.list(rval))
    rval$seed <- list(seed)
}







uncache <- function() {









}


