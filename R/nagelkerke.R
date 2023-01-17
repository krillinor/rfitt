nagelkerke <- function(m, lambda = "min") {
    class_m <- class(m)
    is_glmnet <- "glmnet" %in% class_m
    is_glm <- "glm" %in% class_m

    if (is_glmnet) {
        nulldev <- m$nulldev
        dev <- nulldev * (1 - m$dev.ratio)
        nobs <- m$nobs
    } else if (is_glm) {
        nulldev <- m$null.deviance
        dev <- m$deviance
        nobs <- length(m$linear.predictors)
    } else {
        stop("not supported for this model")
    }

    ll_fitted <- exp(-dev / 2)
    ll_null <- exp(-nulldev / 2)

    r2 <- 1 - (ll_null / ll_fitted)^(2 / nobs)
    if (is_glmnet) {
        if (lambda == "min") {
            r2 <- r2[which.min(m$lambda)]
        } else {
            stop("only supported for for lambda='min'")
        }
    }

    max_r2 <- 1 - ll_null^(2 / nobs)
    r2_n <- r2 / max_r2

    r2_n
}
