#' @export
nagelkerke <- function(m, lambda = NULL) {
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

    ll_null <- -nulldev / 2
    ll_fitted <- -dev / 2

    r2 <- 1 - exp(-2 * (ll_fitted - ll_null) / nobs)
    if (is_glmnet) {
        if (!is.null(lambda)) {
            r2 <- r2[which(m$lambda == lambda)]
        } else {
            r2 <- r2[which.min(m$lambda)]
        }
    }

    max_r2 <- 1 - exp(2 * ll_null / nobs)
    r2_n <- r2 / max_r2

    r2_n
}
