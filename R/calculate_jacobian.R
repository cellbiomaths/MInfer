library(MASS)
library(pheatmap)

#' Calculate Jacobian Matrix
#'
#' This function calculates a Jacobian matrix based on covariance data.
#' @param C1 Covariance matrix.
#' @param stoi Stoichiometric matrix for interactions.
#' @param icount Number of iterations.
#' @return A list containing the Jacobian matrix and related statistics.
#' @export
calculate_jacobian <- function(C1, stoi, icount) {
    N <- nrow(C1)
    Jvec_rep <- matrix(NA, nrow = N^2, ncol = icount)

    for (count in 1:icount) {
        sigma <- 0.001 * rnorm(N)
        D <- diag(-0.5 * sigma^2)
        indnotzero <- t(stoi) != 0

        Aleft <- matrix(0, nrow = N^2, ncol = N^2)
        for (i in 1:N) {
            Aleft[((i - 1) * N + 1):(i * N), ((i - 1) * N + 1):(i * N)] <- t(C1)
        }

        Aright <- matrix(0, nrow = N^2, ncol = N^2)
        for (i in 1:N) {
            for (j in 1:N) {
                Aright[((i - 1) * N + j), ((j - 1) * N + 1):(j * N)] <- C1[i, ]
            }
        }
        A <- Aleft + Aright
        B <- as.vector(t(D))

        X <- ginv(A) %*% B
        Jvec_rep[, count] <- X
    }

    Jvec_rep[is.infinite(Jvec_rep)] <- NA
    Jvec_median <- apply(Jvec_rep, 1, median, na.rm = TRUE)
    Jvec_quantiles <- apply(Jvec_rep, 1, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    Jvec_interquartile <- Jvec_quantiles[3, ] - Jvec_quantiles[1, ]
    Jvec_norm_median <- Jvec_median / (Jvec_interquartile^2)

    J <- matrix(0, nrow = N, ncol = N)
    Jvec <- rep(0, N^2)
    Jvec[indnotzero] <- Jvec_median

    for (i in 1:N) {
        J[i, ] <- Jvec[((i - 1) * N + 1):(i * N)]
    }

    return(list(J = J, Jvec_median = Jvec_median, Jvec_norm_median = Jvec_norm_median, Jvec_interquartile = Jvec_interquartile))
}
