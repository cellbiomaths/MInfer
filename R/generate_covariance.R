#' Generate Covariance Matrices for Each Time Point
#'
#' Calculates covariance matrices for subsets of data based on time points.
#' @param data Numeric matrix of data.
#' @param num_tp Number of time points.
#' @return A list of covariance matrices, one for each time point.
#' @export
generate_covariance <- function(data, num_tp) {
    num_rep <- nrow(data) / num_tp
    cov_tp <- list()

    for (k in 1:num_tp) {
        start <- (k - 1) * num_rep + 1
        end <- k * num_rep
        subset <- as.data.frame(data[start:end, ])
        subset <- as.data.frame(lapply(subset, as.numeric))
        cov_tp[[k]] <- cov(subset)
    }
    return(cov_tp)
}
