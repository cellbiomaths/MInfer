#' Prepare Data for Specific Condition
#'
#' Filters and organizes data based on the given condition.
#' @param data Metabolic data (input matrix).
#' @param condition The condition to filter (e.g., "6C" or "16C").
#' @return A numeric matrix of data for the specified condition.
#' @export
prepare_data <- function(data, condition) {
    condition_data <- as.matrix(data[data$condition == condition, 2:ncol(data)])
    return(condition_data)
}
