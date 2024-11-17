library(MASS)
library(pheatmap)


#' Visualize Jacobian Difference in 3D
#'
#' Creates a 3D perspective plot for the given matrix.
#' @param matrix The matrix to visualize.
#' @export
visualize_3d <- function(matrix) {
    color_palette <- colorRampPalette(c("blue", "white", "red"))(100)
    z_values <- matrix
    zlim <- range(z_values, na.rm = TRUE)
    color_index <- cut(z_values, breaks = 100, labels = FALSE)
    color_index[is.na(color_index)] <- 1

    persp3d(
        x = 1:ncol(matrix),
        y = 1:nrow(matrix),
        z = z_values,
        col = color_palette[color_index],
        xlab = "1st condition",
        ylab = "2nd condition",
        zlab = "J(f)",
        aspect = c(1, 1, 0.5),
        theta = 30, phi = 30
    )
}
