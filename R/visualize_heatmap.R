#' Visualize Jacobian Matrix as Heatmap
#'
#' Plots a heatmap of the given matrix with a custom color palette.
#' @param matrix The matrix to visualize.
#' @param metabolites_fin Vector of metabolite names for axis labels.
#' @param title The title of the heatmap.
#' @export
visualize_heatmap <- function(matrix, metabolites_fin, title = "Heatmap") {
    color_palette <- colorRampPalette(c("blue", "white", "red"))(100)
    max_val <- max(abs(matrix), na.rm = TRUE)
    min_val <- -max_val

    pheatmap(
        matrix,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        display_numbers = FALSE,
        color = color_palette,
        breaks = seq(min_val, max_val, length.out = 101),
        main = title,
        labels_row = t(metabolites_fin),
        labels_col = t(metabolites_fin)
    )
}
