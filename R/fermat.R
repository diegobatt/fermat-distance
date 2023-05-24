#' @export
fermatDistance <- setClass(
    "fermatDistance", slots = c(sp = "matrix", method = "character")
)


#' @export
distances <- function(object) {
    UseMethod("distances")
}

#' @export
distances.fermatDistance <- function(object) {
    object@sp
}

#' @export
distance <- function(object, p_ix, q_ix) {
    UseMethod("distance")
}

#' @export
distance.fermatDistance <- function(object, p_ix, q_ix) {
    object@sp[p_ix, q_ix]
}

#' @export
get_fermat_distance <- function(X, method = "full", alpha = 2) {
    if (!(method %in% c("full", "knn", "landmarks"))) {
        stop("Method is not available")
    }
    dist_matrix <- as.matrix(dist(X, diag = FALSE, upper = FALSE)) ^ alpha
    if (method == "knn") {
        k <- as.integer(sqrt(ncol(dist_matrix)))
        for (i in seq_len(ncol(dist_matrix))) {
            col_dists <- dist_matrix[, i]
            min_k_dist <- sort(col_dists)[k + 1]
            remove_nodes <- dist_matrix[, i] <= min_k_dist
            dist_matrix[remove_nodes, i] = Inf
        }
    }
    g <- igraph::graph_from_adjacency_matrix(dist_matrix, weighted = TRUE)
    new("fermatDistance", sp = igraph::shortest.paths(g), method = method)
}

# X = matrix(rnorm(36),nrow=6)
# fd = get_fermat_distance(X, "l")
