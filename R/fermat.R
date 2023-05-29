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
get_fermat_distance <- function(X, method = "full", alpha = 2, landmarks_frac = 0.1) {
    if (!(method %in% c("full", "knn", "landmarks"))) {
        stop("Method is not available")
    }
    n <- nrow(X)
    dist_matrix <- as.matrix(dist(X, diag = FALSE, upper = FALSE)) ^ alpha
    if (method == "knn") {
        k <- as.integer(sqrt(n))
        for (i in 1:n) {
            col_dists <- dist_matrix[, i]
            min_k_dist <- sort(col_dists)[k + 1]
            remove_nodes <- dist_matrix[, i] >= min_k_dist
            # # 0 in the edge removes the node
            dist_matrix[remove_nodes, i] <- 0
        }
    }
    g <- igraph::graph_from_adjacency_matrix(dist_matrix, weighted = TRUE)
    if (method == "landmarks") {
        landmarks_ids <- sample(n, as.integer(n * landmarks_frac))
        landmarks_sp <- igraph::shortest.paths(g, v = landmarks_ids)
        sp <- matrix(0, n, n)
        dimnames(sp) <- dimnames(dist_matrix)
        for (i in 1:n) {
            sp[i, ] <- matrixStats::colMins(landmarks_sp[, i] + landmarks_sp)
        }
    } else {
        sp <- igraph::shortest.paths(g)
    }
    new("fermatDistance", sp = sp, method = method)
}

# n = 1000
# d = 2
# X = matrix(rnorm(n*d), nrow=n, ncol=d)
# start <- Sys.time()
# fd = get_fermat_distance(X, "full", alpha = 1)
# print( Sys.time() - start )

# start <- Sys.time()
# fd2 = get_fermat_distance(X, "knn", alpha = 1)
# print( Sys.time() - start )

# start <- Sys.time()
# fd3 = get_fermat_distance(X, "landmarks", alpha = 1, landmarks_frac = 0.1)
# print( Sys.time() - start )

