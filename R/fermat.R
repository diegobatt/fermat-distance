library(igraph)

#' @export
fermatDistance <- setClass("fermatDistance", slots = c(sp="matrix", method="character"))


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

get_fermat_distance <- function(X, method, alpha = 2) {
    dist_X <- dist(X, method = "minkowski", diag = FALSE, upper = FALSE, p = alpha)
    G <- graph_from_adjacency_matrix(as.matrix(dist_X), weighted = TRUE)
    new("fermatDistance", sp = shortest.paths(G), method = method)
}


X = matrix(rnorm(36),nrow=6)
fd = get_fermat_distance(X, "l")




