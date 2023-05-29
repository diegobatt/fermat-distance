n <- 100
d <- 2
X <- matrix(rnorm(n*d), nrow=n, ncol=d)
dist_matrix <- as.matrix(dist(X, diag = FALSE, upper = FALSE))

test_that("For L1 distance full method equals distance matrix", {
  fd <- get_fermat_distance(X, "full", alpha = 1)
  expect_equal(as.matrix(fd), dist_matrix)
})

test_that("For all point as landmarks and L1, approximate mathod equals distance matrix", {
  approx_fd <- get_fermat_distance(X, "landmarks", alpha = 1, landmarks_frac = 1)
  expect_equal(as.matrix(approx_fd), dist_matrix)
})

test_that("For all point as landmarks, approximate mathod equals full", {
  approx_fd <- get_fermat_distance(X, "landmarks", alpha = 2, landmarks_frac = 1)
  fd <- get_fermat_distance(X, "full", alpha = 2)
  expect_equal(as.matrix(approx_fd), as.matrix(fd))
})

test_that("Landmarks method have some distances equal to full method", {
  approx_fd <- get_fermat_distance(X, "landmarks", alpha = 2, landmarks_frac = 0.5)
  fd <- get_fermat_distance(X, "full", alpha = 2)
  exact_ratio <- mean(as.matrix(fd) == as.matrix(approx_fd))
  expect_gt(exact_ratio, 0)
})

test_that("For sufficiently large alpha, distance KNN method equals full", {
  alpha <- 6
  knn_fd <- get_fermat_distance(X, "knn", alpha = alpha)
  fd <- get_fermat_distance(X, "full", alpha = alpha)
  expect_equal(as.matrix(knn_fd), as.matrix(fd))
})
