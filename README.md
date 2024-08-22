# Fermat Distance R implementation

Fermat distance R implementation. Based on the paper [Weighted Geodesic Distance Following Fermat's Principle](https://www.researchgate.net/publication/328842164_Weighted_Geodesic_Distance_Following_Fermat's_Principle) and inspired by the [Python implementation](https://github.com/facusapienza21/Fermat-distance)

## Installation

Using devtools

```R
devtools::install_github("diegobatt/fermat-distance")
```

Using renv:
    
```R
renv::install("diegobatt/fermat-distance")
```

NOTE: At some point this will be submitted to CRAN.





## Usage 

An example of using the Fermat distance with synthetic data can be show below:

```R
n <- 100
d <- 2
X <- matrix(rnorm(n*d), nrow=n, ncol=d)
```

The following snippet computes the fermat distance between all pairs of points in the dataset `X` with an alpha value of 1:

```R
fd <- fermat_dist(X, "full", alpha = 1)
```

It follow the theory in [this paper](https://www.researchgate.net/publication/328842164_Weighted_Geodesic_Distance_Following_Fermat's_Principle) that this should equal the raw distance matrix based on Euclidean distance

```R
dist_matrix <- as.matrix(dist(X, diag = FALSE, upper = FALSE))
expect_equal(as.matrix(approx_fd), as.matrix(fd))
```

The `S3` method `as.matrix` can be used to obtain the distance matrix from the `fermatDistance` object returned by the `fermat_dist` function. 

Fast approximation can be used, such as `landmarks` or `knn`:

```R
landmark_fd <- fermat_dist(X, "landmarks", alpha = 2, landmarks_frac = 0.1)
knn_fd <- fermat_dist(X, "knn", alpha = 2)
```

NOTE: Knn method does not accept a `k` parameter, it uses the default value of `sqrt(N)` to ensure guarantees.

