# Set random seed
set.seed(2048)

# Create list (X) of 100 slots, each consisting of a name and a vector
X <- list()
for (i in seq_along(1:100)) {
  X[[i]] <- list(name = paste0("n",i), vect = rnorm(5, 20, 10))
}

# Display structure of X
str(X)

# Create and display vector containing sum of values of each vector within each slot in X
output <- double(100) 
for (i in seq_along(1:100)) {
  output[[i]] <- sum(X[[i]]$vect)
}
output

# extract and unravel all the vectors
vectors <- unlist(lapply(X, function(l) l$vect))

# fill by row a n x 5 matrix with all vectors
M <- matrix(data = vectors, ncol = 5, byrow = TRUE)

# Display sums of all columns of M
sums <- double(5)
for (i in 1:5) {
  sums[[i]] <- sum(M[, i])
} 
sums