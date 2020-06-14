## This function creates a special "matrix" object that can cache its inverse.
## sample is the matrix object that user will submit on the consol

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
        x <<- y
        inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


## Output from R
#source("makeCacheMatrix.R")
#> My_matrix <-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
#> My_matrix$get()
#[,1] [,2]
#[1,]    1    3
#3[2,]    2    4
#> My_matrix$getInverse()
#NULL
#> cacheSolve(My_matrix)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(My_matrix)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> My_matrix$getInverse()
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> My_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
#> My_matrix$get()
#[,1] [,2]
#[1,]    2    1
#[2,]    2    4
#> My_matrix$getInverse()
#NULL
#> cacheSolve(My_matrix)
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333
#> cacheSolve(My_matrix)
#getting cached data
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333
#> My_matrix$getInverse()
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333
#> 