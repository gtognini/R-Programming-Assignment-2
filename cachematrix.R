## Matrix inversion take a long time and it can be beneficial to cache
## the inverse of a matrix, rather than computing it several times.
## The functions below are used to cache the inverse of a matrix.

## The following function makeCacheMatrix creates a matrix that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The following function cacheSolve computes the inverse of the matrix created by the makeCacheMatrix function above, and assumes the matrix is always invertible. If the inverse has already been computed, the function will skip the computation.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
        	message("retrieving cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}

## Test run:
## > x <- rbind(c(2, -5), c(-5/3, 3))
## > matrix1 = makeCacheMatrix(x)
## > matrix1$get()
##          [,1] [,2]
## [1,]  2.000000   -5
## [2,] -1.666667    3

## No cache in the first test run:
## > cacheSolve(matrix1)
##            [,1]       [,2]
## [1,] -1.2857143 -2.1428571
## [2,] -0.7142857 -0.8571429

##Second test run will retrive the cache:
## > cacheSolve(matrix1)
## retrieving cached data
##           [,1]       [,2]
## [1,] -1.2857143 -2.1428571
## [2,] -0.7142857 -0.8571429