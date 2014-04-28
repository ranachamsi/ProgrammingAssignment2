## cachematrix.R
## 
## 
## Usage:
## M <- matrix(c(10, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3)
## cachedM <- makeCacheMatrix(M)
## cacheSolve(cachedM)
## cacheSolve(cachedM)
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL ## Reset cache to NULL when new matrix set or changed
	}
	get <- function() x
	setInverse <- function(mInverse) m <<- mInverse 
	getInverse <- function() m
	list(set = set, get = get,
	   setInverse = setInverse,
	   getInverse = getInverse) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	## The inverse was already calculated and the matrix didn't change
	if(!is.null(m)) { 
		message("getting cached data")
		return(m)
	}
	
	## Either a new matrix or it have been changed
	matrixToInverse <- x$get()
	m <- solve(matrixToInverse)
	## we've calculated the inverse, let's store it to use it again
	x$setInverse(m)
	return(m)
}



