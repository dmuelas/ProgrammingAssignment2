
## Author: dmuelas
## Date: 2014/09/21
## Description: Code of Programming Assigment 2
## Contents:
##          - makeCacheMatrix
##          - cacheSolve

## Name of function: makeCacheMatrix
## Description: This function creates a special "matrix" object that can cache its inverse
## Input: x matrix
## Output: singleton version -- only process inverse once 
makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}

## Name of function: cacheSolve
## Description: This function implements the singleton version of inverse
## Input: x CacheMatrix
## Output: inverse of x 
cacheSolve <- function(x, ...) {

	inv <- x$getinv()

	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}


