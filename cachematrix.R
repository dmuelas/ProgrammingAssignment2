
## Author: dmuelas
## Date: 2014/09/21
## Description: Code of Programming Assigment 2
## Contents:
##          - makeCacheMatrix
##          - cacheSolve

## Name of function: makeCacheMatrix
## Description: This function creates a special "matrix" object that can cache its inverse
## Input: x matrix
## Output: 
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


## Write a short comment describing this function
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


