## These two functions allow for the caching of the inverse of a matrix
## by creating a special object that stores a matrix vector and caches
## its inverse.

## The makeCacheMatrix function creates a special "matrix", which is just
## a list containing a function to do the following:
##    1) set the value of the matrix
##    2) get the value of the matrix
##    3) set the value of the inverse of the matrix
##    4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL

	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() {x}
	setinverse <- function(solve) {inverse <<- solve}
	getinverse <- function() {inverse}

	list (set = set, get = get, 
	      setinverse = setinverse,
	      getinverse = getinverse)	
}


## The cacheSolve funcion calculates the inverse of the special "matrix"
## created using the makeCacheMatrix function.  It first checks to see
## if the inverse of the matrix has already been calculated.  If so, the
## function retrieves the inverse of the matrix from the cache and does
## not perform the computation.  Otherwise, the function calculates the
## inverse of the matrix and sets the value of the inverse of the matrix
## using the setinverse function.

cacheSolve <- function(x, ...) {

        inverse <- x$getinverse()
	if (!is.null(inverse)) {
		message ("getting cached data")
		return(inverse)
	}

	data <- x$get()
	inverse <- solve(data,...)
	x$setinverse(inverse)
	inverse

}
