## This function makes a matrix, that has a cachable inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) s <<- solve
	getSolve <- function() s
	list(set = set, get = get,
			 setSolve = setSolve,
			 getSolve = getSolve)
}


## This function makes finds the inverted matrix or takes it from the cache

cacheSolve <- function(x, ...) {
	s <- x$getSolve()
	if(!is.null(s)) {
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setSolve(s)
	s
}
