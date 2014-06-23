## First function creates a matrix and the related function calls: set, get, getSolve & setSolve
## m is a matrix of NA with the same dimensions as intialized matrix in the makeCacheMarix. This 
## function creates the functions calls that are necessary to compute and/or cache inverse of a matrix.
## The get function call returns the matrix that was defined in the makeCacheMatrix args. 
## The getsolve checks for a cache inverse matrix solution.
## The setsolve stores the inverse matrix solution of a matrix to the cache once it is solved.

makeCacheMatrix <- function(x = matrix()) {
		m <- matrix(data=NA, nrow(x), ncol(x))
		set <- function(y){
			x <<- y 
			m <<- matrix(data=NA, nrow(x), ncol(x))	
		}
		get <- function() x
		setsolve <- function(solve) m <<- solve
		getsolve <- function() m
		list(set = set, get = get, 
		     setsolve = setsolve,
		     getsolve = getsolve)
}
## This functions first calls the getsolve function to make sure check for the solution of inverse
## of the matrix in the Cache if it turns out the matrix is not all NAs it returns the inverse
## matrix in the cache, otherwise if the matrix is NAs it gets the matrix and solves the inverse, 
## then its set the solution of the inverse to the cache sing the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!all(is.na(m))){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setsolve(m)
	m
}

