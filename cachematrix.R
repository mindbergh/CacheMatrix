## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		## Return a list of functions that implement cached matrix
		i <- NULL   
		set <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) {
			## the parameters are the same as build-in function matrix()
			thismatrix <- matrix(datac,nrow,ncol,byrow,dimnames)
			## use matrix() to create a matrix
			x <<- thismatrix
			i <<- NULL   ## reassign i as NULL as x may be changed
		}
		get <- function() x
		setsolve <- function(inv) i <<- inv
		getsolve <- function() i
		list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getsolve()
		if(!is.null(i)) {
				## if i is not NULL, it means that we have a cached solve, just return it
				message("getting cached solve \n")
				return (i)
		}
		## i is NULL, which means we don't have a cached solve, so compute it using solve()
		data <- x$get()
		i <- solve(data, ...)
		x$setsolve(i)
		i
}
