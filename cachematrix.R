## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) {
			thismatrix <- matrix(NA,nrow,ncol,byrow,dimnames)
			x <<- thismatrix
			i <<- NULL
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
		if(!is.null(m)) {
				message("getting cached solve \n")
				return (i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setmean(i)
		i
}
