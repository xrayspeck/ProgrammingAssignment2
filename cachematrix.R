## This function is creating an object that stores a special matrix whose
## inverse can be cached 

## Write a short comment describing this function
## set the value of the matrix
## set the value of the invertion of the matrix
## get the value of matrix
## get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinversion <-function(inversion) inv <<- inversion
	getinversion <-function() inv
	list(set = set, get = get,
	setinversion = setinversion,
	getinversion = getinversion)
}


## This function calculates the inversion of the matrix created in {makeCacheMatrix}.
## It first checks whether the matrix inversion has already been created and gets the
## mean from the cache. Otherwise it calculates the matrix inversion and stores it in  
## the set in the cache via the setinversion function.

cacheSolve <- function(x, ...) {
	inv <- x$getinversion()
	if(!is.null(inv)) {
		message(“getting cached data”)
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,…)
        inv ## Return a matrix that is the inverse of 'x'
}
