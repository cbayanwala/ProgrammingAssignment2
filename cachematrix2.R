## makeCacheMatrix, when initially called is used to set up a list of 
## functions to calculate the mean (if necessary) when it is used in
## conjunction with cacheSolve. Otherwise, the value from the cache is
## returned 


## makeCacheMatrix will be used to define the various functions to be used
## with the incoming matrices. It returns a list with the required functions

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL					## Making Sure that Inverse is a 
							# Null when function is initialized
	set <- function(y){			## Assigning new matrix on a global	
		x <<- y				# scope. Also Cleaning Inverse
		inv <<- NULL			# variable.
	}
	
	get <- function() x			##simply returning stored variable
	setinv <- function(inverse) inv <<- inverse	##assigning calculated
									#inverse to cached
									#variable
	getinv <- function() inv		##simply returning stored inverse
	list(set = set, get = get, setinv = setinv,
		getinv = getinv)
}


## This function checks if the inverse of the supplied matrix already exists.
## If it does, it just returns the cached value, otherwise calculates the
## inverse and returns it

cacheSolve <- function(x, ...) {
	inv <- x$getinv()				## calling cached value of matrix inverse
	if(!is.null(inv)){			## if it is empty, it means matrix is new
		message("getting cached matrix")	##if it is not empty, it returns 
		return(inv)					#cached value of inverse
	}
	mat <- x$get()				## this line is only executed if the above
							# if condition block is false
	inv <- solve(mat,...)			# since there's a new matrix, inverse is
							# calculated using solve function
	x$setinv(inv)				## caching the calculated value of the
							# inverse
	inv						## returning value of inverse
}
