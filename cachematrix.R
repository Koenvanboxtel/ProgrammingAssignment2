## Set the value of the matrix, then get the values
## Set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## first check if the inverse is computed, if true then skip computation
## else compute the inverse and set the value

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
	if(!is.null(inv)) {
		message("this is cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	message("this isn't cached data.")
	inv 
}
