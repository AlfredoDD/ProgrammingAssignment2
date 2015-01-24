## This functions works together, so maybe first function could live alone, and second too, but
## if you use both, you can save time in calculations.

## This first function returns you a list of functions, that you can use in other functions to save time in
## complex calculations. This list of functions (set, get, setinverse, getinverse) permits you pass and see what
## are into the vector. To save time, in this function you calculates the inverse of a matrix and save it in upper workspace
## so in other parts of your code, you can access to this variable without cost of time.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function (y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve(x)
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse =getinverse)
}


## This function returns the inverse of a matrix, but first looks it in the upper environment, because maybe you did that 
## calculation in other moments of the run or in other parts of the code....

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cache data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
