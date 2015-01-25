## This functions works together, so maybe first function could live alone, and second too, but
## if you use both, you can save time in calculations.

## This first function returns you a list of functions, that you can use it in other functions to save time in
## complex calculations. This list of functions (set, get, setinverse, getinverse) permits you pass and see what
## are into the vector. To save time, in this function you calculates the inverse of a matrix and save it in upper workspace
## so in other parts of your code, you can access to this variable without cost of time.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
	set <- function (y) { ## this sentence saves in "x" the matrix data
			x <<- y
			m <<- NULL
	}
	get <- function() x ## this sentence gets the matrix stored in "x" variable
	setinverse <- function(solve) m <<- solve(x) # this sentence "solves" (calculares the inverse of matrix)
	getinverse <- function() m # this sentence gets the stored inverse of matrix
    ## the result of makeCacheMatrix is a list of 4 functions, that you can reuse and call in other code
    ## locations to save time
	list(set = set, get = get, setinverse = setinverse, getinverse =getinverse) 
}


## The next function returns the inverse of a matrix, but first looks it in the upper environment, because maybe you did that 
## calculation in other moments of the run or in other parts of the code....

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'. First of all, puts the inverse from memory in m
	m <- x$getinverse()
	## now, it decides if have to calculate the inverse or not
	if(!is.null(m)){
		message("getting cache data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

## the correct way to incorporate this 2 chained functions is, in any moment that you want to calculate the
## inverse of a matrix, you creates a vector with the result of makeCacheMatrix with the matrix as parameter.
## So, in other moments of your code, if you want to calculates the inverse of a matrix, you can call the
## solve function with the vector result of the application of makeCacheMatrix over your matrix.