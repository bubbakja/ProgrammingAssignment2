## makeCacheMatrix() will create and maintain a matrix 
## cacheSolve() will invert a matrix and cache it for additional requests to invert the same matrix

## Creates a list function that maintains a matrix to help with caching of the inversion by the cacheSolve() function
makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	get <- function() x
	setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}



## on first execution for a given matrix defined by makeCacheMatrix() it creates and stores the matrix inversion
## on additional executions it will provide the cached matrix of the the matrix hasn't changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached matrix inversion")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


