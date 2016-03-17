## The function "makeCacheMatrix" calculates and caches the inverse of a matrix, optimizing the next search   
## in case the matrix is still the same

## MakeCacheMatrix has getters and setters for cached data

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## "cacheSolve" function calculates the inverse of the special matrix created by "makeCacheMatrix" 
## cacheSolve verifies if the inverse matrix calculation is in cache and returns it`s value if cache is not null.
## if cache is null, the function calculates the matrix inverse and stores it in cache 


cacheSolve <- function(x, ...) {
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("cached inverse matrix")
		return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$set_inverse(i)
    i
}
