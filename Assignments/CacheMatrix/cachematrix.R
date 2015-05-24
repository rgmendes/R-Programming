# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        # inverse matrix
        inv <- NULL
        
        # Set for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get for the matrix
        get <- function() x
        
        # Set inverse
        setinv <- function(inverse) inv <<- inverse
        
        # Get inverse
        getinv <- function() inv
        
        # Return the matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Compute the inverse of the matrix
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        # If the inverse is already
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # The inverse is not calculated
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setinv(inv)
        
        # Return
        inv
}