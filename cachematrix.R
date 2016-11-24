
makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        # Sets the value of the data in case inverse was'nt in the chache
        set <- function(y) 
                {
                x <<- y
                mat_inv <<- NULL
        }
        # Gets the value of the data
        get <- function()
                {
                x
        }
        # Calculates the inverse of the data if it wasn't found in the cache
        setinv <- function(solve)
                {
                mat_inv <<- solve
        }
        # Gets the value if the inverse in case it was found in the cache
        getinv <- function() 
                {
                mat_inv
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



cacheSolve <- function(x, ...) {
        mat_inv <- x$getinv()
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setinv(mat_inv)
        mat_inv
}

