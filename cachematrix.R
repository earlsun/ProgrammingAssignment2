## These two functions are designed to receive a matrix, and calculate the inverse. If there is a calculation already
## cached, it retrieves the last calculation instead of recalculating again. These functions reside within another function.

## makeCacheMatrix receives a matrix, and NULLs the inverse cache. It has four functions: set, get, setinverse, getinverse.
## The function does not calculate the inverse, but merely stores and retrieves whatever value the user enters.

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Function checks whether there is a cached inverse value. If there is, it retrieves the stored value.
## If not, it calculates the new inverse value of the matrix, and returns the calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
