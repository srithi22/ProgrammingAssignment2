## These functions are used to cache the inverse of a matrix

## makeCacheMatrix - This function takes a matrix as an input and 
##                   associates list of functions with the matrix.
##                   It also sets the inverse of an matrix in the
##                   global environment so that it can be retrieved
##                   at any point of time later without computing it
##                   again.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inv) {
                inverse <<- inv
        }
        getinverse <- function() {
                inverse
        }
        list(get = get,
        set = set,
        getinverse = getinverse,
        setinverse = setinverse)
}


## cacheSolve - This function returns the inverse of a matrix.
##              It retreives the inverse from the cache if present
##              Else it computes it by making use of functions
##              returned in makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("return from cache")
                return(i)
        }
        data <- x$get()
        inverse <- solve(data)
        i <- x$setinverse(inverse)
        i
}
