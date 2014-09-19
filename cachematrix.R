## makeCacheMetrix() and cacheSolve() functions, which compute and cache inverse of the input matrix

## makeCacheMetrix() acts as the cache, that stores the original and inverted matrices. It also stores getinv and setinv()
## functions

makeCacheMatrix <- function(x = matrix()) {
        answer <- NULL
        set <- function(y) {
                x <<- y
                answer <<- NULL
        }
        get <- function() x
        setinv <- function(inv) answer <<- inv
        getinv <- function() answer
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSole() calls functions defined in teh former function above. If inverse matrix is not set, 
## cacheSolve() calculates it and stores it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        answer <- x$getinv()
        if(!is.null(answer)) {
                message("getting cached data")
                return(answer)
        }
        original <- x$get()
        answer <- solve(original)
        x$setinv(answer)
        answer
}
