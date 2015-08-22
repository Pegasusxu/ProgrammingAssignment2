makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) s <<- inverse
        getinv <- function() s
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(z, ...) {
        t <- z$getinv()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        mat.data <- z$get()
        t <- solve(mat.data, ...)
        z$setinv(t)
        t
}
