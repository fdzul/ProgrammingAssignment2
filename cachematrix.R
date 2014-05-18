## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse
## makeCacheMatrix return a list of function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## Nota: operator can be used to assign a value to an object
## in an environment that is different from the current enviroment.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## This second function, cacheSolve, compute the inverse of the special "matrix" return by makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if (!is.null(inv)){
                message("getting cachedSolve data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}

