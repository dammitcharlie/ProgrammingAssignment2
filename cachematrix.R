## Put comments here that give an overall description of what your
## functions do

##Takes an invertible matrix and returns a list of functions
##which allow for the caching of the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##Takes as a function the return value of makeCacheMatrix
##and returns the inverse of the matrix. If the inverse is cached, it simply
##returns that value. If not, it calculates the inverse and returns it.
cacheSolve <- function(x) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        return(inv)
}