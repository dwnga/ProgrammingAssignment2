## Function 1. makeCacheMatrix 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse} # set inverse value
        getInverse <- function() {inv} ## getinverse value
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## Function 2. cacheSolve
cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)) { ## cacheSolve should retrieve the inverse from the cache
                message("getting cached data")
                return(inv)
        }
       mat<-x$get()
       inv<-solve(mat,...)
       x$setInverse(inv)
       inv
}