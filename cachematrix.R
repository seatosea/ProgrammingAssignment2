## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x=matrix()){
        matrix <- NULL
        set <- function(y){
                matrix <<- y
                matrix <<- NULL
        }
        get <- function(){matrix}
        
        setInverse <- function(inverse) {
                matrix <<- inverse
        }
        
        getInverse <- function() {matrix}
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        matrix <- x$getInverse()
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}
