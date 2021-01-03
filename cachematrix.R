## Because of matrix inversion is usually cost we make a script that has the objective of caching the 
## inverse of a matrix rather than computing repeatedly. For this purpouse we write 2 chunks:

## The objective of this function is to create a special matrix that cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The objective of the second script is to compute the inverse of the output of the last script (special matrix)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data") 
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

## Testing the code
matriz <- matrix(c(1,2,3,4),2,2)
matriz_cache <- makeCacheMatrix(matriz)

cacheSolve(matriz_cache)
