# The first function, makeCacheMatrix,creates a special "matrix", which 
#is really a list containing a functions to:
# -set the value of the matrix
# -get the value of the matrix
# -set the value of the inverse
# -get the value of the inverse
#Usage:
#source("cachematrix.R")
#exampleMatrix = makeCacheMatrix(matrix(c(10,20,30,40), nrow=2, ncol=2))
#exampleMatrix$get() 
#exampleMatrix$set()
#exampleMatrix$getinverse()
#exampleMatrix$setinverse() 
#WARNING: setinverse should never be called directly from outside the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


#the second function, cacheSolve, is a function that returns the inverse of a provided matrix.
#If the inverse has been previously solved, the cached data is returned.
#Otherwise, it solves the inverse, caches the results, and then returns the inverse.
#If using cached results, a message is printed notifying the user.
#Usage: 
#source("cachematrix.R")
#exampleMatrix = makeCacheMatrix(matrix(c(10,20,30,40), nrow=2, ncol=2))
#cacheSolve(exampleMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
