## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) 
                {m <<- matrix
                print(m)
                }
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        tmp<-makeCacheMatrix()
        m <- tmp$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        tmp$set(x)
        data <- tmp$get()
        m <- solve(data)
        tmp$setmatrix(m)
        m
}
