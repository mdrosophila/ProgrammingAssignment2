
## makeCacheMatrix() function creates 4 functions. These 4 functions are 
## retrun in a list. The 4 functions are set() get(),setmatrix(x),and getmatrix().
## <<- sign is used to create a global variables m, to store cached 
## inverse matrix.

## cacheSolve() function returns the inverse of the matrix x. But before it
## caculates its reverse, it calls the makeCacheMatrix function to see
## whether the reverse matrix has already been caculated. If the reverse 
## matrix is cached judding (is.null(m)), the cached data m 
## is used without calculation. Otherwise, proceed to compute matrix reverse 
## value and cache the result in m, by calling tmp$setmatrix(m)

## "Usage example": to solve the reverse of the matrix x {{1,3},{2,4}}
## y<- makeCacheMatrix(x)  to return a function list 
## It also initiates m with unsolved x matrix( m=NULL)
## To solve x, call cacheSolve(y) to solve the matrix x. 
## The solved reverse matrix is stored in m
## When we try to solve x again calling cacheSolve(y), cached data 
## is returned. No caculcation will be doned.
## To calculate a NEW matrix for example z {{3,5},{4,6}}
## y$set(z) set the new matrix to z
## cacheSolve(y) to calculate its "z" reverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) 
                {m <<- matrix
                }
        getmatrix <- function() m
        list(set=set,get = get,
            setmatrix = setmatrix,
            getmatrix = getmatrix)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                print("-------------------------------------")
                print("----------getting cached data--------")
                print("Cached data is:")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)            
        m
}
