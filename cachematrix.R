## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates 4 functions. These 4 functions are 
##retrun in a list. The 4 functions are set() get(),setmatrix(x),and getmatrix().
## <<- sign is used to create a global variables m, to store cached 
##inverse matrix.
## Example: to solve the reverse of the matrix x {{1,3},{2,4}}
##>y<- makeCacheMatrix(x)  to cache unsolved x matrix( m=NULL)
##>cacheSolve(y) to solve the matrix x. The reverse is stored in m
##>when we try to solve x again calling cacheSolve(y) again, cached data 
##is returned. No caculcation will be doned.
## To calculate a NEW matrix for example z {{3,5},{4,6}}
##>y$set(z) set the new matrix to z
##cacheSolve(y) to calculate its reverse


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


## cacheSolve function returns the inverse of the matrix x. But before it
## caculates its reverse, it calls the makeCacheMatrix function to see
##whether the reverse matrix has already been caculated. If the reverse 
##matrix is cached by makeCacheMatrix fucntion (is.null(m)), the cached data m 
##is return. Otherwise, proceed to compute matrix reverse value and to 
##cache the result in m, by calling tmp$setmatrix(m)

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
