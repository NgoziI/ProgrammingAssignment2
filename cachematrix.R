## Put comments here that give an overall description of what your
## functions do

#Below are two functions that are used to create a special object that stores a Matrix and cache its inverse in an environment
#that is different from the current environment.

## Write a short comment describing this function

#This function creates a special "matrix" object and can cache its inverse.The special "matrix" object is really a list containing functions to

#set the value of the Matrix
#get the value of the Matrix
#set the inverse of the Matrix
#get the inverse of the Matrix
#For this assignment, we are assuming that the matrix supplied (that is the x input) is always invertible.
makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(Inverse) I <<- Inverse
        getInverseMatrix <- function() I
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
        
        
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. It first checks to see if the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve function retrieves the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets it in the cache via the setInverseMatrix function.
cacheSolve <- function(x,...) {
        InverseM <- x$getInverseMatrix()
        if(!is.null(InverseM)) {
                message("getting cached Inverse Matrix")
                return(InverseM)
        }
        Matrice <- x$get()
        InverseM <- solve(Matrice)
        x$setInverseMatrix(InverseM)
        
        ## Return a matrix that is the inverse of 'x'
        InverseM
}