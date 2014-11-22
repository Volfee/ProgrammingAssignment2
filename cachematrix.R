## This functions allows to create object that stores matrix and it's inversion in order to 
## get this inversion quickly, when it's inverted vaue has to be used several times.

## This function creates the matrix with the ability to store (cache) it's inversion.
##  *  x is an argument that takes the matrix object.
##  *  It is assumed that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        
        invertedMatrix <- NULL
        
        set <- function(y){
                x <<- y
                invertedMatrix <<- NULL
        }
        
        get <- function() x
        setInverted <- function(inverted) invertedMatrix <<- inverted
        getInverted <- function() invertedMatrix
        
        list(set = set, get = get, setInverted=setInverted, getInverted=getInverted)
}


## This function returns the inverted matrix of given matrix. 
## If the inversion was already calculated it returns the cached value.
## x is an argument that takes the list returned by previous function.

cacheSolve <- function(x, ...) {
        
        invertedMatrix <- x$getInverted()
        
        if(!is.null(invertedMatrix))
        {
                message("getting cached data")
                return(invertedMatrix)
        }
        
        matrix <- x$get()
        invertedMatrix <- solve(matrix)
        x$setInverted(invertedMatrix)
        
        invertedMatrix
}
