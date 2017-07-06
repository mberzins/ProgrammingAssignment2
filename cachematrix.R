#This is a function than generates a vector of functions
#to provide access for setting and getting the the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    #initialize empty matrix
    inv_matrix <- numeric()
    #function for setting the matrix outside the context
    setmat <- function(y){
        x <<- y
        inv_matrix <<- numeric();
    }
    #function fort geting the matrix
    getmat <- function() x
    #function for setting the inverse outside of the context
    setinverse <- function(inverse) inv_matrix <<- inverse
    #function for getting the inverse
    getinverse <- function() inv_matrix
    #names of the list
    list(setmat = setmat, getmat = getmat, setinverse = setinverse, getinverse = getinverse)
}


## Function that returns inverse from memory
## If it does not exist - calculates it

cacheSolve <- function(cm, matrix) {
        #check if the matrix has not changed
        if (identical(cm$getmat(),matrix) == TRUE) {
            #try getting inverse from memory
            inv_matrix <- cm$getinverse()
            #return the inverse, if it is not empty (not treating non-invertible cases)
            if (length(inv_matrix) > 0){
                message("getting cached data")
                return(inv_matrix)
            }
        }
        #if it is empty or the matrices are not identical, get the data, calculate the inverse and return
        #set the matrix first
        cm$setmat(matrix)
        #get the matrix back
        mat <- cm$getmat();
        message("calculating inverse")
        #invert the matrix
        inv_matrix <- solve(mat)
        #set the inverse
        cm$setinverse(inv_matrix)
        inv_matrix
}
