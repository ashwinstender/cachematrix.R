
#---makeCacheMatrix 
#The following function can be used to create a matrix object. The function will request the matrix, gets and as well
#sets the inverse of the specific matrix. The product is a list of functions.
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse) 
}


#---cacheSolve
#The "cacheSolve" function as seen below, computes the inverse of the cached matrix object. When the matrix isn't NULL, 
#a "getting cached data" message is returned whereafter it will return the inverse of the matrix. 
cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null (inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve (mat, ...)
    x$setInverse(inv)
    inv
}

testMatrix <- matrix(c(1, 3, 5, 6, 7, 8, 9, 0, 2), nrow = 3, ncol = 3)
