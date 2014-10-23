## The purpose of the following functions is to reduce, where possible, the 
## computation time used in inverting a matrix. Rather than performing the 
## calculation each time it is requested, the information is cached from the 
## first calculation and simply retrieved if required subsequently.

##The makeCacheMatrix function creates a list containing a function to get 
##and set the values for the matrix (as specified by the user) and its inverse
##(when calculated by the cacheSolve function)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinverse <- function(solve) m <<- solve
    getinverse <- function()m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function first looks up the cached matrix created above to
## determine if a value already exists for the inverse of the matrix. If it 
## does, the function provides a message of "getting cached data" before 
## returning the previously-calculated answer. If the inverse does not already
## exist, the Solve() function is used to calculate it, and it is then 
## set as the inverse in the cached matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
