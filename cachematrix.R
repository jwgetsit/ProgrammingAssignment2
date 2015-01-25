#The two functions below allow caching of potentially time-consuming computation of calculating the inverse.

#makeCacheMatrix creates a special "vector", which is a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   
        m <- NULL
        setmat <- function(y) {
            my_matrix <<- y
            m <<- NULL
        }
        getmat <- function() my_matrix
        setinverse_mat <- function(inverse_mat) m <<- inverse_mat
        getinverse_mat <- function() m
        list(setmat = setmat, getmat = getmat,
        setinverse_mat = setinverse_mat,
        getinverse_mat = getinverse_mat)
    

}

# cacheSolvecalculates the inverse of the special "vector" created with the above function. However, it first checks to see if the inverse has already been calculated and that the matrix has not changed. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the matrix and the inverse in the cache.


cacheSolve <- function(x, current_matrix = matrix(), ...) {
    
    data <- x$getmat()
    m <- x$getinverse_mat()
    if (missing(current_matrix)) {
        current_matrix <- data
    }
    if(!is.null(m) && identical(data, current_matrix)) {
        message("getting cached data")
        return(m)
    }
    
    m <- solve(current_matrix, ...)
    x$setmat(current_matrix)
    x$setinverse_mat(m)
    m # Return a matrix that is the inverse of 'current_matrix'
    
}
