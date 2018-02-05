#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly. The assignment is to write a pair of functions that cache the inverse of a matrix.

#set the value of the matrix
#get the value of the matrix
#set the value of inverse of the matrix
#get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
    
}

#The following function return the inverse of matrix created with the above function. 
#However, it first checks to see if the mean has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates inverse the data and sets the value of inverse of matrix in the cache via the setinv function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}



#To test the function
#test 1
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))


my_matrix$get()
my_matrix$getinv()
cacheSolve(my_matrix)

cacheSolve(my_matrix)
#getting cached data


#test 2
my_matrix2 <- makeCacheMatrix(matrix(c(1, 0, 5, 2, 1, 6, 3, 5, 0), 3, 3))

my_matrix2$get()
my_matrix2$getinv()
cacheSolve(my_matrix2)
