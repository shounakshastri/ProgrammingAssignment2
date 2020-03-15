## This function creates a matrix which should be stored and used for calculating the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    ## This is a second function nested inside the makeCacheMatrix(). This function is used to initialize 'inv'
    ## to NULL. Here we use <<- operator to assign value of y to x as the scopes or environments of the two 
    ## variables are different4
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  

}


## This function first checks the matrix. If it finds the matrix in the cache, then it takes the inverse from the
## cache, thus making the program faster. If not, then it calculates the inverse using the solve() function.
cacheSolve <- function(x, ...) {
       
  inv <- x$get_inv()
  if(!is.null(inv)) { ## Check if the matrix is stored in the cache
    message("getting cached data")
    return(inv)## Return a matrix that is the inverse of 'x'
    
  }
  data <- x$get()
  inv <- solve(data, ...)  ## Calculate the inverse of the matrix because it is not in the cache.
  x$set_inv(inv)
  inv
}
