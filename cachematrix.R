# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(i) inverse <<- i
  get_inverse <- function() inverse
  list(set= set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  
  # If inverse is cached or already computed then return it directly
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  # If inverse is not available then compute using solve() and store/cache its value
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}