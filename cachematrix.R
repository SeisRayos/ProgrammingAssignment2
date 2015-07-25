

#makeCacheMatrix returns a list of functions that can be used to store a matrix and it's inverse.
# It's assumed that the matrix passed as input is invertible



makeCacheMatrix <- function(x = matrix()) {
# initialize the inverse
  inv <- NULL
# define function to save the input matrix
  set <- function(y) {
    x <<- y  
    inv <<- NULL 
  }
#define function to retreive the matrix
  get <- function() x
#define function to cache the matrix inverse
  setinv <- function(inverse) inv <<- inverse
#define function to retrieve the cached inverse matrix
  getinv <- function() inv

  list( set = set, get = get,
        setinv= setinv,
        getinv = getinv)

}


## Function to return the inverse of the matrix argument passed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("returning cached data")
    
    return(inv)
  }
#No inverse cached yet 
  mat.data <- x$get()
# calculating and caching the matix inverse
  inv <- solve(mat.data, ...)
  x$setinv(inv)
  print(inv)
  inv
}


test <-  function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}



