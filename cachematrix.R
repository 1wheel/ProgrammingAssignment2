#returns a list of four functions that get and set a matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
  #cached inverse stored in variable captured by closure
  inverse <- NULL
  
  #change the stored matrix and reset the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #reture the stored martix
  get <- function() x
  
  #change the stored inverse
  setinverse <- function(inverseSetVal) inverse <<- inverseSetVal
  
  #return stored inverse
  getinverse <- function() inverse
  
  #returned list contains all the getter/setter functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#passed the result of makeCacheMatrix, returns the matrix inverse
#if the inverse has already been calculated, the cached value is returned
#otherwise, the calculate the inverse, cache it and return it
cacheinverse <- function(x, ...) {
  
  ##look up the cached inverse
  inverse <- x$getinverse()
  
  #if it exists, we're done
  if(!is.null(inverse)) {
    message("inverse cached")
 
  } else{
   #otherwise, retrive the matrix
    data <- x$get()
    
    #calculate its inverse
    inverse <- solve(data, ...)
    
    #cache the inverse
    x$setinverse(inverse)    
  }
  inverse
}
