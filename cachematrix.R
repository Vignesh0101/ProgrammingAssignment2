

## Set function assigns the inverse of a matrix to a global vector via "<<-" operator (making a cache)

makeCacheMatrix <- function(x = matrix()) 
{
  invmat <- NULL
  set <- function(y)
  {
    x <<- y
    invmat <<- NULL
  }
  #Get matrix function
  get <- function() x
  
  #Set inverse of the matrix function
  setinverse <- function(solve) invmat <<- solve
  getinverse <- function() invmat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## If inverse is already available, it returns it else calculates the inverse

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
  #get the inverse of the matrix
  inversemat <- x$getinverse()
  
  #check if the cached value exists
  if(!is.null(inversemat))
  {
    message("Getting cached Inverse Matrix")
    return(inversemat)
  }
  
  data <- x$get()
  #if the cached value does not exist, do the inverse.
  inversemat <- solve(data, ...)
  x$setinverse(inversemat)
  inversemat
}
