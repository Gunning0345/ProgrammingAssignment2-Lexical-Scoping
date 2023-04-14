##this function is used to create matrix (x) that can be cached by its inverse
##here, x = the object that the user will use to submit via the console

makeCacheMatrix <- function(x = matrix()) 
{

  j <- NULL
  set <- function(y)
    
      {x <<- y
      j <<- NULL}
  
  
  get <- function()x
  
  setInverse <- function(inverse) j <<- inverse
  
  getInverse <- function() j 
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##This is the function used to actually compute the inverse of our
##"makeCacheMatrix" function. If the inverse was already calculated
##(and the matrix was not changed), then it will retrieve the inverse
##via the cache

cacheSolve <- function(x, ...) 
{
  
  ## To return a matrix that is the inverse of 'x'
  
  j <- x$getInverse()
  if(!is.null(j))         
    
     {message("getting cached data")
       return(j)}
  
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
  
}

