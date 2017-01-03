## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix())
{
  # Inverse cached matrix
  inv_mat <- NULL
  
  # set the matrix
  set <- function(y) 
  {
    x <<- y
    
    # make the inverse matrix null if its a new matrix
    inv_mat <<- NULL
  }
    # set the matrix
    
    get <- function() x
    
    # set the inverse of the matrix  
    setinv <- function(inverse) inv_mat <<-inverse
      
    # get the inverse of the matrix  
    getinv <- function() inv_mat
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)

{

    inv_mat <- x$getinv()
  
    #return the inverse matrix form cache If inverse matrix is already calculated
    
    if (!is.null(inv_mat)) 
  
      {
        message("getting cached matrix")
        return(inv_mat)
      } 
    
    else 
    
      {
        #If it is a new matrix then calculate matrix inverse and cache it
        inv_mat <- solve(x$get())
      
        x$setinv(inv_mat)
      
        return(inv_mat)
      
      }
}
