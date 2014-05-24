##This program creates two functions. The main function, cacheSolve checks
##to see if a matrix inverse has been cached, based on the object methods
##created in makeCacheMatrix. If it has, the function will find it and
##show it. Otherwise, an inverse will be computed and cached.

##Many parts of this program are reproduced from the code provided to us
##by the instructors. Appropriate changes were made to achieve the desired
##assignment outcome.

##This function will create all the methods for our matrix object
makeCacheMatrix <- function(x = matrix()) {
  
  ##Initially, no inverse has been computed. We will keep track of changes in inv
  inv <- NULL
  
  ##Build the internal matrix value 
  set <- function(mat){
    x <<- mat
    inv <- NULL
  }
  
  ##A sub-function that simply returns the existing matrix
  get <- function() x
  
  ##Change the functions internal value of inv to the value of inverse, once it is available
  setInverse <- function(inverse) inv <<- inverse
  
  ##Return the value of the inverse that is attached to the matrix, x
  getInverse <- function() inv
  
  ##Set-up the list of methods available on x
  list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
  
}


##This function will either return the existing inverse, or it will
##compute one, if not yet cached.
cacheSolve <- function(x, ...){
  
  ##Grab the inverse of x using the getInverse() method, if it exists. If it doesn't yet exist, a NULL will be returned
  inv <- x$getInverse()
  
  ##Notify user that it has been cached if it, in fact, has
  if(!is.null(inv)){
    message("getting cached Inverse value")
    return(inv)
    
    ##If a NULL is returned for the inverse of the passed matrix, have R compute it and store it into cache  
  } else {
    
    ##Grab and store the matrix
    matrix <- x$get()
    
    ##Compute the inverse and store it
    Inversecomputed <- solve(matrix)
    
    ##Call the method on this inverse that will set it in cache so it doesn't need to be computed again if accessed later
    x$setInverse(Inversecomputed)
    
    ##Return the computed inverse matrix
    Inversecomputed
    
    
    
  }
  
  
}