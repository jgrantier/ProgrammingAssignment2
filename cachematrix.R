## cachematrix.R provides a function that stores a given matrix in a function 
## so that it can be retrieved and inverted with internal functions. Calculating
## the inverse takes processing time, and this way each matrix created only needs
## to be inverted once.

## makeCacheMatrix stores a matrix (passed in the function or reset using setMatrix)
## and its inverse (once it is calculated with cacheSolve).
## There are four functions included:
##  setMatrix - to change the values of a matrix object already created
##  getMatrix - to print (or use) the current value of the matrix
##  setInverse - to set the inverse value of the matrix (in this case by cacheSolve)
##  getInverse - to print (or use) the current value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                                   
                        #sets/resets inverse to zero when function is first run
      setMatrix <- function(y) {                     
            x <<- y    
            inv <<- NULL                             
                        #resets inverse to zero since matrix value has changed
      }
      getMatrix <- function() x
                        
      setInverse <- function(computedInv) inv <<- computedInv
                        #sets inv to value passed in function
                        #in our case this value is computed in cacheSolve()
      getInverse <- function() inv
                       
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
      
}


##cacheSolve returns the inverse of a matrix stored in the makeCacheMatrix function
##above.  If the matrix has already been solved, it just grabs and returns the
#inverse cached with the getInverse function.  Otherwise, it solves for the inverse
##and then saves the value with setInverse so that it can quickly be retrieved from
##cache next time.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
                        #gets the inv of x if it has already been computed/saved
      
      ##if there is a value saved in inv, returns it and exits function
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
                       
      }
      
      ##if there is not a value saved in the function
      
      currentMatrix <- x$getMatrix()
                        #get the current matrix
      inv <- solve(currentMatrix, ...)
                        #calculate the inverse with R's solve function
      x$setInverse(inv)
                        #sets the inverse to be used or recalled later
      inv
                        #returns inv

}
