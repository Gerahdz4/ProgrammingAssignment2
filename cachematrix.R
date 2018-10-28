## Put comments here that give an overall description of what your
## functions do
 #Cache the inverse of a matrix using the scoping rules of R
#in order to make more efficient the inverse calculus
  

## Write a short comment describing this function

#MakeCacheMatrix: Takes a matrix for argument and set the inverse matrix to NULL
#set: Takes a matrix for argument and assigns to x with <<- because is in the parent environment
#get: construct the desired function with the help of gets and sets

makeCacheMatrix <- function(x = matrix()) {
inversa<- NULL
    set<-function(y){
  
       x <<- y
   inversa<- NULL
      
    }
    get <- function() x
    setsolve <<- function(solve) inversa <<- solve
    getsolve <- function() inversa
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
#cacheSolve: takes a matrix for argument from the output of makeCacheMatrix
#cheks if the inverse is empty and if it is calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversa <- x$getsolve()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setsolve(inversa)
  inversa
}
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
