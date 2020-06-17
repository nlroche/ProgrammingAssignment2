## Steps to test:
## 1. Run all of the below code.
## 2. Create a matrix and call it A:
##       A <- matrix(c(4,2,7,6),2,2)
## 3. Create an object that stores the matrix, an inverse, and setters/getters
##       cm <- makeCacheMatrix(A)
## 4. Calculate the inverse of the matrix:
##       cacheSolve(cm)
## 5. Use true matrix multiplication on the matrix and its inverse (expecting the identify matrix):
##       cm$get() %*% cm$getinverse()



## Overall Purpose:
## This pair of functions reduces repeated execution of a function on the same data, in this
## case calculating the inverse of a matrix. This can speed up computation e.g. in loops.



## The first function (makeCacheMatrix) creates:
##  1. the supplied matrix,
##  2. an empty placeholder for the matrix's inverse, and
##  3. an object containing getter and setter functions to access or change the above two items.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The second function calculates the inverse of the matrix created in the first function, but only
## if the inverse is not already cached. It calls getter and setter functions from the object created
## by the first function.

cacheSolve <- function(x, ...) {

   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}