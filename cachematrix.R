## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function take a matrix as an arguement 
## and returns a list of 4 functions. Internally it has 2 vars
## x: a matrix 
## inv: an inverted version of it (calculated and set from outside this function)
## 
## set(y): sets the x matrix
## get(): returns x matrix
## setinv(): sets the inverse of x
## getinv(): returns the inverse of x (note that unless it was set using setinv(), it will return default value NULL)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
   
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(calc_inv) inv <<- calc_inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# 
# This function finda an inverse of matrix x
# But the way it does it is, it uses the list of functions returned by makeCacheMatrix
# First it calls the getinv() func. If it returns NA -> no cached results. So it calculates it
#      If it returns a real value then it uses that instead of re-calculating it

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

a <- matrix(sample(1:100, 100), nrow=10)

print ("original matrix")
print (a)
b <- makeCacheMatrix(a)

c <- cacheSolve(b)
print ("inverted matrix")
print (c)

print ("inverted matrix")
c1 <- cacheSolve(b)
print (c1)




