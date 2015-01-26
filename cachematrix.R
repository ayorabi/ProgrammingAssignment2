## A function to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #intialise the variable
  inv <-NULL
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(solve) inv<<-solve
  getinv<-function()inv
  list(set =set, get = get, setinv=setinv, getinv=getinv)

}

## A function to retrieve the cached inverse if found otherwise calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data,...)
  x$setinv(inv)
  inv
}
