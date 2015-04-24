
##need a solvable matrix like this
##a<- matrix( c(3,5,9,14), 2, 2)

##If you run x<-makeCacheMatrix(a) then x$set(y) puts y, a vector into a hidden x
##x$get() returns that hidden x, x$setmean and x$getmean does the sam for a hidden m
makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<- function (y) {
x<<- y
m<<-NULL
}
get<- function() x
setinverse <- function(solve) m<<-solve
getinverse <- function() m
list(set = set, get = get, 
  setinverse =setinverse, 
  getinverse = getinverse)

}

##cacheSolve(x) runs the functions to get the hidden m if it has been cached for the 
##   variable passed to it.
##   else it puts the mean of x (the vector passed to it) into the hidden m to initialize
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
