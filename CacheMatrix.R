## These functions make it more computationally efficient to
## calculate the inverse of a matrix by creating a cached matrix
## that can be called instead of re-calculating an inverse

makeCacheMatrix <- function(x=matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInv<-function(Inv) m <<- Inv
  getInv<-function() m
  list(set = set, get = get,setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
  m<-x$getInv()
  if (!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m<-solve(data, ...)
  x$setInv(m)
  m
}