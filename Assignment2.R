## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse

  makeCacheMatrix <-function(a=matrix())
  {
    j<-NULL
    set <-function(b)
    {
      a<<-b
      j<<-NULL
    }
    get<-function()a
    setInverse<-function(inverse)j<<-inverse
    getInverse<-function()j
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
  }
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

  cacheSolve <- function(a,...)
  {
    ##Return a matrix that is inverse of 'a'
    j<-a$getInverse()
    if(!is.null(j))
    {
      message("getting cached data")
      return (j)
    }
    mat<-a$get()
    j<-solve(mat,...)
    a$setInverse(j)
    j
  }
  