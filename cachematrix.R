## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first part of the function defines the null variables to store the inverse and normal matrix
#The purpose of the code is to reduce to time it takes to make the computations by using the cache
makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(x1){
        x<<-x1
        inv<<-NULL
      }
      get<-function(){x}
      setInv<-function(inverse)inv<<-inverse
      getInv<-function()inv
      list(set=set,get=get,setInv=setInv,getInv=getInv)
      }
## Write a short comment describing this function
#After making the matrix I use the x$get() function to print the original matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setInv(inv)
  inv
  }
#To get the desired results I call on the makeCacheMatrix function to make the matrix
#And then I use the cacheSolve function that returns the inverse.