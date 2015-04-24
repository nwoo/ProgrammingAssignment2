## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix<-function(x=matrix()){
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve)x<<-solve
        
## The solve function is used to solve for the inverse of object x        

        getinverse<-function()x
        list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}
##
## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function

## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function should retrieve the inverse from the cache

cacheSolve<-function(x,...){
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
