## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
#implement makeCacheMatrux by copying the form of makeVector function in example
#return value is a list consisting of 4 function 

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function()x
        setsol<-function(solve)s<<-solve
        getsol<-function()s
        list(set=set, get=get, setsol=setsol, getsol=getsol)

}


## Write a short comment describing this function
#if cacheSolve has been executed, print a message "getting cached data"
#otherwise, compute the inverse matrix of x, and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsol()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setsol(s)
        s
}
