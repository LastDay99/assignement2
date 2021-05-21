makematrix <- function(M=matrix()){
        inv <- NULL
        set <- function(y) {
                M <<- y
                inv <<- NULL
        }
get <- function () M
setinv <- function(inverse) inv <<- inverse
getinv <- function () inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


cacheinv <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

# a little exemple to show you the result

#matr <- makematrix(matrix(c(1,0,0,2,1,0,3,3,3),ncol=3,nrow=3))
#matr$get()
#cacheinv(matr) to solve the matrix
#cacheinv(matr) this time with the message


