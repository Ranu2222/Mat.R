##Catching the Inverse of a Matrix
##Below are functions to create a special objec that stores a matrix and caches its inverse

## This fnction creates a special "matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix())
{
sub <- NULL
set <- function(y)
{
x <<- y
sub <<- NULL
}
get <- function() x
setInverse <- function(met) sub<<- met
getInverse <- function()sub
list(set= set, get = get, setInverse = setInverse,getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" created above function. 
## it sees if the inverse exists , it retrieves the inverse from the cache.
catcheSolve <- function(x,...)
{
sub  <- x$getInverse()
if(!is.null(sub))
{
message("getting catched data")
return(sub)
}
inv <- x$get()
sub <-  solve(inv,...)
x$setInverse(sub)
sub
}
