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
