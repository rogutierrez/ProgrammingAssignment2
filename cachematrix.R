## Las siguientes funciones almacenan cálculos en la memoria caché de forma que se pueden
## utilizar en cálculos posteriores.

## La función crea una lista para estableser (set) o obtener (get) una matriz y su inversa.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(
    set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


## Esta función calcula la inversa de una matriz cuadrada si es que no está el cálculo en
## la caché. Si el cálculo está, simplemente lo utiliza sin realizar cálculos extra.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
