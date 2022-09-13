models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))


# Compute mean of every column in mtcars:
means <- mtcars %>% 
  map_dbl(mean)
means

# Determine the type of each column in starwars
type <- starwars %>% 
  map_chr(typeof)
type

# Compute the number of unique values in iris
iris %>% 
  map(unique) %>% 
  map_dbl(length)

# Generate 10 random normals from distributions with menas of -10, 0, 10 and 100
x <- c(-10, 0, 10, 100)
normals <- x %>% 
  map(rnorm, n = 10) %>% 
  as.data.frame()
colnames(normals) <- c("-10", "0", "10", "100")

# create function returning a vector indicating whether or not column in df is a factor
factors <- function(dataframe = mtcars) {
  vector <- dataframe %>% 
    map_chr(is.factor)
  return(vector)
}
factors()

# Keep columns of type numeric in babynames:
babynames::babynames %>% 
  keep(is.numeric)

# Determine whether any columns are of type "factor":
mtcars %>% 
  some(is.factor)
# there are no columns that are of type factor

x <- list(1:5, letters, list(10))
# Determine if all elements of x is of type "vector":
x %>% 
  every(is_vector)
# TRUE

# Helpful functions:
# head_while - takes elements from start of vector while condition is true
# tail_while - takes elements from end of vector while condition is true
# detect and detect_index - finds the first element / returns the position of 
# first element satisfying condition






