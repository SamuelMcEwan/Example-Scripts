library(Rcpp)
library(rbenchmark)

##### Fibonacci Sequence #####
# Create a function named 'f' in base R which finds the nth term in a Fibonacci Sequence
f <- function(n) {
  if (n < 2) return(n)
  return(f(n-1) + f(n-2))
}

# Create the same function named 'g' with C++ 
cppFunction(
  'int g(float n) { 
      if (n < 2) return(n); 
      return(g(n-1) + g(n-2));
  }'
)

# Test both functions by finding the first 11 terms of the Fibonacci Sequence
sapply(0:10, f)
sapply(0:10, g)

# Compare computation speeds using rbenchamark
benchmark(h(33), g(33))[,1:4] # C++ runs 100x quicker


##### Cumulative Sum #####
# Create a function named "cumsumR" in base R which calculates the cumulative sum of a vector
cumsum2 <- function(x){
  acc <- 0
  res <- rep(0, length(x))
  for(i in 1:length(x)){
    acc <- acc + x[i]
    res[i] <- acc
  }
  return(res)
}

# Create the same function in C++
cppFunction(
  'NumericVector cumsum1(NumericVector x){
    double acc = 0;
    NumericVector res(x.size());
    for(int i = 0; i < x.size(); i++){
      acc += x[i];
      res[i] = acc;
    }
    return res;
    }'
)

# Test both functions by finding the cumulative sum of the first 10 digits
example_vector <- 1:10
cumsum1(example_vector)
cumsum2(example_vector)

# Compare compuational speeds of base R and C++
example_vector <- rnorm(100000)
benchmark(cumsum1(example_vector), cumsum2(example_vector))[,1:4] # C++ runs 20x quicker
