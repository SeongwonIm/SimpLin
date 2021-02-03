# Wrapper with an error function
simp_lin_R = function(x, y){
  # Throw an error if x and y are not numeric vectors
  if(length(x) != length(y)){
    stop("Bad input: Vectors do not have the same length")
  }
  
  # Throw an error if x and y are not vectors
  if(length(x) < 2 | length(y) < 2){
    stop("Bad input: Input vectors must have length greater than 1")
  }
  
  # Wrap the cpp function and return the output
  output = simp_lin_cpp(x, y)
  return(output)
}