// Use the RcppArmadillo package
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
Rcpp::List simp_lin_cpp(arma::vec x, arma::vec y) {
  double n = x.n_elem; // Sample size
  double mu_x = mean(x);
  double mu_y = mean(y);
  
  // Denominator/Numerator of computing the estimated slope
  double denom = 0;
  double numer = 0;
  for(int i = 0; i < n; i++){
    denom += (x[i] - mu_x)*(x[i] - mu_x);
    numer += (x[i] - mu_x)*(y[i] - mu_y);
  }
  
  // Compute the slope and the intercept using the slope
  double b1 = numer / denom;
  double b0 = mu_y - b1*mu_x;
  
  // Predicted values & Residuals
  arma::vec predicted = b0 + b1*x; // make a vector to store predicted values
  arma::vec residuals = y - predicted; // Make a vector to store residuals
  double SSE = arma::dot(residuals, residuals); // Define Sum of Squared Error for computing standard error later
  
  // Standard Error and the 95% confidence interval
  double MSE = SSE / (n - 2);
  double b1_se = sqrt(MSE/denom);
  double b0_se = sqrt(MSE*(1/n + (mu_x*mu_x)/denom));
  
  double cv = R::qt(0.025, n - 2, FALSE, FALSE); // critical value
  arma::vec b0_ci = {b0 - cv*b0_se, b0 + cv*b0_se};
  arma::vec b1_ci = {b1 - cv*b1_se, b1 + cv*b1_se};
  
  //Make vectors to return as a list
  arma::vec coefficients = {b0, b1};
  arma::vec se = {b0_se, b1_se};
  
  // Return a list
  return List::create(Rcpp::Named("Coefficients") = coefficients,
                      Rcpp::Named("StandardError") = se,
                      Rcpp::Named("b0_95CI") = b0_ci,
                      Rcpp::Named("b1_95CI") = b1_ci,
                      Rcpp::Named("Residuals") = residuals,
                      Rcpp::Named("Predicted") = predicted);
  
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
/*** R
simp_lin_cpp(c(1, 2, 3, 4, 5), c(2, 5, 10, 11, 21))
*/
