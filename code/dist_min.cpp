// header files to include
#include <Rcpp.h>
// use Rcpp namespace to avoid Rcpp::<...> repetition
using namespace Rcpp;
// compute and return minimum distance along with name
// [[Rcpp::export]]
NumericVector dist_min_rcpp(
    NumericMatrix img,
    NumericMatrix surface) {
  
  // init output matrix (x X 3)
  int n = img.ncol();
  int m = surface.ncol();
  // test if rows are equal
  NumericVector minvec_value(n, NA_REAL);
  NumericVector tmp(m, NA_REAL);
  
  // loop through each set of starting points
  for(int i = 0; i < n; i++) {
    NumericVector x = img( _, i );
    for(int j = 0; j < m; j++) {
      NumericVector y = surface( _, j );
      double z = sqrt(sum(pow(x - y, 2)));
      tmp[j] = z ;
    }
    // add to output matrix
    minvec_value[i] = min(tmp);
  }
  
  // return created data frame
  return minvec_value;
}
