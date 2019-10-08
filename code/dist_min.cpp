// header files to include
#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
// use Rcpp namespace to avoid Rcpp::<...> repetition
using namespace Rcpp;
// compute and return minimum distance along with name
// [[Rcpp::export]]
NumericVector dist_min_rcpp(
    NumericMatrix img,
    NumericMatrix surface, 
    bool display_progress=true) {
  
  
  // init output matrix (x X 3)
  int n = img.ncol();
  int m = surface.ncol();
  // test if rows are equal
  NumericVector minvec_value(n, NA_REAL);
  NumericVector tmp(m, NA_REAL);

  Progress p(n, display_progress);
  
  // loop through each set of starting points
  if (Progress::check_abort() )
    return -1.0;
  for(int i = 0; i < n; i++) {
    p.increment(); 
    NumericVector x = img( _, i );
    for(int j = 0; j < m; j++) {
      NumericVector y = surface( _, j );
      double z = sqrt(sum(pow(x - y, 2)));
      tmp[j] = z ;
      if (z == 0) {
        break ;
      }      
    }
    // add to output matrix
    minvec_value[i] = min(na_omit(tmp));
  }
  
  // return created data frame
  return minvec_value;
}
