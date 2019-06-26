#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


//[[Rcpp::export]]
NumericVector qsaxC(const double &n){
  NumericVector tmp(n + 1);
  for(int i = 0; i <= n; i++){
    tmp[i] = i/n;
  }
  return qnorm(tmp);
}

// saxsymbC: symbolize the series so that it can be used for SAX.

//[[Rcpp::export]]
NumericVector saxsymbC(const NumericVector &s, const double &a)
{


  // Note that series "s" will need to be standardized to compare against std. normal quantiles.
  NumericVector qlist = qsaxC(a);   // If you break normal dist into "a" evenly spaced quatiles.
  int ns = s.size();
  NumericVector symbols(ns);
  for (int i = 0; i < ns; i++){

    // Check the number of quantiles smaller than si.
    int nq = 0;
    for(int ii = 0; ii < a; ii++){
      // increment if this element of qlist is smaller than the current si.
      nq = nq + (qlist[ii] < s[i]);
    }

    // The -1 is inserted here because they are used as array indices.
    //  In R, if we use quantile 1, it will be -Inf.  But in C that's quantile 0.
    symbols[i] = nq - 1;
  }
  return symbols;
}

// mindistC: Determine SAX distance between two series which are represented in paa.
//  In: psx = paa & symbolization applied to series x,
//      psy = paa & symbolization applied to series y,
//      a = size of the alphabet, n = original size of the series before paa applied.
//  Out: scalar distance between series
//

//[[Rcpp::export]]
double mindistC(const NumericVector &psx, const NumericVector &psy,
                const double &a, const double &n)
{
  double w = psx.size();                // Extract word length
  NumericVector qlist = qsaxC(a);
  double ssd = 0;
  for (int i = 0; i < w; i++)
  { // for each word,
    int xi = psx[i];
    int yi = psy[i];
    double qrng = 0;
    if (abs(xi - yi) > 1){  // if PAA symbolized series differ by more than 1 (not adjacent).
      if(xi > yi){  // subtract the smaller series' quantile from the larger series quantile.
        qrng = qlist[psx[i]] - qlist[psy[i] + 1];
      }else{
        qrng = qlist[psy[i]] - qlist[psx[i] + 1];
      }

    }
    // Accumulate the square of the difference in PAA symbolized series.
    ssd = ssd + qrng*qrng;
  }

  return sqrt( (n / w) * ssd);
}


//[[Rcpp::export]]
NumericVector paaC(const NumericVector &s, const double &w)
{
  double n = s.size();
  NumericVector paa(w);

  // Currently, no custom parameters were allowed, so word size is basically fixed to 6.
  // if ((w - floor(w)) > 0 || w <= 0) stop("Word length w must be a positive integer.");

  double d = n / w;
  NumericVector bp(w + 1);
  for (int i = 0; i <= w; i++){
    bp[i] = d*i;
  }
  for (int ii = 0; ii < w; ii++){
    double start = bp[ii];       // Start at the breakpoint.
    double end   = bp[ii + 1];   // End at the next breakpoint.

    // These are some details for later.  Partial W (not a divisor of series length).
    // double decstart = ceil(start) - start;
    // double decend   = end - floor(end);

    // Does this need to be floor or ceiling for interval length?
    // Creating sequence start:end
    int intlen = ceil(end) - floor(start);
    NumericVector interval(intlen);
    double intervalsum = 0;
    for (int iii = 0; iii < intlen; iii++){
      intervalsum = intervalsum + s[start + iii];
    }
    paa[ii] = intervalsum / intlen;
  }

  return paa;
}


//[[Rcpp::export]]
bool naC(const NumericVector &x){
  return is_true(any(is_na(x)));
}


//[[Rcpp::export]]
double dsaxC(const NumericVector &x, const NumericVector &y,
             const double &w = 6, const double &alpha = 9)
{
  // ParallelDist does not allow parameters such as W and A to be passed in.

  // Error checking
  // Check for dimension match (needs to be a single column or row vector.)
  if(x.size() != y.size()) stop("Input series must be of equal length.");

  if(w > x.size()) stop("Word length w is not allowed to exceed series length.");

  // Check for NAs.
  if(naC(x) || naC(y)) stop("NA values are not allowed in input series.");

  if ((w - floor(w)) > 0 || w <= 0) stop("Word length w must be a positive integer.");

  // Center and scale the vector.
  NumericVector sx = (x - mean(x)) / sd(x);
  NumericVector sy = (y - mean(y)) / sd(y);


  // TBD: error checking.
  double nx = x.size();
  NumericVector px = paaC(sx, w);
  NumericVector py = paaC(sy, w);


  NumericVector psx = saxsymbC(px, alpha);
  NumericVector psy = saxsymbC(py, alpha);

  double saxdist = mindistC(psx, psy, alpha, nx);
  // return saxdist;
  return saxdist;
}


//[[Rcpp::export]]
NumericVector saxvec(const NumericVector &x, const NumericMatrix &m,
                     const double &w = 6, const double &alpha = 9)
{
  double n = m.nrow();
  NumericVector svec(n);
  for(int i = 0; i < n; i++){
    svec[i] = dsaxC(x, m(i , _), w, alpha);
  }
  return svec;
}


//[[Rcpp::export]]
double deucC(const NumericVector &x, const NumericVector &y)
{
  double n = x.size();
  double ss = 0;  // sum of squares
  // NumericVector dvec(n);
  for(int i = 0; i < n; i++){
    ss += pow(x[i] - y[i], 2.0);
  }
  return pow(ss, 0.5);
}


//[[Rcpp::export]]
NumericVector eucvec(const NumericVector &x, const NumericMatrix &m)
{
  double n = m.nrow();
  NumericVector svec(n);
  for(int i = 0; i < n; i++){
    svec[i] = deucC(x, m(i , _));
  }
  return svec;
}


//[[Rcpp::export]]
double dcplxC(const NumericVector &x, const NumericVector &y)
{
  double n = x.size();
  double sdsx = 0;  // sum of square differences, x
  double sdsy = 0;  // sum of square differences, y

  for(int i = 0; i < (n - 1); i++){
    // Difference the series, square the differences, sum them, take sqrt.
    sdsx += pow(x[i] - x[i + 1], 2.0);
    sdsy += pow(y[i] - y[i + 1], 2.0);
  }
  double sqx = pow(sdsx, 0.5);
  double sqy = pow(sdsy, 0.5);
  return (max(NumericVector::create(sqx, sqy)) / min(NumericVector::create(sqx, sqy)));
}


// Calculate complexity for center against other series.

//[[Rcpp::export]]
NumericVector cplxvec(const NumericVector &x, const NumericMatrix &m)
{
  double n = m.nrow();
  NumericVector svec(n);
  for(int i = 0; i < n; i++){
    svec[i] = dcplxC(x, m(i , _));
  }
  return svec;
}


//[[Rcpp::export]]
double dcidC(const NumericVector &x, const NumericVector &y)
{
  return(dcplxC(x, y) * deucC(x, y));

}


//[[Rcpp::export]]
NumericVector cidvec(const NumericVector &x, const NumericMatrix &m)
{
  double n = m.nrow();
  NumericVector svec(n);
  for(int i = 0; i < n; i++){
    svec[i] = dcidC(x, m(i , _));
  }
  return svec;
}


//[[Rcpp::export]]
NumericMatrix eucdm(const NumericMatrix &m)
{
  double rows = m.nrow();
  // double cols = m.cols();
  NumericMatrix mat(rows, rows);
  for(int r = 0; r < rows; r++){
    // Rcout << "Row: " << r << std::endl;
    for(int c = r; c < rows; c++){
      // Rcout << "Column: " << c << std::endl;
      mat(r, c) = deucC(m(r, _), m(c, _));
      mat(c, r) = mat(r, c);
    }
  }

  return mat;
}


//[[Rcpp::export]]
NumericMatrix saxdm(const NumericMatrix &m,
                    const double &w = 6, const double &alpha = 9)
{
  double rows = m.nrow();

  NumericMatrix mat(rows, rows);
  for(int r = 0; r < rows; r++){
    for(int c = r; c < rows; c++){
      mat(r, c) = dsaxC(m(r, _), m(c, _), w, alpha);
      mat(c, r) = mat(r, c);
    }
  }

  return mat;
}


//[[Rcpp::export]]
NumericMatrix ciddm(const NumericMatrix &m)
{
  double rows = m.nrow();

  NumericMatrix mat(rows, rows);
  for(int r = 0; r < rows; r++){
    for(int c = r; c < rows; c++){
      mat(r, c) = dcidC(m(r, _), m(c, _));
      mat(c, r) = mat(r, c);
    }
  }
  return mat;
}
