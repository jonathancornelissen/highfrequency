#ifndef highfrequencyUtils
#define highfrequencyUtils
arma::uword findFirst(arma::vec& x , const int thresh);
bool overlap(double min1, double max1, double min2, double max2);
arma::mat colCumsum(const arma::mat& x);
#endif