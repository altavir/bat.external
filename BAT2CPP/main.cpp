#include <stdlib.h>
#include <iostream>
#include <vector>
#include <julia.h>

#include "BAT2.hpp"


double testfun(double* xs) {
    return xs[0]*1000 + xs[1];
}


int main(int argc, char *argv[])
{
    std::vector<double> lo = {-10,-10};
    std::vector<double> hi = {10,10};
    BAT::HyperRectBounds bounds(lo,hi);
    BAT::ConstDensity    density(bounds);

    BAT::GenericDensity  likelihood(2, &testfun);

    BAT::MCMCSpec spec(likelihood, density);
    //----
    BAT::MCMCIterator mit(spec, 2);
    return 0;
}    
