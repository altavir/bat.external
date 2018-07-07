#include <stdlib.h>
#include <iostream>
#include <vector>
#include <julia.h>

#include "BAT2.hpp"
#include "julia-cpp.hpp"

double testfun(double* xs) {
    return xs[0]*1000 + xs[1];
}


int main(int argc, char *argv[])
{
    std::vector<double> lo = {-10,-10};
    std::vector<double> hi = {10,10};

    Julia::initialize();
    std::cout << ((void*)jl_pgcstack) << std::endl;
    {
        // jl_value_t* n = 0;
        // GCRoot1 root(&n);
        Julia::Value val1( jl_box_int64(100) );
        std::cout << (void*)val1.juliaValue() << std::endl;

        Julia::Value val2( jl_box_int64(102) );
        std::cout << (void*)val2.juliaValue() << std::endl;
        
        Julia::Value val3( jl_box_float64(1.222) );        
        std::cout << (void*)val3.juliaValue() << std::endl;
        Julia::Value val4( val3.juliaValue() );
        std::cout << (void*)val3.juliaValue() << std::endl;

    }
    {
        Julia::Value val3( jl_box_float64(1.222) );        
        std::cout << (void*)val3.juliaValue() << std::endl;
    }
    // BAT::HyperRectBounds bounds(lo,hi);
    // BAT::ConstDensity    density(bounds);
    // BAT::GenericDensity  likelihood(2, &testfun);
    // BAT::MCMCSpec spec(likelihood, density);
    //----
    // BAT::MCMCIterator mit(spec, 2);
    return 0;
}    
