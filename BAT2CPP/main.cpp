#include <stdlib.h>
#include <iostream>
#include <vector>
#include <julia.h>

#include "BAT2.hpp"
#include "julia-cpp.hpp"

double testfun(double* xs) {
    return xs[0]*1000 + xs[1];
}

double test_add(double a, double b) {
    return a + b;
}

double test_sqr(double a) {
    return a * a;
}

int main(int argc, char *argv[])
{
    // std::vector<double> lo = {-10,-10};
    // std::vector<double> hi = {10,20};

    Julia::initialize();

    {
        jl_value_t* a = 0;
        jl_value_t* b = 0;
        jl_value_t* f = 0;
        jl_value_t* r = 0;
        Julia::Impl::GCRoot4 root(&a, &b, &f, &r);
        
        a = jl_box_float64(12);
        b = jl_box_float64(100000);
        f = Julia::Impl::wrap_c_function_2( &test_add );

        Julia::println(f);

        // std::cout << "--------------\n";
        // b = jl_get_global(jl_main_module, jl_symbol("typeof"));
        // r = jl_call1((jl_function_t*)b, a);
        // Julia::println(r);
        // r = jl_call1((jl_function_t*)b, b);
        // Julia::println(r);
        // r = jl_call1((jl_function_t*)b, f);
        // Julia::println(r);
        // std::cout << "--------------\n";
        // {
        //     jl_value_t* p;
        //     p = Julia::Impl::get_fun_ccall();
        //     std::cout << jl_is_method(p) << '\n';
        //     std::cout << jl_is_method_instance(p) << '\n';
        // }
        // std::cout << "--------------\n";

        // // Julia::println(f);
        r = Julia::Impl::call( (jl_function_t*)f, a, b);
        Julia::println(r);
        
    }
    
    // jl_function_t* j_fun = 0;
    // jl_value_t* two = 0;
    // jl_value_t* f   = 0;
    // Julia::GCRoot3 gc((jl_value_t**)&j_fun, &two, &f);

    // two = jl_box_int64(2);
    // f   = jl_box_voidpointer((void*)&testfun);
    // j_fun = jl_eval_string("function (n :: Int, f :: Ptr{Void})\n"
    //                        "  function fun(xs :: Array{Float64})\n"
    //                "    if( n != length(xs))\n"
    //                "      error(\"Sizes do not match\")\n"
    //                "    end\n"
    //                "    ccall(f, Float64, (Ref{Float64},), xs)\n"
    //                "  end\n"
    //                "  fun\n"
    //                "end");
    // Julia::rethrow();

    
    // jl_function_t* j_wrapped = 0;
    // jl_value_t* arr   = 0;
    // jl_value_t* res   = 0;
    // Julia::Impl::GCRoot3 gc3((jl_value_t**)&j_wrapped, &arr, &res);

    // arr = Julia::array_from_vec(hi);
    // Julia::rethrow();
    
    // j_wrapped = Julia::make_callback(2, testfun);
    // res = jl_call1(j_wrapped, arr);
    // Julia::rethrow();
    // Julia::println(res);
        


    
    // BAT::Value( jl_box_int64(101) );
    
    // std::cout << ((void*)jl_pgcstack) << std::endl;
    // {
    //     // jl_value_t* n = 0;
    //     // GCRoot1 root(&n);
    //     Julia::Value val1( jl_box_int64(100) );
    //     std::cout << (void*)val1.juliaValue() << std::endl;

    //     Julia::Value val2( jl_box_int64(102) );
    //     std::cout << (void*)val2.juliaValue() << std::endl;
        
    //     Julia::Value val3( jl_box_float64(1.222) );        
    //     std::cout << (void*)val3.juliaValue() << std::endl;
    //     Julia::Value val4( val3.juliaValue() );
    //     std::cout << (void*)val3.juliaValue() << std::endl;

    // }
    // {
    //     Julia::Value val3( jl_box_float64(1.222) );        
    //     std::cout << (void*)val3.juliaValue() << std::endl;
    // }
    // BAT::HyperRectBounds bounds(lo,hi);
    // BAT::ConstDensity    density(bounds);
    // BAT::GenericDensity  likelihood(2, &testfun);
    // BAT::MCMCSpec spec(likelihood, density);
    //----
    // BAT::MCMCIterator mit(spec, 2);
    return 0;
}    
