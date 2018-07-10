#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include <cstdint>
#include "julia.h"

#include "BAT2.hpp"
#include "julia-cpp.hpp"


// ----------------------------------------------------------------
// BAT wrappers
// ----------------------------------------------------------------

static jl_module_t* module_BAT = 0;

namespace BAT {

    Value::Value(jl_value_t* val) :
        Julia::Value(val)
    {
        if( !module_BAT ) {
            // Load BAT
            jl_eval_string("import BAT");
            Julia::rethrow();
            //
            jl_value_t* mod = jl_get_global(jl_main_module, jl_symbol("BAT"));
            if( !jl_is_module(mod) ) {
                throw BATException("BAT is not a module");
            }
            Julia::rethrow();
            module_BAT = reinterpret_cast<jl_module_t*>(mod);
        }
    }
}


// ----------------------------------------------------------------
// BAT wrappers for Julia types
// ----------------------------------------------------------------

namespace BAT {

    static jl_value_t*  makeHyperRectBounds (const std::vector<double>& low_bounds,
                                             const std::vector<double>& hi_bounds)
    {
        if( low_bounds.size() != hi_bounds.size() ) {
            throw BAT::BATException("Inequal dimensions of bounds for HyperRectBounds");
        }
        // Constants
        jl_function_t* j_con         = Julia::get_function("BAT", "HyperRectBounds");
        jl_value_t*    j_hard_bounds = Julia::eval_string("BAT.hard_bounds");
        // Variable declarations
        jl_value_t* j_arr_low = 0;
        jl_value_t* j_arr_hi  = 0;
        jl_value_t* j_res     = 0;
        Julia::GCRoot3 gc_root(&j_arr_low, &j_arr_hi, &j_res);
        // Evaluation
        j_arr_low = Julia::array_from_vec(low_bounds);
        j_arr_hi  = Julia::array_from_vec(hi_bounds);
        j_res     = Julia::call3(j_con, j_arr_low, j_arr_hi, j_hard_bounds);
        return j_res;
    }

    BAT::HyperRectBounds::HyperRectBounds(const std::vector<double>& low_bounds,
                                          const std::vector<double>& hi_bounds) :
        ParamVolumeBounds( makeHyperRectBounds(low_bounds,hi_bounds) )
    {}

    // ----------------------------------------

    static jl_value_t* makeConstDensity( const HyperRectBounds& bounds,
                                         double val)
    {
        jl_value_t*    j_val = 0;
        jl_value_t*    j_res = 0;
        Julia::GCRoot2 gc_root(&j_val, &j_res);
        jl_function_t* j_con = 0;
        //
        j_val = jl_box_float64(val);
        j_con = Julia::get_function("BAT", "ConstDensity");
        j_res = Julia::call2(j_con, bounds.juliaValue(), j_val);
        return j_res;
    }

    BAT::ConstDensity::ConstDensity(const HyperRectBounds& bounds,
                                    double val) :
        AbstractDensity(makeConstDensity(bounds, val)),
        m_bounds(bounds)
    {}

    BAT::ConstDensity::~ConstDensity() {}

    // ----------------------------------------

    static jl_value_t* makeGenericDensity(int nparam,
                                          double (*fun)(double*))
    {
        jl_function_t* j_con  = Julia::get_function("BAT", "GenericDensity");
        //
        jl_value_t* j_n    = 0;
        jl_value_t* j_fun  = 0;
        jl_value_t* j_res  = 0;
        Julia::GCRoot3 gc_root(&j_n, &j_fun, &j_res);
        //
        j_n    = jl_box_int64(nparam);
        j_fun  = Julia::make_callback(nparam, fun);
        j_res  = Julia::call2(j_con, j_fun, j_n);
        return j_res;
    }

    GenericDensity::GenericDensity(int nparam, double (*fun)(double*)) :
        AbstractDensity(makeGenericDensity(nparam, fun))
    {}

    // ----------------------------------------

    static jl_value_t* makeMCMCSpec(const AbstractDensity& likelihood,
                                    const AbstractDensity& prior)
    {
        jl_function_t* j_con = Julia::get_function("BAT","MCMCSpec");
        //
        jl_value_t* j_mh    = 0;
        jl_value_t* j_res   = 0;
        Julia::GCRoot2 gc(&j_mh, &j_res);
        //
        j_mh  = Julia::eval_string("BAT.MetropolisHastings()"); // FIXME: constant!
        j_res = Julia::call3(j_con,
                             j_mh,
                             likelihood.juliaValue(),
                             prior.juliaValue());
        return j_res;
    }
    
    BAT::MCMCSpec::MCMCSpec(const AbstractDensity& likelihood,
                            const AbstractDensity& prior) :
        Value( makeMCMCSpec(likelihood, prior) )
    {}

    
// {

//     m_value->preserve(j_res);
// }

// BAT::MCMCSpec::~MCMCSpec() {}

// // ----------------------------------------------------------------
// // Main API
// // ----------------------------------------------------------------

// BAT::MCMCIterator::MCMCIterator(const MCMCSpec& spec,
//                                 int nchains) :
//     m_nchains(nchains)
// {
//     jl_function_t* j_con = julia_get_function("BATCPP", "mcmc_create_chains");
//     //
//     jl_value_t* j_n   = 0;
//     jl_value_t* j_res = 0;
//     Julia::GCRoot2 gc_root(&j_n, &j_res);
//     //
//     j_n = jl_box_int64(nchains);
//     j_res = jl_call2(j_con, spec.julia_value(), j_n);
//     //
//     m_value->preserve(j_res);
// }

// BAT::MCMCIterator::~MCMCIterator() {
// }

// void BAT::MCMCIterator::generate(std::vector<std::vector<std::vector<double> > >& out, int nsamples) {
//     // Learn number of parameters
//     int dim;
//     {
//         jl_function_t* j_nparam = julia_get_function("BATCPP", "mcspec_nparams");
//         //
//         jl_value_t* j_r;
//         Julia::GCRoot1 gc_root(&j_r);
//         //
//         j_r = jl_call1(j_nparam, m_value->m_val);
//         if( jl_is_int32(j_r) ) {
//             dim = jl_unbox_int32(j_r);
//         } else if( jl_is_uint32(j_r) ) {
//             dim = jl_unbox_uint32(j_r);
//         } else if( jl_is_int64(j_r) ) {
//             dim = jl_unbox_int64(j_r);
//         } else if( jl_is_uint64(j_r) ) {
//             dim = jl_unbox_uint64(j_r);
//         } else {
//             throw BATException("Cannot find out dimensions");
//         }
//     }
//     // Prepare buffers
//     out.resize(m_nchains);
//     for(int i = 0; i < m_nchains; i++) {
//         out[i].resize(dim);
//         for(size_t j = 0; j < dim; j++) {
//             out[i][j].resize(nsamples);
//         }
//     }
//     //
//     // FIXME:
}


// ----------------------------------------------------------------
// Exception & Co
// ----------------------------------------------------------------

BAT::BATException::BATException(const char* msg) :
    std::runtime_error(msg)
{}

BAT::BATException::BATException(const std::string& msg) :
    std::runtime_error(msg)
{}
