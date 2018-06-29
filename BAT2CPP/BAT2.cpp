#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include <cstdint>
#include <boost/noncopyable.hpp>
#include "julia.h"

#include "BAT2.hpp"
#include "julia-cpp.hpp"


// ----------------------------------------------------------------
// function helpers
// ----------------------------------------------------------------


static jl_function_t* julia_get_function(const char* module, const char* fun) {    
    // Module lookup
    jl_value_t* j_modval = jl_get_global(jl_main_module, jl_symbol(module));
    if( !j_modval ) {
        throw BAT::JuliaException("No module returned");
    }
    if( !jl_is_module(j_modval) ) {
        throw BAT::JuliaException("Not a module");
    }
    jl_module_t* j_mod = reinterpret_cast<jl_module_t*>(j_modval);
    // Function lookup
    jl_value_t* j_funval = jl_get_global(j_mod, jl_symbol(fun));
    if( !j_funval ) {
        throw BAT::JuliaException("No function found");
    }
    // FIXME: find out typeof!
    // if( !jl_is_method_instance(j_funval) ) {
    //     throw BAT::JuliaException("Not a method");
    // }
    return reinterpret_cast<jl_function_t*>(j_funval);
}

// Throw exception on Julia error
static void throw_on_jl_error(const char* errmsg = 0) {
    jl_value_t* val = jl_exception_occurred();
    if( val ) {
        jl_function_t* f = jl_get_function(jl_base_module, "showerror");
        jl_call2(jl_get_function(jl_base_module, "showerror"),
                 jl_stderr_obj(),
                 jl_exception_occurred());
        jl_printf(jl_stderr_stream(), "\n");
        //
        std::string msg = std::string("Julia error: ") + jl_typeof_str(val);
        if( errmsg ) {
            msg += " [";
            msg += errmsg;
            msg += "]";
        }
        throw BAT::JuliaException(msg);
    }
}

// Create Julia array from C++ vector. Returned value is not rooted
static jl_value_t* julia_array_from_vec(const std::vector<double>& vec) {
    // Allocate array
    jl_value_t* ty  = jl_apply_array_type((jl_value_t*)jl_float64_type, 1);
    throw_on_jl_error("Type generation");
    jl_array_t* arr = jl_alloc_array_1d(ty, vec.size());
    throw_on_jl_error("Array allocation");
    // Fill allocated array with data
    double *data = (double*)jl_array_data(arr);
    for(int i = 0; i < vec.size(); i++) {
        data[i] = vec[i];
    }
    return (jl_value_t*)arr;
}

// Call exit hook
static void finalize_julia() {
    jl_atexit_hook(0);
}


// ----------------------------------------------------------------
// Internals
// ----------------------------------------------------------------

namespace BAT {
    namespace Impl {
        // Class which is used to keep Julia values alive by putting
        // them into top-level map
        class GCBarrier {
        public:
            GCBarrier() :
                m_val(0)
            {}

            int preserve(jl_value_t* v) {
                assert(m_val == 0 && "GCBarrier::preserve must be called at most once");
                m_val = v;
                //
                jl_function_t* j_fun = julia_get_function("BATCPP", "gc_ref");
                jl_call1(j_fun, m_val);
                throw_on_jl_error();
                return 0;
            }

            ~GCBarrier() {
                if( m_val ) {
                    jl_function_t* j_fun = julia_get_function("BATCPP", "gc_unref");
                    jl_call1(j_fun, m_val);
                }
            }

            void test() {
                jl_function_t* f = jl_get_function(jl_main_module, "println");
                jl_call1(f, m_val);
            }

            jl_value_t* m_val;
        };
       
    }
}


BAT::JuliaValue::JuliaValue() :
    m_value( new BAT::Impl::GCBarrier() )
{
    Julia::initialize();
}

BAT::JuliaValue::~JuliaValue()
{}

jl_value_t* BAT::JuliaValue::julia_value() const {
    return m_value->m_val;
}


BAT::HyperRectBounds::HyperRectBounds(const std::vector<double>& low_bounds,
                                      const std::vector<double>& hi_bounds)
{
    if( low_bounds.size() != hi_bounds.size() ) {
        throw BAT::BATException("Inequal dimensions of bounds for ConstDensity");
    }
    int m_dim = low_bounds.size(); // FIXME;
    jl_function_t* j_con         = julia_get_function("BAT", "HyperRectBounds");
    jl_value_t*    j_hard_bounds = jl_eval_string("BAT.hard_bounds");
    // Variable declarations
    jl_value_t* j_arr_low = 0;
    jl_value_t* j_arr_hi  = 0;
    jl_value_t* j_res     = 0;
    Julia::GCRoot3 gc_root(&j_arr_low, &j_arr_hi, &j_res);
    //
    j_arr_low = julia_array_from_vec(low_bounds);
    j_arr_hi  = julia_array_from_vec(hi_bounds);
    // FIXME: we hardcode type of bounds
    j_res     = jl_call3(j_con, j_arr_low, j_arr_hi, j_hard_bounds);
    //
    m_value->preserve(j_res);
}

BAT::ConstDensity::ConstDensity(const HyperRectBounds& bounds,
                                double val) :
    m_bounds(bounds)
{
    jl_value_t*    j_val = 0;
    jl_value_t*    j_res = 0;
    Julia::GCRoot2 gc_root(&j_val, &j_res);
    jl_function_t* j_con = 0;
    //
    j_val = jl_box_float64(val);
    j_con = julia_get_function("BAT", "ConstDensity");
    j_res = jl_call2(j_con, m_bounds.julia_value(), j_val);
    //
    m_value->preserve(j_res);
    m_value->test();
}

BAT::ConstDensity::~ConstDensity() {}


BAT::GenericDensity::GenericDensity(int nparam, double (*fun)(double*)) {
    jl_function_t* j_con  = julia_get_function("BAT",    "GenericDensity");
    jl_function_t* j_wrap = julia_get_function("BATCPP", "wrap_c_call");
    //
    jl_value_t* j_n    = 0;
    jl_value_t* j_fptr = 0;
    jl_value_t* j_fun  = 0;
    jl_value_t* j_res  = 0;
    
    Julia::GCRoot4 gc_root(&j_n, &j_fptr, &j_fun, &j_res);
    //
    j_n    = jl_box_int64(nparam);
    j_fptr = jl_box_voidpointer((void*)fun);
    j_fun  = jl_call2(j_wrap, j_n, j_fptr);
    j_res  = jl_call2(j_con, j_fun, j_n);
    //
    m_value->preserve(j_res);
}

BAT::GenericDensity::~GenericDensity() {}


BAT::MCMCSpec::MCMCSpec(const AbstractDensity& likelihood,
                        const AbstractDensity& prior)
{
    jl_function_t* j_con = julia_get_function("BAT","MCMCSpec");
    //
    jl_value_t* j_mh    = 0;
    jl_value_t* j_res   = 0;
    Julia::GCRoot2(&j_mh, &j_res);
    //
    j_mh  = jl_eval_string("BAT.MetropolisHastings()");
    j_res = jl_call3(j_con, j_mh,
                     likelihood.julia_value(),
                     prior.julia_value());
    //
    m_value->preserve(j_res);
}

BAT::MCMCSpec::~MCMCSpec() {}

// ----------------------------------------------------------------
// Main API
// ----------------------------------------------------------------

BAT::MCMCIterator::MCMCIterator(const MCMCSpec& spec,
                                int nchains) :
    m_nchains(nchains)
{
    jl_function_t* j_con = julia_get_function("BATCPP", "mcmc_create_chains");
    //
    jl_value_t* j_n   = 0;
    jl_value_t* j_res = 0;
    Julia::GCRoot2 gc_root(&j_n, &j_res);
    //
    j_n = jl_box_int64(nchains);
    j_res = jl_call2(j_con, spec.julia_value(), j_n);
    //
    m_value->preserve(j_res);
}

BAT::MCMCIterator::~MCMCIterator() {
}

void BAT::MCMCIterator::generate(std::vector<std::vector<std::vector<double> > >& out, int nsamples) {
    // Learn number of parameters
    int dim;
    {
        jl_function_t* j_nparam = julia_get_function("BATCPP", "mcspec_nparams");
        //
        jl_value_t* j_r;
        Julia::GCRoot1 gc_root(&j_r);
        //
        j_r = jl_call1(j_nparam, m_value->m_val);
        if( jl_is_int32(j_r) ) {
            dim = jl_unbox_int32(j_r);
        } else if( jl_is_uint32(j_r) ) {
            dim = jl_unbox_uint32(j_r);
        } else if( jl_is_int64(j_r) ) {
            dim = jl_unbox_int64(j_r);
        } else if( jl_is_uint64(j_r) ) {
            dim = jl_unbox_uint64(j_r);
        } else {
            throw BATException("Cannot find out dimensions");
        }
    }
    // Prepare buffers
    out.resize(m_nchains);
    for(int i = 0; i < m_nchains; i++) {
        out[i].resize(dim);
        for(size_t j = 0; j < dim; j++) {
            out[i][j].resize(nsamples);
        }
    }
    //
    // FIXME: 
}


// ----------------------------------------------------------------
// Exception & Co
// ----------------------------------------------------------------

BAT::JuliaException::JuliaException(const char* msg) :
    std::runtime_error(msg)
{}

BAT::JuliaException::JuliaException(const std::string& msg) :
    std::runtime_error(msg)
{}

BAT::BATException::BATException(const char* msg) :
    std::runtime_error(msg)
{}

BAT::BATException::BATException(const std::string& msg) :
    std::runtime_error(msg)
{}
