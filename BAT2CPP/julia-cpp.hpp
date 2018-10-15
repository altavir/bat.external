#ifndef JULIA_CPP_HPP
#define JULIA_CPP_HPP
/*
 * Utilities for making calling Julia from C++ somewhat bearable
 */
#include <stdexcept>
#include <memory>
#include <vector>
#include <julia.h>

#include "julia-cpp-impl.hpp"

namespace Julia {
    class GCBarrier;

    // ================================================================
    // High level API
    // ================================================================

    // Convert Julia error into exception
    class Exception : public std::runtime_error {
    public:
        Exception(const char* msg);
        Exception(const std::string& msg);
    };

    // Represents pointer to value in Julia heap. It won't be garbage
    // collected as long as object is alive.
    class Value {
    public:
        // Create value from value in Julia heap.
        explicit Value(jl_value_t*);
        // Get pointer to Julia value. Use with care!
        jl_value_t* juliaValue() const;
    private:
        std::shared_ptr<GCBarrier> m_value;
    };


    // Initialize Julia runtime. It's safe to call this function
    // multiple times
    void initialize();

    // Check if operation raised Julia exception and convert it into
    // C++ exception of type JuliaException.
    void rethrow(const char* errmsg = 0);


    
    // ================================================================
    // Low level API
    // ================================================================


    // Convert C++ array into Julia array. Content of array is copied
    // and return value should be rooted to prevent GC.
    jl_value_t* array_from_vec(const std::vector<double>& vec);

    // Get function from module with given name
    jl_function_t* get_function(const char* module, const char* fun);

    // Create Julia function from C functions
    jl_function_t* make_callback(int nparam, double (*fun)(double*));

    // Call function with 1 parameter and throw exception if something
    // goes wrong
    jl_value_t* call1(jl_function_t* fun,
                      jl_value_t* a);

    // Call function with 1 parameter and throw exception if something
    // goes wrong
    jl_value_t* call2(jl_function_t* fun,
                      jl_value_t* a,
                      jl_value_t* b);

    // Call function with 1 parameter and throw exception if something
    // goes wrong
    jl_value_t* call3(jl_function_t* fun,
                      jl_value_t* a,
                      jl_value_t* b,
                      jl_value_t* c);

    // Evaluate strings
    jl_value_t* eval_string(const char* str);

    // Print Julia value (primarily useful for debugging)
    void println(jl_value_t* val);
    void println(const Value& val);
};
#endif /* JULIA_CPP_HPP */
