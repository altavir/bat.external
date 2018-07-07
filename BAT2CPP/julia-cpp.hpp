#ifndef JULIA_CPP_HPP
#define JULIA_CPP_HPP
/*
 * Utilities for making calling Julia from C++ somewhat bearable
 */
#include <stdexcept>
#include <memory>
#include <julia.h>

namespace Julia {

    // Convert Julia error into exception
    class Exception : public std::runtime_error {
    public:
        Exception(const char* msg);
        Exception(const std::string& msg);
    };


    class GCBarrier;

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

    // Base class for keeping Julia values alive. Note that unlike
    // Julia C macros several such value could be in same scope.
    template<int N>
    struct GCRootBase {
        GCRootBase() {
            n    = (2 * N + 1);
            prev = jl_pgcstack;
            jl_pgcstack = (jl_gcframe_t*)this;
        }
        ~GCRootBase() { jl_pgcstack = jl_pgcstack->prev; }
        intptr_t     n;             // Number of values in array (fudged)
        void*        prev;          // Previous block
        jl_value_t** stack[N];      // Values to preserve

        GCRootBase(const GCRootBase&)             = delete;
        GCRootBase& operator= (const GCRootBase&) = delete;
    };

    // Prevent 1 value for being GC collected
    struct GCRoot1 : public GCRootBase<1> {
        GCRoot1(jl_value_t** arg1) {
            stack[0] = arg1;
        }
    };

    // Prevent 2 value for being GC collected
    struct GCRoot2 : public GCRootBase<2> {
        GCRoot2(jl_value_t** arg1, jl_value_t** arg2) {
            stack[0] = arg1;
            stack[1] = arg2;
        }
    };

    // Prevent 3 value for being GC collected
    class GCRoot3 : public GCRootBase<3> {
    public:
        GCRoot3(jl_value_t** arg1, jl_value_t** arg2, jl_value_t** arg3) {
            stack[0] = arg1;
            stack[1] = arg2;
            stack[2] = arg3;
        }
    };

    // Prevent 2 value for being GC collected
    class GCRoot4 : public GCRootBase<4> {
    public:
        GCRoot4(jl_value_t** arg1, jl_value_t** arg2, jl_value_t** arg3, jl_value_t** arg4) {
            stack[0] = arg1;
            stack[1] = arg2;
            stack[2] = arg3;
            stack[3] = arg4;
        }
    };

    // Initialize Julia runtime. It's safe to call this function
    // multiple times
    void initialize();

    // Check if operation raised Julia exception and convert it into
    // C++ exception of type JuliaException.
    void rethrow(const char* errmsg = 0);

};
#endif /* JULIA_CPP_HPP */
