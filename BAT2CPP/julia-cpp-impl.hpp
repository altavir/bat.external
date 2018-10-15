#ifndef JULIA_IMPL__HPP__
#define JULIA_IMPL__HPP__


#include <julia.h>

namespace Julia::Impl {
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

    // Prevent 4 value for being GC collected
    class GCRoot4 : public GCRootBase<4> {
    public:
        GCRoot4(jl_value_t** arg1, jl_value_t** arg2, jl_value_t** arg3, jl_value_t** arg4) {
            stack[0] = arg1;
            stack[1] = arg2;
            stack[2] = arg3;
            stack[3] = arg4;
        }
    };
};

#endif /* JULIA_IMPL__HPP__ */
