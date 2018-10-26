#ifndef JULIA_IMPL__HPP__
#define JULIA_IMPL__HPP__
// Lower level API for calling Julia. It works with bare pointer to
// Julia value so one needs to take care to avoid GC collection of
// value
#include <julia.h>
#include <iostream>
#include <string>

namespace Julia {
    // Convert Julia error into exception
    class Exception : public std::runtime_error {
    public:
        Exception(const char* msg);
        Exception(const std::string& msg);
    };

    // ----------------------------------------------------------------

    // Template "class" for data types which could be directly
    // converted to and from Julia values
    template<typename T>
    struct Convert {
        static jl_value_t*        juliaType()     = delete;
        static const std::string& juliaTypeName() = delete;
    };

    template<>
    struct Convert<double> {
        static jl_value_t*        juliaType();
        static const std::string& juliaTypeName();
    };
}

namespace Julia::Impl {
    // ----------------------------------------------------------------

    // Base class for keeping Julia values alive. Note that unlike
    // Julia C macros several such value could be in same scope.
    template<int N>
    struct GCRootBase {
        GCRootBase() {
            n    = (2 * N + 1);
            prev = jl_pgcstack;
            jl_pgcstack = (jl_gcframe_t*)this;
        }
        ~GCRootBase() {
            std::cout << "TOP  = " << (void*)(jl_pgcstack) << '\n';
            std::cout << "PREV = " << (void*)(jl_pgcstack->prev) << '\n';            
            jl_pgcstack = jl_pgcstack->prev;
        }
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

    // ----------------------------------------------------------------

    // Wrappers which rethrow Julia exceptions as C++ exceptions
    jl_value_t* call(jl_function_t*);
    jl_value_t* call(jl_function_t*, jl_value_t*);
    jl_value_t* call(jl_function_t*, jl_value_t*, jl_value_t*);
    jl_value_t* call(jl_function_t*, jl_value_t*, jl_value_t*, jl_value_t*);

    // Create Julia tuple 
    jl_value_t* make_tuple();
    jl_value_t* make_tuple(jl_value_t*);
    jl_value_t* make_tuple(jl_value_t*, jl_value_t*);
    jl_value_t* make_tuple(jl_value_t*, jl_value_t*, jl_value_t*);
    
    // ----------------------------------------------------------------
    jl_value_t* wrap_c_function_0_worker(
        jl_value_t*        c_funptr,
        const std::string& return_ty
        );
    jl_value_t* wrap_c_function_1_worker(
        jl_value_t*        c_funptr,
        const std::string& return_ty,
        const std::string& paramA
        );
    jl_value_t* wrap_c_function_2_worker(
        jl_value_t*        c_funptr,
        const std::string& return_ty,
        const std::string& paramA,
        const std::string& paramB
        );

    template<typename R>
    jl_value_t* wrap_c_function_0(R (*fun)()) {
        // Types
        const std::string& tyR = Convert<R>::juliaTypeName();
        //
        jl_value_t* r      = 0;
        jl_value_t* funptr = 0;
        Impl::GCRoot2(&r, &funptr);
        //
        funptr = jl_box_voidpointer((void*)fun);
        r      = wrap_c_function_0_worker(funptr, tyR);
        return r;
    }

    template<typename R, typename A>
    jl_value_t* wrap_c_function_1(R (*fun)(A)) {
        // Types
        const std::string& tyR = Convert<R>::juliaTypeName();
        const std::string& tyA = Convert<A>::juliaTypeName();
        //
        jl_value_t* r      = 0;
        jl_value_t* funptr = 0;
        Impl::GCRoot2(&r, &funptr);
        //
        funptr = jl_box_voidpointer((void*)fun);
        r      = wrap_c_function_1_worker(funptr, tyR, tyA);
        return r;
    }

    template<typename R, typename A, typename B>
    jl_value_t* wrap_c_function_2(R (*fun)(A,B)) {
        // Types
        const std::string& tyR = Convert<R>::juliaTypeName();
        const std::string& tyA = Convert<A>::juliaTypeName();
        const std::string& tyB = Convert<A>::juliaTypeName();
        //
        jl_value_t* r      = 0;
        jl_value_t* funptr = 0;
        Impl::GCRoot2(&r, &funptr);
        //
        funptr = jl_box_voidpointer((void*)fun);
        r      = wrap_c_function_2_worker(funptr, tyR, tyA, tyB);
        return r;
    }


};

#endif /* JULIA_IMPL__HPP__ */
