#ifndef JULIA_CPP_HPP
#define JULIA_CPP_HPP
/*
 * Utilities for making calling Julia from C++ somewhat bearable. API
 * is divided in two parts: low-level and high level. Main difference
 * is memory management.
 *
 * Low level API work with Julia types directly and it's caller
 * responsibility to ensure that Julia's GC won't snatch values from
 * under your nose.
 *
 * High level API tries to automate that process and to hide memory
 * management details as much as possible.
 */
#include <stdexcept>
#include <memory>
#include <vector>
#include <julia.h>

namespace Julia::LL::Impl {
    class GCBarrier;
}


namespace Julia::LL {
    // Exception which is thrown when it's not possible to convert
    // Julia value to C++ value. Its use should be confined to
    // Julia::Convert template
    struct ConversionError : public std::runtime_error {
        ConversionError() :
            std::runtime_error("")
        {}
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
        ~GCRootBase() {
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
}


namespace Julia {
    // C++ wrapper for Julia exceptions. Julia exceptions are
    // converted to C++ exceptions by calling rethrow.
    class Exception : public std::runtime_error {
    public:
        Exception(const char* msg);
        Exception(const std::string& msg);
    };

    // Conversion between C++ and Julia values. Specializations should
    // provide following:
    //
    // - Repr typedef which is used for storing converting value
    //   before passing it to function
    //
    // Following sttic functions for conversion:
    //
    // static Convert<T>::Repr fromJulia(jl_value_t*);
    // static jl_value_t*      toJulia(Convert<T>::Repr);
    //
    // fromJulia should signal conversion error by throwing
    // ConversionError. fromJulia shouldn't throw exceptions of any
    // other type and tojulia shouldn't throw any. This is because
    // they called below C calls tp Julia which make it impossible to
    // unwind stack.
    template<typename T>
    struct Convert {
    };

    // Specialization for double
    template<>
    struct Convert<double> {
        typedef double Repr;
        static double      fromJulia(jl_value_t*);
        static jl_value_t* toJulia  (double x) {
            return jl_box_float64(x);
        }
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
        std::shared_ptr<LL::Impl::GCBarrier> m_value;
    };

    // Initialize Julia runtime. It's safe to call this function
    // multiple times
    void initialize();

    // Check if operation raised Julia exception and convert it into
    // C++ exception of type JuliaException.
    void rethrow(const char* errmsg = 0);






    // // ================================================================
    // // Low level API
    // // ================================================================


    // // Convert C++ array into Julia array. Content of array is copied
    // // and return value should be rooted to prevent GC.
    // jl_value_t* array_from_vec(const std::vector<double>& vec);

    // // Get function from module with given name
    // jl_function_t* get_function(const char* module, const char* fun);

    // Print Julia value (primarily useful for debugging)
    void println(jl_value_t* val);
    void println(const Value& val);
};


namespace Julia::LL::Impl {
    // Wrapper function which does all required data conversion for
    // functions
    template<typename T>
    jl_value_t* wrapCallback0(void* fBox) {
        auto funPtr = reinterpret_cast<T (*)()>(fBox);
        T res = (*funPtr)();
        return Convert<T>::toJulia(res);
    }

    template<typename T, typename A>
    jl_value_t* wrapCallback1(void* fBox, jl_value_t* a) {
        auto funPtr = reinterpret_cast<T (*)(A)>(fBox);
        try {
            typename Convert<A>::Repr parA = Convert<A>::fromJulia(a);
            T res = (*funPtr)(parA);
            return Convert<T>::toJulia(res);
        } catch (const LL::ConversionError&) {
            jl_error("Cannot convert parameter");
            return 0;
        }
    }

    template<typename T, typename A, typename B>
    jl_value_t* wrapCallback2(void* fBox, jl_value_t* a, jl_value_t* b) {
        auto funPtr = reinterpret_cast<T (*)(A,B)>(fBox);
        try {
            typename Convert<A>::Repr parA = Convert<A>::fromJulia(a);
            typename Convert<B>::Repr parB = Convert<B>::fromJulia(b);
            T res = (*funPtr)(parA, parB);
            return Convert<T>::toJulia(res);
        } catch (const LL::ConversionError&) {
            jl_error("Cannot convert parameter");
            return 0;
        }
    }

    // Wrappers which is called by Julia
    jl_value_t* wrapWorker( void* wrapper, void* fun);
};

namespace Julia::LL {
    // Convert value to Julia function which could be called from
    // julia code
    template<typename T>
    jl_value_t* wrapFunction( T(*fun)() ) {
        return Impl::wrapWorker( (void*)&Impl::wrapCallback0<T>, (void*)fun );
    }

    template<typename T, typename A>
    jl_value_t* wrapFunction( T(*fun)(A) ) {
        return Impl::wrapWorker( (void*)&Impl::wrapCallback1<T,A>, (void*)fun );
    }

    template<typename T, typename A, typename B>
    jl_value_t* wrapFunction( T(*fun)(A,B) ) {
        return Impl::wrapWorker( (void*)&Impl::wrapCallback2<T,A,B>, (void*)fun );
    }
}

#endif /* JULIA_CPP_HPP */
