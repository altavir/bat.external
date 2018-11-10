#include "julia-cpp.hpp"

#include <iostream>

// Well.. with no better option let turn to CPP
#define CHECK_UNBOX_INT(n) \
    if( jl_typeis(val, jl_int##n##_type) ) { return jl_unbox_int##n(val); }
#define CHECK_UNBOX_UINT(n) \
    if( jl_typeis(val, jl_uint##n##_type) ) { return jl_unbox_uint##n(val); }


// Set of Julia values which should be prevented from GC
static jl_value_t*     g_barrier = 0;
// GC root for value above. Here it's allocated on heap
static Julia::LL::GCRoot1* gc_root   = 0;
// Julia globals. They're set during initialization
static jl_value_t* fun_get      = 0;
static jl_value_t* fun_setindex = 0;
static jl_value_t* fun_detete   = 0;
static jl_value_t* fun_plus     = 0;
static jl_value_t* fun_print    = 0;
static jl_value_t* fun_typeof   = 0;
//
static jl_value_t* fun_callback2 = 0;

// Call exit hook
static void finalize_julia() {
    jl_atexit_hook(0);
    // FIXME: delete gc_root while making sure we won't mess up stack
    //        of GC roots
}

// Put value to global map of IDs
static void stash_value(jl_value_t* val) {
    jl_value_t* zero = 0;
    jl_value_t* one  = 0;
    jl_value_t* n    = 0;
    Julia::LL::GCRoot4 gc(&zero, &one, &n, &val);
    // Constants;
    zero = jl_box_int64(0);
    one  = jl_box_int64(1);
    // Increase counter in map preventing GC by 1
    n = jl_call3(fun_get, g_barrier, val, zero); Julia::rethrow();
    n = jl_call2(fun_plus, n, one);              Julia::rethrow();
    jl_call3(fun_setindex, g_barrier, n, val);   Julia::rethrow();
}

// Decrease refcount to Julia value to prevent its garbage collection
static void throw_away(jl_value_t* val) {
    jl_value_t* zero = 0;
    jl_value_t* one  = 0;
    jl_value_t* n    = 0;
    Julia::LL::GCRoot3 gc(&zero, &one, &n);
    // Constants;
    zero = jl_box_int64(0);
    one  = jl_box_int64(-1);
    // Decrease counter and remove value from map
    //
    // NOTE: We must not throw exceptions from destructor.
    //       Throwing in the destructor is absolutely
    //       haram!
    n = jl_call3(fun_get, g_barrier, val, zero);
    if( jl_unbox_int64(n) <= 1 ) {
        jl_call2(fun_detete, g_barrier, val);
    } else {
        n = jl_call2(fun_plus, n, one);
        jl_call3(fun_setindex, g_barrier, n, val);
    }
}

// Evaluate value and put it global table to prevent GC.
static jl_value_t* eval_and_stash(const char* expr) {
    jl_value_t* x = 0;
    Julia::LL::GCRoot1 gc(&x);
    x = jl_eval_string(expr); Julia::rethrow();
    stash_value(x);
    return x;
}


// ================================================================
// Implementation details
// ================================================================
namespace Julia::LL::Impl {
    // Internal class which puts Julia value into ObjectIdDict to
    // prevent their garbage collection.
    class GCBarrier {
    public:
        GCBarrier(jl_value_t* val);
        ~GCBarrier();
        jl_value_t *m_value;
    private:
        GCBarrier(const GCBarrier&)            = delete;
        GCBarrier& operator=(const GCBarrier&) = delete;
    };


    GCBarrier::GCBarrier(jl_value_t* val) :
        m_value(val)
    {
        stash_value(val);
    }

    GCBarrier::~GCBarrier() {
        throw_away(m_value);
    }


    jl_value_t* wrapWorker( void* callback, void* fBox)
    {
        jl_value_t* funWrapper = 0;
        jl_value_t* funReal    = 0;
        jl_value_t* closure    = 0;
        GCRoot3 gc(&funWrapper, &funReal, &closure);

        funWrapper = jl_box_voidpointer( (void*)callback );
        funReal    = jl_box_voidpointer( fBox );
        closure    = jl_call2( (jl_function_t*)fun_callback2,
                               funWrapper,
                               funReal);
        return closure;
    }
}

namespace Julia {
    using LL::GCRoot1;
    using LL::GCRoot2;
    using LL::GCRoot3;
    using LL::GCRoot4;

    double Convert<double>::fromJulia(jl_value_t* val) {
        if( jl_typeis(val, jl_float32_type) ) {
            return jl_unbox_float32(val);
        }
        if( jl_typeis(val, jl_float64_type) ) {
            return jl_unbox_float64(val);
        }
        throw LL::ConversionError();
    }

    float Convert<float>::fromJulia(jl_value_t* val) {
        if( jl_typeis(val, jl_float32_type) ) {
            return jl_unbox_float32(val);
        }
        throw LL::ConversionError();
    }

    uint8_t Convert<uint8_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_UINT(8);
        throw LL::ConversionError();
    }
    int8_t Convert<int8_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_INT (8);
        throw LL::ConversionError();
    }

    uint16_t Convert<uint16_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_INT (8);
        CHECK_UNBOX_UINT(8);
        CHECK_UNBOX_UINT(16);
        throw LL::ConversionError();
    }
    int16_t Convert<int16_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_INT (8);
        CHECK_UNBOX_UINT(8);
        CHECK_UNBOX_INT (16);
        throw LL::ConversionError();
    }

    uint32_t Convert<uint32_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_INT (8);
        CHECK_UNBOX_UINT(8);
        CHECK_UNBOX_INT (16);
        CHECK_UNBOX_UINT(16);
        CHECK_UNBOX_UINT(32);
        throw LL::ConversionError();
    }
    int32_t Convert<int32_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_INT (8);
        CHECK_UNBOX_UINT(8);
        CHECK_UNBOX_INT (16);
        CHECK_UNBOX_UINT(16);
        CHECK_UNBOX_INT (32);
        throw LL::ConversionError();
    }

    uint64_t Convert<uint64_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_INT (8);
        CHECK_UNBOX_UINT(8);
        CHECK_UNBOX_INT (16);
        CHECK_UNBOX_UINT(16);
        CHECK_UNBOX_INT (32);
        CHECK_UNBOX_UINT(32);
        CHECK_UNBOX_UINT(64);
        throw LL::ConversionError();
    }
    int64_t Convert<int64_t>::fromJulia(jl_value_t* val) {
        CHECK_UNBOX_INT (8);
        CHECK_UNBOX_UINT(8);
        CHECK_UNBOX_INT (16);
        CHECK_UNBOX_UINT(16);
        CHECK_UNBOX_INT (32);
        CHECK_UNBOX_UINT(32);
        CHECK_UNBOX_INT (64);
        throw LL::ConversionError();
    }


    Value::Value(jl_value_t* val) :
        m_value(new LL::Impl::GCBarrier(val))
    {}

    jl_value_t* Value::juliaValue() const {
        return m_value->m_value;
    }

    void initialize() {
        if( jl_is_initialized() )
            return;

        // Initialize Julia and register finalization code
        jl_init();
        gc_root = new GCRoot1(&g_barrier);
        atexit(finalize_julia);
        // Get addresses of global functions
        fun_get      = jl_get_global(jl_main_module, jl_symbol("get"));
        fun_setindex = jl_get_global(jl_main_module, jl_symbol("setindex!"));
        fun_detete   = jl_get_global(jl_main_module, jl_symbol("delete!"));
        fun_plus     = jl_get_global(jl_main_module, jl_symbol("+"));
        fun_print    = jl_get_global(jl_main_module, jl_symbol("println"));
        fun_typeof   = jl_get_global(jl_main_module, jl_symbol("typeof"));
        // Create GC barrier
        g_barrier = jl_eval_string("IdDict{Any,Int64}()"); rethrow();
        fun_callback2 = eval_and_stash(
            "(wrapper,fun) -> (a,b) -> ccall(wrapper, Any, (Ptr{Cvoid},Any,Any), fun,a,b)");
    }

    void rethrow(const char* errmsg) {
        jl_value_t* val = jl_exception_occurred();
        if( val ) {
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
            throw Exception(msg);
        }
    }

    // ----------------------------------------------------------------

    jl_value_t* array_from_vec(const std::vector<double>& vec) {
        jl_value_t* ty  = 0;
        jl_array_t* arr = 0;
        GCRoot2 gc(&ty, (jl_value_t**)&arr);
        // Allocate array.
        ty  = jl_apply_array_type((jl_value_t*)jl_float64_type, 1);
        Julia::rethrow("Julia::array_from_vec: Type generation");
        arr = jl_alloc_array_1d(ty, vec.size());
        Julia::rethrow("Julia::array_from_vec: Array allocation");
        // Fill allocated array with data
        double *data = (double*)jl_array_data(arr);
        for(size_t i = 0; i < vec.size(); i++) {
            data[i] = vec[i];
        }
        return (jl_value_t*)arr;
    }

    jl_function_t* get_function(const char* module, const char* fun) {
        // Module lookup
        jl_value_t* j_modval = jl_get_global(jl_main_module, jl_symbol(module));
        if( !j_modval ) {
            throw Julia::Exception("No module returned");
        }
        if( !jl_is_module(j_modval) ) {
            throw Julia::Exception("Not a module");
        }
        jl_module_t* j_mod = reinterpret_cast<jl_module_t*>(j_modval);
        // Function lookup
        jl_value_t* j_funval = jl_get_global(j_mod, jl_symbol(fun));
        if( !j_funval ) {
            throw Julia::Exception("No function found");
        }
        // FIXME: find out typeof!
        // if( !jl_is_method_instance(j_funval) ) {
        //     throw Julia::Exception("Not a method");
        // }
        return reinterpret_cast<jl_function_t*>(j_funval);
    }

    void println(jl_value_t* val) {
        jl_call1(fun_print, val);
    }

    void println(const Value& val) {
        jl_call1(fun_print, val.juliaValue());
    }
}


Julia::Exception::Exception(const char* msg) :
    std::runtime_error(msg)
{}

Julia::Exception::Exception(const std::string& msg) :
    std::runtime_error(msg)
{}
