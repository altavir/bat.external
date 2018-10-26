#include "julia-cpp.hpp"

#include <iostream>

// Set of Julia values which should be prevented from GC
static jl_value_t*     g_barrier = 0;
// GC root for value above. Here it's allocated on heap
static Julia::Impl::GCRoot1* gc_root   = 0;
// Julia globals. They're set during initialization
static jl_value_t* fun_get      = 0;
static jl_value_t* fun_setindex = 0;
static jl_value_t* fun_detete   = 0;
static jl_value_t* fun_plus     = 0;
static jl_value_t* fun_print    = 0;
static jl_value_t* fun_typeof   = 0;
static jl_value_t* fun_tuple    = 0;
//
static jl_value_t* ty_Float64   = 0;


// const char* prog_callback_c_raw =
//     "function (n :: Int, f :: Ptr{Void})\n"
//     "  function fun(xs :: Array{Float64})\n"
//     "    if( n != length(xs))\n"
//     "      error(\"Sizes do not match\")\n"
//     "    end\n"
//     "    ccall(f, Float64, (Ref{Float64},), xs)\n"
//     "  end\n"
//     "  fun\n"
//     "end";

// Call exit hook
static void finalize_julia() {
    jl_atexit_hook(0);
    // FIXME: delete gc_root while making sure we won't mess up stack
    //        of GC roots
}

static void stash_value(jl_value_t* val) {
    // Just in case we put parameter into GC root to prevent its
    // accidental collection
    jl_value_t* zero = 0;
    jl_value_t* one  = 0;
    jl_value_t* n    = 0;
    Julia::Impl::GCRoot4 gc(&zero, &one, &n, &val);
    // Constants;
    zero = jl_box_int64(0);
    one  = jl_box_int64(1);
    // Increase counter in map preventing GC by 1
    n = jl_call3(fun_get, g_barrier, val, zero); Julia::rethrow();
    n = jl_call2(fun_plus, n, one);              Julia::rethrow();
    jl_call3(fun_setindex, g_barrier, n, val);   Julia::rethrow();
}


// ================================================================
// Implementation details
// ================================================================
namespace Julia::Impl {

    jl_value_t* call(jl_function_t* fun) {
        jl_value_t* r = 0;
        GCRoot1 gc(&r);
        r = jl_call0(fun);
        rethrow();
        return r;
    }

    jl_value_t* call(jl_function_t* fun, jl_value_t* a) {
        jl_value_t* r = 0;
        GCRoot1 gc(&r);
        r = jl_call1(fun, a);
        rethrow();
        return r;
    }

    jl_value_t* call(jl_function_t* fun, jl_value_t* a, jl_value_t* b) {
        jl_value_t* r = 0;
        GCRoot1 gc(&r);
        r = jl_call2(fun, a, b);
        rethrow();
        return r;
    }

    jl_value_t* call(jl_function_t* fun, jl_value_t* a, jl_value_t* b, jl_value_t* c) {
        jl_value_t* r = 0;
        GCRoot1 gc(&r);
        r = jl_call3(fun, a, b, c);
        rethrow();
        return r;
    }

    jl_value_t* make_tuple() {
        return call((jl_function_t*)fun_tuple);
    }

    jl_value_t* make_tuple(jl_value_t* a) {
        return call((jl_function_t*)fun_tuple, a);
    }

    jl_value_t* make_tuple(jl_value_t* a, jl_value_t* b) {
        return call((jl_function_t*)fun_tuple, a, b);
    }

    jl_value_t* make_tuple(jl_value_t* a, jl_value_t* b, jl_value_t* c) {
        return call((jl_function_t*)fun_tuple, a, b, c);
    }

    jl_value_t* wrap_c_function_0_worker(
        jl_value_t*        c_funptr,
        const std::string& return_ty
        )
    {
        jl_value_t* f = 0;
        jl_value_t* r = 0;
        GCRoot2 root(&f, &r);
        f = jl_eval_string(("f -> () -> ccall(f, "+return_ty+", (), x1)").c_str());
        rethrow();
        r = call((jl_function_t*)f, c_funptr);
        return r;
    }
    
    jl_value_t* wrap_c_function_1_worker(
        jl_value_t*        c_funptr,
        const std::string& return_ty,
        const std::string& paramA
        )
    {
        jl_value_t* f = 0;
        jl_value_t* r = 0;
        GCRoot2 root(&f, &r);
        f = jl_eval_string(("f -> x1 -> ccall(f, "+return_ty+", ("+paramA+",), x1)").c_str());
        rethrow();
        r = call((jl_function_t*)f, c_funptr);
        return r;
    }    

    jl_value_t* wrap_c_function_2_worker(
        jl_value_t*        c_funptr,
        const std::string& return_ty,
        const std::string& paramA,
        const std::string& paramB
        )
    {
        jl_value_t* f = 0;
        jl_value_t* r = 0;
        GCRoot2 root(&f, &r);
        f = jl_eval_string(
            ("f -> (x1,x2) -> ccall(f, "+return_ty+", ("+paramA+","+paramB+"), x1, x2)").c_str());
        rethrow();
        r = call((jl_function_t*)f, c_funptr);
        return r;
    }    
};


namespace Julia {
    using Impl::GCRoot1;
    using Impl::GCRoot2;
    using Impl::GCRoot3;
    using Impl::GCRoot4;;

    
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
        // Just in case we put parameter into GC root to prevent its
        // accidental collection
        jl_value_t* zero = 0;
        jl_value_t* one  = 0;
        jl_value_t* n    = 0;
        GCRoot4 gc(&zero, &one, &n, &val);
        // Constants;
        zero = jl_box_int64(0);
        one  = jl_box_int64(1);
        // Increase counter in map preventing GC by 1
        n = jl_call3(fun_get, g_barrier, m_value, zero); rethrow();
        n = jl_call2(fun_plus, n, one);                  rethrow();
        jl_call3(fun_setindex, g_barrier, n, m_value);   rethrow();
    }

    GCBarrier::~GCBarrier() {
        jl_value_t* zero = 0;
        jl_value_t* one  = 0;
        jl_value_t* n    = 0;
        GCRoot3 gc(&zero, &one, &n);
        // Constants;
        zero = jl_box_int64(0);
        one  = jl_box_int64(-1);
        // Decrease counter and remove value from map
        //
        // NOTE: We must not throw exceptions from destructor.
        //       Throwing in the destructor is absolutely
        //       haram!
        n = jl_call3(fun_get, g_barrier, m_value, zero);
        if( jl_unbox_int64(n) <= 1 ) {
            jl_call2(fun_detete, g_barrier, m_value);
        } else {
            n = jl_call2(fun_plus, n, one);
            jl_call3(fun_setindex, g_barrier, n, m_value);
        }
    }

    Value::Value(jl_value_t* val) :
        m_value(new GCBarrier(val))
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
        fun_tuple    = jl_get_global(jl_main_module, jl_symbol("tuple"));
        // Read types
        ty_Float64   = jl_eval_string("Float64");
        // Create GC barrier
        g_barrier = jl_eval_string("IdDict{Any,Int64}()");
        // Set up callbacks
        // callback_simple_1 = (jl_function_t*)eval_and_stash(
        //     "(f,r,p) -> (a) -> ccall(f,r,(p,),a)");
        // callback_simple_2 = (jl_function_t*)eval_and_stash(
        //     "(r,p) -> (a,b) -> ccall(r,p,a,b)");
        rethrow();
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
    
    jl_value_t* Convert<double>::juliaType() {
        return ty_Float64;
    }
    
    static std::string tyname_Float64 = "Float64";
    const std::string& Convert<double>::juliaTypeName() {
        return tyname_Float64;
    };

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

    jl_function_t* make_callback(int nparam, double (*fun)(double*)) {
        // jl_function_t* j_fun  = 0;
        // jl_value_t*    j_npar = 0;
        // jl_value_t*    j_fptr = 0;
        // GCRoot3 gc((jl_value_t**)(&j_fun), &j_npar, &j_fptr);
        // //
        // j_npar = jl_box_int64(nparam);
        // j_fptr = jl_box_voidpointer((void*)fun);
        // j_fun  = jl_call2(callback_c_raw, j_npar, j_fptr);
        // rethrow();
        // return j_fun;
        return 0;
    }

    void println(jl_value_t* val) {
        jl_call1(fun_print, val);
    }

    void println(const Value& val) {
        jl_call1(fun_print, val.juliaValue());
    }

    jl_value_t* eval_string(const char* str) {
        jl_value_t* r = 0;
        GCRoot1 gc(&r);
        r = jl_eval_string(str);
        rethrow();
        return r;
    }
}


Julia::Exception::Exception(const char* msg) :
    std::runtime_error(msg)
{}

Julia::Exception::Exception(const std::string& msg) :
    std::runtime_error(msg)
{}
