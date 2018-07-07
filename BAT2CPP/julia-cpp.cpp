#include "julia-cpp.hpp"

#include <iostream>

// Set of Julia values which should be prevented from GC
static jl_value_t*     g_barrier = 0;
// GC root for value above. Here it's allocated on heap
static Julia::GCRoot1* gc_root   = 0;
// Julia globals. They're set during initialization
static jl_value_t* fun_get      = 0;
static jl_value_t* fun_setindex = 0;
static jl_value_t* fun_detete   = 0;
static jl_value_t* fun_plus     = 0;
static jl_value_t* fun_print    = 0;

// Call exit hook
static void finalize_julia() {
    jl_atexit_hook(0);
    // FIXME: delete gc_root while making sure we won't mess up stack
    //        of GC roots
}

namespace Julia {

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
        // Create GC barrier
        g_barrier = jl_eval_string("ObjectIdDict()");
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

}


Julia::Exception::Exception(const char* msg) :
    std::runtime_error(msg)
{}

Julia::Exception::Exception(const std::string& msg) :
    std::runtime_error(msg)
{}
