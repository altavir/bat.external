#include "julia-cpp.hpp"


// Call exit hook
static void finalize_julia() {
    jl_atexit_hook(0);
}


namespace Julia {
    void initialize() {
        if( jl_is_initialized() )
            return;
    
        // Initialize Julia and register finalization code
        jl_init();
        atexit(finalize_julia);
        // Load required libraries. Note that import returns Void
        // which is allocated statically so we don't need to track it
        jl_eval_string("import BAT");
        rethrow();
        jl_eval_string("import BATCPP");
        rethrow();
    }


    void rethrow(const char* errmsg) {
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
