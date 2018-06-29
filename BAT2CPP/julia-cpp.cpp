#include "julia-cpp.hpp"
#include "BAT2.hpp"

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
        throw_on_jl_error();
        jl_eval_string("import BATCPP");
        throw_on_jl_error();
    }
}

