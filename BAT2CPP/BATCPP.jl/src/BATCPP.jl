module BATCPP

export create_chains
export gc_ref
export gc_unref
export gc_info

using BAT


## Garbage collection [prevention]
const gc_preserve = ObjectIdDict() # reference counted closures

function gc_ref(x::ANY)
    global gc_preserve
    # isbits(x) && error("can't gc-preserve an isbits object")
    gc_preserve[x] = (get(gc_preserve, x, 0)::Int) + 1
    x
end

function gc_unref(x::ANY)
    global gc_preserve
    @assert !isbits(x)
    count = get(gc_preserve, x, 0)::Int - 1
    if count <= 0
        delete!(gc_preserve, x)
    end
    nothing
end

function gc_info()
    global gc_preserve
    println(gc_preserve)
end


function wrap_c_call(n :: Int, f :: Ptr{Void})
    function fun(xs :: Array{Float64})
        if( n != length(xs))
            error("Sizes do not match")
        end
        ccall(f, Float64, (Ref{Float64},), xs)
    end
    fun
end
    
function mcspec_nparam(spec :: BAT.MCMCSpec)
    BAT.nparam( spec.likelihood )
end
    
# Start burn-in and create chains
function mcmc_create_chains(
    chainspec::BAT.MCMCSpec,
    nchains::Integer,
    tuner_config::BAT.AbstractMCMCTunerConfig = BAT.AbstractMCMCTunerConfig(chainspec.algorithm),
    convergence_test::BAT.MCMCConvergenceTest = BAT.GRConvergence(),
    init_strategy::BAT.MCMCInitStrategy       = BAT.MCMCInitStrategy(tuner_config),
    burnin_strategy::BAT.MCMCBurninStrategy   = BAT.MCMCBurninStrategy(tuner_config),
    exec_context::BAT.ExecContext             = BAT.ExecContext();
    strict_mode::Bool                         = false,
    ll::BAT.LogLevel                          = BAT.LOG_INFO,
)
    # Create tuners
    tuners = BAT.mcmc_init(
        chainspec,
        nchains,
        exec_context,
        tuner_config,
        convergence_test,
        init_strategy;
        ll = ll,
    )
    # Burnin
    BAT.mcmc_tune_burnin!(
        (),
        tuners,
        convergence_test,
        burnin_strategy,
        exec_context;
        strict_mode = strict_mode,
        ll = ll
    )
    #
    map(x -> x.chain, tuners)
end


# function mcmc_iteration_step(
#     outputbuf :: Array{Array{Float64}},
#     chains    :: Array{BAT.MCMCIterator},
#     nsamples  :: Int
#     ll::BAT.LogLevel                          = BAT.LOG_INFO,
# )
#     # FIXME: I need to thread exec_context around
#     exec_context = BAT.ExecContext()
#     #
    
# end

end # module
