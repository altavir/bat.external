#ifndef BAT2__HPP__
#define BAT2__HPP__

#include <vector>
#include <stdexcept>
#include <memory>

#include "julia-cpp.hpp"


namespace BAT {
    // Exception in the BAT wrapper
    class BATException : public std::runtime_error {
    public:
        BATException(const char* msg);
        BATException(const std::string& msg);
    };

    // Any value which uses code from BAT. It ensures that BAT is
    // actually loaded in the constructor
    class Value : public Julia::Value {
    public:
        explicit Value(jl_value_t* val);
    };
   
    
    
    //
    class AbstractParamBounds : public BAT::Value {
    protected:
        explicit AbstractParamBounds(jl_value_t* val):
            Value(val)
        {}
    };

    //
    class ParamVolumeBounds : public AbstractParamBounds {
    protected:
        explicit ParamVolumeBounds(jl_value_t* val):
            AbstractParamBounds(val)
        {}
    };

    //
    class HyperRectBounds : public ParamVolumeBounds {
    public:
        HyperRectBounds(const std::vector<double>& low_bounds,
                        const std::vector<double>& hi_bounds);
    };

    
    /// Abstract base class corresponding to prior in MCMCSpec
    class AbstractDensity : public BAT::Value {
    protected:
        AbstractDensity(jl_value_t* val) :
            Value(val)
        {}
    };

    // Corresponds to constant density prior
    class ConstDensity : public AbstractDensity {
    public:
        ConstDensity(const HyperRectBounds& bounds,
                     double val = 0);
        virtual ~ConstDensity();
    private:
        HyperRectBounds m_bounds;
    };

    // 
    class GenericDensity : public AbstractDensity {
    public:
        GenericDensity(int nparam, double (*fun)(double*));
    };

    class MCMCSpec : public BAT::Value {
    public:
        MCMCSpec(const AbstractDensity& likelihood,
                 const AbstractDensity& prior);
    };
    
    // /// Thin wrapper class for accessing Julia implementation of BAT2
    // class MCMCIterator : public JuliaValue {
    // public:
    //     /// Create MCMC iterator from likelihood defined as C function
    //     MCMCIterator(const MCMCSpec& spec,
    //                  int nchains);
    //     virtual ~MCMCIterator();

    //     // Generate samples. They will be stored as structure of arrays.
    //     void generate(std::vector<std::vector<std::vector<double> > >& out, int nsamples);
    // private:
    //     int m_nchains;
    // };
}
#endif /* BAT2__HPP__ */
