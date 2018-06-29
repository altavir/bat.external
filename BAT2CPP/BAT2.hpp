#ifndef BAT2__HPP__
#define BAT2__HPP__

#include <vector>
#include <stdexcept>
#include <boost/shared_ptr.hpp>

namespace BAT {
    namespace Impl {
        class GCBarrier;
    };

    // Exception in the BAT wrapper
    class BATException : public std::runtime_error {
    public:
        BATException(const char* msg);
        BATException(const std::string& msg);
    };



    // Wrapper class around boxed Julia value. It ensures that value
    // will not be garbage collected while object alive. In order to
    // allow copyability pointer is kept inside shared pointer
    class JuliaValue {
    protected:
        // Allocate pointer for Julia and set it to NULL. Derived
        // classes are expected to set it actual Julia value in
        // constructor.
        JuliaValue();
    public:
        // Remove value from persistent map which allows Julia GC to
        // collect it.
        virtual ~JuliaValue();
        // FIXME: these aren't parts of public API!

        // Retrieve pointer to julia value. Implementation must ensure
        // that value won't be garbage collected while object is alive
        jl_value_t* julia_value() const;
    protected:
        boost::shared_ptr<Impl::GCBarrier> m_value;
    };


    //
    class AbstractParamBounds : public JuliaValue {
    };

    //
    class ParamVolumeBounds : public AbstractParamBounds {
    };

    //
    class HyperRectBounds : public ParamVolumeBounds {
    public:
        HyperRectBounds(const std::vector<double>& low_bounds,
                        const std::vector<double>& hi_bounds);
    };


    
    /// Abstract base class corresponding to prior in MCMCSpec
    class AbstractDensity : public JuliaValue {
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

    // Class 
    class GenericDensity : public AbstractDensity {
    public:
        GenericDensity(int nparam, double (*fun)(double*));
        virtual ~GenericDensity();
    };

    class MCMCSpec : public JuliaValue {
    public:
        MCMCSpec(const AbstractDensity& likelihood,
                 const AbstractDensity& prior);
        virtual ~MCMCSpec();
    private:
        boost::shared_ptr<Impl::GCBarrier> m_likelihood;
    };
    
    /// Thin wrapper class for accessing Julia implementation of BAT2
    class MCMCIterator : public JuliaValue {
    public:
        /// Create MCMC iterator from likelihood defined as C function
        MCMCIterator(const MCMCSpec& spec,
                     int nchains);
        virtual ~MCMCIterator();

        // Generate samples. They will be stored as structure of arrays.
        void generate(std::vector<std::vector<std::vector<double> > >& out, int nsamples);
    private:
        int m_nchains;
    };
}
#endif /* BAT2__HPP__ */
