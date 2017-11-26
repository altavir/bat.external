package hep.bat2

import kotlin.coroutines.experimental.buildSequence

/**
 * A single sample
 */
data class Sample<T>(val value: T, val weight: Double)

/**
 * Sample generator
 */
interface Sampler<T> {
    /**
     * Generate next sample
     * @param prev previous sample. If null, generate independent sample
     */
    fun next(prev: Sample<T>? = null): Sample<T>

    /**
     * Convert this sampler to infinite lazy sequence using coroutines
     */
    fun asSequence(): Sequence<Sample<T>> {
        return buildSequence {
            var last: Sample<T> = next(null)
            while (true) {
                yield(last)
                last = next(last);
            }
        }
    }
}

abstract class MCIntegrator<T> : Integrator<T> {
    /**
     * Build objective functions from parameters
     */
    protected abstract fun buildModel(parameters: Parameters): NFunction

    protected abstract fun buildSampler(integrand: Integrand<NFunction>): Sampler<T>

    private val Integrand<NFunction>.model: NFunction
        get() = this["model"] as NFunction

    private val Integrand<NFunction>.sampleSize: Int
        get() = this["sampleSize"] as Int

    private val Integrand<NFunction>.sampler: Sampler<T>
        get() = this["sampleSize"] as Sampler<T>

    override fun integrate(integrand: Integrand<T>): Integrand<T> {
        TODO()
    }

}