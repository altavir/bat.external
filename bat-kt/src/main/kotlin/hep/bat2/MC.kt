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

interface Model<T>{
    operator fun invoke(sample: Sample<T>): Number
}

abstract class MCIntegrator<T> : Integrator<T> {
    /**
     * Build objective functions from parameters
     */
    protected abstract fun buildModel(integrand: Integrand<T>): Model<T>

    /**
     * build Sampler
     */
    protected abstract fun buildSampler(integrand: Integrand<T>): Sampler<T>

    /**
     * Use integrated value to produce result
     */
    protected abstract fun reduce(integrand: Integrand<T>, integral: Number): Integrand<T>

    private val Integrand<T>.model: Model<T>
        get() = opt("model") as Model<T>? ?: buildModel(this)

    private val Integrand<T>.sampleSize: Int
        get() = opt("sampleSize") as Int? ?: 10000

    private val Integrand<T>.sampler: Sampler<T>
        get() = opt("sampler") as Sampler<T>? ?: buildSampler(this)

    override fun integrate(integrand: Integrand<T>): Integrand<T> {
        TODO()
    }

}