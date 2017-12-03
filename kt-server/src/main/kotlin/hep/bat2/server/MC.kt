package hep.bat2.server

import kotlinx.coroutines.experimental.CommonPool
import kotlinx.coroutines.experimental.async
import kotlin.coroutines.experimental.CoroutineContext
import kotlin.coroutines.experimental.buildSequence

/**
 * Kotlin based MC integrator
 * @author Alexander Nozik
 */

/**
 * A single sample
 */
data class Sample<out T>(val value: T, val weight: Double)

/**
 * Sample generator
 * @param T the type of sample value
 */
interface Sampler<T> {
    /**
     * Generate next sample
     * @param prev previous sample. If null, generate independent sample
     */
    suspend fun next(prev: Sample<T>? = null): Sample<T>

    /**
     * Convert this sampler to infinite lazy sequence using coroutines
     */
    fun <R> asSequence(dispatcher: CoroutineContext = CommonPool, capacity: Int = 10000, transform: suspend (Sample<T>) -> R): Sequence<R> {
        return buildSequence {
            async {
                var last: Sample<T> = next(null)
                while (true) {
                    yield(transform(last))
                    last = next(last);
                }
            }
        }
    }

}

/**
 * @param T the type of sample value
 * @param R the type of model computation result
 */
abstract class MCIntegrator<T, R> : Integrator<R> {

    /**
     * Monte-Carlo model
     * @param T sample type
     * @param R intermediate result of model application to sample
     */
    interface Model<T, out R> {
        suspend operator fun invoke(sample: Sample<T>): R
    }

    /**
     * Build objective functions from parameters
     */
    protected abstract fun buildModel(integrand: Integrand<R>): Model<T, R>

    /**
     * build Sampler
     */
    protected abstract fun buildSampler(integrand: Integrand<R>): Sampler<T>

    /**
     * Can later add debug hooks here
     */
    protected open suspend fun Integrand<R>.map(model: Model<T, R>, sample: Sample<T>): R {
        return model(sample)
    }

    /**
     * Generate integration result from lazy sequence. Sequence is considered to end when null is returned
     */
    protected suspend abstract fun Integrand<R>.reduce(values: Sequence<R>): Integrand<R>

    private val Integrand<R>.model: Model<T, R>
        get() = opt("model", Model::class.java) as Model<T, R>? ?: buildModel(this)

    protected val Integrand<R>.sampleSize: Int
        get() = opt("sampleSize", Int::class.java) ?: 10000

    private val Integrand<R>.sampler: Sampler<T>
        get() = opt("sampler", Sampler::class.java) as Sampler<T>? ?: buildSampler(this)

    override suspend fun integrate(integrand: Integrand<R>): Integrand<R> {
        val sequence = integrand.sampler.asSequence { integrand.map(integrand.model, it) }
        return integrand.reduce(sequence)
    }
}

typealias Vector = DoubleArray

class RealSpaceMCIntegrator : MCIntegrator<Vector, Double>() {
    override fun buildModel(integrand: Integrand<Double>): Model<Vector, Double> {
        TODO()
    }

    override fun buildSampler(integrand: Integrand<Double>): Sampler<Vector> {
        TODO()
    }

    suspend override fun Integrand<Double>.reduce(values: Sequence<Double>): Integrand<Double> {
        val res = values.average();
        return Integrand(res, this.parameters);
    }


}