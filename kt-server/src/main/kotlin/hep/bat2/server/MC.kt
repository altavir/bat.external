package hep.bat2.server

import kotlinx.coroutines.experimental.CommonPool
import kotlinx.coroutines.experimental.channels.ProducerJob
import kotlinx.coroutines.experimental.channels.produce
import kotlin.coroutines.experimental.CoroutineContext

/**
 * Kotlin based MC integrator
 * @author Alexander Nozik
 */

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
    suspend fun next(prev: Sample<T>? = null): Sample<T>

    /**
     * Convert this sampler to infinite lazy sequence using coroutines
     */
    fun <R> asProducer(dispatcher: CoroutineContext = CommonPool, capacity: Int = 10000, transform: suspend (Sample<T>) -> R): ProducerJob<R> {
        return produce<R>(context = dispatcher, capacity = capacity) {
            var last: Sample<T> = next(null)
            while (true) {
                send(transform(last))
                last = next(last);
            }
        }
    }
}

abstract class MCIntegrator<T, R> : Integrator<T> {

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
    protected abstract fun buildModel(integrand: Integrand<T>): Model<T, R>

    /**
     * build Sampler
     */
    protected abstract fun buildSampler(integrand: Integrand<T>): Sampler<T>

    /**
     * Can later add debug hooks here
     */
    protected open suspend fun Integrand<T>.map(model: Model<T, R>, sample: Sample<T>): R {
        return model(sample)
    }

    /**
     * Generate integration result from lazy sequence. Sequence is considered to end when null is returned
     */
    protected suspend abstract fun Integrand<T>.reduce(values: ProducerJob<R>): Integrand<T>

    private val Integrand<T>.model: Model<T, R>
        get() = opt("model") as Model<T, R>? ?: buildModel(this)

    protected val Integrand<T>.sampleSize: Int
        get() = opt("sampleSize") as Int? ?: 10000

    private val Integrand<T>.sampler: Sampler<T>
        get() = opt("sampler") as Sampler<T>? ?: buildSampler(this)

    override suspend fun integrate(integrand: Integrand<T>): Integrand<T> {
        val model = integrand.model
        val producer = integrand.sampler.asProducer { integrand.map(model, it) }
        return integrand.reduce(producer)
    }

}