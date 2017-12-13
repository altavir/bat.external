package hep.bat2.server

import hep.bat2.api.Matrix
import hep.bat2.api.NFunction
import hep.bat2.api.Parameters
import kotlinx.coroutines.experimental.CommonPool
import kotlinx.coroutines.experimental.async
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.distribution.MultivariateRealDistribution
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.SynchronizedRandomGenerator
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
    fun <R> apply(dispatcher: CoroutineContext = CommonPool, transform: suspend (Sample<T>) -> R): Sequence<Pair<Sample<T>, R>> {
        return buildSequence {
            async {
                var last: Sample<T> = next(null)
                while (true) {
                    yield(Pair(last, transform(last)))
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
    protected suspend abstract fun Integrand<R>.reduce(sequence: Sequence<Pair<Sample<T>, R>>): Integrand<R>

    private val Integrand<R>.model: Model<T, R>
        get() = opt("model", Model::class.java) as Model<T, R>? ?: buildModel(this)

    protected val Integrand<R>.sampleSize: Int
        get() = opt("sampleSize", Int::class.java) ?: 10000

    private val Integrand<R>.sampler: Sampler<T>
        get() = opt("sampler", Sampler::class.java) as Sampler<T>? ?: buildSampler(this)

    override suspend fun integrate(integrand: Integrand<R>): Integrand<R> {
        val sequence = integrand.sampler.apply { integrand.map(integrand.model, it) }
        return integrand.reduce(sequence)
    }
}

typealias Vector = DoubleArray

//samplers

class ImportanceSampler(val distribution: MultivariateRealDistribution) : Sampler<Vector> {
    suspend override fun next(prev: Sample<Vector>?): Sample<Vector> {
        val vector = distribution.sample()
        val weight = 1.0 / distribution.density(vector)
        return Sample(vector, weight);
    }
}

//models

class FunctionModel(val function: NFunction) : MCIntegrator.Model<Vector, Double> {
    suspend override fun invoke(sample: Sample<Vector>): Double {
        return function.value(sample.value.asList()).toDouble()
    }
}

/**
 * Build a model from parameters
 */
internal fun buildRealSpaceModel(parameters: Parameters): MCIntegrator.Model<Vector, Double> {
    return when {
        parameters.has("objectiveFunction", NFunction::class.java) ->
            FunctionModel(parameters.get("objectiveFunction", NFunction::class.java))
        else -> throw RuntimeException("Can't create model from parameters")
    }
}

internal fun buildRealSpaceSampler(parameters: Parameters): Sampler<Vector> {
    //synchronized mersenne-twister generator
    val generator = SynchronizedRandomGenerator(MersenneTwister())
    return when {
        parameters.has("covariance", Matrix::class.java) -> {
            val covariance = parameters.get("covariance", Matrix::class.java)
            if (covariance.columnsNum != covariance.rowsNum) {
                throw RuntimeException("Covariance matrix dimension mismatch")
            }
            val center = DoubleArray(covariance.columnsNum) { 0.0 }
            val covArray = covariance.toDoubleArray()
            ImportanceSampler(MultivariateNormalDistribution(generator, center, covArray))
        }
        else -> throw RuntimeException("Can't create sampler from parameters")
    }
}

class RealSpaceMCIntegrator : MCIntegrator<Vector, Double>() {
    override fun buildModel(integrand: Integrand<Double>): Model<Vector, Double> {
        return buildRealSpaceModel(integrand.parameters)
    }

    override fun buildSampler(integrand: Integrand<Double>): Sampler<Vector> {
        return buildRealSpaceSampler(integrand.parameters)
    }

    suspend override fun Integrand<Double>.reduce(sequence: Sequence<Pair<Sample<Vector>, Double>>): Integrand<Double> {
        val res = sequence.take(sampleSize).map { it.second / it.first.weight }.average()
        return Integrand(res, this.parameters);
    }


}