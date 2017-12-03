package hep.bat2


/**
 * Parameter class covers all possible parameter types
 */
sealed class Parameter

/**
 * A univariate function
 */
abstract class Function : Parameter() {
    abstract fun value(argument: Numeric): Numeric
}

/**
 * A multivariate function returning sing
 */
abstract class NFunction : Parameter() {

    /**
     * Dimension of the function
     */
    abstract val dimension: Int;

    /**
     * names for the arguments. By default generates "parameter_N" names, but could be overridden
     */
    open val names: List<String> by lazy { (0 until dimension).map { "parameter_$it" } }

    /**
     * the value in a specific point. Should throw an exception in case of argument length mismatch
     */
    abstract fun value(arguments: List<Numeric>): Numeric

    /**
     * Use named arguments
     */
    fun value(arguments: Map<String, Numeric>): Numeric {
        return value(names.map { arguments[it] ?: throw IllegalArgumentException("Argument with name $it not found") })
    }
}

/**
 * Many-to-many function
 */
abstract class VectorFunction : Parameter() {
    /**
     * Dimension of arguments
     */
    abstract val inputDimension: Int;

    /**
     * Dimension of the output
     */
    abstract val outputDimentsion: Int;

    /**
     * names for the arguments. By default generates "parameter_N" names, but could be overridden
     */
    open val inputNames: List<String> by lazy { (0 until inputDimension).map { "parameter_$it" } }

    open val outputNames: List<String> by lazy { (0 until outputDimentsion).map { "parameter_$it" } }

    /**
     * The value of this vector-function
     */
    abstract fun value(arguments: List<Numeric>): List<Numeric>

    fun value(arguments: Map<String, Numeric>): Map<String, Numeric> {
        val res = value(inputNames.map { arguments[it] ?: throw IllegalArgumentException("Argument with name $it not found") })
        return mapOf(*res.mapIndexed { index, value -> Pair(outputNames[index], value) }.toTypedArray())
    }
}

/**
 * Tree of values
 */
abstract class Tree : Parameter() {
    abstract val keys: List<String>
    abstract operator fun get(key: String): Value
}

/**
 * A single value parameter
 */
class ValueParameter(val value: Value) : Parameter()

/**
 * A matrix or vector
 */
abstract class Matrix : Parameter() {
    /**
     * the number of rows in matrix
     */
    abstract val rowsNum: Int
    /**
     * The numbers of columns in matrix
     */
    abstract val columnsNum: Int

    abstract operator fun get(i: Int, j: Int): Numeric

}

/**
 * A container for parameters of task or task result
 */
class Parameters {
    private val parameters: MutableMap<String, MutableSet<Parameter>> = HashMap();

    fun put(role: String, parameter: Parameter) {
        parameters.getOrElse(role) { HashSet() }.add(parameter)
    }

    fun setAll(map: Map<String, Parameter>) {
        map.forEach { put(it.key, it.value) }
    }

    fun putAll(params: Parameters) {
        params.parameters.forEach { this.parameters.getOrElse(it.key) { HashSet() }.addAll(it.value) }
    }

    operator fun get(role: String): Set<Parameter> {
        return parameters[role] ?: emptySet()
    }
}

/**
 * General representation of server which could execute a string command with paramters
 */
interface External {
    operator fun invoke(task: String, params: Parameters): Parameters
}