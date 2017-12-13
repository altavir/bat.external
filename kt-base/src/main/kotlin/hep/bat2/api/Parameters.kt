package hep.bat2.api


/**
 * numeric type for external function calls
 */
typealias Numeric = Number

/**
 * Posible primitive value types
 */
sealed class Value {
    abstract val number: Numeric
    abstract val string: String
    abstract val bool: Boolean
}

class NumericValue(override val number: Numeric) : Value() {
    override val string: String = number.toString()
    override val bool: Boolean = number.toDouble() > 0
}

class BooleanValue(override val bool: Boolean) : Value() {
    override val number: Numeric = if (bool) {
        1.0
    } else {
        0.0
    }
    override val string: String = bool.toString()
}

class StringValue(override val string: String) : Value() {
    override val number: Numeric = string.toDouble()
    override val bool: Boolean = string == "true"
}


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
    abstract val outputDimension: Int;

    /**
     * names for the arguments. By default generates "parameter_N" names, but could be overridden
     */
    open val inputNames: List<String> by lazy { (0 until inputDimension).map { "par_$it" } }

    open val outputNames: List<String> by lazy { (0 until outputDimension).map { "par_$it" } }

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
    abstract val rowsDimension: Int
    /**
     * The numbers of columns in matrix
     */
    abstract val columnsDimension: Int

    open val rowNames: List<String> by lazy { (0 until rowsDimension).map { "par_$it" } }

    open val columnNames: List<String> by lazy { (0 until columnsDimension).map { "par_$it" } }

    abstract operator fun get(i: Int, j: Int): Numeric

}

/**
 * A container for parameters of task or task result
 */
interface Parameters  {
    /**
     * The list of roles which are provided by this parameter list
     */
    val roles: Array<String>

    /**
     * Get the set of parameters corresponding to given role
     */
    operator fun get(role: String): Set<Parameter>
}

