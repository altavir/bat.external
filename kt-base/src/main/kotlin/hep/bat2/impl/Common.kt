package hep.bat2.impl

import hep.bat2.api.*

/**
 * Common implementation of parameters in Kotlin
 */

class KParameters(private val map: Map<String, MutableSet<Parameter>>) : Parameters {

    override val roles: Array<String> = map.keys.toTypedArray()

    override operator fun get(role: String): Set<Parameter> {
        return map[role] ?: emptySet()
    }
}

//kotlin wrappers for parameter classes

class KFunction(private val function: (Numeric) -> Numeric) : Function() {
    override fun value(argument: Numeric): Numeric {
        return function(argument)
    }
}

class KNFunction(override val names: List<String>, private val function: (List<Numeric>) -> Numeric) : NFunction() {
    override val dimension: Int = names.size

    override fun value(arguments: List<Numeric>): Numeric {
        return function(arguments)
    }
}

class KVectorFunction(
        override val inputNames: List<String>,
        override val outputNames: List<String>,
        private val function: (List<Numeric>) -> List<Numeric>
) : VectorFunction() {
    override val inputDimension: Int = inputNames.size
    override val outputDimension: Int = outputNames.size

    override fun value(arguments: List<Numeric>): List<Numeric> {
        return function(arguments)
    }
}

class KMatrix(
        override val rowNames: List<String>,
        override val columnNames: List<String>,
        private val values: Array<Array<Double>>
) : Matrix() {
    init {
        if (values.size != columnNames.size || values[0].size != rowNames.size) {
            throw RuntimeException("Dimension mismatch in matrix construction")
        }
    }

    override val rowsDimension: Int = rowNames.size
    override val columnsDimension: Int = columnNames.size

    override fun get(i: Int, j: Int): Numeric {
        return values[i][j]
    }
}

class KTree(private val map: Map<String, Value>) : Tree() {
    override val keys: List<String> = map.keys.toList()

    override fun get(key: String): Value {
        return map[key] ?: throw ParameterNotFoundException(key)
    }
}