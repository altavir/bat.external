package hep.bat2.jvmbridge

import hep.bat2.api.*
import hep.bat2.impl.*

private fun NativeServer.tree_entry.asValue(): Pair<String, Value> {
    return when (type) {
        1 -> Pair(key, NumericValue(this.`val`.int_value))
        2 -> Pair(key, NumericValue(this.`val`.double_value))
        3 -> Pair(key, BooleanValue(this.`val`.boolean_value > 0))
        4 -> Pair(key, StringValue(this.`val`.string_value))
        else -> throw RuntimeException("Unknown native value type")
    }
}

private fun NativeServer.parameter_entry.asParameter(): Pair<String, Parameter> {
    return when (this.type) {
        1 -> Pair(role, KTree(par.tree_value.entries.associate { it.asValue() }))
        2 -> TODO()//Pair(role, this.par.matrix_value.toParameter())
        3 -> Pair(role, KFunction { arg -> par.function_value.apply(arg.toDouble()) })
        4 -> {
            val n_function = par.n_function_value
            Pair(role, KNFunction(n_function.names.toList()) { arg -> n_function.func.apply(arg.map { it.toDouble() }.toDoubleArray()) })
        }
        5 -> {
            val vector_function = par.vector_function_value
            Pair(
                    role,
                    KVectorFunction(
                            vector_function.input_names.toList(),
                            vector_function.output_names.toList()
                    ) { arg ->
                        vector_function.func.apply(arg.map { it.toDouble() }.toDoubleArray()).toList()
                    }
            )
        }
        else -> throw RuntimeException("Native parameter type not supported")
    }
}

fun NativeServer.parameters.convert(): Parameters {
    return KParameters(entries.map { it.asParameter() }.groupBy({ it.first }) { it.second })
}

