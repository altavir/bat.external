package hep.bat2


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