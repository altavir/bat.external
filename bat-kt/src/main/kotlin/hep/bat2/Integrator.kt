package hep.bat2


typealias Parameters = Map<String, Any>

/**
 * General representation of integratable entity.
 * @param value current calculated value. Null in case no calculations were performed
 * @param parameters list of parameters needed for integration
 */
class Integrand<T>(val value: T?, val parameters: Parameters) {
    /**
     * get a parameter with a specific role
     */
    operator fun get(key: String): Any {
        return parameters[key] ?: throw RuntimeException("Parameter with key '$key' is not found");
    }

    fun opt(key: String): Any? {
        return parameters[key]
    }
}

interface Integrator<T> {

    /**
     * Build an integrand from given set of parameters
     * @param integrand previously used integrand if it exists
     */
    fun buildIntegrand(parameters: Parameters, integrand: Integrand<T>? = null): Integrand<T> {
        val map = integrand?.parameters?.toMutableMap()?.apply { putAll(parameters) } ?: parameters.toMutableMap()
        return Integrand(integrand?.value, map)
    }

    /**
     * Perform integration using given integrand
     */
    suspend fun integrate(integrand: Integrand<T>): Integrand<T>;

    /**
     * A shortcut call to perform single integration operation with given parameters
     */
    suspend fun integrate(parameters: Map<String, Any>): T? {
        return integrate(buildIntegrand(parameters)).value
    }
}