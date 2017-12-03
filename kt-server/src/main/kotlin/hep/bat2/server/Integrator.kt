package hep.bat2.server

import hep.bat2.ParameterNotFoundException
import hep.bat2.Parameters


/**
 * General representation of integratable entity.
 * @param value current calculated value. Null in case no calculations were performed
 * @param parameters list of parameters needed for integration
 */
class Integrand<R>(val value: R?, val parameters: Parameters) {
    /**
     * get a parameter with a specific role
     */
    operator fun <T> get(role: String, type: Class<T>): T {
        return parameters[role, type] ?: throw ParameterNotFoundException(role, "Parameter with role $role and type $type not found")
    }

    fun <T> opt(role: String, type: Class<T>): T? {
        return parameters[role, type]
    }
}

interface Integrator<R> {

    /**
     * Build an integrand from given set of parameters
     * @param integrand previously used integrand if it exists
     */
    fun buildIntegrand(parameters: Parameters, integrand: Integrand<R>? = null): Integrand<R> {
        val map = integrand?.parameters?.apply { putAll(parameters) } ?: parameters
        return Integrand(integrand?.value, map)
    }

    /**
     * Perform integration using given integrand
     */
    suspend fun integrate(integrand: Integrand<R>): Integrand<R>;

    /**
     * A shortcut call to perform single integration operation with given parameters
     */
    suspend fun integrate(parameters: Parameters): R? {
        return integrate(buildIntegrand(parameters)).value
    }
}