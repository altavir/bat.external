package hep.bat2.impl

import hep.bat2.api.Parameter
import hep.bat2.api.Parameters

/**
 * Utilities
 */

/**
 * Join tw different parameter sets
 */
operator fun Parameters.plus(other: Parameters): Parameters {
    val thisPars = roles.associate { Pair(it, get(it)) }
    val otherPars = other.roles.associate { Pair(it, other[it]) }
    val sum = thisPars + otherPars
    return KParameters(sum)
}

operator fun Parameters.plus(pair: Pair<String, Parameter>): Parameters {
    val pars = roles.associate { Pair(it, get(it).toMutableSet()) }
    pars.getOrElse(pair.first) { HashSet() }.add(pair.second);
    return KParameters(pars)
}