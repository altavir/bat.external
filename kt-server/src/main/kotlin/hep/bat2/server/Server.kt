package hep.bat2.server

import hep.bat2.AmbiguousParameterException
import hep.bat2.External
import hep.bat2.Parameters
import hep.bat2.TaskNotSupportedException

enum class Task {
    INTEGRATE
}


fun <T> Parameters.opt(role: String, type: Class<T>): T? {
    val results = get(role).filter { type.isInstance(it) }
    return when (results.size) {
        0 -> null
        1 -> type.cast(results.first())
        else -> throw AmbiguousParameterException(role, "Ambiguous parameter resolution for role $role and type $type")
    }
}

operator fun <T> Parameters.get(role: String, type: Class<T>): T {
    return opt(role, type)!!
}

fun Parameters.has(role: String, type: Class<*>): Boolean {
    return get(role).find { type.isInstance(it) } != null
}

class Server : External {
    override fun invoke(task: String, params: Parameters): Parameters {
        when (task.toUpperCase()) {
            Task.INTEGRATE.name -> return integrate(params)
            else -> throw TaskNotSupportedException(task)
        }
    }

    fun integrate(params: Parameters): Parameters {
        TODO()
    }
}