package hep.bat2.api

/**
 * General representation of server which could execute a string command with paramters
 */
interface Server {
    fun run(task: String, params: Parameters): Parameters
}