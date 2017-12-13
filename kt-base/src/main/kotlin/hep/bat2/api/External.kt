package hep.bat2.api

/**
 * General representation of server which could execute a string command with paramters
 */
interface External {
    operator fun invoke(task: String, params: Parameters): Parameters
}