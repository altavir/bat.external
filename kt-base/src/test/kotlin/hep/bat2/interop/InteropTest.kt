package hep.bat2.interop

import hep.bat2.api.NumericValue
import hep.bat2.api.Tree
import hep.bat2.impl.KFunction
import hep.bat2.impl.KParameters
import hep.bat2.impl.KTree
import kotlin.test.assertEquals


/**
 * A basic function to test servers
 */
fun testServer(server: hep.bat2.api.Server) {
    val function = KFunction { x ->
        println("The function is called with argument $x")
        x.toDouble() + 1
    }

    val tree = KTree("a" to NumericValue(1.0), "b.c" to NumericValue(2.0))
    val parameters = KParameters("func" to function, "tree" to tree)

    //invoke a task that returns input parameters
    val echo = server.run("echo", parameters)

    val echoTree = echo["tree"] as? Tree ?: error("wrong echo tree type")
    val value = echoTree["b.c"] ?: error("wrong echo response")
    assertEquals(value.number.toDouble(), 2.0)
}