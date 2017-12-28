package hep.bat2.server

import hep.bat2.api.Matrix
import hep.bat2.api.Numeric

fun Matrix.toArray(): Array<Array<Numeric>> {
    return Array(columnsDimension) { i ->
        Array(rowsDimension) { j ->
            get(i, j)
        }
    }
}

fun Matrix.toDoubleArray(): Array<DoubleArray> {
    return Array(columnsDimension) { i ->
        DoubleArray(rowsDimension) { j ->
            get(i, j).toDouble()
        }
    }
}