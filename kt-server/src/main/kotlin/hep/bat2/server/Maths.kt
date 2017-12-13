package hep.bat2.server

import hep.bat2.api.Matrix
import hep.bat2.api.Numeric

fun Matrix.toArray(): Array<Array<Numeric>> {
    return Array(columnsNum) { i ->
        Array(rowsNum) { j ->
            get(i, j)
        }
    }
}

fun Matrix.toDoubleArray(): Array<DoubleArray> {
    return Array(columnsNum) { i ->
        DoubleArray(rowsNum) { j ->
            get(i, j).toDouble()
        }
    }
}