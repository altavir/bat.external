package hep.bat2

/**
 * a simple n-dimensional function returnign single number. Zero dimension function is a number
 */
interface NFunction {
    /**
     * The dimension of function
     */
    val n: Int

    /**
     *
     */
    fun invoke(arg: List<Number>): Number;
}

