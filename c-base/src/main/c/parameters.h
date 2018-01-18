#ifndef C_BASE_PARAMETERS_H
#define C_BASE_PARAMETERS_H


#include <stdbool.h>

/**
* Parameter description
*/

typedef char *String;

typedef bool Boolean;


/**
* Allowed parameter types
*/
typedef enum {
    TREE = 1,
    VECTOR = 2,
    MATRIX = 3,
    FUNCTION = 4,
    N_FUNCTION = 5,
    MAPPING = 6,
    BLOB = 7
} ParameterType;

/**
* type of the tree value
*/
typedef enum {
    INT = 1,
    DOUBLE = 2,
    BOOLEAN = 3,
    STRING = 4
} ValueType;

/**
* single value union
*/
typedef union {
    int intValue;
    double doubleValue;
    Boolean booleanValue;
    String stringValue;
} Value;

/**
* a single tree entry
*/
typedef struct {
    /**
     * The key fo this entry. The name is represented by a sequence of name tokens separated by `.`.
     */
    String key;
    /**
     * The type of the value
     */
    ValueType type;
    /**
     * Value itself
     */
    Value value;
} TreeEntry;

/**
* an immutable value tree
*/
typedef struct {
    /**
     * A number of entries in the tree
     */
    short size;
    /**
     * An array of entries
     */
    TreeEntry *entries;
} Tree;

/**
* A fixed size vector with names
*/
typedef struct {
    /**
     * Length of the vector
     */
    short size;
    /**
     * Optional array of names for vector. If null, use default naming.
     * TODO define default naming.
     */
    String *names;
    /**
     * An array of size {@code size}
     */
    double *values;
} Vector;

/**
* a matrix
*/
typedef struct {
    /**
     * Number of rows
     */
    short rows;
    /**
     * Optional array of names for rows
     */
    String *rowNames;
    /**
     * Number of columns
     */
    short columns;
    /**
     * Optional array of names for columns
     */
    String *columnNames;
    /**
     * Array of rows
     */
    double **values;
} Matrix;

/**
* a simple function
*/
typedef double(*Function)(double);

/**
*  n-function
*/
typedef struct {
    /**
     * The size of input array
     */
    short size;
    /**
     * Optional array of names
     */
    String *names;

    /**
     * Reference to function. Since vector is names, it is possible in some cases that dimension of input vector is larger,
     * then dimension of function. In this case use subvector with components defined by {@code names}
     */
    double (*func)(Vector);
} NFunction;

/**
* a mapping
*/
typedef struct {
    /**
     * A size of input vector
     */
    short inputSize;
    /**
     * Optional list of input names
     */
    String *inputNames;
    /**
    * A size of output vector
    */
    short outputSize;
    /**
     * Optional list of output names
     */
    String *outputNames;

    /**
     * Function reference
     */
    Vector (*func)(Vector);
} Mapping;

/**
 * A binary object
 */
typedef struct {
    /**
     * The size of data in bytes
     */
    unsigned long size;
    /**
     * Pointer to data block
     */
    unsigned char *data;
} Blob;

/**
* a parameter
*/
typedef union {
    Tree treeValue;
    Vector vectorValue;
    Matrix matrixValue;
    Function functionValue;
    NFunction nFunctionValue;
    Mapping mappingValue;
    Blob blobValue;
} Parameter;

/**
* A single entry in parameter list
*/
typedef struct {
    /**
     * The role for given parameter. Multiple parameters could share the same role
     */
    String role;
    /**
     * The type of parameter
     */
    ParameterType type;
    /**
     * Parameter itself
     */
    Parameter par;
} ParameterEntry;

/**
* a set of parameters
*/
typedef struct {
    /**
     * The size of parameters set
     */
    short size;
    /**
     * Array of parameter entries
     */
    ParameterEntry *entries;
} Parameters;


#endif //C_BASE_PARAMETERS_H