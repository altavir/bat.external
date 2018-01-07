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
    MAPPING = 6
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
    String key;
    ValueType type;
    Value value;
} TreeEntry;

/**
* an immutable value tree
*/
typedef struct {
    short size;
    TreeEntry *entries;
} Tree;

/**
* A fixed size vector with names
*/
typedef struct {
    short size;
    String *names;
    double *values;
} Vector;

/**
* a matrix
*/
typedef struct {
    short rows;
    String *rowNames;
    short columns;
    String *columnNames;
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
    short size;
    String *names;
    double (*func)(Vector);
} NFunction;

/**
* a vector function
*/
typedef struct {
    short inputSize;
    String *inputNames;
    short outputSize;
    String *outputNames;

    Vector (*func)(Vector);
} Mapping;

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
} Parameter;

/**
* A single entry in parameter list
*/
typedef struct {
    String role;
    ParameterType type;
    Parameter par;
} ParameterEntry;

/**
* a set of parameters
*/
typedef struct {
    short size;
    ParameterEntry *entries;
} Parameters;
