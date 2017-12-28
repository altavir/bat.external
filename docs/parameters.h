/**
* Parameter description
*/

/**
* Allowed parameter types
*/
typedef enum {
    TREE = 1,
    MATRIX = 2,
    FUNCTION = 3,
    N_FUNCTION = 4,
    VECTOR_FUNCTION = 5
} parameter_type;

/**
* type of the tree value
*/
typedef enum {
    INT = 1,
    DOUBLE = 2,
    BOOLEAN = 3,
    STRING = 4
} value_type;

/**
* single value union
*/
typedef union {
    int int_value;
    double double_value;
    bool boolean_value;
    char* string_value;
} value;

/**
* a single tree entry
*/
typedef struct{
    char* key;
    value_type type;
    value val;
} tree_entry;

/**
* an immutable value tree
*/
typedef struct {
    tree_entry entries[];
} tree;

/**
* a matrix
*/
typedef struct {
    char* row_names[];
    char* column_names[];
    double values[][];
} matrix;

/**
* a simple function
*/
typedef double(*function)(double);

/**
*  n-function
*/
typedef struct {
    char* names[];
    double(*func)(double[]);
} n_function;

/**
* a vector function
*/
typedef struct {
    char* input_names[];
    char* output_names[];
    double (*func)(double[])[];
} vector_function;

/**
* a parameter
*/
typedef union {
    tree tree_value;
    matrix matrix_value;
    function function_value;
    n_function n_function_value;
    vector_function vector_function_value;
} parameter;

/**
* A single entry in parameter list
*/
typedef struct {
    char* role;
    parameter_type type;
    parameter par;
} parameter_entry;

/**
* a set of parameters
*/
typedef struct {
    parameter_entry entries[];
} parameters;
