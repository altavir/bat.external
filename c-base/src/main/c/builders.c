//
// Created by darksnake on 06.01.2018.
//
#include "builders.h"
#include "parameters.h"


TreeEntry buildIntValue(String key, const int value) {
    TreeEntry entry;
    entry.key = (String) key;
    entry.type = INT;
    entry.value.intValue = value;
    return entry;
}

TreeEntry buildDoubleValue(String key, const double value) {
    TreeEntry entry;
    entry.key = (String) key;
    entry.type = DOUBLE;
    entry.value.doubleValue = value;
    return entry;
}

TreeEntry buildBooleanValue(String key, const Boolean value) {
    TreeEntry entry;
    entry.key = (String) key;
    entry.type = BOOLEAN;
    entry.value.booleanValue = value;
    return entry;
}

TreeEntry buildStringValue(String key, String value) {
    TreeEntry entry;
    entry.key = (String) key;
    entry.type = STRING;
    entry.value.stringValue = value;
    return entry;
}

Tree emptyTree() {
    Tree tree;
    tree.size = 0;
    tree.entries = NULL;
    return tree;
}

Tree append(Tree tree, TreeEntry entry) {
    tree.size++;
    tree.entries = realloc(tree.entries, tree.size * sizeof(TreeEntry));
    tree.entries[tree.size - 1] = entry;
    return tree;
}

Tree appendInt(Tree tree, String key, const int value) {
    return append(tree, buildIntValue(key, value));
}

Tree appendDouble(Tree tree, String key, const double value) {
    return append(tree, buildDoubleValue(key, value));
}

Tree appendBoolean(Tree tree, String key, const Boolean value) {
    return append(tree, buildBooleanValue(key, value));
}

Tree appendString(Tree tree, String key, String value) {
    return append(tree, buildStringValue(key, value));
}

Tree appendNode(Tree tree, String key, Tree node) {
    for (int i = 0; i < node.size; i++) {
        TreeEntry oldEntry = node.entries[i];
        TreeEntry newEntry;
        newEntry.type = oldEntry.type;
        newEntry.value = oldEntry.value;
        //Use the same value but change the key as "$key.$old_key"
        strcpy(newEntry.key, key);
        strcat(newEntry.key, ".");
        strcat(newEntry.key, oldEntry.key);
        append(tree, newEntry);
    }
    return tree;
}

Vector buildVector(short size, double *values, String *names) {
    Vector vector;
    vector.size = size;
    vector.values = values;
    vector.names = names;
    return vector;
}

Matrix buildMatrix(short rows, String *rowNames, short columns, String *columnNames, double **values) {
    Matrix matrix;
    matrix.rows = rows;
    matrix.rowNames = rowNames;
    matrix.columns = columns;
    matrix.columnNames = columnNames;
    matrix.values = values;
    return matrix;
}

NFunction buildNFunction(short size, String *names, double (*func)(Vector)) {
    NFunction nFunction;
    nFunction.size = size;
    nFunction.names = names;
    nFunction.func = func;
    return nFunction;
}

Mapping
buildMapping(short inputSize, String *inputNames, short outputSize, String *outputNames, Vector (*func)(Vector)) {
    Mapping vectorFunction;
    vectorFunction.inputSize = inputSize;
    vectorFunction.inputNames = inputNames;
    vectorFunction.outputSize = outputSize;
    vectorFunction.outputNames = outputNames;
    vectorFunction.func = func;
    return vectorFunction;
}

Parameters emptyParameters() {
    Parameters parameters;
    parameters.size = 0;
    parameters.entries = NULL;
    return parameters;
}

Parameters appendParameterEntry(Parameters parameters, ParameterEntry entry) {
    parameters.size++;
    parameters.entries = realloc(parameters.entries, parameters.size * sizeof(ParameterEntry));
    parameters.entries[parameters.size - 1] = entry;
    return parameters;
}

Parameters appendTree(Parameters parameters, String role, Tree value) {
    ParameterEntry entry;
    entry.role = role;
    entry.type = TREE;
    entry.par.treeValue = value;
    return appendParameterEntry(parameters, entry);
}

Parameters appendVector(Parameters parameters, String role, Vector value) {
    ParameterEntry entry;
    entry.role = role;
    entry.type = VECTOR;
    entry.par.vectorValue = value;
    return appendParameterEntry(parameters, entry);
}

Parameters appendMatrix(Parameters parameters, String role, Matrix value) {
    ParameterEntry entry;
    entry.role = role;
    entry.type = MATRIX;
    entry.par.matrixValue = value;
    return appendParameterEntry(parameters, entry);
}

Parameters appendFunction(Parameters parameters, String role, Function value) {
    ParameterEntry entry;
    entry.role = role;
    entry.type = FUNCTION;
    entry.par.functionValue = value;
    return appendParameterEntry(parameters, entry);
}

Parameters appendNFunction(Parameters parameters, String role, NFunction value) {
    ParameterEntry entry;
    entry.role = role;
    entry.type = N_FUNCTION;
    entry.par.nFunctionValue = value;
    return appendParameterEntry(parameters, entry);
}

Parameters appendMapping(Parameters parameters, String role, Mapping value) {
    ParameterEntry entry;
    entry.role = role;
    entry.type = MAPPING;
    entry.par.mappingValue = value;
    return appendParameterEntry(parameters, entry);
}

//TODO all allocation is done on stack. Is it good?