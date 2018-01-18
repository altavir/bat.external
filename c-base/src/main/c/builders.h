//
// Created by darksnake on 06.01.2018.
//

#ifndef C_BASE_BUILDERS_H
#define C_BASE_BUILDERS_H

#include <mem.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parameters.h"

/**
 * Build an empty tree
 * @return
 */
Tree emptyTree();

/**
 * Append an entry to the tree
 * @param tree not null
 * @param entry
 * @return
 */
Tree appendValue(Tree tree, TreeEntry entry);

/**
 * Append integer to the tree
 * @param tree
 * @param key
 * @param value
 * @return
 */
Tree appendInt(Tree tree, String key, int value);

/**
 * Append double to tree
 * @param tree
 * @param key
 * @param value
 * @return
 */
Tree appendDouble(Tree tree, String key, double value);

/**
 * Append boolean to tree
 * @param tree
 * @param key
 * @param value
 * @return
 */
Tree appendBoolean(Tree tree, String key, Boolean value);

/**
 * Append string to tree
 * @param tree
 * @param key
 * @param value
 * @return
 */
Tree appendString(Tree tree, String key, String value);

/**
 * Append a tree node to tree
 * @param tree
 * @param key
 * @param node
 * @return
 */
Tree appendNode(Tree tree, String key, Tree node);

/**
 *
 * @param size
 * @param values
 * @param names optional
 * @return
 */
Vector buildVector(short size, double values[], String names[]);

/**
 * Create new matrix
 * @param rows
 * @param rowNames optional
 * @param columns
 * @param columnNames optional
 * @param values
 * @return
 */
Matrix buildMatrix(short rows, String rowNames[], short columns, String columnNames[], double **values);

/**
 * Convenience method to create a Function
 * @param func
 * @return
 */
Function buildFunction(double (*func)(double));

/**
 * Create new NFunction
 * @param size
 * @param names
 * @param func
 * @return
 */
NFunction buildNFunction(short size, String names[], double (*func)(Vector));

/**
 * Create a mapping
 * @param inputSize
 * @param inputNames
 * @param outputSize
 * @param outputNames
 * @param func
 * @return
 */
Mapping buildMapping(
        short inputSize,
        String inputNames[],
        short outputSize,
        String outputNames[],
        Vector (*func)(Vector)
);

/**
 * Create a blob
 * @param size
 * @param data
 * @return
 */
Blob buildBlob(unsigned long size, unsigned char *data);

/**
 * Generate an empty set of parameters
 * @return
 */
Parameters emptyParameters();

/**
 * Append an entry to set of parameters
 * @param parameters
 * @param entry
 * @return
 */
Parameters appendParameterEntry(Parameters parameters, ParameterEntry entry);

/**
 * Append a tree to the list of parameters
 * @param parameters
 * @param role
 * @param value
 * @return
 */
Parameters appendTree(Parameters parameters, String role, Tree value);

/**
 * Append a vector to the list of parameters
 * @param parameters
 * @param role
 * @param value
 * @return
 */
Parameters appendVector(Parameters parameters, String role, Vector value);

/**
 * Append a matrix to the list of parameters
 * @param parameters
 * @param role
 * @param value
 * @return
 */
Parameters appendMatrix(Parameters parameters, String role, Matrix value);

/**
 * Append a function to the list of parameters
 * @param parameters
 * @param role
 * @param value
 * @return
 */
Parameters appendFunction(Parameters parameters, String role, Function value);

/**
 * Append NFunction to the list of parameters
 * @param parameters
 * @param role
 * @param value
 * @return
 */
Parameters appendNFunction(Parameters parameters, String role, NFunction value);

/**
 * Append mapping to the list of parameters
 * @param parameters
 * @param role
 * @param value
 * @return
 */
Parameters appendMapping(Parameters parameters, String role, Mapping value);

/**
 * Append a blob to the list of parameters
 * @param parameters
 * @param role
 * @param blob
 * @return
 */
Parameters appendBlob(Parameters parameters, String role, Blob blob);

/**
 * Free allocated memory for a set of parameters.
 * WARNING! Any use of parameters after memory is freed could cause segmentation fault
 * @param pars
 */
void freeParameters(Parameters pars);

#endif //C_BASE_BUILDERS_H
