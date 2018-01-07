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
 *
 * @param tree not null
 * @param entry
 * @return
 */
Tree append(Tree tree, TreeEntry entry);

Tree appendInt(Tree tree, String key, int value);

Tree appendDouble(Tree tree, String key, double value);

Tree appendBoolean(Tree tree, String key, Boolean value);

Tree appendString(Tree tree, String key, String value);

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
 *
 * @param rows
 * @param rowNames optional
 * @param columns
 * @param columnNames optional
 * @param values
 * @return
 */
Matrix buildMatrix(short rows, String rowNames[], short columns, String columnNames[], double **values);

NFunction buildNFunction(short size, String names[], double (*func)(Vector));

Mapping buildMapping(short inputSize,
                     String inputNames[],
                     short outputSize,
                     String outputNames[],
                     Vector (*func)(Vector));

Parameters emptyParameters();

Parameters appendParameterEntry(Parameters parameters, ParameterEntry entry);

Parameters appendTree(Parameters parameters, String role, Tree value);

Parameters appendVector(Parameters parameters, String role, Vector value);

Parameters appendMatrix(Parameters parameters, String role, Matrix value);

Parameters appendFunction(Parameters parameters, String role, Function value);

Parameters appendNFunction(Parameters parameters, String role, NFunction value);

Parameters appendMapping(Parameters parameters, String role, Mapping value);

#endif //C_BASE_BUILDERS_H
