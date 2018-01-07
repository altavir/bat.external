//
// Created by darksnake on 07.01.2018.
//
#include "server.h"
#include "builders.h"
#include "parameters.h"


/**
 * echo server implementation
 */
String echoTask = "echo";

Parameters call(String task, Parameters parameters) {
    Tree tree = emptyTree();

    for (int i = 0; i < parameters.size; i++) {
        ParameterEntry entry = parameters.entries[i];
        append(tree, entry);
        switch (entry.type) {
            case TREE:
                appendNode(tree, entry.role, entry.par.treeValue);
                break;
            case VECTOR:
                appendString(tree, entry.role, "vector");
                break;
            case MATRIX:
                appendString(tree, entry.role, "matrix");
                break;
            case FUNCTION:
                appendString(tree, entry.role, "function");
                break;
            case N_FUNCTION:
                appendString(tree, entry.role, "nFunction");
                break;
            case MAPPING:
                appendString(tree, entry.role, "mapping");
                break;
        }
    }

    Parameters result = appendTree(emptyTree(), "result", tree);

    return result;
}

Tree check(String task, Parameters parameters) {
    Tree result = emptyTree();
    if (strcmp(task, echoTask) == 0) {
        appendBoolean(result, "OK", true);
    } else {
        appendBoolean(result, "OK", false);
        appendString(result, "error", "Task not supported");
    }
    return result;
}

StringList tasks() {
    StringList result;
    result.size = 1;
    result.values = malloc(sizeof(String));
    result.values[0] = echoTask;
    return result;
}

