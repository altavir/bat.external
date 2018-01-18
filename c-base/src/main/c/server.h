#ifndef C_BASE_SERVER_H
#define C_BASE_SERVER_H

#include "parameters.h"

/**
 * Invoke given task with a list of parameters and return the result
 * @param task
 * @param parameters
 * @return
 */
Parameters call(String task, Parameters parameters);

/**
 * Check if given parameters are valid for the task and return result as a tree
 * @param task
 * @param parameters
 * @return
 */
Tree check(String task, Parameters parameters);

/**
 * List of strings
 */
typedef struct {
    int size;
    String* values;
} StringList;

/**
 * A list of available tasks
 * @return
 */
StringList tasks();

#endif //C_BASE_SERVER_H