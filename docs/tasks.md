# Task mechanics

Server could implement a number of tasks. Each task is defined by its name and specification of input and output parameters.

## Echo

Echo (task name `echo`) is a test task for compatibility checks. It returns the same parameters it received plus
additional tree called `result`, which contains a node for each of received parameters with the same name as a role of 
the parameter.

### Input

Any parameters

### Output

The same parameters as in input plus tree containing description of input parameters.

**TODO: update echo specification**

## Integrate

Integrate the presented model as a functions and return single number.

### Input

* *config* (**Tree**): **TODO**

* *model* (**Tree**, **NFunction**): Definition of model

### Output

* *result* (**Tree**): **TODO**

