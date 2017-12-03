package hep.bat2

class TaskNotSupportedException(val task: String, message: String = "") : RuntimeException(message);

class ParameterNotFoundException(val param: String, message: String = "") : RuntimeException(message);

class AmbiguousParameterException(val param: String, message: String = "") : RuntimeException(message);