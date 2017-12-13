package hep.bat2.api

class TaskNotSupportedException(val task: String, message: String = "") : RuntimeException(message);

class ParameterNotFoundException(val param: String, message: String = "") : RuntimeException(message);

class AmbiguousParameterException(val param: String, message: String = "") : RuntimeException(message);