package hep.bat2.jvmbridge;

import com.sun.jna.*;

import javax.security.auth.callback.Callback;
import java.util.Arrays;
import java.util.List;

public class NativeServer implements Library {
    public static final String JNA_LIBRARY_NAME = "NativeServer";
    public static final NativeLibrary JNA_NATIVE_LIB = NativeLibrary.getInstance(NativeServer.JNA_LIBRARY_NAME);
    public static final NativeServer INSTANCE = (NativeServer) Native.loadLibrary(NativeServer.JNA_LIBRARY_NAME, NativeServer.class);

    /**
     * enum values
     */
    public static interface parameter_type {
        /**
         * <i>native declaration : line 10</i>
         */
        public static final int TREE = 1;
        /**
         * <i>native declaration : line 11</i>
         */
        public static final int MATRIX = 2;
        /**
         * <i>native declaration : line 12</i>
         */
        public static final int FUNCTION = 3;
        /**
         * <i>native declaration : line 13</i>
         */
        public static final int N_FUNCTION = 4;
        /**
         * <i>native declaration : line 14</i>
         */
        public static final int VECTOR_FUNCTION = 5;
    }


    /**
     * enum values
     */
    public static interface value_type {
        /**
         * <i>native declaration : line 21</i>
         */
        public static final int INT = 1;
        /**
         * <i>native declaration : line 22</i>
         */
        public static final int DOUBLE = 2;
        /**
         * <i>native declaration : line 23</i>
         */
        public static final int BOOLEAN = 3;
        /**
         * <i>native declaration : line 24</i>
         */
        public static final int STRING = 4;
    }


    public static class value extends Union {
        public int int_value;
        public double double_value;
        public byte boolean_value;
        /**
         * C type : char*
         */
        public String string_value;

        public value() {
            super();
        }

        public value(double double_value) {
            super();
            this.double_value = double_value;
            setType(Double.TYPE);
        }

        public value(byte boolean_value) {
            super();
            this.boolean_value = boolean_value;
            setType(Byte.TYPE);
        }

        /**
         * @param string_value C type : char*
         */
        public value(String string_value) {
            super();
            this.string_value = string_value;
            setType(String.class);
        }

        public value(int int_value) {
            super();
            this.int_value = int_value;
            setType(Integer.TYPE);
        }

        public static class ByReference extends value implements Structure.ByReference {

        }


        public static class ByValue extends value implements Structure.ByValue {

        }


    }


    public static class tree_entry extends Structure {
        /**
         * C type : char*
         */
        public String key;
        /**
         * @see value_type
         * C type : value_type
         */
        public int type;
        /**
         * C type : value
         */
        public NativeServer.value val;

        public tree_entry() {
            super();
        }

        protected List<String> getFieldOrder() {
            return Arrays.asList("key", "type", "val");
        }

        /**
         * @param key  C type : char*<br>
         * @param type @see value_type<br>
         *             C type : value_type<br>
         * @param val  C type : value
         */
        public tree_entry(String key, int type, NativeServer.value val) {
            super();
            this.key = key;
            this.type = type;
            this.val = val;
        }

        public static class ByReference extends tree_entry implements Structure.ByReference {

        }


        public static class ByValue extends tree_entry implements Structure.ByValue {

        }


    }


    public static class tree extends Structure {
        /**
         * C type : tree_entry[]
         */
        public tree_entry[] entries;

        public tree() {
            super();
        }

        protected List<String> getFieldOrder() {
            return Arrays.asList("entries");
        }

        /**
         * @param entries C type : tree_entry[]
         */
        public tree(tree_entry[] entries) {
            super();
            this.entries = entries;
        }

        public static class ByReference extends tree implements Structure.ByReference {

        }


        public static class ByValue extends tree implements Structure.ByValue {

        }


    }


    public static class matrix extends Structure {
        /**
         * C type : char*[]
         */
        public String[] row_names;
        /**
         * C type : char*[]
         */
        public String[] column_names;
        /**
         * C type : double[][]
         */
        public Pointer values;

        public matrix() {
            super();
        }

        protected List<String> getFieldOrder() {
            return Arrays.asList("row_names", "column_names", "values");
        }

        /**
         * @param row_names    C type : char*[]
         * @param column_names C type : char*[]
         * @param values       C type : double[][]
         */
        public matrix(String[] row_names, String[] column_names, Pointer values) {
            super();
            this.row_names = row_names;
            this.column_names = column_names;
            this.values = values;
        }

        public static class ByReference extends matrix implements Structure.ByReference {

        }


        public static class ByValue extends matrix implements Structure.ByValue {

        }


    }


    public static class n_function extends Structure {
        /**
         * C type : char*[]
         */
        public String[] names;
        /**
         * C type : FuncCallback*
         */
        public FuncCallback func;

        public interface FuncCallback extends Callback {
            double apply(double[] doubleArr1);
        }


        public n_function() {
            super();
        }

        protected List<String> getFieldOrder() {
            return Arrays.asList("names", "func");
        }

        /**
         * @param names C type : char*[]<br>
         * @param func  C type : FuncCallback*
         */
        public n_function(String[] names, FuncCallback func) {
            super();
            this.names = names;
            this.func = func;
        }

        public static class ByReference extends n_function implements Structure.ByReference {

        }


        public static class ByValue extends n_function implements Structure.ByValue {

        }


    }


    public static class vector_function extends Structure {
        /**
         * C type : char*[]
         */
        public String[] input_names;
        /**
         * C type : char*[]
         */
        public String[] output_names;
        /**
         * C type : FuncCallback*
         */
        public FuncCallback func;

        public interface FuncCallback extends Callback {
            double[] apply(double[] doubleArr1);
        }


        public vector_function() {
            super();
        }

        protected List<String> getFieldOrder() {
            return Arrays.asList("input_names", "output_names", "func");
        }

        /**
         * @param input_names  C type : char*[]<br>
         * @param output_names C type : char*[]<br>
         * @param func         C type : FuncCallback*
         */
        public vector_function(String[] input_names, String[] output_names, FuncCallback func) {
            super();
            this.input_names = input_names;
            this.output_names = output_names;
            this.func = func;
        }

        public static class ByReference extends vector_function implements Structure.ByReference {

        }


        public static class ByValue extends vector_function implements Structure.ByValue {

        }


    }


    public static class parameter extends Union {
        /**
         * C type : tree
         */
        public NativeServer.tree tree_value;
        /**
         * C type : matrix
         */
        public NativeServer.matrix matrix_value;
        /**
         * C type : function
         */
        public NativeServer.function function_value;
        /**
         * C type : n_function
         */
        public NativeServer.n_function n_function_value;
        /**
         * C type : vector_function
         */
        public NativeServer.vector_function vector_function_value;

        public parameter() {
            super();
        }

        /**
         * @param vector_function_value C type : vector_function
         */
        public parameter(NativeServer.vector_function vector_function_value) {
            super();
            this.vector_function_value = vector_function_value;
            setType(NativeServer.vector_function.class);
        }

        /**
         * @param tree_value C type : tree
         */
        public parameter(NativeServer.tree tree_value) {
            super();
            this.tree_value = tree_value;
            setType(NativeServer.tree.class);
        }

        /**
         * @param matrix_value C type : matrix
         */
        public parameter(NativeServer.matrix matrix_value) {
            super();
            this.matrix_value = matrix_value;
            setType(NativeServer.matrix.class);
        }

        /**
         * @param n_function_value C type : n_function
         */
        public parameter(NativeServer.n_function n_function_value) {
            super();
            this.n_function_value = n_function_value;
            setType(NativeServer.n_function.class);
        }

        /**
         * @param function_value C type : function
         */
        public parameter(NativeServer.function function_value) {
            super();
            this.function_value = function_value;
            setType(NativeServer.function.class);
        }

        public static class ByReference extends parameter implements Structure.ByReference {

        }


        public static class ByValue extends parameter implements Structure.ByValue {

        }


    }


    public static class parameter_entry extends Structure {
        /**
         * C type : char*
         */
        public String role;
        /**
         * @see parameter_type
         * C type : parameter_type
         */
        public int type;
        /**
         * C type : parameter
         */
        public NativeServer.parameter par;

        public parameter_entry() {
            super();
        }

        protected List<String> getFieldOrder() {
            return Arrays.asList("role", "type", "par");
        }

        /**
         * @param role C type : char*<br>
         * @param type @see parameter_type<br>
         *             C type : parameter_type<br>
         * @param par  C type : parameter
         */
        public parameter_entry(String role, int type, NativeServer.parameter par) {
            super();
            this.role = role;
            this.type = type;
            this.par = par;
        }

        public static class ByReference extends parameter_entry implements Structure.ByReference {

        }


        public static class ByValue extends parameter_entry implements Structure.ByValue {

        }


    }


    public static class parameters extends Structure {
        /**
         * C type : parameter_entry[]
         */
        public parameter_entry[] entries;

        public parameters() {
            super();
        }

        protected List<String> getFieldOrder() {
            return Arrays.asList("entries");
        }

        /**
         * @param entries C type : parameter_entry[]
         */
        public parameters(parameter_entry[] entries) {
            super();
            this.entries = entries;
        }

        public static class ByReference extends parameters implements Structure.ByReference {

        }


        public static class ByValue extends parameters implements Structure.ByValue {

        }


    }


    public interface function extends Callback {
        double apply(double double1);
    }


}
