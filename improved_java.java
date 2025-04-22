import java.util.ArrayList;
import java.util.List;

/**
 * Processes a list of integers according to business rules.
 */
public class NumberProcessor {

    /**
     * Retains the last parameter processed by the business logic.
     */
    private static String lastProcessedParameter = "initial";

    /**
     * Gets the last processed parameter value.
     * @return The parameter.
     */
    public static String getLastProcessedParameter() {
        return lastProcessedParameter;
    }

    /**
     * Processes the input list by collecting integers greater than 10, as a comma-separated string.
     * Updates the global parameter record.
     * 
     * @param values    the list of integers to process (may be null).
     * @param parameter label or identifier for the process cycle.
     * @return a comma-separated string of valid numbers, or empty string if none found.
     * @throws IllegalArgumentException if parameter is null.
     */
    public static String processValues(List<Integer> values, String parameter) {
        if (parameter == null) {
            throw new IllegalArgumentException("Parameter must not be null");
        }
        lastProcessedParameter = parameter;

        if (values == null) {
            System.out.println("No input list for param: " + parameter);
            return "";
        }

        StringBuilder result = new StringBuilder();
        for (Integer val : values) {
            if (val != null && val > 10) {
                if (result.length() > 0) {
                    result.append(",");
                }
                result.append(val);
            }
        }

        if (result.length() == 0) {
            System.out.println("No items found matching criteria for param: " + parameter);
        }

        return result.toString();
    }

    /**
     * Safely divides x by y, throwing an exception for division by zero.
     * 
     * @param x dividend
     * @param y divisor
     * @return x divided by y
     * @throws ArithmeticException if y == 0
     */
    public static int safeDivide(int x, int y) {
        if (y == 0) {
            throw new ArithmeticException("Division by zero is not allowed.");
        }
        return x / y;
    }

    public static void main(String[] args) {
        List<Integer> data = new ArrayList<>();
        data.add(5);
        data.add(15);
        data.add(null);
        data.add(25);
        data.add(8);

        System.out.println("Initial lastProcessedParameter: " + getLastProcessedParameter());

        String processedData = null;
        try {
            processedData = processValues(data, "run1");
        } catch (IllegalArgumentException e) {
            System.err.println("Error during processValues: " + e.getMessage());
        }
        System.out.println("Processed Data: " + processedData);
        System.out.println("lastProcessedParameter after run1: " + getLastProcessedParameter());

        try {
            System.out.println("\nTrying division:");
            int divResult = safeDivide(10, 0);
            System.out.println("Division result: " + divResult);
        } catch (ArithmeticException e) {
            System.err.println("Caught division error: " + e.getMessage());
        }

        System.out.println("\nTrying processValues with null list:");
        try {
            processedData = processValues(null, "run2");
            System.out.println("Processed Data (null list): " + processedData);
        } catch (IllegalArgumentException e) {
            System.err.println("Error during processValues (null list): " + e.getMessage());
        }
        System.out.println("lastProcessedParameter after run2: " + getLastProcessedParameter());
    }
}
