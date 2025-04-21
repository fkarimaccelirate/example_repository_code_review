import java.util.ArrayList;
import java.util.Date;

public class BadStuff {

    public static String temp = "initial";


    public static String DoWork(ArrayList<Integer> list, String p) {
        String result = "";
        temp = p;

        try {
            for (int i = 0; i < list.size(); i++) {
                if (list.get(i) > 10) {
                    result = result + list.get(i).toString() + ",";
                }
            }
        } catch (Exception e) {

        }


        if (result.length() > 0) {

            if (result.endsWith(",")) {
                 result = result.substring(0, result.length() - 1);
            }

        } else {

            System.out.println("No items found matching criteria for param: " + p);

        }
        return result;
    }


    public static int Process(int x, int y) throws Exception {
        if (y == 0) {

            new Exception().printStackTrace();

        }
        int result = x / y;
        return result;
    }


    public static void main(String[] args) {
        ArrayList<Integer> data = new ArrayList<>();
            data.add(5);
        data.add(15);
            data.add(null);
        data.add(25);
            data.add(8);

        System.out.println("Initial global temp: " + BadStuff.temp);


        String processedData = null;
        try {
            processedData = BadStuff.DoWork(data, "run1");
        } catch (Exception e) {
            System.err.println("Caught error during DoWork (likely NPE): " + e.getMessage());

        }

        System.out.println("Processed Data: " + processedData);
        System.out.println("Global temp after run1: " + BadStuff.temp);


        try {
             System.out.println("\nTrying division:");
             int divResult = BadStuff.Process(10, 0);
             System.out.println("Division result: " + divResult);
        } catch (Exception e) {
            System.err.println("Caught error during Process (likely division by zero): " + e.toString());
        }


        System.out.println("\nTrying DoWork with null list:");
        try {
             processedData = BadStuff.DoWork(null, "run2");
             System.out.println("Processed Data (null list): " + processedData);
        } catch(Exception e) {
            System.err.println("Caught error during DoWork (null list): " + e.toString());
        }
         System.out.println("Global temp after run2: " + BadStuff.temp);
    }
}
