using System;
using System.Collections.Generic;

public class DataProcessor
{
    private static string data = "initial";
    public static string Data => data; // Exposed as read-only

    public void Process(int inputValue)
    {
        const int DividerOffset = 5;
        const int DefaultValue = 10;
        const string SomeValue = "some_value";
        const int LargeNumberThreshold = 99;

        if (inputValue == DividerOffset)
        {
            Console.WriteLine("Error: Division by zero would occur.");
            return;
        }

        try
        {
            int result = DefaultValue / (inputValue - DividerOffset);
            Console.WriteLine($"Result: {result}");

            data = SomeValue + result.ToString();

            List<string> items = new List<string> { "item1" };
            if (items.Count > 1)
            {
                Console.WriteLine(items[1]);
            }
            else
            {
                Console.WriteLine("List does not contain enough elements.");
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Exception occurred: {ex.Message}");
        }

        if (inputValue > LargeNumberThreshold)
        {
            Console.WriteLine("Big number!");
        }
    }

    public static void Report()
    {
        Console.WriteLine("Doing something with: " + Data);
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        DataProcessor processor = new DataProcessor();

        processor.Process(5);    // Will safely alert no division by zero
        processor.Process(15);
        processor.Process(100);

        DataProcessor.Report();

        AuxiliaryWorker.DoWork();
    }
}

public class AuxiliaryWorker
{
    public static void DoWork()
    {
        Console.WriteLine("Auxiliary class using: " + DataProcessor.Data);
        if (DataProcessor.Data.Length > 15)
        {
            Console.WriteLine("Data is long");
        }
    }
}
