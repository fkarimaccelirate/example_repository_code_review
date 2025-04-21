using System;
using System.Collections.Generic;

public class BadStuff
{
    public static string data = "initial";

    public void Process(int x)
    {
        int y = 10;
        string s = "some_value";

        try
        {
            int result = y / (x - 5);
            Console.WriteLine("Result: " + result);

            BadStuff.data = s + result.ToString();

            List<string> l = new List<string>();
            l.Add("item1");
            Console.WriteLine(l[1]);

        }
        catch (Exception ex)
        {
            // Intentionally left empty to demonstrate bad practice
        }

        if (x > 99)
        {
            Console.WriteLine("Big number!");
        }
    }

    public static void Util()
    {
        Console.WriteLine("Doing something with: " + BadStuff.data);
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        BadStuff bs = new BadStuff();

        bs.Process(5);
        bs.Process(15);
        bs.Process(100);

        BadStuff.Util();

        AnotherBadClass.DoWork();
    }
}

public class AnotherBadClass
{
    public static void DoWork()
    {
        Console.WriteLine("Another class using: " + BadStuff.data);
        if (BadStuff.data.Length > 15)
        {
            Console.WriteLine("Data is long");
        }
    }
}
