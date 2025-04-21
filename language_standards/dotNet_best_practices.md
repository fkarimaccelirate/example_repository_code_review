# **Official Best Practice Guidelines for .NET (C\#) Development**

## **3..NET (C\#) Best Practices (Microsoft)**

### **3.1. Introduction & Core Philosophy**

Microsoft provides comprehensive coding conventions and style guidelines for.NET development, with a strong focus on C\#.27 The core philosophy emphasizes writing code that is clear, readable, maintainable, and leverages the modern features of the C\# language and.NET platform.27 A key aspect of the Microsoft approach is the tight integration with development tooling, particularly Visual Studio and the.NET SDK (including Roslyn analyzers). Consistency is encouraged and facilitated through the use of .editorconfig files, which allow teams to define and enforce code style rules directly within their projects.30 These rules cover naming, formatting, and language usage preferences.

### **3.2. Naming Conventions** 27

Microsoft's naming guidelines for C\# promote consistency and clarity:

* **Casing:**  
  * PascalCase is used for types (classes, structs, interfaces, enums, delegates, records), namespaces, public members (methods, properties, events, constants, public fields).33  
  * camelCase is used for method parameters and local variables.33 For primary constructor parameters, PascalCase is used on record types, while camelCase is used on class and struct types.27  
* **Interfaces:** Names should start with a capital I followed by PascalCase (e.g., IComponentConnector, IReadOnlyList).33  
* **Fields:**  
  * private or internal instance fields should be prefixed with an underscore (\_) followed by camelCase (e.g., \_connectionString).33  
  * static fields may use the s\_ prefix, and thread static fields may use t\_.33  
* **Attributes:** Custom attribute class names should end with the suffix Attribute.33  
* **Enums:** Use a singular noun for enum types that do not represent flags, and a plural noun for enum types that do represent flags (marked with \[Flags\]).33  
* **Generic Type Parameters:** Use descriptive names prefixed with T (e.g., TKey, TValue). For simple, single type parameters where the meaning is clear, T alone is acceptable (e.g., List\<T\>). Consider indicating constraints in the name (e.g., TConstraint where TConstraint : IConstraint).33  
* **General Rules:** Prefer clarity over brevity; use meaningful and descriptive names.33 Avoid abbreviations unless they are widely known and accepted (e.g., IO, UI).33 Do not use two consecutive underscores (\_\_) in identifiers.33 Identifiers must start with a letter or underscore.33 Use the @ prefix to use reserved keywords as identifiers (e.g., @event).33

**Table:.NET (C\#) Naming Conventions Summary**

| Identifier Type | Convention | Example(s) |
| :---- | :---- | :---- |
| Types (Class, Struct, Enum, etc.) | PascalCase | AppDomain, HttpClient, FileMode |
| Interfaces | I prefix \+ PascalCase | IComparer\<T\>, IDisposable, IWorkerQueue |
| Methods | PascalCase | ToString(), GetHashCode(), ConnectAsync() |
| Properties, Events, Constants | PascalCase | Length, DefaultTimeout, Click |
| Public Fields | PascalCase | *(Generally discouraged; prefer properties)* |
| Parameters | camelCase | buffer, cancellationToken, userName |
| Local Variables | camelCase | index, totalCount, customerName |
| Private/Internal Instance Fields | \_ prefix \+ camelCase | \_items, \_logger, \_workerQueue |
| Generic Type Parameters | Descriptive name prefixed with T, or single T | T, TInput, TOutput, TSession |

### **3.3. Code Structure and Formatting** 27

Consistent formatting is enforced through .editorconfig settings and IDE defaults.

* **Indentation:** Use four spaces per indentation level. Do not use tabs.27 Configurable via .editorconfig: indent\_style \= space, indent\_size \= 4\.31  
* **Braces:** Use the "Allman" style, where opening and closing braces are placed on their own new lines and aligned with the surrounding indentation level.27 Microsoft recommends always using braces, even for single-line blocks (if, for, etc.) 30 (csharp\_prefer\_braces \= true).  
* **Spacing:** Use a single space after the // comment delimiter.27 Use spaces around binary operators (a \+ b) 31 and after keywords like if, for, catch, and commas in argument lists.32 Do not use spaces after opening parentheses ( or before closing parentheses ) 32, nor between square brackets \[ and \].32  
* **Newlines:** Place a blank line between method definitions and property definitions.27 .editorconfig allows fine-grained control over newlines before/after specific constructs (e.g., csharp\_new\_line\_before\_open\_brace, csharp\_new\_line\_between\_query\_expression\_clauses).31  
* **Statements and Declarations:** Write only one statement per line.27 Write only one declaration per line.27  
* **Line Length:** While not a strict compiler rule, documentation examples often limit lines to 65 characters for readability on various devices.27 In general practice, limits like 100 or 120 characters are common. Break long lines for clarity, preferably before binary operators.27  
* **using Directives:** Place all using directives at the top of the file, outside of any namespace declarations.27 Order them with System.\* namespaces first, followed by others alphabetically.  
* **File-Scoped Namespaces:** In C\# 10 and later, prefer file-scoped namespace declarations (namespace MyNamespace;) for files containing a single namespace, reducing nesting.27  
* **Modifiers:** The preferred order of modifiers (e.g., public static readonly) can be defined and enforced via .editorconfig (csharp\_preferred\_modifier\_order).30  
* **Configuration via .editorconfig:** Microsoft heavily promotes the use of .editorconfig files to define and enforce these formatting and style rules consistently across a team or project.30 Visual Studio and the.NET CLI respect these files, providing warnings or errors for violations.

### **3.4. Language Usage** 27

Microsoft guidelines encourage the use of modern C\# features to write concise, readable, and efficient code.

* **Modern Features:** Embrace newer language features like pattern matching, expression-bodied members, tuples, records, using declarations, and nullable reference types where they improve clarity and conciseness.27 Avoid outdated constructs.27  
* **var (Implicit Typing):** Use var for local variable declarations only when the assigned type is clearly apparent from the right-hand side of the assignment (e.g., var stream \= new MemoryStream();, var count \= (int)obj;, var name \= "Example";).27 Avoid var if the type is not obvious or if it hinders readability. Use explicit types in foreach loop variables (foreach (Customer customer in customers)) but implicit types (var) are acceptable in for loops (for (var i \= 0;...)). Do not use var when the type is dynamic.27  
* **LINQ (Language Integrated Query):** Utilize LINQ queries and methods for querying and manipulating collections, as it often improves readability compared to manual loops.27 Use meaningful names for query variables (e.g., var seattleCustomers \=...). Use PascalCase for properties of anonymous types created in select clauses. Use multiple from clauses or method syntax (SelectMany) for accessing inner collections rather than complex join clauses where appropriate.27  
* **Asynchronous Programming:** Use async and await for I/O-bound operations (network calls, file access, database queries) to improve responsiveness and scalability.27 In library code, use Task.ConfigureAwait(false) to avoid potential deadlocks when resuming on the original context is not necessary.27 Avoid async void methods except for top-level event handlers; prefer async Task.34  
* **Data Types:** Use the C\# language keywords for built-in types (e.g., int, string, double, object) instead of the corresponding.NET runtime type names (System.Int32, System.String, System.Double, System.Object).27 Generally prefer int over unsigned integer types (uint, ulong) unless specifically dealing with bit manipulation or interoperability requiring unsigned types.27  
* **Object and Collection Initialization:** Use object initializers (var point \= new Point { X \= 0, Y \= 0 };) and collection initializers (var list \= new List\<int\> { 1, 2, 3 };) to simplify instantiation.27 Use the concise new() target-typed expression where the type is known (List\<string\> names \= new();).27  
* **Null Handling:** Use is null and is not null for null checks (dotnet\_style\_prefer\_is\_null\_check\_over\_reference\_equality\_method).30 Utilize the null-conditional operator (?.) for safe member access and the null-coalescing operator (??) for providing default values (dotnet\_style\_null\_propagation, dotnet\_style\_coalesce\_expression).30 Enable and adhere to nullable reference types (\#nullable enable) to prevent NullReferenceExceptions at compile time.34  
* **Expression-bodied Members:** Use expression bodies (=\>) for simple, single-expression implementations of methods, properties (getters), constructors, operators, and accessors to reduce boilerplate code (csharp\_style\_expression\_bodied\_\*).30  
* **Properties:** Prefer auto-implemented properties (public int Age { get; set; }) when no additional logic is needed in the accessors (dotnet\_style\_prefer\_auto\_properties).30  
* **Tuples:** When returning multiple values, prefer using value tuples with explicit element names ((int count, double sum)) over anonymous tuples or out parameters (dotnet\_style\_explicit\_tuple\_names).30 Use inferred tuple names (dotnet\_style\_prefer\_inferred\_tuple\_names) when the names are obvious from the assigned variables.30  
* **Pattern Matching:** Leverage pattern matching features (e.g., type patterns, property patterns, relational patterns) to simplify complex conditional logic, especially for type checks followed by casts or null checks (csharp\_style\_pattern\_matching\_over\_\*).30

### **3.5. Exception Handling** 27

Proper exception handling ensures application stability and provides useful diagnostic information.

* **Catch Specific Exceptions:** Only catch exceptions that the code can genuinely handle or recover from. Avoid catching generic base types like System.Exception or System.SystemException unless it's at a top-level handler intended to log unexpected errors before application termination, or if using an exception filter.27 Catching specific exception types (e.g., FileNotFoundException, ArgumentNullException) allows for tailored error handling logic.27  
* **Resource Management (IDisposable):** Use the using statement or using declaration for any object that implements IDisposable. This guarantees that the Dispose() method is called to release unmanaged resources (like file handles, network connections, database connections) even if an exception occurs.27 This is the preferred modern replacement for manual try/finally blocks solely used for calling Dispose().

### **3.6. Security Guidelines** 35

Microsoft places a strong emphasis on security throughout the.NET development lifecycle, providing extensive guidance and platform features. Security is treated as a primary concern, not an afterthought.

* **Input Validation and Sanitization:** Treat all external input (user input, data from files, network requests) as untrusted. Rigorously validate input against expected formats, lengths, and ranges. Sanitize input to prevent injection attacks like SQL Injection (SQLi) and Cross-Site Scripting (XSS).35 In ASP.NET Core, leverage model binding and data annotations for built-in validation.35  
* **Authentication and Authorization:** Implement robust authentication mechanisms, strongly recommending Multi-Factor Authentication (MFA).35 Utilize standard, secure protocols like OAuth 2.0 and OpenID Connect for identity management.35 Leverage built-in frameworks like ASP.NET Core Identity for managing users, roles, and claims.35 Implement authorization logic using role-based or policy-based approaches to enforce the principle of least privilege.37  
* **Data Protection and Encryption:** Always use HTTPS (TLS) to encrypt data in transit.35 Configure security-related HTTP headers like Content Security Policy (CSP), HTTP Strict Transport Security (HSTS), and X-Content-Type-Options to mitigate common web vulnerabilities.35 Use the.NET Data Protection APIs for encrypting sensitive data at rest, such as configuration secrets, connection strings, or authentication tokens.35  
* **Cryptography:** Use strong, modern cryptographic algorithms provided by the.NET platform.35 Avoid implementing custom cryptographic algorithms. Leverage built-in APIs for hashing, encryption, and digital signatures. Stay updated on best practices for key management and algorithm choices.  
* **Transport Layer Security (TLS):** Target newer.NET versions (e.g.,.NET Framework 4.8+, or modern.NET Core/.NET 5+) for support of the latest TLS versions (TLS 1.3).39 **Crucially, do not explicitly specify a TLS version in code.** Instead, allow the operating system to negotiate the highest mutually supported secure protocol by using SecurityProtocolType.SystemDefault for ServicePointManager or SslProtocols.None / SslProtocols.SystemDefault for SslStream.39 Audit code thoroughly to remove any hardcoded references to older protocols like SSL3, TLS 1.0, or TLS 1.1.39 Understand how AppContext switches and Windows Registry settings can influence TLS behavior, but prefer OS defaults.39  
* **Dependency Management (NuGet Security):** Software composition analysis is critical. Regularly update all NuGet package dependencies to their latest stable versions to incorporate security patches.35 Utilize tools integrated into the.NET ecosystem (e.g., dotnet list package \--vulnerable, NuGetAudit in Visual Studio, GitHub Dependabot, GitHub Advanced Security) to automatically scan for known vulnerabilities in both direct and transitive dependencies.35 Understand the full dependency graph (dotnet nuget why).40 Use package source mapping and lock files (packages.lock.json) to ensure repeatable builds and mitigate risks like dependency confusion.40 Consider configuring client trust policies to require signed packages.40  
* **Avoid Deprecated/Insecure Features:** **Explicitly avoid** using legacy.NET security mechanisms: Code Access Security (CAS), running partially trusted code, the AllowPartiallyTrustedCallersAttribute (APTCA),.NET Remoting, Distributed Component Object Model (DCOM), and Binary Formatters (BinaryFormatter).36 These are no longer considered effective security boundaries. For code sandboxing or isolation, use modern OS-level mechanisms like separate processes, containers (Hyper-V, Docker), AppContainers, or virtualization.36  
* **Platform Security Baselines:** Apply recommended security configurations for the underlying operating system and platform components. Utilize Microsoft security baselines as a well-tested starting point for hardening configurations.41