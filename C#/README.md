# C# (CSharp) Algorithms in OpenSauce

This folder provides beginner-friendly, production-ready implementations of classic Data Structures and Algorithms (DSA) in C#.

## Prerequisites

- .NET SDK 8.0 or later installed. Download from: https://dotnet.microsoft.com/download
- A terminal with `dotnet` on PATH (Windows PowerShell, macOS Terminal, Linux shell).

## Getting Started

1. Verify installation:

```bash
dotnet --version
```

2. Compile and run a single file (no project needed):

```bash
dotnet new console -n Scratch -o .tmp && del .tmp/Program.cs 2>$null && copy /Y C#/searching/BinarySearch.cs .tmp/Program.cs >NUL && dotnet run --project .tmp && rmdir /S /Q .tmp
```

Alternatively, from any algorithm directory you can do:

```bash
dotnet run --project .
```

Each algorithm file includes a `Main()` method with example test cases so you can run files standalone by placing them into a console project or using your IDE.

## Conventions

- PascalCase for class names, methods, and file names (e.g., `BinarySearch.cs`, `MergeSort.cs`).
- XML documentation comments for public classes and methods using `/// <summary> ... </summary>`.
- Time and space complexity documented above each method.
- Clear indentation and code style that follows .NET standards.
- No unused variables, redundant code, or inconsistent formatting.

## Structure

```
C#/
  arrays/
  sorting/
  searching/
  linkedlists/
  stacks_queues/
  trees/
  graphs/
```

## How to Run Examples

Most files are self-contained. The quickest way is to create a new console project and copy the example file as `Program.cs`.

```bash
dotnet new console -n DsaPlayground
copy C#/sorting/MergeSort.cs DsaPlayground/Program.cs
dotnet run --project DsaPlayground
```

On non-Windows systems, replace `copy` with `cp` and `del/rmdir` with appropriate commands.

## Contribution Notes

- Follow OpenSauce repository conventions and submit focused pull requests per topic.
- Ensure every algorithm compiles and runs using the .NET CLI.
- Keep implementations modular and heavily commented for beginners.
- Include example usage in `Main()` with expected outputs.


