# Mini-project: R Programming | Create a wrangling function

*Note: Projects that center on writing functions share a lot of similarity in structure. There is considerable overlap in the instructions across mini-projects, but there are also important differences. Please read carefully.*

Primary skills/categories targeted:

- Assessed: 6, 7, 8, 9, 10, 11, 12
- Unassessed: 1, 2, 3, 4, 5, 8, 12, 13

## Description

In this project, you will define a function for data wrangling. Your final product should be an .R script that defines your function, then compares the output with that of the original function. "Wrangling" is loosely defined here. It should be a function that serves a useful and plausible purpose for data preparation.

You may choose to create a .qmd notebook to complete this project, but I wouldn’t recommend it. Typically functions are defined in scripts, not notebooks. If you want to use a .qmd, I suggest actually defining the function in an .R script, then sourcing the script and calling the function in the .qmd.

In the instructions, I refer to your “function” (singular), but you define as many as you like. Defining several simple functions can be a way to show the same range of skills as one more complex function.

**Note: You may use your final research project data to guide the development of your function in this mini-project.** This is an exception to the general rule that mini-projects must be fully distinct from your research project. You do not need to request permission to use your project data for this mini-project. **However, code you submit for this mini-project will not also count toward demonstrating objectives on your final project.** 

## Instructions

1. Identify a data wrangling problem that needs a solution. The problem should apply to multiple contexts, such that a function defined to solve the problem can be used in more than one place. Ideally you should identify a need and develop the concept for your function based on a need you actually have (like something for your research project data wrangling), but you can alternatively select from one of the following examples if needed. Create a function that:
    1. Guesses what the best data type for a column in an existing data frame should be (similar to how `read_delim()` operates during import)
    2. Creates a complete summary table with a consistent structure that works flexibly with different input data
    3. Reshapes non-tidy data in a given export format (e.g., .edf EyeLink files, Praat/TextGrid, exports from some particular SQL database) into a tidy format suitable for analysis and visualization
    4. Joins more than two dataframes together with an argument to specify what kind of join to use (left, right, inner, outer)
2. Conceptually and programmatically define your function. See the example script in the `hello_world()` mini-project to get a sense of what this looks like.
    1. Write a “conceptual definition” of the function using plain-English comments. Start broad and narrow as you work.
    2. Fill in the code to match your commented function structure, revising the conceptual definition and comments as needed.
3. Run your function on a diverse set of test cases. Try to devise cases that would result in the function processing arguments in different ways. For example, arguments that would pass to different paths of a conditional statement, different data types for arguments that accept multiple, or leaving an optional argument blank (i.e., using the default option) for a portion of the test cases.

Minimally, your final script should include:

1. ***Many* comments.** Since the comments should help you conceptualize and structure your function, there will probably be many more than usual, and they will be much more verbose than usual.
2. **A “working” function.** It may not be impressive or do everything you wanted it to do. It may not give you the output you expect it to, but *it doesn’t throw an error when you run it* (at least not usually).
3. **A range of examples.** Depending on the complexity the function you define, this may only be a few cases or may be dozens of combinations of arguments. Use comments to note where output is notable in some way, like highlighting how your function handles a particularly unusual argument, when the function gives unexpected output, or when the function throws an error.

### Challenge suggestions

To make things trickier, show more skills, or earn engagement points, try one or more of the following:

1. Define multiple functions. As noted above, defining several simple functions can be a way to show the same range of skills as one more complex function. Beyond that, demonstrate off-the-syllabus skills and earn high engagement by defining multiple moderately complex functions and/or using one defined function in the definition of a different function.
2. Use conditional logic to control errors. In other words, when you find a test case that “breaks” your function with a fatal error, add code that purposefully stops the function and returns a custom, informative error message. A good starting place for this that applies to just about any function is restricting what data types each argument can be before running any of the function’s “real” code.
3. Include both required and optional arguments. Be sure to choose sensible default options for any optional arguments and structure the order of arguments in a reasonable and intuitive way.
4. Write documentation for your function. Use the CRAN format (what you see in the viewer when you type `?some_function`)as a template.

## Submission

1. Copy the `assessment.md` doc from your top-level directory into this project subdirectory.
2. Complete the assessment document.
3. On Canvas, go to the next available open mini-project assignment. In the text box, include:
    1. The name of the project: “Create a wrangling function”
    2. A link to the subdirectory for your mini-project
        1. If you completed the project in a separate repository, your `assessment.md` file in the subdirectory should explain how to access your work.
    3. Any additional info you'd like your grader to know
4. Submit the Canvas assignment. Your grader will access the directory you've linked to, grade your work on the assessment doc, push the doc back to your repository, and mark your Canvas assignment as complete.