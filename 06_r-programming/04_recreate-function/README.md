# Mini-project: R Programming | Recreate an existing function

*Note: Projects that center on writing functions share a lot of similarity in structure. There is considerable overlap in the instructions across mini-projects, but there are also important differences. Please read carefully.*

Primary skills/categories targeted:

- Assessed: 6, 7, 8, 9, 10, 11, 12
- Unassessed: 1, 2, 4, 5, 8

## Description

In this project, you will recreate one or more existing functions from base R or a package of your choice. Your final product should be an .R script that defines your function, then compares the output with that of the original function.

You may choose to create a .qmd notebook to complete this project, but I wouldn’t recommend it. Typically functions are defined in scripts, not notebooks. If you want to use a .qmd, I suggest actually defining the function in an .R script, then sourcing the script and calling the function in the .qmd.

In the instructions, I refer to your “function” (singular), but you recreate as many as you like. Recreating several simple functions can be a way to show the same range of skills as one more complex function.

## Instructions

1. Choose an existing function to recreate from base R, the tidyverse, or another package you are independently familiar with. It should be a function you use frequently and feel confident using in your own code. Don’t worry too much about how complex it is.
2. Examine how the existing function is defined. You can do this either by looking in the package’s GitHub repo or by typing the name of the function (e.g., `paste` or `filter` , not `paste()` or `?filter`) into the RStudio console. You may find the definition completely impossible to parse, and that’s fine! Ask yourself (with the understanding that it’s ok to not be able to answer any or all of these):
    1. What is the general logic behind the function? 
    2. Which functions contained within the function do I know how to use?
    3. Are there arguments that you don’t usually use? If so, which parts of the definition are mapped to those arguments? (Corollary: which ones *aren’t* mapped to anything related to those arguments?)
    4. Will having this definition as a reference help me guide defining my own function? If yes, do that! If no, put it aside completely for now. You don’t need to complicate things for yourself.
3. Write a “conceptual definition” of the function using comments. (See the W3C2 slides for an example of what this might look like for the `filter()` function.) Focus on outlining the way the function works for your typical use cases. In other words, if your function has 20 optional arguments but you have only ever used the 2 required arguments, forget about those 20 optional arguments for now; pretend the function works only in the ways you’re already familiar with.
4. Use `function()` to assign the beginnings of a real (code) definition to your recreation. **Important: Give your function a unique name so you can easily run each at any point.** If you recreate `filter()` , call it something like `my_filter()` , `filter_new()` , or `filter2()` , etc.
5. Using your conceptual definition to guide you, fill in as much code as you can to define a function that works like the original. You will likely need to revise your conceptual outline as you work and notice points that need correction, more specification, or major additions/deletions.
6. Run the original function on a diverse set of test cases. Try to devise cases that would result in the function processing arguments in different ways. For example, arguments that would pass to different paths of a conditional statement, different data types for arguments that accept multiple, or leaving an optional argument blank (i.e., using the default option) for a portion of the test cases.
7. Run the same test cases when your new function. Which cases, if any:
    1. Return the same output as the original?
    2. Return output similar — but not identical — to the original?
    3. Run successfully with very different — but sensible — output from the original?
    4. Run successfully with completely unexpected output?
    5. Do not run without error?
8. Continue refining your function and repeating the test cases until you are happy with your end product. It is not necessary for you to fully and accurately recreate the function! Just give it your best shot and walk us through your reasoning, attempted solutions, and unresolved problem points.

Minimally, your final script should include:

1. ***Many* comments.** Since the comments should help you conceptualize and structure your function, there will probably be many more than usual, and they will be much more verbose than usual.
2. **A “working” function.** Even if you make very little headway into recreation, you should end up with a function that runs without errors. Maybe it doesn’t do anything at all or does something totally different from the original, but it doesn’t throw an error when you run it.
3. **A range of examples.** Assuming you get some kind of replicated function defined (not just an empty function that runs without error), run your function and the original on a range of similar arguments. Depending on the function you choose and the complexity of what you’re able to reproduce, this may just be 2-3 cases, but try to get about 10(+) if it’s reasonable to do so.
    1. If you don’t get a working replicated function, give a few examples of the original function working in different test cases, and then use comments to explain what you think is going on based on your conceptual definition.

### Challenge suggestions

To make things trickier, show more skills, or earn engagement points, try one or more of the following:

1. Increase the number of arguments you account for in your replication. The addition of an optional argument is essentially the addition of a conditional statement, so you can approach incorporating new arguments into your function one by one.
2. Recreate the function using more than one conceptual or programmatic approach. For example, define `my_function_1` as a series of conditional statements and `my_function_2` with loops. Or define `my_function_tidy` with tidyverse functions and `my_function_baseR` using only functions in base R.
3. Recreate multiple functions. As noted above, recreating several simple functions can be a way to show the same range of skills as one more complex function. Beyond that, demonstrate off-the-syllabus skills and earn high engagement by recreating multiple moderately complex functions and/or using one recreated function in the recreation of a different function.

## Submission

1. Copy the `assessment.md` doc from your top-level directory into this project subdirectory.
2. Complete the assessment document.
3. On Canvas, go to the next available open mini-project assignment. In the text box, include:
    1. The name of the project: “Recreate an existing function”
    2. A link to the subdirectory for your mini-project
        1. If you completed the project in a separate repository, your `assessment.md` file in the subdirectory should explain how to access your work.
    3. Any additional info you'd like your grader to know
4. Submit the Canvas assignment. Your grader will access the directory you've linked to, grade your work on the assessment doc, push the doc back to your repository, and mark your Canvas assignment as complete.