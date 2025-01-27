# Mini-project: Data wrangling | Group “make-a-mess”

*Note: Some projects that center on dataset manipulation share similarity in structure. There is considerable overlap in the instructions across some mini-projects, but there are also important differences. Please read carefully.*

Primary skills/categories targeted:

- Assessed: 5, 6, 8, 9, 10, 11, 12
- Unassessed: 1, 2, 5, 9, 14

Use notebooks to additionally demonstrate quarto or markdown skills.

## Description

In this project, you will work together with a classmate to each “(un)wrangle” a dataset for the other(s) to recreate. This will work best in pairs so that you can swap datasets (I’ll refer to “partners” in these instructions), but you can adjust things to do in a small group if you prefer.

This project is similar in structure to the group cleaning mini-project. The primary difference is that in this case you are not aiming for anything clean or tidy or trying to repair your partner’s work. Instead, you are manipulating data in whatever ways you like to challenge your partner’s abilities to recognize where and how the manipulation occurred.

## Instructions

1. Each partner should select a publicly available dataset. You can use the `data()` function to view a list of dataframes loaded with base R. Some packages contain additional example dataframes to explore.
2. In an .R script or rmd/qmd notebook *in your own assessment repo*, transform your selected dataframe however you like. You will not share this file with your partner. Use comments (.R files) and/or markdown text (.Rmd/.qmd notebooks) to explain the steps you’re taking.
    1. Your resulting dataset does not need to be clean, tidy, messy, long, wide, or anything else. It simply needs to be transformed. Write out your final dataset to a .csv file.
    2. Limit your transformation to functions tidyverse functions or other functions we have discussed in class. You may use functions from other packages if and only if you give your partner advance warning and the opportunity to decline.
3. Give the transformed .csv file you wrote to your partner (who will give their messy data to you). Don’t share the mess-making file!
    1. You may choose to provide your partner with a `read_csv()` function to use for importing your file. There are a few reasons why you may want to have control over exactly how the import process happens, but this is particularly useful if you want your columns to be read in as specific data types. Look at the structured data wrangling exercises for examples of this.
4. In an .R script or rmd/qmd notebook *in your own assessment repo*, recreate your partner’s transformed dataset. This is basically your own custom version of the structured leveled data wrangling/recreation mini-projects. You can copy that structure entirely if it’s helpful. Minimally:
    1. Begin with reading in the original dataset and the goal dataset from your partner’s .csv. (Use their `read_csv()` function if they provided you with one.)
    2. In comments or markdown, note what changes you notice and make a plan for how to recreate them.
    3. Make the changes! Get as close as you can to your partner’s transformed dataset, and note where you weren’t able to match it.
5. Discuss the process with your partner after you have both finished your recreations. Did you miss anything? Did you use use similar or different approaches for each aspect of wrangling? Is either the original or transformed dataset tidy? If not, was there a point in your transformation process where it was?

Your transformation can be in either a script or notebook. However you decide to approach the task, your mess-making should minimally: 1) load required libraries, 2) set a random seed, 3) use tidyverse functions to transform a publicly available dataset, and 4) write the transformed dataset to a .csv file to share with your group.

**Be sure to use frequent, informative comments and/or markdown text to explain decision-making and procedure for transforming both datasets.**

### Challenge suggestions

To make things trickier, show more skills, or earn engagement points, try one or more of the following:

1. Incorporate relatively more complex transformations that create puzzles for your partner. For example, if the original data has variables height and weight, you could add a BMI variable and then delete the height column. Your partner would need to recreate the height column by deriving values based on weight and BMI.
2. Use the `daff` package to keep track of your progress as you work. If you choose to do this, I recommend cleaning in a .qmd rather than a script so that you can include chunks for checkpoint comparisons. See the cleaning walkthrough and Level 1 cleaning mini-project for how to incorporate daff functions.
3. Keep swapping data. After you and your partner have matched either other’s wrangled dataset, keep going to transform it even more and swap back. See how many rounds you can go before things get irreproducible. 
4. With your partner’s permission, include functions from outside the tidyverse. Your scripts should still primarily use tidyverse functions and, ideally, perform the transformation in a single pipeline (even the non-tidyverse functions).
5. Work with multiple datasets to practice transforming with the “combining data” set of functions, in particular the `join` family of functions. You may need to search outside the preinstalled R datasets to find 2 datasets suitable for joining. You could also begin with one dataset, break it in two, transform each a little, then join them back together.

## Submission

1. Copy the `assessment.md` doc from your top-level directory into this project subdirectory.
2. Complete the assessment document.
    1. Thoroughly complete any sections about collaboration. It should be perfectly clear who you worked with and who contributed what.
    2. Complete the (usually optional, here required) reflection, discussing the experience of collaborating. You can additionally reflect on other aspects of the project if you like.
3. Your mini-project’s subdirectory should include (presented here in the order you will probably create each):
    1. this README
    2. an [assessment.md](http://assessment.md/) file completed by you and only you
    3. .csv of the original data *you* started with, if applicable (i.e., if it was not a built-in dataset)
    4. script or notebook that transforms that original data
    5. .csv of the transformed data you created
    6. .csv of the original data *your partner* started with, if applicable (i.e., if it was not a built-in dataset)
    7. .csv of the transformed data your partner gave you
    8. script or notebook that transforms data to match your partner’s
    9. .csv of the resulting, transformed dataset
4. On Canvas, go to the next available open mini-project assignment. In the text box, include:
    1. The name of the project: “Group make-a-mess”
    2. A link to the subdirectory for your mini-project
        1. If you completed the project in a separate repository, your `assessment.md` file in the subdirectory should explain how to access your work.
    3. Name of partner (or group members)
    4. Any additional info you'd like your grader to know
5. Submit the Canvas assignment. Your grader will access the directory you've linked to, grade your work on the assessment doc, push the doc back to your repository, and mark your Canvas assignment as complete.