# chatGPT

## Design of Experiments

### 1. Recognition of the problem

I'm interested in knowing which LLM model (ChatGPT or Gemini) performs better in doing certain tasks.

1. are both models good at simple tasks? for example extraction.
2. are both models good at difficult tasks? for example categorization, math problems.

### 2. Response variable and choice of factors
variable: number of true answers from the models.
factor: LLM models with 2 levels (ChatGPT, Gemini)
experimental and observational units: each pdf file.

### 3. single-factor experiment:

Randomization:

Consider there are 20 papers to be tested (20 replicates). so we randomize the level-replicate pair to create a completely randomized experiment.

Replication:
Each experiemental unit is measured once at each level.

### 4. Sampling method:
Equal allocation of stratified sampling. 

Why Balance Representation?
Avoiding Dominance by Large Groups:
In populations where one group is much larger than others (e.g., Science books dominating your bookshelf), a simple random sample might over-represent the larger group and under-represent smaller groups, leading to biased insights.
Ensuring Comparability:
If the goal is to compare characteristics or performance across categories (e.g., average rating of Science vs. Romance books), balanced representation allows for fair comparisons without one category overpowering the analysis.
Highlighting Minority Groups:
Smaller groups (e.g., Short Stories or Documentary books) might be overlooked if sampling is proportional to group size. Balanced representation ensures their voices are included.

### simple comparative experiment

### comparing 2 proportions: RR, OR...