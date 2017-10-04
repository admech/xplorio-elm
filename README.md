# Xplorio
Be awesome at modeling.

*WORK IN PROGRESS !*

------------------------------------------------------------------------------

## Workflow:

1. __Define__
   the `model`'s `inputs` (names and domains), `output`s (named 2D series) and the `calculation` (a function mapping a set of concrete values of `input`s to a `result`: a set of concrete 2D series for the `output`s))

2. __Select__
   for which input values you'd like to have `result`s: choose a subset of each `input`'s domain -- and Xplorio will sweep through all possible sets of `input`s' values in there. You can choose several such subsets.

3. __Explore__
   `result`s for different parameters: create charts of `output`'s values for the `input`s of interest.

The `result`s of the `calculation`s are persisted, so what you do after defining the model is: simulate for some set of `input`s, explore some charts from the `results`, order more `calculation`s as required, maybe update the `model` and start over, ultimately getting a full detailed understanding of your system's behavior.

__You are saved from all the hassle__ of:
* setting up a high-performance distributed computation environment,
* scheduling computations on many workers and retrieving results,
* managing storage of data points, charts and corresponding input parameters.

Now go get the models running!

------------------------------------------------------------------------------

## To do:

1. Define:
    1. ~~adding and removing `input`s,~~
    2. ~~defining an `input`'s domain (range or set),~~
    3. ~~adding and removing `output`s,~~
    4. defining the calculation:
         entering a github repo and showing how it will be launched,
    5. ~~defining a mapping from the generated CSV to `output` values.~~
2. Select:
    1. displaying `input` domains,
    2. selecting subsets of `input` domains,
    3. showing `wanted`, `in progress` and `available` subsets.
3. Explore:
    1. charting `output` values,
    2. including series into charts, based on selected `input` values.


