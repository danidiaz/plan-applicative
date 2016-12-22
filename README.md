# plan-applicative

A writer-like Applicative/Arrow for tracking progress and estimating resources.

## Motivation

I run scripts in my machine. Their logic is simple and predictable, even if the
steps are many and take long to complete. 

The following infuriating situations happen:

- The script fails at minute 45 because of a syntax error.
- The script fails at minute 45 because it requests a missing resource whose
  avaliablility could have been checked at script start.
- It is difficult to tell how far along we are at minute 45.

The first problem is solved by using a statically typed language or, for
dynamic languages, some kind of [static](https://pypi.python.org/pypi/pyflakes)
[analysis](https://github.com/bbatsov/rubocop) tool.

For the second problem, we need to have a summary of the resources the
computation will require, before running the computation. This can be done by
hand, adding a new check at the beginning of the script when we change
something further down. But it's easy to forget to do so, and the initial
checks can become out of sync with the main code. It would be nice if each step
of the computation foresaw its own resource needs and these accumulated
automatically as we composed the steps.

For the third problem, we need a channel that notifies you whenever a step of
the computation starts or finishes. Bonus points if nested steps are supported.

