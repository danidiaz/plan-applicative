# plan-applicative

A writer-like Applicative/Arrow for tracking progess and estimating resources.

## Motivation

I run scripts in my machine. Their logic is simple and predictable, even if the
steps are many and take long to complete. 

The following infuriating situations happen:

    - The script fails at minute 45 because of a syntax error.
    - The script fails at minute 45 because it requests some resource that
      isn't present (a port number to listen on, perhaps an external command
      that isn't installed) and its presence wansn't checked at script start
      time.
    - It is difficult to determine at minute 45  how far the computation has
      progressed, because there are no logs, or the logs are too detailed and
      each step generates similar-looking lines.

The first problem is solved by using a statically typed language or, for
dynamic languages, some kind of [static](https://pypi.python.org/pypi/pyflakes)
[analysis](https://github.com/bbatsov/rubocop) tool.

For the second problem, we need to have a summary of the resources that will be
used by the computation before running it. This can be done by hand, adding a
new check at the beginning of the script when we change something further down.
But it's easy to forget to do so, and the initial checks can become out of sync
with the main code. It would be nice if each step of the computation foresaw
its own resource needs and these accumulated automatically as we composed the
steps.

For the third problem, we need a channel that notifies you whenever a step of
the computation starts or finishes. Bonus points if composite steps are
allowed.

This library tries to alleviate the last two problems.


