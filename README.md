# plan-applicative

A writer-like Applicative/Arrow for resource estimation and progress tracking.

## Motivation

I run scripts in my machine. Their logic is simple and predictable, even if the
steps are many and take long to complete. 

The following infuriating situations happen:

- The script fails at minute 45 because of a syntax error.
- The script fails at minute 45 because it requests a missing resource whose
  availability could have been checked when the script started.
- It is difficult to ascertain how far along the execution we are at minute 45.

The first problem is solved by using a statically typed language or, for
dynamic languages, some kind of [static](https://pypi.python.org/pypi/pyflakes)
[analysis](https://github.com/bbatsov/rubocop) tool.

For the second problem, we need to have a summary of the resources that the
computation will require, before running the computation itself. This can be
done by hand, adding a new check at the beginning of the script when we change
something further down. But it's easy to forget to do so, and the initial
checks can become out of sync with the main code. It would be nice if each step
of the computation foresaw its own resource needs and these accumulated
automatically as we composed the steps.

For the third problem, we need a channel that notifies you whenever a step of
the computation starts or finishes. Bonus points if nested steps are supported.

## Problems

Currently the *ApplicativeDo* extension doesn't work very well with this
package's *Applicative* because an extant bug in GHC:
[#10892](https://ghc.haskell.org/trac/ghc/ticket/10892). Sequencing actions
whose values are ignored gives an error.

## Inspiration

- [StaticArrow](http://hackage.haskell.org/package/arrows-0.4.4.1/docs/Control-Arrow-Transformer-Static.html)
  from the [arrows](http://hackage.haskell.org/package/arrows) package.

- Not exactly an inspiration (as I don't understand the stuff well enough) but
  Tomas Petricek's work on ["coeffects"](http://tomasp.net/coeffects/) seems
  relevant for helping applications to "fail early". See section 1.1 of his
  [thesis](http://tomasp.net/academic/theses/coeffects/thesis.pdf).

