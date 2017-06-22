# Contributing

First off, thank you for considering contributing to Verigraph. Please take a
moment to review this document in order to make the contribution process easy
and effective for everyone involved.

Following these guidelines helps to communicate that you respect the time of the
developers managing and developing this open source project. In return, they
should reciprocate that respect in addressing your issue, assessing changes, and
helping you finalize your pull requests.

There are many ways to contribute, from writing tutorials or wiki pages,
improving the documentation, submitting bug reports and feature requests or
writing code which can be incorporated into Verigraph itself.

As for everything else in the project, the contributions to Verigraph are
governed by our [Code of Conduct](CODE_OF_CONDUCT.md).

## 1. Where do I go from here?

If you've noticed a bug or have a question,
[search the issue tracker](https://github.com/Verites/verigraph/issues?q=something)
to see if someone else in the community has already created a ticket.
If not, go ahead a[make one](https://github.com/Verites/verigraph/issues/new).

## 2. Fork & create a branch

If this is something you think you can fix, then
[fork Verigraph](https://help.github.com/articles/fork-a-repo)
and create a branch with a descriptive name.

## 3. Get the test suite running

Make sure you have the latest `stack` installed, then you can execute thes tests
by running:

```sh
stack test
```

Also run a build after the tests to ensure that everything is being built
correctly:

```sh
stack build
```

## 4. Did you find a bug?

* **Ensure the bug was not already reported** by searching on GitHub under
  [Issues](https://github.com/Verites/verigraph/issues).

* If you're unable to find an open issue addressing the problem,
  [open a new one](https://github.com/Verites/verigraph/issues/new).
  Be sure to include a **title and clear description**, as much relevant
  information as possible,
  and a **code sample** or an **executable test case** demonstrating the expected
  behavior that is not occurring.

## 5. Implement your fix or feature

At this point, you're ready to make your changes. Feel free to ask for help;
everyone is a beginner at first :smile_cat:

## 6. Make a Pull Request

At this point, you should switch back to your master branch and make sure it's
up to date with Verigraph's master branch:

```sh
git remote add upstream git@github.com:verites/verigraph.git
git checkout master
git pull upstream master
```

Then update your feature branch from your local copy of master, and push it.

```sh
git checkout <your-branch>
git rebase master
git push --set-upstream origin <your-branch>
```

Finally, go to GitHub and
[make a Pull Request](https://help.github.com/articles/creating-a-pull-request)
:D

Travis CI will run our test suite. We care about quality, so your Pull Request
won't be merged until all tests pass.

## 7. Keeping your Pull Request updated

If a maintainer asks you to "rebase" your Pull Request, they're saying that a
lot of code has changed, and that you need to update your branch so it's easier
to merge.

To learn more about rebasing in Git, there are a lot of
[good](http://git-scm.com/book/en/Git-Branching-Rebasing)
[resources](https://help.github.com/articles/interactive-rebase),
but here's the suggested workflow:

```sh
git checkout <your-branch>
git pull --rebase upstream master
git push --force-with-lease <your-branch>
```

## 8. Merging a Pull Request (maintainers only)

A Pull Request can only be merged into master by a maintainer if:

* It is passing CI.
* It has no requested changes.
* It is up to date with current master.

Any maintainer is allowed to merge a Pull Request if all of these conditions are
met.
