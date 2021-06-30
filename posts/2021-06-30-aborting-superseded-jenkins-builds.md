---
title: Aborting supersede Jenkins builds
tags: howto, jenkins, ci
excerpt: |
  How to get Jenkins to abort running builds that are supersede by a later
  build.
---

Even small teams can find that their continuous integration servers get bogged
down with redundant builds. Some developers like to "commit early, commit often"
and if they also "open a PR early and leave it sitting there in draft mode" then
you can quite easily have multiple builds linting and building and testing code
that has already been supersede.

Happily, you can use the [Pipeline: Milestone Step][1] plugin to have Jenkins
terminate already running builds of the same job. The goal here is
straighforward:

[1]: https://plugins.jenkins.io/pipeline-milestone-step/

> My multibranch pipeline build is notified of PR-123. It creates a new job
> called PR-123 and starts build 1.
>
> I push another commit to the branch. GitHub notifies Jenkins and Jenkins
> starts PR-123 build 2. I now have two running builds -- build 1 and build 2
> -- but the outcome of build 1 is no longer useful.
>
> Suppose I notice a type and immediately push a fix. This would result in
> *three* running builds, *two* of them useless.

Milestones can be useful in a range of circumstances but I mostly want to take
advantage of one feature:

> When a build passes a milestone, any older build that passed the previous
> milestone but not this one is aborted.

If we use the `BUILD_ID` as the milestone, then we can use this to abort old
jobs when a new one starts.

```
node {
    stage("Prepare") {
        milestone label: '', ordinal: Integer.parseInt(env.BUILD_ID) - 1
        milestone label: '', ordinal: Integer.parseInt(env.BUILD_ID)
        checkout scm
    }
    stage("One") {
        sh """sleep 60"""
    }
    stage("Two") {
        sh """sleep 120"""
    }
    stage("Three"){
        sh """sleep 180"""
    }
}
```

Like everything involving Jenkins, there are bound to be heaps of interactions
with other features and scenarios where it doesn't work reliably. Good luck.
