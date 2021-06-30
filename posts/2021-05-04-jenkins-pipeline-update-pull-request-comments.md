---
title: Making GitHub pull request comments in a Jenkins pipeline
tags: howto, jenkins, ci
excerpt: |
  How to get a Jenkins multibranch pipeline build to post comments
  on your pull-requests.
---

Sometimes all you want from your continuous integration setup is one bit: red vs
green. In other circumstances, you might like more detailed feedback. For
configuration management scenarios I like to see a [summary of] the planned
changes as a comment on my pull request.

If you use the [Pipeline: GitHub](https://plugins.jenkins.io/pipeline-github/)
Jenkins plugin you can use code like this to:

1. Delete any previous comments made by your CI/CD pipeline on that PR.
2. Post a comment to the PR.

```
node {
    stage("Prepare") {
        checkout scm
    }

    stage("Build") {
        echo "..."
    }

    stage("Comment") {
        if (env.CHANGE_ID) {
            for (comment in pullRequest.comments) {
                /* Where "automation-user" is the scm account. */
                if (comment.user == "automation-user") {
                    pullRequest.deleteComment(comment.id)
                }
            }
            def date = sh(returnStdout: true, script: "date -u").trim()
            pullRequest.comment("Build ${env.BUILD_ID} ran at ${date}")
        }
    }
}
```
