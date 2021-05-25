---
title: Making GitHub pull request comments in a Jenkins pipeline
---

```
node {
    stage("Prepare") {
        checkout scm

        if (env.CHANGE_ID) {
            for (comment in pullRequest.comments) {
                if (comment.user == "automation-user") {
                    pullRequest.deleteComment(comment.id)
                }
            }
        }
    }

    stage("Build") {
        echo "..."
    }

    stage("Comment") {
        if (env.CHANGE_ID) {
            def date = sh(returnStdout: true, script: "date -u").trim()
            pullRequest.comment("Build ${env.BUILD_ID} ran at ${date}")
        }
    }
}
```
