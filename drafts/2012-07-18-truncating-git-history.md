---
title: Truncating git history
tags: git
location: Syndey, New South Wales
excerpt: 
  My blog is stored in a git repo with a long and storied history. Most of that
  history isn't *mine*, so I've truncated it. This is how.
---

git checkout --orphan temp e41d7f633c45c46bd42e97cecf93204191d9e4c9
git commit -m "Truncate history"
git rebase --onto temp e41d7f633c45c46bd42e97cecf93204191d9e4c9 hakyll
