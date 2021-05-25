---
title: Using jgit in-memory with local file system repositories
location: Sydney, New South Wales
tags: howto, java, git, jgit
excerpt:
  The slight tweak you need to avoid NullPointerException when using jgit
  InMemoryRepository to fetch from remotes on the local file system.
---

I've recently worked on a project that used [jgit][1] to access configuration
files in repositories hosted on a GitHub. For various reasons we're using
`jgit`'s in-memory support with code that looks a little bit like
`CloneRemoteRepositoryIntoMemoryAndReadFile.java` from [jgit-cookbook][2]:

```.java
DfsRepositoryDescription repoDesc = new DfsRepositoryDescription();
InMemoryRepository repo = new InMemoryRepository(repoDesc);
Git git = new Git(repo);
git.fetch()
        .setRemote(REMOTE_URL)
        .setRefSpecs(new RefSpec("+refs/heads/*:refs/heads/*"))
        .call();
repo.getObjectDatabase();
ObjectId lastCommitId = repo.resolve("refs/heads/" + BRANCH);
RevWalk revWalk = new RevWalk(repo);
RevCommit commit = revWalk.parseCommit(lastCommitId);
RevTree tree = commit.getTree();
TreeWalk treeWalk = new TreeWalk(repo);
treeWalk.addTree(tree);
treeWalk.setRecursive(true);
treeWalk.setFilter(PathFilter.create(FILE_TO_READ));
if (!treeWalk.next()) {
    return;
}
ObjectId objectId = treeWalk.getObjectId(0);
ObjectLoader loader = repo.open(objectId);
loader.copyTo(System.out);
```

This works more or less how you would expect when `REMOTE_URL` is genuinely
remote (e.g. `https://` or `user@host:path` or similar) but results in
`NullPointerExceptions` with a local repository (e.g. `file://`, `/path`,
etc.) We are using local repositories in our integration tests (so that we
don't need to add yet more fragile, uninteresting network service mocking).

The problem is that `Repository` subclasses use an instance of `FS` to access
the local file system when required but `InMemoryRepository`, for superficially
understandable reasons, leaves that field `null`. If you want your
`InMemoryRepository` to be able to operate with on-disk remotes, you need to
supply that `FS` instance during construction:

```.java
DfsRepositoryDescription repoDesc = new DfsRepositoryDescription();
InMemoryRepository repo = new InMemoryRepository.Builder()
  .setRepositoryDescription(repoDesc)
  .setFS(FS.detect())
  .build();
Git git = new Git(repo);
```

Then any subsequent operations on those `Repository` or `Git` objects that
that need to access the local file system will be able to do so (even though
the repository itself is in-memory).

[1]: https://github.com/eclipse/jgit/
[2]: https://github.com/centic9/jgit-cookbook/
