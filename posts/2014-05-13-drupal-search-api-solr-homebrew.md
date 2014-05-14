---
title: Drupal, Search API and Apache Solr with Homebrew on OS X
location: Sydney, New South Wales
tags: howto, drupal, search, solr, homebrew
excerpt: 
  This is a short, no-frills guide to installing Apache Solr with homebrew on
  OS X, then using it with the Search API Drupal modules.
---

Installing [Apache Solr][solr] with [Homebrew][homebrew] on OS X and using it
with the [Search API][searchapi] modules for [Drupal][drupal] is pretty easy.
This article will run through the process and includes a few tweaks to make
things more convenient for those like me, who do this on different projects
fairly regularly.

[solr]: http://lucene.apache.org/solr/
[homebrew]: http://brew.sh/
[searchapi]: http://drupal.org/project/search_api
[drupal]: https://drupal.org/

# Installing Apache Solr

Apache Solr is a bit of a monolith (if you use it like most people I've seen):
you just download it and unpack it somewhere. Indeed, this is exactly what the
homebrew formula does. Run this command to get it installed:

````{.bash}
brew install solr
````

It also includes a handy shell script to start Solr using a configuration
directory you specify on the command line. Alas, it *doesn't* set a few
properties which would be nice to have available in our Solr configuration
files. But it *is* a very simple shell script, and we have text editors!

Edit the `solr` shell script brew installed (you can use `which solr` to find
it) and make it look like this:

````{.bash}
#!/bin/sh
DIST_PATH="/usr/local/Cellar/solr/4.7.2/libexec/dist"
CONTRIB_PATH="/usr/local/Cellar/solr/4.7.2/libexec/contrib"

if [ -z "$1" ]; then
  echo "Usage: $ solr path/to/config/dir"
else
  cd /usr/local/Cellar/solr/4.7.2/libexec/example && java -server $JAVA_OPTS \
    -Dsolr.solr.home=$1 \
    "-Dsolr.dist.dir=${DIST_PATH}" \
    "-Dsolr.contrib.dir=${CONTRIB_PATH}" \
    -jar start.jar
fi
````

(The `4.7.2` part will change depending on the version of Solr you have
installed; make sure it's correct.)

We've adding two new properties specify where to find the JAR files, etc. that
come in the Solr distribution. We'll use these when we're tweaking the Solr
configuration files that come with the Drupal modules.

# Installing Drupal

Now that Solr is installed, start a new Drupal site and install the Search API
modules. I'm a command line person, so I do something like this:

````{.bash}
PROJECT_NAME=searchdemo
PROJECT="${HOME}/Sites/${PROJECT_NAME}"
mkdir -p "${PROJECT}"
````

Start a new Drupal project, and download and install the Search API module and
all its friends:

````{.bash}
cd "${PROJECT}"
drush dl drupal --drupal-project-rename=htdocs
cd htdocs
mysqladmin create "${PROJECT_NAME}"
drush site-install --db-url=mysql://me:password@localhost/${PROJECT_NAME}
drush en search_api search_api_solr search_api_attachments facetapi
````

# Building a Solr core for Search API

Now lets create a new Solr core for Search API to use. We'll start with one of
the example configurations that come with Solr (watch out for the Solr version
number in this path):

````{.bash}
cp -a /usr/local/Cellar/solr/4.7.2/libexec/example/solr "${PROJECT}/solr"
````

This configuration includes a single Solr "core" called `collection1`. We'll
duplicate that, then drop the Search API Solr configuration on top of it.

````{.bash}
cd "${PROJECT}/solr/"
mkdir -p "${PROJECT_NAME}/"{conf,data}
cp -a "collection1/conf"/* "${PROJECT_NAME}/conf/"
cp -a ${PROJECT}/htdocs/sites/all/modules/search_api_solr/solr-conf/4.x/* "${PROJECT_NAME}/conf/"
````

Now to tweak it. First, give the new core it's own name so that Solr can
identify it correctly:

````{.bash}
echo "name=${PROJECT_NAME}" > "${PROJECT_NAME}/core.properties"
````

Then update the `solrconfig.xml` file that came with Search API Solr to use the
`solr.dist.dir` and `solr.contrib.dir` properties we added to the `solr` shell
script above. Basically replace any mention of `../../dist/` in your
`solrconfig.xml` file with `${solr.dist.dir:../../dist/}`. This says "use the
configured value for `solr.dist.dir` or, if there isn't one, `../../dist/`". It
should already be done for `solr.contrib.dir` but you might like to double
check, just in case.

If you want to use any optional/non-core features you'll need to update the
`<lib>` tags (these are the main places you'll need to make the `solr.dist.dir`
change) to use the correct JAR file names.


I'll be using the extraction functionality to index the content of PDF and
other files, so I needed to uncomment and correct the tag for
`solr-cell-\d.*\.jar`:

````{.xml}
<lib dir="${solr.dist.dir:../../../dist/}" regex="solr-cell-\d.*\.jar" />
````

# Running it

Now go configure your Drupal site to use your new Apache Solr instance. You
should configure Search API to communicate with Solr at
`http://searchapi.dev:8983/solr/${PROJECT_NAME}` (you should put the real value
here, not the variable name). Then enable an index, configure it, and build
some search functionality!

