---
title: Post banner images with Hakyll
tags: howto, hakyll, images, imagemagick, haskell
location: Sydney, New South Wales
excerpt: 
  This post describes a proof-of-concept Hakyll site which associates a banner
  image with each post, automatically generates several versions of it, and
  makes them available as variables for use in templates.
---

I've just created a new [hakyll-banner-images-demo][repo] repository on GitHub
which demonstrates how your [Hakyll][] site can:

1. Associate a banner image with each post.

2. Generate multiple versions (sizes) of these banner images.

3. Provide the generated images in variables for use in post templates.

[repo]: https://github.com/thsutton/hakyll-banner-images-demo
[Hakyll]: http://hackage.haskell.org/package/hakyll

Each post in the site is represented by a directory containing an `index.md`
Markdown file and, optionally, a `banner.png` image file. This repository
contains two posts, one with and one without an image:

    posts/2013-10-23-post-without-banner/index.md
    posts/2013-10-24-post-with-banner/banner.png
    posts/2013-10-24-post-with-banner/index.md

The [`site.hs`][site.hs] Haskell program contains only the most basic Hakyll
directives to process the Markdown files (indeed, I haven't even bothered to
override the date code; you'll need to put the date in the Markdown file
metadata). In addition, it contains the proof-of-concept code to handle the
banner images.

[site.hs]: https://github.com/thsutton/hakyll-banner-images-demo/blob/master/site.hs

The `imageProcessor` function is a helper to construct the Hakyll `Rules ()` to
process a set of banner images and the `Context a` allowing them to be used in
the associated posts and templates. This function takes a `Pattern` which
matches the banner images and a list describing the different versions to
generate of each image.

````{.haskell}
let (postImages, postImageField) = imageProcessor "posts/*/banner.png"
                                     [ ("small" , Just (200,100))
                                     , ("medium", Just (600,300))
                                     , ("full"  , Nothing)
                                     ]
````

The first argument to `imageProcessor` is the pattern identifying the image to
be processed. This pattern *must* end in a full filename. The second argument
is a list of versions to create. Each version has a name (the `String`) and
image processing instructions (`Nothing` to copy the image, `Just (x,y)` to
scale and crop the image to the given dimensions using ImageMagick's `convert`
command). This generates a `Rules ()` value to process the images (`postImages`
in the code) and a `Context a` to make them available in posts and templates.

The `Rules ()` value can be "run" just like any `create` or `match` statement
and the `Context a` value can be used in the context of post with a path that
matches the image pattern (ignoring the filename). This context defines one
variable for each of the image versions being generated. In this code, the
variables are:

- `banner-small` is a 200x100 image.
- `banner-medium` is a 600x300 image.
- `banner-full` is the original image.

These names are generated from the filename `banner.png` in the image pattern
(this is why the pattern must have a filename) and the name of each version.

With this configuration, the posts included in the repository result in the
following files being generated:

    posts/2013-10-23-post-without-banner/index.html
    posts/2013-10-24-post-with-banner/index.html
    posts/2013-10-24-post-with-banner/full-banner.png
    posts/2013-10-24-post-with-banner/medium-banner.png
    posts/2013-10-24-post-with-banner/small-banner.png

As expected, each post has an `index.html` file and the single banner image has
`small`, `medium`, and `full` versions.

The implementation of `imageField` and the assumption that the pattern ends in
a file name are pretty crappy but, overall, I'm quite happy with this approach.
